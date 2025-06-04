-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2024 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Matyáš Hebert <xheberm00>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy SIGNAL
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru

   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti

   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data

   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_INV  : out std_logic;                      -- pozadavek na aktivaci inverzniho zobrazeni (1)
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove SIGNALy
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is
        -- registr CNT a signaly
        SIGNAL en_CNT_inc: STD_LOGIC_VECTOR(1 downto 0) := (OTHERS => '0'); -- "01" = increment, "10" = decrement
        SIGNAL CNT_reg_init: STD_LOGIC;
        SIGNAL CNT_reg: STD_LOGIC_VECTOR(7 downto 0);

        -- registr TMP a signal load
        SIGNAL en_TMP_reg_load: STD_LOGIC;
        SIGNAL TMP_reg: STD_LOGIC_VECTOR(7 downto 0);

        -- register PTR a signal inc
        SIGNAL en_PTR_inc: STD_LOGIC_VECTOR(1 downto 0) := (OTHERS => '0'); -- "01" = increment, "10" = decrement
        SIGNAL PTR_reg: STD_LOGIC_VECTOR(12 downto 0) := (OTHERS => '0');

        -- register PC a signal inc
        SIGNAL en_PC_inc: STD_LOGIC_VECTOR(1 downto 0) := (OTHERS => '0'); -- "01" = increment, "10" = decrement
        SIGNAL PC_reg: STD_LOGIC_VECTOR(12 downto 0) := (OTHERS => '0');

        -- Multiplexory
        SIGNAL address_mul_slc: STD_LOGIC;
        SIGNAL data_mul_slc: STD_LOGIC_VECTOR(1 downto 0);

        -- Stavy konecneho automatu
        type fsm_state is (JMP_STATE, LEFT_PARENTHESIS_STATE, RIGHT_PARENTHESIS_STATE, LEFT_PARENTHESIS_WAIT_STATE, RIGHT_PARENTHESIS_WAIT_STATE, INIT_STATE, LOAD_STATE, READ_AT_STATE, FETCH_INSTR_STATE, DECODE_STATE, TMP_REG_LOAD_STATE, TMP_REG_STORE_STATE, PTR_REG_INC_STATE, PTR_REG_DEC_STATE, MEM_INC_STATE, MEM_DEC_STATE, INPUT_1_STATE, INPUT_2_STATE, OUTPUT_WAIT_STATE, PRINT_CHAR_STATE, HALT_STATE);
        -- Aktualni a nasledujici stav, pocatecnim stavem je inicializace
	SIGNAL current_state: fsm_state := INIT_STATE;
        SIGNAL next_state: fsm_state;

	-- Signál skoku '1' = vpred (forward), '2' = vzad (backward)
        SIGNAL jmp_dir: STD_LOGIC;

BEGIN
        -- registr CNT proces
        PROCESS (CLK, RESET) is
        BEGIN
                IF RESET = '1' THEN
			-- v pripade reset signalu vynulovani registru
                        CNT_reg <= (OTHERS => '0');
                ELSIF CLK = '1' and CLK'event THEN -- pro nabeznou hranu hodin
                        IF en_CNT_inc = "01" THEN
				-- zvyseni hodnoty registru CNT o 1
                                CNT_reg <= CNT_reg + 1;
                        ELSIF en_CNT_inc = "10" THEN
				-- znizeni hodnoty registru CNT o 1
                                CNT_reg <= CNT_reg - 1;
                        ELSIF CNT_reg_init = '1' THEN
				-- inicializace registru CNT na hodnotu 1
                                CNT_reg <= (OTHERS => '0');
                                CNT_reg <= CNT_reg + 1;
                        END IF;
                END IF;
        END PROCESS;

        -- registr TMP proces
        PROCESS (CLK, RESET) is
        BEGIN
                IF RESET = '1' THEN
			-- vynulovani registru TMP v pripade reset
                        TMP_reg <= (OTHERS => '0');
                ELSIF CLK = '1' and CLK'event and en_TMP_reg_load = '1' THEN
			-- nacteni dat do registru TMP pri nabezne hrane hodin
                        TMP_reg <= DATA_RDATA;
                END IF;
        END PROCESS;

        -- register PTR proces
        PROCESS (CLK, RESET) is
        BEGIN
                IF RESET='1' THEN
			-- vynulovani registru PTR v pripade reset
                        PTR_reg <= (OTHERS => '0');
                ELSIF CLK = '1' and CLK'event THEN -- v pripade nabezne hrany hodin
                        IF en_PTR_inc="01" THEN
                                PTR_reg <= PTR_reg + 1; -- zvyseni hodnoty registru PTR o 1
                        ELSIF en_PTR_inc="10" THEN
                                PTR_reg <= PTR_reg - 1; -- znizeni hodnoty registru PTR o 1
                        END IF;
                END IF;
        END PROCESS;

        -- registr PC proces
        PROCESS (CLK, RESET) is
        BEGIN
                IF RESET='1' THEN
			-- vynulovani registru PC v pripade reset
                        PC_reg <= (OTHERS => '0');
                ELSIF CLK = '1' and CLK'event THEN -- v pripade nabezne hrany hodin
                        IF en_PC_inc="01" THEN
				-- zvyseni hodnoty registru o 1
                                PC_reg <= PC_reg + 1;
                                IF PC_reg="1111111111111" THEN -- preteceni registru
                                        PC_reg <= (OTHERS => '0');
                        	END IF;
                        ELSIF en_PC_inc="10" THEN
				-- znizeni hodnoty registru o 1
                                PC_reg <= PC_reg - 1;
                                IF PC_reg=0 THEN -- podteceni registru
                                        PC_reg <= (OTHERS => '1');
                                END IF;
                        END IF;
                END IF;
        END PROCESS;

        -- multiplexor 1
        PROCESS (PTR_reg, PC_reg, address_mul_slc) is
        BEGIN
		-- volime zda DATA_ADDR (adresa pro zapis a cteni) bude nastavena na registr PTR ci PCdle signalu address_mul_slc
                IF address_mul_slc = '0' THEN
                        DATA_ADDR <= PTR_reg;
                ELSIF address_mul_slc = '1' THEN
                        DATA_ADDR <= PC_reg;
                END IF;
        END PROCESS;

        -- multiplexor 2
        PROCESS (IN_DATA, TMP_reg, DATA_RDATA, data_mul_slc) is
        BEGIN
		-- volime odkud se budou zapisovat data zda ze vstupu, registru TMP ci ctenou hodnotu z pameti
                IF data_mul_slc = "00" THEN
                        DATA_WDATA <= IN_DATA;
                ELSIF data_mul_slc = "01" THEN
                        DATA_WDATA <= DATA_RDATA + 1;
                ELSIF data_mul_slc = "10" THEN
                        DATA_WDATA <= DATA_RDATA - 1;
                ELSIF data_mul_slc = "11" THEN
                        DATA_WDATA <= TMP_reg;
                END IF;
        END PROCESS;

        -- stavovy registr
        PROCESS (CLK, RESET) is
        BEGIN
                IF RESET='1' THEN
			-- nastaveni pocatecniho stavu v pripade reset
                        current_state <= INIT_STATE;
                ELSIF CLK = '1' and CLK'event and EN = '1' THEN
			-- nastaveni nasledujici stavu na aktualni stav v pripade enable
                        current_state <= next_state;
                END IF;
        END PROCESS;

        -- logika stavu (logika konecneho automatu)
        PROCESS (current_state, EN, IN_DATA, DATA_RDATA, OUT_BUSY) is
        BEGIN
                -- inicializace signalu
                en_PC_inc <= "00"; -- inkrementace a dekrementace registru PC
                en_PTR_inc <= "00"; -- inkrementace a dekrementace registru PTR
                DATA_EN <= '0'; -- pametovy pristup
                DATA_RDWR <= '0'; -- rezim cteni/psani
                address_mul_slc <= '0';
                data_mul_slc <= "00";
                en_TMP_reg_load <= '0'; -- deaktivace nacitani registru TMP
                IN_REQ <= '0'; -- deaktivace pozadavku na vstupni data
                OUT_WE <= '0'; -- deaktivace zapisu na vystup
                OUT_DATA <= (OTHERS => '0'); -- vychozi data
                OUT_INV <= '1'; -- indikace neplatneho (invalid) vystupu
                en_CNT_inc <= "00"; -- inkrementace a dekrementace registru CNT
                CNT_reg_init <= '0'; -- inicializacni signal registru CNT

                case current_state is
                        -- pocatecni stav inicializace
                        WHEN INIT_STATE =>
				-- neni pripraveno na provedeni instrukce a program neskoncil
                                READY <= '0';
                                DONE <= '0';
                                IF EN='1' THEN
					-- pokud muzeme nacitad data prechazime do stavu LOAD
                                        next_state <= LOAD_STATE;
                                ELSE
					-- pokud nacitani dat neni povoleno zustavame ve stavu INIT
                                        next_state <= INIT_STATE;
                                END IF;

                        -- stav cteni oddelavace @
                        WHEN READ_AT_STATE =>
				-- inkrementace registru PTR
                                en_PTR_inc <= "01";
                                IF DATA_RDATA=X"40" THEN
					-- oddelovac nalezen jsme pripraveni provest instukci
                                        READY <= '1';
                                        next_state <= FETCH_INSTR_STATE;
                                ELSE
					-- opakujeme nacitani
                                        next_state <= LOAD_STATE;
                                END IF;

                        -- stav nacitani
                        WHEN LOAD_STATE =>
                                DATA_RDWR <= '1'; -- rezim pro zapis
                                DATA_EN <= '1'; -- povoleni pristupu k pameti
                                address_mul_slc <= '0'; -- do pameti pristupujeme zkrz adresu v PTR
                                next_state <= READ_AT_STATE; -- prechod do stavu cteni oddelovace

                        -- stav nacteni instrukce pameti (pote co byl nalezen @)
                        WHEN FETCH_INSTR_STATE =>
				DATA_RDWR <= '1'; -- rezim pro zapis
				DATA_EN <= '1'; -- povoleni pristupu k pameti
                                address_mul_slc <= '1'; -- do pameti pristupujeme skrz adresu v PC
                                next_state <= DECODE_STATE; -- prechazime k dekodovani instrukce


			-- stav dekodovani instrukce
                        WHEN DECODE_STATE =>
                                IF DATA_RDATA = X"21" THEN -- !
                                        next_state <= TMP_REG_STORE_STATE; -- ulozeni hodnoty TMP do pameti
                                ELSIF DATA_RDATA = X"24" THEN -- $
                                        DATA_RDWR <= '1';
                                        DATA_EN <= '1';
                                        address_mul_slc <= '0';
                                        next_state <= TMP_REG_LOAD_STATE; -- nacteni vstupu do pameti
                                ELSIF DATA_RDATA = X"2B" THEN -- +
                                        DATA_RDWR <= '1'; -- rezim pro zapis
                                        DATA_EN <= '1'; -- povoleni pristupu k pameti
                                        address_mul_slc <= '0'; -- do pameti pristupujeme skrz adresu v PTR
                                        next_state <= MEM_INC_STATE; -- inkrementace pametoveho mista
                                ELSIF DATA_RDATA = X"2C" THEN -- ,
                                        next_state <= INPUT_1_STATE;
                                ELSIF DATA_RDATA = X"2D" THEN -- -
                                        DATA_RDWR <= '1'; -- rezim pro zapis
                                        DATA_EN <= '1'; -- povoleni pristupu do pameti
                                        address_mul_slc <= '0'; -- do pameti pristupujeme skrz adresu v PTR
                                        next_state <= MEM_DEC_STATE; -- dekrementace pametoveho mista
                                ELSIF DATA_RDATA = X"2E" THEN -- .
                                        next_state <= OUTPUT_WAIT_STATE; -- presun do stavu output_wait
                                ELSIF DATA_RDATA = X"3C" THEN -- <
                                        next_state <= PTR_REG_DEC_STATE; -- dekrementace ukazatele do pameti
                                ELSIF DATA_RDATA = X"3E" THEN -- >
                                        next_state <= PTR_REG_INC_STATE; -- inkrementace registry PTR
                                ELSIF DATA_RDATA = X"40" THEN -- @
                                        next_state <= HALT_STATE; -- dosahli jsme konce kodove casti pameti
                                ELSIF DATA_RDATA = X"5B" THEN -- [
                                        DATA_RDWR <= '1'; -- rezim pro zapis
                                        DATA_EN <= '1'; -- povoleni pristupu do pameti
                                        address_mul_slc <= '0';
                                        next_state <= LEFT_PARENTHESIS_STATE; -- prechazime do stavu pro levou zavorku (pocatek cyklu)
                                ELSIF DATA_RDATA = X"5D" THEN -- ]
                                        DATA_RDWR <= '1';
                                        DATA_EN <= '1';
                                        address_mul_slc <= '0';
                                        next_state <= RIGHT_PARENTHESIS_STATE; -- prechazime do stavu pro pravou zavorku (ukonceni cyklu)
                                ELSE
                                        en_PC_inc <= "01"; -- neznama instrukce prechazime na dalsi
                                        next_state <= FETCH_INSTR_STATE; -- nacitame dalsi instrukci ve stavy FETCH
                                END IF;

                        -- inkrementace registru PTR
                        WHEN PTR_REG_INC_STATE =>
                                en_PC_inc <= "01"; -- inkrementace PC pro nacteni dalsi instrukce
                                en_PTR_inc <= "01";
				next_state <= FETCH_INSTR_STATE; -- navrat do stavu pro nacteni instrukce

                        -- dekrementace registru PTR
                        WHEN PTR_REG_DEC_STATE =>
                                en_PTR_inc <= "10";
                                en_PC_inc <= "01"; -- inkrementace PC pro nacteni dalsi instrukce
                                next_state <= FETCH_INSTR_STATE; -- navrat do stavu pro nacteni instrukce

                        -- inkrementace hodnoty v pameti (+)
                        WHEN MEM_INC_STATE =>
                                address_mul_slc <= '0'; -- vyber PTR jako adresy pro pamet
                                data_mul_slc <= "01"; -- zvoleni zdroje dat pro inkrementaci
                                DATA_EN <= '1';
                                DATA_RDWR <= '0';
                                en_PC_inc <= "01";
                                next_state <= FETCH_INSTR_STATE; -- nacteni dalsi instrukce

                        -- dekrementace hodnoty v pameti (-)
                        WHEN MEM_DEC_STATE =>
                                address_mul_slc <= '0';
                                data_mul_slc <= "10";
                                DATA_EN <= '1'; -- povoleni pristupu do pameti
                                DATA_RDWR <= '0'; -- rezim zapis
                                en_PC_inc <= "01"; -- inkrementace PC pro nacteni dalsi instrukce
                                next_state <= FETCH_INSTR_STATE;

                        -- nacteni hodnoty do registru TMP
                        WHEN TMP_REG_LOAD_STATE =>
                                en_TMP_reg_load <= '1'; -- povoleni nacteni dat do TMP
                                en_PC_inc <= "01";
                                next_state <= FETCH_INSTR_STATE; -- nacteni dalsi instrukce

                        -- ulozeni hodnoty TMP do pameti
                        WHEN TMP_REG_STORE_STATE =>
                                address_mul_slc <= '0';
                                data_mul_slc <= "11"; -- zvoleni TMP jako zdroj dat pro zapis
                                DATA_EN <= '1'; -- povoleni pristupu do pameti
                                DATA_RDWR <= '0'; -- rezim zapis
                                en_PC_inc <= "01";
                                next_state <= FETCH_INSTR_STATE;

                        -- vypis hodnoty z pameti na vystup
                        WHEN OUTPUT_WAIT_STATE =>
                                IF OUT_BUSY = '1' THEN
                                        next_state <= OUTPUT_WAIT_STATE; -- cekani na uvolneni vystupu
                                ELSIF OUT_BUSY = '0' THEN -- vystup je volny
                                        DATA_RDWR <= '1';
                                	DATA_EN <= '1';
                                	address_mul_slc <= '0'; -- cteme z PTR
                               		IF DATA_RDATA = X"24" THEN -- $
                                        	next_state <= TMP_REG_LOAD_STATE;
                                	ELSIF DATA_RDATA = X"2B" THEN -- +
                                        	next_state <= MEM_INC_STATE;
                                	ELSIF DATA_RDATA = X"2D" THEN -- -
                                        	next_state <= MEM_DEC_STATE;
                                	ELSIF DATA_RDATA = X"2E" THEN -- .
                                        	next_state <= PRINT_CHAR_STATE;
                                	ELSIF DATA_RDATA = X"5B" THEN -- [
                                        	next_state <= LEFT_PARENTHESIS_STATE;
                                	ELSIF DATA_RDATA = X"5D" THEN -- ]
                                        	next_state <= RIGHT_PARENTHESIS_STATE;
                                	END IF;
                        	END IF;

			-- vypis znaku z pameti
                        WHEN PRINT_CHAR_STATE =>
                                OUT_WE <= '1'; -- povoleni zapisu na vystup
                                OUT_INV <= '0'; -- nastaveni vystupu jako platneho
                                OUT_DATA <= DATA_RDATA; -- vystup hodnoty z pameti
                                en_PC_inc <= "01";
                                next_state <= FETCH_INSTR_STATE; -- nacteni dalsi instrukce

                        -- nacteni vstupu
                        WHEN INPUT_1_STATE =>
                                IN_REQ <= '1'; -- zadost o vstupni data
                                IF IN_VLD = '0' THEN
                                        next_state <= INPUT_1_STATE; -- cekani na dostupnost vstupnich dat
                                ELSIF IN_VLD = '1' THEN
                                        next_state <= INPUT_2_STATE; -- prechod do nacteni vstupu
                                END IF;

			-- zapis vstupu do pameti
                        WHEN INPUT_2_STATE =>
                                address_mul_slc <= '0';
                                data_mul_slc <= "00";
                                DATA_EN <= '1';
                                DATA_RDWR <= '0'; -- rezim zapisu
                                en_PC_inc <= "01";
                                next_state <= FETCH_INSTR_STATE; -- nacteni dalsi instukce

			-- zpracovani leve zavorky
                        WHEN LEFT_PARENTHESIS_STATE =>
                                en_PC_inc <= "01";
                                IF DATA_RDATA = "00000000" THEN
                                        CNT_reg_init <= '1'; -- inicializace CNT pro sledovani zavorek
                                        jmp_dir <= '1'; -- skok smer vpred
                                        next_state <= JMP_STATE; -- prechod do stavu skoku
                                ELSE
                                        next_state <= FETCH_INSTR_STATE; -- nacteni dalsi instrukce
                                END IF;

			-- zpracovani prave zavorky
                        WHEN RIGHT_PARENTHESIS_STATE =>
                                IF DATA_RDATA = "00000000" THEN
                                        en_PC_inc <= "01";
                                        next_state <= FETCH_INSTR_STATE; -- cyklus skoncil pokracujemedale v programy
                                ELSE
                                        CNT_reg_init <= '1';
                                        en_PC_inc <= "10";
                                        jmp_dir <= '0';
                                        next_state <= JMP_STATE; -- skaceme vlevo na nejblizsi [
                                END IF;

			-- cekani na pravou zavorku pri podminenem skoku
                        WHEN LEFT_PARENTHESIS_WAIT_STATE =>
                                IF CNT_reg = "00000000" THEN
                                        en_PC_inc <= "01"; -- inkrementace PC
                                        next_state <= FETCH_INSTR_STATE; -- nacteni dalsi instrukce
                                ELSE
                                        en_PC_inc <= "10";
                                        jmp_dir <= '0';
                                        next_state <= JMP_STATE; -- skaceme vlevo
                                        IF DATA_RDATA = X"5B" THEN -- [
                                                en_CNT_inc <= "10"; -- dekrementace CNT
                                        ELSIF DATA_RDATA = X"5D" THEN -- ]
                                                en_CNT_inc <= "01"; -- inkrementace CNT
                                        END IF;
                                END IF;

			-- cekani na levou zavorku pri podminenem skoku
                        WHEN RIGHT_PARENTHESIS_WAIT_STATE =>
                                en_PC_inc <= "01";
                                IF CNT_reg = "00000000" THEN
                                        next_state <= FETCH_INSTR_STATE; -- nacteni dalsi instrukce
                                ELSE
                                        jmp_dir <= '1';
                                        next_state <= JMP_STATE; -- skaceme v pravo
                                        IF DATA_RDATA = X"5B" THEN -- [
                                                en_CNT_inc <= "01"; -- inkrementace CNT
                                        ELSIF DATA_RDATA = X"5D" THEN -- ]
                                                en_CNT_inc <= "10"; -- dekrementace CNT
                                        END IF;
                                END IF;

			-- stav skoku
                        WHEN JMP_STATE =>
                                DATA_EN <= '1';
                                DATA_RDWR <= '1';
                                address_mul_slc <= '1';
                                IF jmp_dir = '0' THEN
					-- prejdeme do stavu cekani na levou zavorku v pripade skoku vlevo
                                        next_state <= LEFT_PARENTHESIS_WAIT_STATE;
                                ELSE
					-- prejceme do stavu cekani na pravou zavorku v pripade skoku vpravo
                                        next_state <= RIGHT_PARENTHESIS_WAIT_STATE;
                                END IF;

                        -- stav konce programu
                        WHEN HALT_STATE =>
                                IF RESET='1' THEN
					-- v pripade resetu pokracujeme v pocatecnim stavu
                                        next_state <= INIT_STATE;
                                ELSE
					-- zustavame v konecnem stavu
                                        next_state <= HALT_STATE;
                                END IF;
                                DONE <= '1';    -- program ukoncen
                END CASE;
        END PROCESS;
END BEHAVIORAL;
