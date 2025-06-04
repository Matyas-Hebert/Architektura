# Simple Instruction Processor

---

## Overview

This VHDL design implements a **finite state machine (FSM)** that executes a small custom instruction set.

It fetches the memory, decode the instructions and perform the operations.

The FSM supports these instructions: `@`, `+`, `-`, `[`, `]`, `$`, and `!` to manipulate memory and control flow.

---

## Features

- **Synchronous Reset and Clocking**  
- Memory access control with read/write enable signals  
- Increment/decrement of program counter (PC) and pointer (PTR) registers  
- Input/output handling with handshaking signals (`IN_REQ`, `IN_VLD`, `OUT_BUSY`)  
- Loop control via bracket instructions (`[` and `]`)  
- Halt state to stop execution cleanly  

---

## Usage

1. Connect the clock (`CLK`), reset (`RESET`), and enable (`EN`) signals.  
2. Wire memory interface signals (`DATA_RDATA`, `DATA_EN`, `DATA_RDWR`, etc.) to your RAM or ROM modules.  
3. Connect input/output ports (`IN_DATA`, `IN_VLD`, `IN_REQ`, `OUT_DATA`, `OUT_BUSY`, `OUT_WE`, `OUT_INV`) to peripherals.  
4. Assert `EN` and release `RESET` to start program execution.  
5. Monitor the `DONE` signal to detect program completion.

---

## Instruction Set

| Instruction | ASCII Code | Description                          |
|-------------|------------|------------------------------------|
| `@`         | `0x40`     | **Halt** - stops the processor      |
| `+`         | `0x2B`     | Increment memory at PTR             |
| `-`         | `0x2D`     | Decrement memory at PTR             |
| `.`         | `0x2E`     | Output memory at PTR                |
| `,`         | `0x2C`     | Input to memory at PTR              |
| `[`         | `0x5B`     | Loop start - jump forward if zero  |
| `]`         | `0x5D`     | Loop end - jump backward if non-zero |
| `$`         | `0x24`     | Load input into TMP register        |
| `!`         | `0x21`     | Store TMP register to memory        |
