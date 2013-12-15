FlashForth readme file
---------------------------

FlashForth is licensed acording to the GNU General Public License

Look in the user guide http://flashforth.sourceforge.net, 
and in the word list words.txt for further information


REVISION HISTORY
----------------

FlashForth V3.9
----------------
- The configuration file p18f-main.cfg syntax changed
- HI prints the highest available address for each memory type
- <> returns well formed flag
- Quad and triple precision math words in qmath.txt
- .FREE prints the available free memory of each type.
- Default return stack save area increased to 31 cells.
- ?ABORT logic inverted due to alignement with standard forth.
- Error messages ALREADY DEFINED and COMPILE ONLY.
- Write to unimplemented flash prevented. This prevents USB PICs from hanging.
- Print out restart reason when FF starts. See datasheet for STKPTR and RCON for details.
- Support for PIC18(L)F2X/4XK22 devices
- MPLABX project files.
- Smaller return stack usage.

FlashForth V3.8
---------------
- Double number support
- The USB version can also run with USB disconnected.
- Unbuffered UART TX option
- Configuration of buffer sizes from configuration file
- 18F14K50, 18F2553, 18F2458, 18F4458, 18F4553 USB serial emulation.
- Configuration of turnkey delay from configuration file
- CPU idle mode power save. IDLE BUSY words.
- CPU load measurement. LOAD fetches the load of the previous 256 ms period.
- Load indicator LED option
- 2CON, 2VARIABLE

FlashForth V3.7
---------------
- Use FF via USB serial emulation on PIC18F2455 family.
  USB code distributed as a library.
- FT0 FT1 FL0 FL1 renamed to U1+ U1- FL+ FL-
- UKEY UKEY? UEMIT renamed to 'KEY 'KEY? 'EMIT
- COLD replaced by EMPTY and WARM
- SAFE_CHECK replaced by write protection of kernel flash area.
- USB TURNKEY delay is 8 seconds. 2 seconds with UART.
- Some internal dictionary words hidden
- Minor optimisations.
- FF can be programmed via a bootloader.
- Possibility to link to C-libraries
- ACCEPT KEY EMIT KEY? can be used also in background tasks.
  In that case revectoring of I/O words in the task is required
  Compilation and writing to flash and eeprom is only
  possible from the operator task.

FlashForth V3.61
----------------
- Corrected short/long branch decision bug in CF,

FlashForth V3.6
---------------
- Corrected CRLF handling in ACCEPT
  CRLF, CR and LF are accepted as end of line. 
- Binary communication option in TX1 and RX1
- FL0 FL1 Memory write protection
- FT0 FT1 Disable flow control on TX1/RX1

FlashForth V3.5
---------------
- Corrected divison words.
- ,"
- HOLD area per task at the end of TIB
  Last 10 bytes of TIB is reserved for HOLD. See QUIT.
- Flow control selectable at compile time.

FlashForth V3.4
---------------
This version is closer to V4.x
- Register allocation changed to allow more words to be used in interrupts.
  M+ U/MOD U/ / UM* * can be used in interrupts , but may be cause too long interrupt
  execution times, causing for example UART overruns.
- TICKS and MS has been made interrupt safe. Earlier erratic delays could occur.
- XOFF is not received by FF anymore. 
  Earlier the terminal could hang, due to XOFF being echoed by FF.
  FF does not react on XOFF, it is assumed that the PC host can handle all input.
- Ctrl-o makes a warm start. Practical for interrupting endless loops.
- WARM now zeroes also the OPERATOR task area, thus avoiding some cyclic crash scenarios.
- EMIT no longer filters away control characters.
  This allows for possible VTxxx support.
- Vectored EMIT, EMIT?, KEY, KEY?
- User vectors UEMIT UEMIT? UKEY UKEY?
- Optimised UART routines
- TX1, RX1, RX1?
- Interrupt RX buffer is reduced to 64 bytes.
- Interrupt TX buffer is reduced to 32 bytes.
- FSR2 taken into use. It is called Aptr... in the code.
- Better handling of CR/LF combinations in ACCEPT
- Compile only flag and check.
- Added TASK: and TASKS.
- Maximum wordname length is now 15 characters.
  The compile only flag uses a bit.
- Tail call optimisation. The last call/rcall and return
  is changed to a goto. Looping to the current word beeing
  defined is also possible.
- Optimisations to fit all the new functionality into $2000 bytes. 
- P pointer is saved and restored by PAUSE.
- .ID added.
- CFA>NFA renamed to C>N
- NFA>CFA renamed to N>C
- NFA>LFA removed. 2- can be used instead.
- [I and I] added
- BTFSC, BTFSS, BCF, BSF, ANDLW, MOVF, added to the core dictionary
- Added >PR
- Added a help system
- Added 8BLINK
- I2C words updated 
- I2C words for ds1307, 24aa1025, tcn75

FlashForth V3.3
---------------
- SKIP corrected so that TAB is handled as a matching char.
- WARM corrected so that FF compiles also with GPASM
- Line Feed is now ignored (but echoed) by ACCEPT.
  So random linefeeds will not confuse FF.

FlashForth V3.2
---------------
- User interrupt vector added.
  The interrupt routine can be written in assembly or in Forth.
  IRQ DI EI ;I 
- Added DEFER IS VALUE TO
- Inlining of code words. 
  INLINE INLINED IN? IN,
- Faster compilation. Dictionary pointers are updated in RAM instead of EEPROM
  during compilation state.
- POSTPONE corrected to use LITERAL instead of LIT.
- CON for defining constants as inline literals.
- RDROP replaces UNNEXT
- POSTPONE IMMEDIATE are in the core flash dictionary.
- PROMPT deferred vector.
- SP was wrongly initialised in WARM before the TURNKEY word is
  executed. This could cause a crash in the TURNKEY execution
  at startup. The SP is now correctly initialised. 
- User Guide version 0.1 . Its a text file.
- CASE OF ENDOF ENDCASE
- Timer Interrupt based servo control words.
 
FlashForth V3.1
---------------
- CMOVE fixed. Uses !p>r instead of !p
- 2/ changed from unsigned to signed divide
- 0= and 0< optimised
- LIT removed from the visible dictionary.

FlashForth V3.0
---------------
- DO LOOP +LOOP I UNLOOP removed
  Replaced by FOR NEXT R@ UNNEXT and the pointer words
- A pointer register called P and associated words has been introduced.
- Assembler rewritten. It has now structured conditionals.
- IHERE and I, IC, exposed to user.
- ROM changed to FLASH
- FLASH RAM EEPROM stores the current data space context.
  It can be restored by the word >CS.
- The compiler generates some optimised code
  - Inline literals
  - Inline ZEROSENSE
  - Inline FOR NEXT
  - DUP and 0= before IF UNTIL WHILE are optimised away
- NONAME: changed to :NONAME
- U.2 U.4 replaced by U.R
- Assembler structured conditionals are in the core and are used to implement
  the Forth conditionals.
- @EX introduced
- SEE displays unknown code as numbers only.
- Input numbers can be prefixed by # $ % for decimal hex or binary conversion
  independently of BASE. 

FlashForth V2.1
---------------
- 16 bit divide added. 32/16 bit consumes 796 cycles, 16/16 bit 298 cycles.
- Numeric conversion uses 16-bit division, earlier 32 bit division was used.
- 16 bit words / u/ u/mod are now using 16 bit u/mod.
- 32 bit division and multiplication removed from kernel.
  Only um* and um/mod is in left in the kernel.
  These can be compiled from math.fth based on um* and um/mod.
- XLOOP exits the current word from within a DO LOOP.
- LEAVE terminates a DO LOOP when it encounters LOOP the next time.
  It does not leave immediately.
- Small optimisations. N= verify_imem IC@ 
- RUN END optimised for space, SINGLE added to end all tasks except the
  operator task.
- CT and JT optimised ( or at least changed).
- TICKS and MS stepping can be configured to TMR1 or TMR2 or TMR3
  when FF is compiled.
- Added NOP to the assembler.
- SEE now shows unkown data as hexdump.
- EMIT filters not supported control characters. -> less terminal hangups.
- DUMP has now start address and count (Thanks to Pete Zawasky )
- Some PIC control register definitions are in pic.fth (Thanks to Andrew Smith)
- u.4 u.2 are new words for printing 4 and two hex digits

FlashForth V2.0
---------------
- PAUSE and multitasker for background tasks added.
- Interrupt based TICKS word increments every millisecond
- MS is based on TICKS interrupts
- User variables added
- FF is now case sensitive. Hex numbers must be in lower case.
- COLD instead of INIT
- INI instead of START
- User INI execution is now prevented by a hit on ESC
- 8 bit wide bitmask words added MSET, MCLR, MTST
- Memory protection of code area and internal ram varables.
  Can be skipped at compile time.
- EVALUATE, DUMP, IMMEDIATE, POSTPONE can be excluded from the basic dictionary
  so that you can add own code below h'2000'.
- The multitasker can be excluded at compile time.
- FORGET removed altogether since it is not robust.
- Code optimisations of WARM COLD DUP OVER ROT SWAP ?0= N= SKIP SCAN 

FlashForth V1.5
---------------
- Corrections for PIC errata in the 18F252 and 18F258 series.

FlashForth V1.4
---------------
- WARM used TYPE before the serial routine variables were initialised.
- The START variable will be set to 0 by COLD.
- Slightly more compact system, allowing more code below 0x2000.

FlashForth V1.3
---------------
- Interrupt based serial transmitter
  EMIT blocks only if the TX buffer is full
  The size of the TX buffer is 48 characters.
- Rewritten Serial RX routine.
- Removed FM/MOD since I dont need it.
- Added noname:
- jt.fth, ct.fth, seect.fth, seen.fth
- Flashing of code is more robust than earlier. It works
  even if there is an intensive 100 us background interrupt
  load. Take the TIMER0 code into use with -DTMR0 to try it.
- The PAD area is fixed, it does not float above allocated ram.
  PAD size is 56 bytes.
- TIBSIZE is 72 bytes. Keep your source line lengths below that
  or you may see erratic behaviour.
- WORD copies data to 'RAM HERE', the first unallocated RAM address.
- If any of the memory allocation pointers for rom, ram and eeprom
  become out of range, the kernel will COLD the system and you will
  need to recompile all your forth code.

FlashForth V1.2
---------------
- Support for more PIC processors
  18f242, 18f442, 18f252, 18f452, 18f248, 18f258, 18f248, 18f458, 18f452
  18f2455, 18f2550, 18f4455, 18f4550 
  18f2525, 18f2620, 18f4525, 18f4620
  18f220, 18f2520, 18f4420, 18f4520
- The intercharacter delay in the terminal emulator is not required anymore.
  Works at least when pasting source code into a minicom or TeraTerm window. 
- Inline assembler introduced.
- Interrupt based serial receiver.
- Data space is set to ram only when entering QUIT. The user must manage this hxxself otherwise.
- Current number base shown in the prompt.
- Current data space shown in the prompt. 0 = rom, 1 = eeprom, 2 = ram.
- Current stack contents shown after the prompt
- c@++ ( addr -- addr+1 c ) introduced ( same as count actually)
- Bugfixes.
  1.Trailing TABs did'nt work.
  2.MARKER did not save/restore the START variable

FlashForth V1.0
------------------
- First release
