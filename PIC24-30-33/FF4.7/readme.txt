FlashForth V4.7 readme file
---------------------------

FlashForth is licensed acording to the GNU General Public License

Look in the user guide at http://flashforth.sourceforge.net/ for further information.


REVISION HISTORY
----------------

FlashForth V1.0
------------------
- First release

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

FlashForth V1.4
---------------
- WARM used TYPE before the serial routine variables were initialised.
- The START variable will be set to 0 by COLD.
- Slightly more compact system, allowing more code below 0x2000.

FlashForth V1.5
---------------
- Corrections for PIC errata in the 18F252 and 18F258 series.

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

FlashForth V3.1
---------------
- CMOVE fixed. Uses !p>r instead of !p
- 2/ changed from unsigned to signed divide
- 0= and 0< optimised
- LIT removed from the visible dictionary.

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
 
FlashForth V3.3
---------------
- SKIP corrected so that TAB is handled as a matching char.
- WARM corrected so that FF compiles also with GPASM
- Line Feed is now ignored (but echoed) by ACCEPT.
  So random linefeeds will not confuse FF.

FlashForth V4.0
---------------
- Supports only dsPIC30F devices
- User deferred words KEY KEY? EMIT EMIT?  and UDEFER
- dsPIC words CF, AS, CF@ CF! BRA, RETFIE, RETURN, I] [I AIVT IVT INT!
- Compile only words
- Character queue words CQ: >CQ CQ> >CQ? CQ>? CQ0
- UART 1 words TX1 TX1? RX1 RX1? U1RXQ U1TXQ
- Default project for dsPIC30F3012a2 on MPLAB 6.40
- ACCEPT handles CR, LF and CRLF in a hopefully good way
- dsPIC is faster due to more inlining and 16 bit instruction set
  1 cycle multiply, 17 cycle divide.
- Assembler is not yet written but the base words exist.
  The assembler conditionals and the words needed by the forth compiler already exist.
- KHZ renamed to CPU_CLK
- PIC18F will be lifted to 4.x status later, maybe. 

FlashForth V4.1
---------------
- Serial routines made stable.
- Added CWD
- Optimised NIP

FlashForth V4.2
---------------
- Rewritten UART handling. 
  Disable the UART FIFO in your PC to make XON/XOFF work better.
  Otherwise characters may be lost.
- Debug info after exception reset
- Faster ram access due to memory map change
  RAM:0-1fff    FLASH::2000-fbff   EEPROM: fc00-ffff
- Bit assembler words, see task-test.fth for an example 
- Some optimisations
- Corrections in PICK and (SEE)

FlashForth V4.3
---------------
- Corrections to PICK and RSHIFT

FlashForth V4.4
---------------
- Correction to INT! It did not work at all before.
- Debug output only after a software reset. 
  In case of memory, stack or math exception,
  the user area of the offending task,
  and INTCON1, RP, SP, UP are displayed.
- P register switched in PAUSE
- CMOVE optimised when source and destination addresses
  are both in RAM.
- P register and RCOUNT included in [i i] 
- rtc_30.fth contains a simple Timer2 based
  Real Time Clock. 

FlashForth v4.5
---------------
- Optimised NEXT
- Added tail call optimisation and tail looping.
- >BODY made visible
- UART RX now ignores XON/XOFF, earlier these went into the RX FIFO. 
- TO defined in core.fth
- IDUMP. Dumps also the high byte of each instruction word.

FlashForth v4.6
---------------
- Added SM/REM /MOD MOD M* */MOD */
- Added compile option for ctrl-o. Ctrl-o warm start FF.
- return, made visible
 
FlashForth v4.7 preliminary
---------------------------
- Support for all PIC24, PIC33, PIC30 devices.
  Enough memory is needed.
- UDEFER removed
  User variable vectoring aligned with FF3.7
- Devices without EEPROM use EEPROM emulation in FLASH.
  Needs 4(6) Kbytes of flash to store TURNKEY, FLASH DP, RAM DP, LATEST.
- EMITQ and UEMITQ removed.
- PROMPT moved to RAM. Default at WARM start is .ST.
- Added PFL. PFL is a constant for the FLASH memory mapping prefix.
  It is dependent on the RAM size.
- Interrupt words do not yet work for PIC24 or PIC33 devices.
  TODO: The vectors must be moved to RAM.
- TODO CTS/RTS flow control.
