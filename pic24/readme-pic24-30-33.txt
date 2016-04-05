FlashForth PIC24-30-33 readme file
----------------------------------

FlashForth is licensed acording to the GNU General Public License

Look in the user guide http://flashforth.sourceforge.net, 
and in the word list wordsAll.txt for further information.


REVISION HISTORY
----------------

FlashForth V5.0
---------------
- X@ X!
- 8->16 bit task parameters
- FLOAT support
- Python shell with command line editing and history. 
  #send command for uploading files to FF.
- DO ?DO LEAVE UNLOOP I J LOOP +LOOP
- Old LEAVE used with FOR NEXT has changed to ENDIT
- CON changed to CONSTANT (Inline code)
- CONSTANT changed to CO: (uses DOCREATE)
- IS and TO made immediate words 
- Improved XON/XOFF flow control
- Aligned FL+/FL- with other FF
- Aligned ABORT" with other FF
- Call C function from Forth example.
- ACCEPT end of line can be CR or LF or CRLF
- HI, .FREE

FlashForth v4.8
----------------
- Double number support.
- Configuration of buffer sizes from configuration file
- IDLE mode power save, works also with multitasking. IDLE, BUSY words.
- CPU load measurement, LOAD fetches the load of the previous 256 ms period.
- Load indicator LED option
- 2CON, 2VARIABLE

FlashForth v4.71
----------------
- MTST bug fixed
- UKEY, UKEY?, UEMIT -> 'KEY, 'KEY?, 'EMIT
- ACCEPT KEY EMIT KEY? can be used also in background tasks.
  In that case revectoring of I/O words in the task is required
  Compilation and writing to flash and eeprom is only
  possible from the operator task.
- Optimisations
- User interrupts must be disabled if the interrupt routine is
  on flash page that is beeing modified.
  It is upto the user to recognize this.

FlashForth v4.7
---------------
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
- INT! stores the interrupt vector based on interrupt numbers.
- dsPIC30F stores the interrupt vector directly to flash in the Alternate Interrupt Vector table
- dsPIC33 and PIC24 stores the interrupt vector into a table in ram
  The 64 First Vectors are supported by default
- Added BCLR, BSET, BTST bit vector words.
  Can address bit vectors upto 64K size
- UART2 support with TX2, RX2, RX2? TX2?.
- The RTC interrupt example is corrected.
- Optional faster flash and eeprom write algorithm.
- FILL ERASE BLANKS added.
- RP@ added
- IDLE mode added. It reduces power consumption when
  waiting for input from the operator on UART1.
- All unused peripherals are disabled by the PMD registers to save power.
  See the interrupt example of how to enable the Timer2 peripheral.
- Peephole optimisation. The following sequence will be removed.
  mov W0, [++W14]
  mov [W14--], W0
  Test it.
  : test 45 + ;
  see test
- RTS HW flow control
- U1+ U1- U2+ U2- switches flow control on/off
- FL+, FL- switches flash write protection on/off
- Fcy shows the peripheral and cpu clocking frequency.
- An experimental flash write mode that writes more seldom to flash.
- 3 sieve implementations for speed testing.

FlashForth v4.6
---------------
- Added SM/REM /MOD MOD M* */MOD */
- Added compile option for ctrl-o. Ctrl-o warm start FF.
- return, made visible

FlashForth v4.5
---------------
- Optimised NEXT
- Added tail call optimisation and tail looping.
- >BODY made visible
- UART RX now ignores XON/XOFF, earlier these went into the RX FIFO.
- TO defined in core.fth
- IDUMP. Dumps also the high byte of each instruction word.

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

FlashForth V4.3
---------------
- Corrections to PICK and RSHIFT

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

FlashForth V4.1
---------------
- Serial routines made stable.
- Added CWD
- Optimised NIP

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

