;;; FlashForth device configuration for the PIC24F16KAxxx
config FOSCSEL, FNOSC_FRCPLL
config FOSC, FCKSM_CSDCMD & SOSCSEL_SOSCLP & POSCFREQ_MS & OSCIOFNC_OFF & POSCMOD_NONE
config FWDT, FWDTEN_ON 
config FPOR, MCLRE_OFF & BORV_V18 & I2C1SEL_PRI & PWRTEN_ON & BOREN_BOR3
config FDS, DSWDTEN_OFF& DSBOREN_OFF

.equ FREQ_OSC, (8000000*4)	 ;Clock (Crystal)frequency (Hz)
.equ BAUDRATE, 38400		 ; Serial baudrate
.equ AUTOBAUD, 0             ; 0 = to use fixed baudrate
                             ; 1 = Autobaud, First char after reset must be 'U' (0x55)

.equ FCY, (FREQ_OSC/2)

; Set to one to use the alternate UART pins
.equ USE_ALTERNATE_UART_PINS, 0

; Default number base
.equ BASE_DEFAULT, 16      ; 16 = hexadecimal, 10 = decimal

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 0

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

;;; Memory mapping prefixes
;;; To be set to the amount of ram + 0x800.
;;; The rest if the memory space is available for flash
;;; You can increase the addressable flash range be decreasing the addressable ram.
;;; Below is the setting for max amount of ram for dsPIC24F16KA102
.equ PFLASH,  0x1000

;;; Memory mapping prefixe for EEPROM 
.equ PEEPROM, 0xfe00          ; 512 bytes of eeprom

;;; Meory control defines
.equ EEPROM_ERASE, 0x4058   ; Erase one word
.equ EEPROM_WRITE, 0x4004	; Write one word
.equ FLASH_ERASE,  0x4058	; Erase one row (64/96 bytes)
.equ FLASH_WRITE,  0x4004   ; Write one row (64/96 bytes)
.equ PAIVT,        0x100
.equ IBUFSIZEL,    0x0040
.equ IBUFSIZEH,    0x0020
.equ IBUFLEN1,     0x0020   ; Inner write loop
.equ IBUFLEN2,     0x0001   ; Outer write loop
.equ IBUFMASK,     0xffc0

; Flash write delay
.equ write_delay, 20
.equ BAUD_DIV1, (FCY/4/BAUDRATE)-1		  ; UART divisor
.equ MS_PR_VAL, (FCY/1000)			      ; Timer value for 1 ms ticks
