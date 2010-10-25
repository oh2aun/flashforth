;;; FlashForth device configuration for the dsPIC30Fxxxx
config __FOSC, CSW_ON_FSCM_OFF & LPRC & FRC_PLL8
;config __FOSC, HS;FRC; HS            ;Turn off clock switching and
                              ;fail-safe clock monitoring and
                              ;use the External HS Clock as the
                              ;system clock

config __FWDT, WDT_ON              ;Turn on Watchdog Timer

config __FBORPOR, PBOR_ON & BORV_27 & PWRT_16 & MCLR_EN
                                    ;Set Brown-out Reset voltage and
                                    ;and set Power-up Timer to 16msecs
                                            
config __FGS, CODE_PROT_OFF         ;Set Code Protection Off


;.equ FREQ, 16000000;
.equ FREQ, (7322000*8)					  ;Clock (Crystal)frequency (Hz)
.equ BAUDRATE, 38400					  ; Serial baudrate

.equ BAUD_DIV1, (FREQ/64/BAUDRATE)-1		; UART divisor
.equ MS_PR_VAL, (FREQ/4/1000)				; Timer value for 1 ms ticks

; Set to one to use the alternate UART pins
.equ USE_ALTERNATE_UART_PINS, 1

; Flash write delay
.equ write_delay, 20

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 0

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

.equ PFLASH,       0x1000    ; 2 Kbytes RAM  61 Kbytes flash
.equ PEEPROM,      0xfc00    ; 1 Kbyte EEPROM
.equ EEPROM_ERASE, 0x4044     
.equ EEPROM_WRITE, 0x4004
.equ FLASH_ERASE,  0x4041
.equ FLASH_WRITE,  0x4001
.equ PAIVT,        0x80
.equ IBUFSIZEL,      0x0040
.equ IBUFSIZEH,      0x0020
.equ IBUFLEN1,       0x0020
.equ IBUFLEN2,       0x0001
.equ IBUFMASK,       0xffc0


