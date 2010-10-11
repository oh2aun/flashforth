;**********************************************************************
;                                                                     *
;    Filename:      ff30.inc                                          *
;    Date:          10.10.2010                                        *
;    File Version:  4.7                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     * 
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2009  Mikael Nordman
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License version 3 as 
; published by the Free Software Foundation.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Modified versions of FlashForth must be clearly marked as such, 
; in the name of this file, and in the identification
; displayed when FlashForth starts.
;**********************************************************************
.ifdecl    __dsPIC33F  
  .include "p33Fxxxx.inc"  
  .include "p33_config.inc"
.endif  
 
.ifdecl    __PIC24H  
  .include "p24Hxxxx.inc"  
  .include "p24hj_config.inc"
.endif  
 
.ifdecl    __PIC24FK  
  .include "p24Fxxxx.inc"  
  .include "p24fk_config.inc"
.endif  
 
.ifdecl    __PIC24FJ
  .include "p24Fxxxx.inc"  
  .include "p24fj_config.inc"
.endif  

.ifdecl    __PIC24F
  .include "p24Fxxxx.inc"  
  .include "p24f_config.inc"
.endif  

.ifdecl    __dsPIC30F  
  .include "p30Fxxxx.inc"  
  .include "p30f_config.inc"
.endif

.if 0
;..............................................................................
;Configuration bits:
;..............................................................................
;.equ FREQ, 16000000;
.equ FREQ, (7322000*8)					  ;Clock (Crystal)frequency (Hz)
.equ BAUDRATE, 38400					  ; Serial baudrate

.equ BAUD_DIV1, (FREQ/64/BAUDRATE)-1		; UART divisor
.equ MS_PR_VAL, (FREQ/4/1000)				; Timer value for 1 ms ticks

; Set to one to use the alternate UART pins
.ifndef USE_ALTERNATE_UART_PINS
.equ USE_ALTERNATE_UART_PINS, 1                ; Must be 0 for 24F08KAxxx
.endif

; Flash write delay
.equ write_delay, 1

; Set to 1 for extra debug info after an exception reset
.equ DEBUG_INFO, 1

; Set to 1 to allow control-o to reset FlashForth
.equ CTRL_O_WARM_RESET, 1

.endif
