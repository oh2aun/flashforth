;**********************************************************************
;                                                                     *
;    Filename:      usbcdc.asm                                        *
;    Date:          09.11.2020                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     *
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2020  Mikael Nordman
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
; in the header of this file, and in the identification
; displayed by the word VER.
;**********************************************************************
; NOTE!
; Vendor ID 0xfaf0 and Product ID 0xfaf0 are used as temporary IDs for TESTING ONLY
; For a commercial product you MUST obtain your own Vendor ID and Product ID !
#define U_PID 0xfaf0  ; Product ID used for testing FlashForth
#define U_VID 0xfaf0  ; Vendor ID used for testing FlashForth

.dseg 
.org  0x100 
usb_config_status:  .byte 1
bmRequestType:      .byte 1
bRequest:           .byte 1
wValue:             .byte 2
wIndex:             .byte 2
wLength:            .byte 2
line_coding:        .byte 7
;*******************************************************************************
.cseg
device_dsc:
        .db   0x12,0x01  ; Size of this descriptor in bytes
                         ; DEVICE descriptor type
        .dw   0x0110     ; USB Spec Release Number in BCD format
        .db   0x02,0x00  ; Class Code CDC device
                         ; Subclass 
        .db   0x00,0x20  ; Protocol
                         ; EP0 packet size = 32 bytes
        .dw   U_VID      ; Vendor ID
        .dw   U_PID      ; Product ID
        .dw   0x0000     ; Device release Number in BCD
        .db   0x00,0x01  ; Manufacturer string index
                         ; Product string Index
        .db   0x00,0x01  ; Device serial number string index
                         ; Number of possible configurations

SD000:
        .db   0x04,0x03  ; sizeof descriptor in bytes
                         ; STRING descriptor type
        .dw   0x0409     ; Language code

; Product string
SD001:
        .db   0x0a,0x03  ; sizeof descriptor in bytes
                         ; STRING descriptor type
        .dw   'F','F','5','0'

USB_CFG:
        .db   0x09, 0x02, 0x3e, 0x00
        .db   0x02, 0x01, 0x00, 0x80
        .db   0x32, 0x09, 0x04, 0x00
        .db   0x00, 0x01, 0x02, 0x02
        .db   0x01, 0x00, 0x05, 0x24
        .db   0x00, 0x10, 0x01, 0x04
        .db   0x24, 0x02, 0x02, 0x05
        .db   0x24, 0x06, 0x00, 0x01
        .db   0x07, 0x05, 0x82, 0x03
        .db   0x08, 0x00, 0x10, 0x09
        .db   0x04, 0x01, 0x00, 0x02
        .db   0x0a, 0x00, 0x00, 0x00
        .db   0x07, 0x05, 0x03, 0x02
        .db   0x08, 0x00, 0x00, 0x07
        .db   0x05, 0x84, 0x02, 0x08
        .db   0x00, 0x00

/*
        .db   0x09     ; length
        .db   0x02     ; configuration descriptor
        .dw   0x003e   ; total length
        .db   0x02     ; number of interfaces
        .db   0x01     ; configuration id
        .db   0x00     ; string descriptor index
        .db   0x80     ; attributes (bus powered)
        .db   0x32     ; maxpower 100 mA
        .db   0x09     ; length
        .db   0x04     ; interface descriptor
        .db   0x00     ; interface 0
        .db   0x00     ; alternate setting
        .db   0x01     ; number of end points
        .db   0x02     ; interface class code
        .db   0x02     ; interface subclass
        .db   0x01     ; interface protocol
        .db   0x00     ; string descriptor index
        .db   0x05,0x24,0x00,0x10,0x01 ; interface header FD  
        .db   0x04,0x24,0x02,0x02      ; interface ACM FD
        .db   0x05,0x24,0x06,0x00,0x01 ; interface Union FD
        .db   0x07,0x05,0x82,0x03,0x08,0x00,0x10 ; endpoint notification
        .db   0x09,0x04,0x01,0x00,0x02,0x0a,0x00,0x00,0x00 ; interface data
        .db   0x07,0x05,0x03,0x02,0x08,0x00,0x00 ; endpoint data out
        .db   0x07,0x05,0x84,0x02,0x08,0x00,0x00 ; endpoint data in
*/

;*******************************************************************************

        fdw     EMPTY_L
USB_ON_L:
        .db     NFA|4,"usb+",0
USB_ON:
        lds     t0, USBSTA
        cpi     t0, 3
        brne    USB_ON_RET
        lds     t0, USBCON
        cpi     t0, 0x30
        brne    USB_ON_RET
        sbi_    UHWCON, UVREGE      ; Enable USB regulator
        ldi     t0, 0x12
        out_    PLLCSR, t0          ; Configure to use 16mHz oscillator
USB_PLL_WAIT:
        in_     t0, PLLCSR
        andi    t0, (1<<PLOCK)
        breq    USB_PLL_WAIT      ; Wait for PLL Lock to be achieved
        sbi_    USBCON, USBE      ; Enable USB Controller
        cbi_    USBCON, FRZCLK      ; Unfreeze the clock
        sts     UDCON, r_zero        ; Full speed mode and ATTACH
        sts     usb_config_status, r_zero
USB_ON_RET:
        ret


        fdw     USB_ON_L
USB_OFF_L:
        .db     NFA|4,"usb-",0
USB_OFF:
        sts     usb_config_status, r_zero
        sts     UDCON, r_one      ; DETACH
        ldi     t0, 0x30
        sts     USBCON, t0        ; Disable the USB controller
        sts     PLLCSR, r_zero    ; Disable the PLL
        sts     UHWCON, r_zero    ; Disable the USB regulator
        ret

USB_device_service:
        lds     t0, USBCON
        cpi     t0, 0x90
        brne    USB_RET
        lds     t0, UDINT
        andi    t0, (1 << EORSTI)
        breq    USB_RET1
        ; USB RESET
        sts     UENUM, r_zero
        sbi_    UECONX, EPEN
        sts     UECFG0X, r_zero
        ldi     t0, 0x22        ; 32 byte endpoint, 1 bank, allocate the memory
        sts     UECFG1X, t0
        sts     usb_config_status, r_zero
        sts     UERST, r_one      ; Reset control endpoint
        sts     UERST, r_zero
USB_RET1:
        sts     UDINT, r_zero
USB_RET:
        ret

GET_DESCRIPTOR_SEND:
        rcall   USB_WAIT_TX
        lds     t0, UEINTX
        andi    t0, (1<<RXOUTI)
        brne    USB_RET
GET_DESCRIPTOR_SEND1:
        lpm     t0, z+
        sts     UEDATX, t0
        lds     t0, UEBCLX
        cpi     t0, 0x20
        brne    GET_DESCRIPTOR_SEND2
        rcall   TX_WAIT
GET_DESCRIPTOR_SEND2:
        dec     t1
        brne    GET_DESCRIPTOR_SEND1
        lds     t0, UEBCLX
        cpi     t0, 0
        breq    USB_RET
        rjmp    TXINI_CLR

USB_IN_MSG:
        ld      t0, z+
        sts     UEDATX, t0
        dec     t1
        brne    USB_IN_MSG
        ret

USB_STALL:
        lds     t0, UECONX
        ori     t0, (1 << STALLRQ) | (1 << EPEN)
        sts     UECONX, t0
        ret

USB_ep_service:
        lds     t0, USBCON
        cpi     t0, 0x90
        brne    USB_RET
        lds     t0, UENUM
        sts     UENUM, r_zero      ; Control endpoint
        lds     t0, UEINTX
        andi    t0, (1<<RXSTPI)|(1<<RXOUTI)
        breq    USB_RET
        ldi     zl, low(bmRequestType)
        ldi     zh, high(bmRequestType)
        ldi     t1, 8
        rcall   USB_OUT_MSG
        ldi     t0, ~((1<<RXSTPI)|(1<<RXOUTI)|(1<<TXINI))
        rcall   UEINTX_CLR
        lds     t0, bmRequestType
        andi    t0, 0x80
        breq    SET_STUFF
GET_STUFF:
        lds     t0, bRequest
        cpi     t0, 0x06
        breq    GET_DESCRIPTOR_
        cpi     t0, 0x08
        breq    GET_CONFIG
        cpi     t0, 0x00
        breq    GET_STATUS_
        cpi     t0, 0x21
        breq    GET_LINE_CODING
        rjmp    USB_STALL
GET_CONFIG:
        lds     t0, usb_config_status
        sts     UEDATX, t0
        rjmp    TXINI_SET
GET_STATUS_:
        sts     UEDATX, r_zero
        rcall   TXINI_SET
        sts     UEDATX, r_zero
        rjmp    TXINI_SET
GET_LINE_CODING:
        ldi     zl, low(line_coding)
        ldi     zh, high(line_coding)
        ldi     t1, 7
        rjmp    USB_IN_MSG

GET_DESCRIPTOR_:
        lds     t0, wValue+1
        cpi     t0, 1
        breq    GET_DEVICE_DESCRIPTOR
        cpi     t0, 2
        breq    GET_CONFIGURATION_DESCRIPTOR
        cpi     t0, 3
        breq    GET_STRING_DESCRIPTOR
        rjmp    USB_STALL
GET_DEVICE_DESCRIPTOR:
        ldi     zl, low(device_dsc<<1)
        ldi     zh, high(device_dsc<<1)
        ldi     t1, 0x12
        rjmp    GET_DESCRIPTOR_SEND
GET_CONFIGURATION_DESCRIPTOR:
        ldi     zl, low(USB_CFG<<1)
        ldi     zh, high(USB_CFG<<1)
        lds     t1, wLength
        rjmp    GET_DESCRIPTOR_SEND
GET_STRING_DESCRIPTOR:
        lds     t0, wIndex
        cpi     t0, 0
        brne    GET_STR_01
GET_STR_00:
        ldi     zl, low(SD000<<1)
        ldi     zh,  high(SD000<<1)
        ldi     t1, 4
        rjmp    GET_DESCRIPTOR_SEND
GET_STR_01:
        ldi     zl, low(SD001<<1)
        ldi     zh,  high(SD001<<1)
        ldi     t1, 0xa
        rjmp    GET_DESCRIPTOR_SEND
;**************************************************************
SET_STUFF:
        lds     t0, bRequest
        cpi     t0, 0x09
        breq    USB_SET_CONFIGURATION
        cpi     t0, 0x05
        breq    SET_ADDR
        cpi     t0, 0x22
        breq    SET_CONTROL_LINE_STATE
        cpi     t0, 0x20
        breq    SET_LINE_CODING
        rjmp    USB_STALL
SET_LINE_CODING:
        rcall   TX_WAIT
        ldi     zl, low(line_coding)
        ldi     zh, high(line_coding)
        ldi     t1, 7
        rjmp    USB_OUT_MSG
USB_SET_CONFIGURATION:
        rcall   TX_WAIT
        ; Interrupt Notification EP
        sts     UENUM, r_two
        sts     UECONX, r_one
        ldi     t0, 0xc1    ; EP type Interrupt IN
        sts     UECFG0X, t0
        sts     UECFG1X, r_two  ; Single bank, 8 bytes, allocate memory
        ; Bulk IN EP
        ldi     t0,  3
        sts     UENUM, t0
        sts     UECONX, r_one
        ldi     t0, 0x80    ; EP type Bulk IN
        sts     UECFG0X, t0
        sts     UECFG1X, r_two  ; Single bank, 8 bytes, allocate memory
        ; Bulk OUT EP
        ldi     t0, 4
        sts     UENUM, t0
        sts     UECONX, r_one
        ldi     t0, 0x81    ; EP type Bulk OUT
        sts     UECFG0X, t0
        sts     UECFG1X, r_two  ; Single bank, 8 bytes, allocate memory
        ; Reset the end points
        ldi     t0, 0x1c
        sts     UERST, t0
        sts     UERST, r_zero
        lds     t0, wValue
        sts     usb_config_status, t0
        ret
SET_ADDR:
        lds     t0, wValue
        sts     UDADDR, t0
        rcall   TX_WAIT
        lds     t0, UDADDR
        ori     t0, 0x80
        sts     UDADDR, t0
        ret

SET_CONTROL_LINE_STATE:
TX_WAIT:
        rcall   TXINI_CLR
USB_WAIT_TX:
        lds     t0, UEINTX
        andi    t0, (1<<TXINI)
        breq    USB_WAIT_TX
        ret

USB_OUT_MSG:
        lds     t0, UEDATX
        st      z+, t0
        dec     t1
        brne    USB_OUT_MSG
        ret

TXINI_SET:
        ldi     t0, (1<<TXINI)
UEINTX_SET:
        lds     t4, UEINTX
        or      t4, t0
        sts     UEINTX, t4
        ret

FIFOCON_CLR:
        ldi     t0, ~(1<<FIFOCON)
        rjmp    UEINTX_CLR
TXINI_CLR:
        ldi     t0, ~(1<<TXINI)
UEINTX_CLR:
        lds     t4, UEINTX
        and     t4, t0
        sts     UEINTX, t4
        ret


; RXU?   -- flag   True if character waiting
        fdw     USB_OFF_L
RXUQ_L:
        .db     NFA|4,"rxu?",0
RXUQ_:
        lds     t0, usb_config_status
        cpi     t0, 0
        breq    RXUQ_2
        pushtos
        ldi     t0, 3
        sts     UENUM,t0
        lds     tosl, UEBCLX
        ldi     tosh, 0
        ret
RXUQ_2:
        jmp     FALSE_

        fdw     RXUQ_L
RXU_L:
        .db     NFA|3,"rxu"
RXU_:
        call    PAUSE
        lds     t0, usb_config_status
        cpi     t0, 0
        breq    RXU_
        ldi     t0, 3
        sts     UENUM,t0
        lds     t0, UEINTX
        andi    t0, (1<<FIFOCON)
        breq    RXU_
        ldi     t0, ~(1<<RXOUTI)
        rcall   UEINTX_CLR
        lds     t0, UEBCLX
        cpi     t0, 0
        breq    RXU_2
        pushtos
        lds     tosl, UEDATX
        ldi     tosh, 0
RXU_2:
        lds     t0, UEBCLX
        cpi     t0, 0
        brne    RXU_3
        rcall   FIFOCON_CLR
RXU_3:        
        ret

        fdw     RXU_L
TXU_L:
        .db     NFA|3,"txu"
TXU_:
        call    PAUSE
        lds     t0, usb_config_status
        cpi     t0, 0
        breq    TXU_2
        ldi     t0, 4
        sts     UENUM, t0
        lds     t0, UEINTX
        andi    t0, (1<<TXINI)
        breq    TXU_
        rcall   TXINI_CLR
        sts     UEDATX, tosl
        rcall   FIFOCON_CLR
TXU_2:
        jmp     DROP

