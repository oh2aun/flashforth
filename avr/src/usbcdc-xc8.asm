;**********************************************************************
;                                                                     *
;    Filename:      usbcdc-xc8.asm                                    *
;    Date:          27.11.2021                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     *
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2021  Mikael Nordman
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
;*******************************************************************************
.section .text
device_dsc:
        .byte   0x12,0x01  ; Size of this descriptor in bytes
                           ; DEVICE descriptor type
        .word   0x0110     ; USB Spec Release Number in BCD format
        .byte   0x02,0x00  ; Class Code CDC device
                         ; Subclass 
        .byte   0x00,0x20  ; Protocol
                         ; EP0 packet size = 32 bytes
        .word   U_VID      ; Vendor ID
        .word   U_PID      ; Product ID
        .word   0x0000     ; Device release Number in BCD
        .byte   0x00,0x01  ; Manufacturer string index
                           ; Product string Index
        .byte   0x00,0x01  ; Device serial number string index
                           ; Number of possible configurations

SD000:
        .byte   0x04,0x03  ; sizeof descriptor in bytes
                         ; STRING descriptor type
        .word   0x0409     ; Language code

; Product string
SD001:
        .byte   0x0a,0x03  ; sizeof descriptor in bytes
                         ; STRING descriptor type
        .word   'F','F','5','0'

USB_CFG:
        .byte 0x09     ; length
        .byte 0x02     ; configuration descriptor
        .word 0x003e   ; total length
        .byte 0x02     ; number of interfaces
        .byte 0x01     ; configuration id
        .byte 0x00     ; string descriptor index
        .byte 0x80     ; attributes (bus powered)
        .byte 0x32     ; maxpower 100 mA
        .byte 0x09     ; length
        .byte 0x04     ; interface descriptor
        .byte 0x00     ; interface 0
        .byte 0x00     ; alternate setting
        .byte 0x01     ; number of end points
        .byte 0x02     ; interface class code
        .byte 0x02     ; interface subclass
        .byte 0x01     ; interface protocol
        .byte 0x00     ; string descriptor index
        .byte 0x05,0x24,0x00,0x10,0x01 ; interface header FD  
        .byte 0x04,0x24,0x02,0x02      ; interface ACM FD
        .byte 0x05,0x24,0x06,0x00,0x01 ; interface Union FD
        .byte 0x07,0x05,0x82,0x03,0x08,0x00,0xff ; endpoint notification
        .byte 0x09,0x04,0x01,0x00,0x02,0x0a,0x00,0x00,0x00 ; interface data
        .byte 0x07,0x05,0x03,0x02,0x08,0x00,0x00 ; endpoint data out
        .byte 0x07,0x05,0x84,0x02,0x08,0x00,0x00 ; endpoint data in

;*******************************************************************************

        fdw     EMPTY_L
USB_ON_L:
        .byte   NFA|4
        .ascii  "usb+"
        .align  1
USB_ON:
        lds     t0, USBSTA
        cpi     t0, 3
        brne    USB_ON_RET
        lds     t0, USBCON
        cpi     t0, 0x30
        brne    USB_ON_RET
        m_sbi   UHWCON, UVREGE      ; Enable USB regulator
        ldi     t0, 0x12
        m_out   PLLCSR, t0          ; Configure to use 16mHz oscillator
USB_PLL_WAIT:
        m_in    t0, PLLCSR
        andi    t0, (1<<PLOCK)
        breq    USB_PLL_WAIT      ; Wait for PLL Lock to be achieved
        m_sbi   USBCON, USBE      ; Enable USB Controller
        m_cbi   USBCON, FRZCLK      ; Unfreeze the clock
        sts     UDCON, r_zero        ; Full speed mode and ATTACH
        sts     usb_config_status, r_zero
USB_ON_RET:
        ret

TX0_LOOP1:
        m_in    t0, UCSR0A
        sbrs    t0, 5        ; UDRE0, UDRE USART Data Register Empty
        rjmp    TX0_LOOP1
        m_out   UDR0_, t1


        fdw     USB_ON_L
USB_OFF_L:
        .byte   NFA|4
        .ascii  "usb-"
        .align  1
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
        m_sbi   UECONX, EPEN
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
        lds     t0, wLength
        cp      t0, t1
        lds     t0, wLength+1
        cpc     t0, r_zero
        brcc    1f
        lds     t1, wLength     ; send no more than requested
1:
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
        rcall   USB_WAIT_TX
        ld      t0, z+
        sts     UEDATX, t0
        dec     t1
        brne    USB_IN_MSG
        rjmp    TXINI_CLR

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
        ldi     zl, lo8(bmRequestType)
        ldi     zh, hi8(bmRequestType)
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
        ldi     zl, lo8(line_coding)
        ldi     zh, hi8(line_coding)
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
        ldi     zl, lo8(device_dsc)
        ldi     zh, hi8(device_dsc)
        ldi     t1, 0x12
        rjmp    GET_DESCRIPTOR_SEND
GET_CONFIGURATION_DESCRIPTOR:
        ldi     zl, lo8(USB_CFG)
        ldi     zh, hi8(USB_CFG)
        ldi     t1, 0x3e //wLength
        rjmp    GET_DESCRIPTOR_SEND
GET_STRING_DESCRIPTOR:
        lds     t0, wIndex
        cpi     t0, 0
        brne    GET_STR_01
GET_STR_00:
        ldi     zl, lo8(SD000)
        ldi     zh,  hi8(SD000)
        ldi     t1, 4
        rjmp    GET_DESCRIPTOR_SEND
GET_STR_01:
        ldi     zl, lo8(SD001)
        ldi     zh,  hi8(SD001)
        ldi     t1, 0xa
        rjmp    GET_DESCRIPTOR_SEND
;**************************************************************
SET_STUFF:
        lds     t0, bRequest
        cpi     t0, 0x09
        breq    USB_SET_CONFIGURATION
        cpi     t0, 0x05
        breq    SET_ADDR
        cpi     t0, 0x20
        breq    SET_LINE_CODING
        cpi     t0, 0x22
        breq    SET_CONTROL_LINE_STATE
        rjmp    USB_STALL
SET_LINE_CODING:
        lds     t0, UEINTX
        andi    t0, (1<<RXOUTI)
        breq    SET_LINE_CODING
        ldi     zl, lo8(line_coding)
        ldi     zh, hi8(line_coding)
        ldi     t1, 7
        rjmp    USB_OUT_MSG
USB_SET_CONFIGURATION:
        rcall   TXINI_CLR
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
        rjmp    INIT_LINE_CODING
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
        rcall   RXOUTI_CLR
        rjmp    TXINI_CLR

TXINI_SET:
        ldi     t0, (1<<TXINI)
UEINTX_SET:
        lds     t4, UEINTX
        or      t4, t0
        sts     UEINTX, t4
        ret

RXOUTI_CLR:
        ldi     t0, ~(1<<RXOUTI)
        rjmp    UEINTX_CLR
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

INIT_LINE_CODING:
        ; line coding 00 0x96 00 00 00 00 0x08
        ldi     t0, 0x96
        sts     line_coding+1, t0
        sts     line_coding, r_zero
        sts     line_coding+2, r_zero
        sts     line_coding+3, r_zero
        sts     line_coding+4, r_zero
        sts     line_coding+5, r_zero
        ldi     t0, 0x08
        sts     line_coding+6, t0
        ret


; RXU?   -- flag   True if character waiting
        fdw     USB_OFF_L
RXUQ_L:
        .byte   NFA|4
        .ascii  "rxu?"
        .align  1
RXUQ_:
        lds     t0, usb_config_status
        cpi     t0, 0
        breq    RXUQ_2
        m_dup
        ldi     t0, 3
        sts     UENUM,t0
        lds     tosl, UEBCLX
        ldi     tosh, 0
        ret
RXUQ_2:
        jmp     FALSE_

        fdw     RXUQ_L
RXU_L:
        .byte     NFA|3
        .ascii  "rxu"
        .align  1
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
        rcall   RXOUTI_CLR
        lds     t0, UEBCLX
        cpi     t0, 0
        breq    RXU_2
        m_dup
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
        .byte   NFA|3
        .ascii  "txu"
        .align  1
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

