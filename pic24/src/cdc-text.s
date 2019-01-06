;**********************************************************************
;                                                                     *
;    Filename:      cdc-text.s                                        *
;    Date:          06.01.2019                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     *
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2019  Mikael Nordman
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
.text
device_dsc:
        .byte   0x12    ; Size of this descriptor in bytes
        .byte   0x01    ; DEVICE descriptor type
        .word   0x0200  ; USB Spec Release Number in BCD format
        .byte   0x02    ; Class Code CDC device
        .byte   0x00    ; Subclass 
        .byte   0x00    ; Protocol
        .byte   0x08    ; EP0 packet size
        .word   U_VID   ; Vendor ID
        .word   U_PID   ; Product ID
        .word   0x0000  ; Device release Number in BCD
        .byte   0x00    ; Manufacturer string index
        .byte   0x01    ; Product string Index
        .byte   0x00    ; Device serial number string index
        .byte   0x01    ; Number of possible configurations

SD000:
        .byte   0x04    ; sizeof descriptor in bytes
        .byte   0x03    ; STRING descriptor type
        .word   0x0409  ; Language code

; Product string
SD001:
        .byte   0x0a    ; sizeof descriptor in bytes
        .byte   0x03    ; STRING descriptor type
        .word   'F','F','5','0'

USB_SD:
        .word   SD000
        .word   SD001

USB_CFG:
        .byte   0x09     ; length
        .byte   0x02     ; configuration descriptor
        .word   0x0043   ; total length
        .byte   0x02     ; number of interfaces
        .byte   0x01     ; configuration id
        .byte   0x00     ; string descriptor index
        .byte   0x80     ; attributes (bus powered)
        .byte   0x32     ; maxpower 100 mA
        .byte   0x09     ; length
        .byte   0x04     ; interface descriptor
        .byte   0x00     ; interface 0
        .byte   0x00     ; alternate setting
        .byte   0x01     ; number of end points
        .byte   0x02     ; interface class code
        .byte   0x02     ; interface subclass
        .byte   0x01     ; interface protocol
        .byte   0x00     ; string descriptor index
        .byte   0x05,0x24,0x00,0x10,0x01 ; interface header FD  
        .byte   0x04,0x24,0x02,0x02      ; interface ACM FD
        .byte   0x05,0x24,0x06,0x00,0x01 ; interface Union FD
        .byte   0x05,0x24,0x01,0x00,0x01 ; interface Call Management FD
        .byte   0x07,0x05,0x81,0x03,0x08,0x00,0x10 ; endpoint notification
        .byte   0x09,0x04,0x01,0x00,0x02,0x0a,0x00,0x00,0x00 ; interface data
        .byte   0x07,0x05,0x02,0x02,0x08,0x00,0x00 ; endpoint data out
        .byte   0x07,0x05,0x82,0x02,0x08,0x00,0x00 ; endpoint data in

;*******************************************************************************
USBDriverService:
        btsc    U1OTGIE, #ACTVIE
        rcall   USBWake
        btsc    U1PWRC, #USUSPND
        return
        btsc    U1IR, #URSTIF
        rcall   USBReset
        btsc    U1IR, #IDLEIF
        rcall   USBSuspend
        btss    usb_device_state, #0
        return
USBDriverService_2:
        btss    U1IR, #TRNIF
        return
        rcall   USBCtrlEPService
        mov     #__TRNIF, W0
        mov     W0, U1IR
        return
;*******************************************************************************
USBSuspend:
        btss    U1OTGIE, #ACTVIE
        return
        mov	#__IDLEIF, W0
	mov	W0, U1IR         ; clear IDLEIF
        bset    U1OTGIE, #ACTVIE
        return
USBWake:
        btss    U1OTGIR, #ACTVIF
        return
	mov	#__ACTVIF, W0
	mov	W0, U1OTGIR        ; clear ACTVIF interrupt flag
        bclr    U1PWRC, #USUSPND
        bclr    U1OTGIE, #ACTVIE
        return
USBReset:
        setm    U1IR
        clr 	U1ADDR
	clr	U1EP1
	clr	U1EP2
USBReset_1:
	btst	U1IR, #TRNIF
	bra	z, USBReset_2
	mov	__TRNIF, W0
	mov	W0, U1IR            ; clear TRNIF
	; clr some variables maybe
	bra	USBReset_1
USBReset_2:
	mov	#0xD, W0
	mov	W0, U1EP0	    ; Ep0 is control endpoint
        rcall   USBPrepareForNextSetupTrf
        mov     #DEFAULT_STATE, W0
	mov.b   WREG, usb_device_state
        bclr    U1CON, #PKTDIS
return1:
        return
;*******************************************************************************
USBCtrlEPService:
        mov.b   U1STAT, WREG
        and.b   #0xf8, W0
        bra     z, USBCtrlEPService_out        ; USTAT == EP00_OUT
        and.b   #0xf0, W0
        bra     nz, return1
        bra     USBCtrlTrfInHandler            ; USTAT == EP00_IN
USBCtrlEPService_out:
        mov.b   ep0ostat, WREG                 ; USTAT == EP00_OUT
        and.b   #0x3C, W0
        xor.b   #0x34, W0                      ; 0x34 (0xd) SETUP_TOKEN
        bra     z, USBCtrlTrfSetupHandler
        bra     USBCtrlTrfOutHandler
USBCtrlTrfSetupHandler:
        clr.b   ctrl_trf_state                 ; WAIT_SETUP
        bclr.b  usb_status, #MUID_USB9
        clr.b   count
        rcall   USBCheckStdRequest
        rcall   USBCheckCdcRequest
        bra     USBCtrlEPServiceComplete
;*******************************************************************************
USBCtrlTrfOutHandler:
        mov     #CTRL_TRF_RX, W0      ; 2
        cp.b    ctrl_trf_state
        bra     nz, USBPrepareForNextSetupTrf
        rcall   USBCtrlTrfRxService
        mov     #(_DAT1|_USIE|_DTSEN), W0
        btsc.b  ep0ostat, #__DTS
        mov     #(_USIE|_DTSEN), W0
        mov.b   WREG, ep0ostat
        return

USBPrepareForNextSetupTrf:
        clr.b    ctrl_trf_state
        mov     #CDC_INT_EP_SIZE, W0
        mov.b   WREG, ep0ocnt
        mov     #ep0buf, W0
        mov     W0, ep0oadr
        mov     #(_USIE|_DTSEN), W0      ;0x88
        mov.b   WREG, ep0ostat
        clr.b   ep0istat
        return
;*******************************************************************************
USBCtrlTrfInHandler:
        mov     #ADR_PENDING_STATE, W0
        cp.b    usb_device_state
        bra     nz, USBCtrlTrfInHandler_2
        mov.b   ep0buf+2, WREG
        mov.b   WREG, U1ADDR
        mov     #DEFAULT_STATE, W1
        btss    SR, #Z
        mov     #ADDRESS_STATE, W1
        mov     W1, usb_device_state
USBCtrlTrfInHandler_2:
        dec.b   ctrl_trf_state, WREG
        bra     nz, USBPrepareForNextSetupTrf
        rcall   USBCtrlTrfTxService
        mov     #(_USIE|_DTSEN), W0          ;0x88
        btss    ep0istat, #__DTS
        mov     #(_DAT1|_USIE|_DTSEN), W0    ;0xC8
        mov.b   WREG, ep0istat
        return
;*******************************************************************************
USBCtrlTrfRxService:
        mov.b   ep0ocnt, WREG
        mov     #ep0buf, W3
        mov     dPtr, W2
;**********************************************
ramcp:
        dec.b   W0, W0
        bra     n, ramcpreturn
        ze      W0, W0
        repeat  W0
        mov.b   [W3++], [W2++]
        bclr.b  usb_status, #MEM
ramcpreturn:
        return
;**********************************************
USBCtrlTrfTxService:
        mov     #(CDC_INT_EP_SIZE+1), W0
        cp.b    count                  ; count - (CDC_INT_EP_SIZE)
        mov     #CDC_INT_EP_SIZE, W0   ; moveLen =  CDC_INT_EP_SIZE
        bra     nn, SUB
LT:
        mov.b   count, WREG            ; moveLen = count
SUB:
        mov.b   WREG, ep0icnt
        sub.b   count                  ; count = count - moveLen
romcp:
        mov     #ep0buf, W2        ; DST ptr
        mov     dPtr, W3
        btss    usb_status, #MEM
        bra     romcp2
        mov     dPtr, W3   
        bra     ramcp
romcp1:
        tblrdl.b [W3++], W1
        mov.b   W1, [W2++]
romcp2:
        dec.b   W0, W0
        bra     c, romcp1
        mov     W3, dPtr
        return

;*******************************************************************************
USBCtrlEPServiceComplete:
        mov     #CDC_INT_EP_SIZE, W0
        mov.b   WREG, ep0ocnt
        mov     #ep0buf, W0
        mov     WREG, ep0oadr
        btsc    usb_status, #MUID_USB9
        bra     DATADIR_DEV_TO_HOST
        mov     #(_USIE|_BSTALL), W0       ;0x84
        mov.b   WREG, ep0ostat
        mov.b   WREG, ep0istat
        bra     USBCtrlEPServiceComplete_4
DATADIR_DEV_TO_HOST:
        btss    ep0buf, #7
        bra     DATADIR_HOST_TO_DEV
        mov.b   ep0buf+7, WREG
        bra     nz, DATADIR_DEV_TO_HOST_2
        mov.b   count, WREG
        cp.b    ep0buf+6                ; F - W
        bra     c, DATADIR_DEV_TO_HOST_2
        mov.b   ep0buf+6, WREG
        mov.b   WREG, count
DATADIR_DEV_TO_HOST_2:
        rcall   USBCtrlTrfTxService
        mov     #CTRL_TRF_TX, W0
        mov.b   WREG, ctrl_trf_state
        mov     #ep0buf, W0
        mov     WREG, ep0iadr
        mov     #_USIE,  W0                   ; 0x80
        mov.b   WREG, ep0ostat
        mov     #(_DAT1|_USIE|_DTSEN), W0   ;0xC8
        mov.b   WREG, ep0istat
        bra     USBCtrlEPServiceComplete_4
DATADIR_HOST_TO_DEV:
        mov     #CTRL_TRF_RX, W0
        mov.b   WREG, ctrl_trf_state
        clr.b   ep0icnt
        mov     #(_DAT1|_USIE|_DTSEN), W0     ;0xC8
        mov.b   WREG, ep0istat
        mov.b   WREG, ep0ostat
        mov     #ep0buf, W0
        mov     WREG, ep0oadr
USBCtrlEPServiceComplete_4:
        bclr    U1CON, #PKTDIS
        return

;*******************************************************************************
USBCheckStdRequest:
        mov     #0x60, W0
        and.b   ep0buf, WREG
        bra     nz, RETURN__
        bset    usb_status, #MUID_USB9
        mov.b   ep0buf+1, WREG
        cp.b    W0, #0x9
        bra     z, SET_CFG   ; 9(J) == SET_CFG
        cp.b    W0, #0x6
        bra     z, GET_DSC   ; 6(G) == GET_DSC
        cp.b    W0, #0x5     ; 5(F) == SET_ADDR
        bra     nz, RETURN__
SET_ADR:
        mov     #ADR_PENDING_STATE, W0
        mov.b   WREG, usb_device_state
SESSION_OWNER_USB9:
RETURN__:
        return
SET_CFG:
        mov     #ADDRESS_STATE, W0
        cp0.b   ep0buf+2
        bra     z, SET_CFG1
        mov     #CONFIGURED_STATE, W0       ; USB ACTIVE CFG != 0
SET_CFG1:
        mov.b   WREG, usb_device_state
        cp0.b   ep0buf+2
        bra     z, SESSION_OWNER_USB9
;*******************************************************************************
CDCInitEP:
        mov     #0x96, W0
        mov     W0, line_coding
        clr     line_coding+2
        clr     line_coding+4
        mov     #8, W0
        mov     W0,line_coding+6
        mov     #0x15, W0
        mov     W0, U1EP1
        mov     #0x1D, W0
        mov     W0, U1EP2
        clr.b   ep1istat          ; CDC notification end point not used
        mov     #8, W0
        mov.b   WREG, ep2ocnt
        mov     #1, W0
        mov.b   WREG, ep2icnt
        clr     ep2optr
        mov     #cdc_data_rx, W0
        mov     W0, ep2oadr
        mov     #cdc_data_tx, W0
        mov     W0, ep2iadr
        mov     #(_USIE|_DTSEN), W0  ;0x88
        mov.b   WREG, ep2ostat
        clr.b   ep2istat
        clr     cdc_data_tx
        mov     #1, W0
        rcall   TXU_SEND2
        bra     SESSION_OWNER_USB9
;*******************************************************************************
GET_DSC:
        mov     #0x80, W0
        cp.b    ep0buf
        bra     nz, GetDsc_6
        mov.b   ep0buf+3, WREG
        cp.b    W0, #0x3
        bra     z, GET_DSC_STR
        cp.b    W0, #0x2
        bra     z, GET_DSC_CFG
        cp.b    W0, #0x1
        bra     nz, GetDsc_6
GET_DSC_DEV:
        mov     #handle(device_dsc), W2
        mov     #0x12, W0
        bra     GetDsc_4
GET_DSC_CFG:
        mov     #handle(USB_CFG), W2
        mov     #0x43, W0
        BRA     GetDsc_4
GET_DSC_STR:
        clr     W0
        mov.b   ep0buf+2, WREG
        add     W0, W0, W0      ; 2*
        mov     #handle(USB_SD), W1
        addc    W0, W1, W1

        tblrdl  [W1], W2
        tblrdl.b [W2], W0
GetDsc_4:
        mov     W2, dPtr
        mov.b   WREG, count
GetDsc_6:
        return

USBCheckCdcRequest:
        mov.b   ep0buf, WREG     ; IF INTF & CLASS &  TO_DEVICE
        and.b   #0x7f, W0              ;
        xor.b   #0x21, W0
        bra     nz, return6
        mov     #1, W0
        subr.b  ep0buf+4, WREG            ; IF COMM_INTF || DATA_INTF
        bra     n, return6
        mov     #0x20, W0
        sub.b   ep0buf+1, WREG ; W0 = F - W0
        cp.b    W0, #0x0                    ; SET_LINE_CODING 0x20
        bra     z,SET_LINE_CODING
        cp.b    W0, #1                      ; GET_LINE_CODING 0x21
        bra     z,GET_LINE_CODING
        cp.b    W0, #2                      ; SET_CONTROL_LINE_STATE 0x22
        bra     nz, return6
SET_CONTROL_LINE_STATE:
SET_MUID_CDC:
SET_LINE_CODING:
        mov     #line_coding, W0
        mov     W0, dPtr
        bset    usb_status, #MUID_USB9
return6:
        return
       bra     SET_MUID_CDC
GET_LINE_CODING:
        mov     #7, w0
        mov.b   WREG, count
        bset.b  usb_status, #MEM
        bra     SET_MUID_CDC

