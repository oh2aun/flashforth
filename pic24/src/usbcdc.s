;**********************************************************************
;                                                                     *
;    Filename:      usbcdc.s   NOT READY YET                          *
;    Date:          30.11.2018                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     *
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2018  Mikael Nordman
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
.equ U_PID, 0xfaf0  ; Product ID used for testing FlashForth
.equ U_VID, 0xfaf0  ; Vendor ID used for testing FlashForth

.include "p24Fxxxx.inc"

; Buffer Descriptor Status Register Initialization Parameters
.equ _BSTALL,     0x04                ; Buffer Stall enable
.equ _DTSEN,      0x08                ; Data Toggle Synch enable
;.equ _INCDIS,     0x10                ; Address increment disable
;.equ _KEN,        0x20                ; SIE keeps buff descriptors enable
.equ _DAT0,       0x00                ; DATA0 packet expected next
.equ _DAT1,       0x40                ; DATA1 packet expected next
.equ _DTSMASK,    0x40                ; DTS Mask
.equ _USIE,       0x80                ; SIE owns buffer
.equ _UCPU,       0x00                ; CPU owns buffer

; Buffer Descriptor Status Register bits
.equ __UOWN,  7
.equ __DTS,   6
.equ __DTSEN, 3
    
; Flags
.equ __URSTIF, 0x01
.equ __IDLEIF, 0x10
.equ __ACTVIF, 0x10
.equ __TRNIF,  0x08
    
.equ CDC_INT_EP_SIZE,         8
.equ CDC_BULK_OUT_EP_SIZE,    1
.equ CDC_BULK_IN_EP_SIZE,     1

.global USBInit
.global USBDriverService
.global cdc_data_tx
.global cdc_data_rx
.global usb_device_state
.global ep3istat
.global ep3icnt
.global ep3ostat
.global ep3ocnt
.extern asmemit

.bss 
.section buf1,bss,address(0x1200)
;.org 0x800   ; Align to 512 byte / 256 word boundary
;.align 512
bdt_base:
ep0ocnt:  	.space 1
ep0ostat:	.space 1
ep0oadr:	.space 2
ep0icnt:  	.space 1
ep0istat:	.space 1
ep0iadr:	.space 2

setupPkt:       .space CDC_INT_EP_SIZE ;8

ep2ocnt:  	.space 1
ep2ostat:	.space 1
ep2oadr:	.space 2
ep2icnt:  	.space 1
ep2istat:	.space 1
ep2iadr:	.space 2
ep3ocnt:  	.space 1
ep3ostat:	.space 1
ep3oadr:	.space 2
ep3icnt:  	.space 1
ep3istat:	.space 1
ep3iadr:	.space 2

; Control transfer session owner
ctrl_trf_session_owner: .space 1
.equ MUID_NULL,               0
.equ MUID_USB9,               1

; Control Transfer States
ctrl_trf_state: .space 1
.equ WAIT_SETUP,          0
.equ CTRL_TRF_TX,         1
.equ CTRL_TRF_RX,         2

; USB Device States - To be used with usb_device_state
usb_device_state: .space 1
.equ DETACHED_STATE,          0
.equ ATTACHED_STATE,          0
.equ POWERED_STATE,           0
.equ DEFAULT_STATE,           1
.equ ADR_PENDING_STATE,       3
.equ ADDRESS_STATE,           0x07
.equ ADDRESS_STATE2,          0x83  ; Shifted to h'07'
.equ CONFIGURED_STATE,        0x0f
            .align 8
ep0buf:	    .space CDC_INT_EP_SIZE ;8
moveLen:    .space 2               ; local variable
count:      .space 2
            .align 2
pSrc:       .space 2
pDst:       .space 2
cdc_data_rx:.space 2
cdc_data_tx:.space 2
line_coding:.space 8
mem:        .space 2           ; 0 = flash ; 0xff = ram


;*******************************************************************************
.text
device_dsc:
        .word   0x0112       ; Size of this descriptor in bytes
                             ; DEVICE descriptor type
        .word      0x0200    ; USB Spec Release Number in BCD format
        .word      0x0002;   ; Class Code CDC device
                             ; Subclass code = 00
        .word      0x0800    ; Protocol code = 00
                             ; EP0 packet size = 8
        .word      U_VID     ; Vendor ID
        .word      U_PID     ; Product ID
        .word      0x0000    ; Device release Number in BCD
        .word      0x0201    ; Manufacturer string index
                             ; Product string Index
        .word      0x0100    ; Device serial number string index
                             ; Number of possible configurations

SD000:
        .word      0x0304    ; sizeof descriptor in bytes
                             ; STRING descriptor type
        .word      0x0409

; Manufacturer string
SD001:
        .word      0x0304    ; sizeof descriptor in bytes
                          ; STRING descriptor type
        .word      'M'

; Product string
SD002:
        .word      0x030a    ; sizeof descriptor in bytes
                          ; STRING descriptor type
        .word      'F','F','5','0'

USB_SD:
        .word      SD000
        .word      SD001
        .word      SD002
USB_CFG:
        .byte  0x09,0x02,0x43,0x00,0x02,0x01,0x00,0x80,0x32,0x09,0x04,0x00,0x00,0x01,0x02,0x02
        .byte  0x01,0x00,0x05,0x24,0x00,0x10,0x01,0x04,0x24,0x02,0x02,0x05,0x24,0x06,0x00,0x01
        .byte  0x05,0x24,0x01,0x00,0x01,0x07,0x05,0x82,0x03,0x08,0x00,0x02,0x09,0x04,0x01,0x00
        .byte  0x02,0x0a,0x00,0x00,0x00,0x07,0x05,0x03,0x02,0x01,0x00,0x00,0x07,0x05,0x83,0x02
        .byte  0x02,0x00,0x00

emit:
        btss    U1STA, #TRMT
        bra     emit
        mov     W4, U1TXREG
        return
USBInit:
	bclr	IEC5, #6	    ; Disable master USB interrupts
	clr	U1IE                ; Disable USB interrupts
	clr	U1EIE               ; Disable USB interrupts
	setm	U1IR
	setm	U1EIR
	clr	U1EP0		    ; r0 Clear all of the endpoint control registers
	clr	U1EP1               ; r0
	clr	U1EP2               ; r0
	clr	U1EP3               ; r0
	clr	U1CNFG1             ; r0
	clr	U1CNFG2             ; r0
	bclr	U1OTGCON, #2	    ; r0 Only USB device mode
	bset	U1PWRC, #0	    ; Power up the USB module
	mov	#bdt_base, W0
	lsr	W0, #8, W0
	mov	W0, U1BDTP1	    ; Set the BDT base address
	clr	ep0ocnt
	clr	ep2ocnt
	clr	ep3ocnt
	clr	U1ADDR              ; r0; Clear usb device address 
	bclr	U1CON, #5	    ; Enable packet processing
	bclr	U1CON, #1	    ; Ping pong puffer pointers are not reset
        bset    U1CON, #0
USBInit_1:
	btst	U1IR, #3	    ; TRNIF ?
	bra	z, USBInit_2
	mov	#8, W0              ; TRNIF
	mov	W0, U1IR            ; clear TRNIF
	; clr some variables maybe
	bra	USBInit_1
USBInit_2:
	mov	#0xD, W0
	mov	W0, U1EP0	    ; Ep0 is control endpoint
	mov	#setupPkt, W0
	mov	W0, ep0oadr
	mov	#CDC_INT_EP_SIZE, W0
	mov.b	WREG, ep0ocnt
	mov	#(_DAT0|_BSTALL), W0
	mov.b	WREG, ep0ostat
	bset.b	ep0ostat, #__UOWN
	clr	usb_device_state
	return

;*******************************************************************************
USBDriverService:
        btsc    U1OTGIE, #ACTVIE
        rcall   USBWake
        btsc    U1PWRC, #USUSPND
        return
        btsc    U1IR, #URSTIF
        rcall   USBReset
        btsc    U1IR, #IDLEIF
        return; rcall   USBSuspend
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
        mov     #'s', W4
        rcall   emit
        mov	#__IDLEIF, W0
        btss    U1OTGIE, #ACTVIE
	mov	W0, U1IR         ; clear IDLEIF
        bset    U1PWRC, #USUSPND
        return
USBWake:
        mov     #'w', W4
        rcall   emit
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
	clr	U1EP2
	clr	U1EP3
USBReset_1:
	btst	U1IR, #3	    ; TRNIF ?
	bra	z, USBReset_2
	mov	#8, W0              ; TRNIF
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
        mov     #'r', W4
        rcall   emit
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
        mov.b   ep0ostat, WREG                   ; USTAT == EP00_OUT
        and.b   #0x3C, W0
        xor.b   #0x34, W0                      ; 0x34 (0xd) SETUP_TOKEN
        bra     z, USBCtrlTrfSetupHandler
        bra     USBCtrlTrfOutHandler
USBCtrlTrfSetupHandler:
        clr.b   ctrl_trf_state                 ; WAIT_SETUP
        clr.b   ctrl_trf_session_owner         ; MUID_NULL
        clr.b   count
        rcall   USBCheckStdRequest
       ; btss    ctrl_trf_session_owner, #2
        rcall   USBCheckCdcRequest
        bra     USBCtrlEPServiceComplete
;*******************************************************************************
USBCtrlTrfOutHandler:
;        mov     #'o', W4
;        rcall   emit
        mov     CTRL_TRF_RX, W0      ; 2
        cp.b    ctrl_trf_state
        bra     nz, USBPrepareForNextSetupTrf
        rcall   USBCtrlTrfRxService
        mov     #0xc8, W0
        btsc.b  ep0ostat, #__DTS
        mov     #0x88, W0
        mov.b   WREG, ep0ostat
        return

USBPrepareForNextSetupTrf:
;        mov     #'n', W4
;        rcall   emit
        clr.b    ctrl_trf_state
        mov     #CDC_INT_EP_SIZE, W0
        mov.b   WREG, ep0ocnt
        mov     #setupPkt, W0
        mov     W0, ep0oadr
        mov     #(_USIE|_DTSEN), W0      ;0x88
        mov.b   WREG, ep0ostat
        clr.b   ep0istat
        return
;*******************************************************************************
USBCtrlTrfInHandler:
;        mov     #'i', W4
;        rcall   emit
        mov     #ADR_PENDING_STATE, W0
        cp.b    usb_device_state
        bra     nz, USBCtrlTrfInHandler_2
        mov.b   setupPkt+2, WREG
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
        mov.b   WREG, moveLen
        mov     #ep0buf, W3
        mov     pDst, W2
;**********************************************
ramcp:
        dec.b   moveLen, WREG
        bra     n, ramcpreturn
        se      W0, W0
        repeat  W0
        mov.b   [W3++], [W2++]
        clr.b   mem
ramcpreturn:
        return
ramcp_tx:
        mov     pSrc, W3
        bra     ramcp
;**********************************************
USBCtrlTrfTxService:
        mov     #(CDC_INT_EP_SIZE+1), W0
        cp.b    count                  ; count - (CDC_INT_EP_SIZE)
        bra     n, LT
GTE:
        mov     #CDC_INT_EP_SIZE, W0
        mov.b   WREG, moveLen          ; moveLen =  CDC_INT_EP_SIZE
        bra     SUB
LT:
        mov.b   count, WREG
        mov.b   WREG, moveLen          ; moveLen = count
SUB:
        mov.b   moveLen, WREG
        mov.b   WREG, ep0icnt
        sub.b   count                  ; count = count - moveLen
romcp:
        mov     #ep0buf, W2        ; DST ptr
        mov     pSrc, W3
        cp0.b   mem
        bra     nz, ramcp_tx
        bra     romcp2
romcp1:
        tblrdl.b [W3++], W0
        mov.b   W0, [W2++]
romcp2:
        dec.b   moveLen
        bra     c, romcp1
        mov     W3, pSrc
        return

;*******************************************************************************
USBCtrlEPServiceComplete:
        mov     #CDC_INT_EP_SIZE, W0
        mov.b   WREG, ep0ocnt
        mov     #setupPkt, W0
        mov     WREG, ep0oadr
        btsc    ctrl_trf_session_owner, #2
        bra     DATADIR_DEV_TO_HOST
        mov     #(_USIE|_BSTALL), W0       ;0x84
        mov.b   WREG, ep0ostat
        mov.b   WREG, ep0istat
        bra     USBCtrlEPServiceComplete_4
DATADIR_DEV_TO_HOST:
        btss    setupPkt, #7
        bra     DATADIR_HOST_TO_DEV
        mov.b   setupPkt+7, WREG
        bra     nz, DATADIR_DEV_TO_HOST_2
        mov.b   count, WREG
        cp.b    setupPkt+6                ; F - W
        bra     c, DATADIR_DEV_TO_HOST_2
        mov.b   setupPkt+6, WREG
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
        and.b   setupPkt, WREG
        bra     nz, RETURN__
        bset    ctrl_trf_session_owner, #2
;        mov     #'s', W4
;        rcall   emit
        mov.b   setupPkt+1, WREG
        cp.b    W0, #0x9
        bra     z, SET_CFG   ; 9(J) == SET_CFG
        cp.b    W0, #0x6
        bra     z, GET_DSC   ; 6(G) == GET_DSC
        cp.b    W0, #0x5     ; 5(F) == SET_ADDR
        bra     nz, RETURN__
SET_ADR:
        mov     #'A', W4
        rcall   emit
        mov     #ADR_PENDING_STATE, W0
        mov.b   WREG, usb_device_state
SESSION_OWNER_USB9:
RETURN__:
        return
SET_CFG:
        mov     #'C', W4
        rcall   emit
        mov     #ADDRESS_STATE, W0
        cp0.b   setupPkt+2
        bra     z, SET_CFG1
        mov     #CONFIGURED_STATE, W0       ; USB ACTIVE CFG != 0
SET_CFG1:
        mov.b   WREG, usb_device_state
        cp0.b   setupPkt+2
        bra     z, SESSION_OWNER_USB9
;*******************************************************************************
CDCInitEP:
        mov     #'I', W4
        rcall   emit
        mov     #0x15, W0
        mov     W0, U1EP2
        mov     #0x1D, W0
        mov     W0, U1EP3
        clr.b   ep2istat          ; CDC notification end point not used
        mov     #1, W0
        mov.b   WREG, ep3ocnt
        mov.b   WREG, ep3icnt
        mov     #cdc_data_rx, W0
        mov     W0, ep3oadr
        mov     #cdc_data_tx, W0
        mov     W0, ep3iadr
        mov     #(_USIE|_DTSEN), W0  ;0x88
        mov.b   WREG, ep3ostat
        clr.b   ep3istat
        bra     SESSION_OWNER_USB9
;*******************************************************************************
GET_DSC:
        mov     #0x80, W0
        cp.b    setupPkt
        bra     nz, GetDsc_6
        mov.b   setupPkt+3, WREG
        cp.b    W0, #0x3
        bra     z, GET_DSC_STR
        cp.b    W0, #0x2
        bra     z, GET_DSC_CFG
        cp.b    W0, #0x1
        bra     nz, GetDsc_6
GET_DSC_DEV:
        mov     #'1', W4
        rcall   emit
        mov     #handle(device_dsc), W0
        mov     W0, pSrc
        mov     #0x12, W0
        bra     GetDsc_4
GET_DSC_CFG:
        mov     #'2', W4
        rcall   emit
        mov     #handle(USB_CFG), W0
        mov     W0, pSrc
        mov     #0x43, W0
        BRA     GetDsc_4
GET_DSC_STR:
        mov     #'3', W4
        rcall   emit
        clr     W0
        mov.b   setupPkt+2, WREG
        add     W0, W0, W0      ; 2*
        mov     #handle(USB_SD), W1
        addc    W0, W1, W1

        tblrdl  [W1], W2
        mov     W2, pSrc
        tblrdl.b [W2], W0
GetDsc_4:
        mov     W0, count
GetDsc_6:
        return

USBCheckCdcRequest:
        mov.b   setupPkt, WREG     ; IF INTF & CLASS &  TO_DEVICE
        and.b   #0x7f, W0              ;
        xor.b   #0x21, W0
        bra     nz, return6
        mov     #1, W0
        subr.b  setupPkt+4, WREG            ; IF COMM_INTF || DATA_INTF
        bra     n, return6
        mov.b   setupPkt+1, WREG
        sub.b   0x20                        ; SET_LINE_CODING 0x20
        bra     z,SET_LINE_CODING
        sub.b   #1, W0                      ;  GET_LINE_CODING 0x21
        bra     z,GET_LINE_CODING
        sub.b   #1, W0                      ; SET_CONTROL_LINE_STATE 0x22
        bra     nz, return6
SET_CONTROL_LINE_STATE:
        mov     #'4', W4
        rcall   emit
SET_MUID_CDC:
        bset    ctrl_trf_session_owner, #2
return6:
        return
SET_LINE_CODING:
        mov     #'5', W4
        rcall   emit
        mov     #line_coding, W0
        mov     W0, pDst
        bra     SET_MUID_CDC
GET_LINE_CODING:
        mov     #'6', W4
        rcall   emit
        mov     #line_coding, W0
        mov     W0, pSrc
        mov     #7, w0
        mov.b   WREG, count
        setm.b  mem
        bra     SET_MUID_CDC
.end
