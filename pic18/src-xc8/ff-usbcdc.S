;**********************************************************************
;                                                                     *
;    Filename:      usbcdc.S                                          *
;    Date:          26.12.2021                                        *
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

#include <xc.inc>

; Buffer Descriptor Status Register Initialization Parameters
#define _BSTALL     0x04                ; Buffer Stall enable
#define _DTSEN      0x08                ; Data Toggle Synch enable
#define _INCDIS     0x10                ; Address increment disable
#define _KEN        0x20                ; SIE keeps buff descriptors enable
#define _DAT0       0x00                ; DATA0 packet expected next
#define _DAT1       0x40                ; DATA1 packet expected next
#define _DTSMASK    0x40                ; DTS Mask
#define _USIE       0x80                ; SIE owns buffer
#define _UCPU       0x00                ; CPU owns buffer

; Buffer Descriptor Status Register bits
#define __UOWN      7
#define __DTS       6
#define __DTSEN     3

#define CDC_EP_SIZE         8
#define CDC_BULK_OUT_EP_SIZE    8
#define CDC_BULK_IN_EP_SIZE     8

PSECT   udata_acs
count:          ds 1
moveLen        equ PRODL           ; local variable
dPtr:           ds 2
ep2icount:      ds 1
ep2itmo:        ds 1
ep2optr:        ds 1

; Control transfer session owner
usb_status:     ds 1
#define MEM         0       ; 0 = FLASH, 1 = RAM
#define MUID_USB9   1       ; Control transfer session owner

; Control Transfer States
ctrl_trf_state: ds 1
#define WAIT_SETUP          0
#define CTRL_TRF_TX         1
#define CTRL_TRF_RX         2

;*******************************************************************************
PSECT code
device_dsc:
        db   0x12    ; Size of this descriptor in bytes
        db   0x01    ; DEVICE descriptor type
        dw   0x0200  ; USB Spec Release Number in BCD format
        db   0x02    ; Class Code CDC device
        db   0x00    ; Subclass 
        db   0x00    ; Protocol
        db   0x08    ; EP0 packet size
        dw   U_VID   ; Vendor ID
        dw   U_PID   ; Product ID
        dw   0x0000  ; Device release Number in BCD
        db   0x00    ; Manufacturer string index
        db   0x01    ; Product string Index
        db   0x00    ; Device serial number string index
        db   0x01    ; Number of possible configurations

SD000:
        db   0x04    ; sizeof descriptor in bytes
        db   0x03    ; STRING descriptor type
        dw   0x0409  ; Language code

; Product string
SD001:
        db   0x0a    ; sizeof descriptor in bytes
        db   0x03    ; STRING descriptor type
        dw   'F','F','5','0'

USB_CFG:
        db   0x09     ; length
        db   0x02     ; configuration descriptor
        dw   0x003e   ; total length
        db   0x02     ; number of interfaces
        db   0x01     ; configuration id
        db   0x00     ; string descriptor index
        db   0x80     ; attributes (bus powered)
        db   0x32     ; maxpower 100 mA
        db   0x09     ; length
        db   0x04     ; interface descriptor
        db   0x00     ; interface 0
        db   0x00     ; alternate setting
        db   0x01     ; number of end points
        db   0x02     ; interface class code
        db   0x02     ; interface subclass
        db   0x01     ; interface protocol
        db   0x00     ; string descriptor index
        db   0x05,0x24,0x00,0x10,0x01 ; interface header FD  
        db   0x04,0x24,0x02,0x02      ; interface ACM FD
        db   0x05,0x24,0x01,0x00,0x01 ; interface Call Management FD
        db   0x07,0x05,0x81,0x03,0x08,0x00,0xff ; endpoint notification
        db   0x09,0x04,0x01,0x00,0x02,0x0a,0x00,0x00,0x00 ; interface data
        db   0x07,0x05,0x02,0x02,0x08,0x00,0x00 ; endpoint data out
        db   0x07,0x05,0x82,0x02,0x08,0x00,0x00 ; endpoint data in
        align 2
;*******************************************************************************
USBDriverService:
        banksel ep0ostat
        btfsc   ACTVIE        ; Is suspend mode ?
        rcall   USBWake
        btfsc   SUSPND
        return
        BTFSC   URSTIF 
        rcall   USBReset
        btfsc   IDLEIF
        rcall   USBSuspend
        btfss   BANKMASK(usb_device_state), 0, B
        return
USBDriverService_2:
        BTFSS   TRNIF 
        return
        RCALL   USBCtrlEPService
        BCF     TRNIF 
        return
;*******************************************************************************
USBSuspend:
        bsf     ACTVIE   ; Signal suspend mode to the driver
        bcf     IDLEIF
        bsf     SUSPND
        return
USBWake:
        btfss   ACTVIF
        return
        bcf     SUSPND
        bcf     ACTVIE
        bcf     ACTVIF
        return
USBReset:
        banksel UEIR
        CLRF    UIR, A
        CLRF    UADDR, B
        CLRF    UEP1, B
        CLRF    UEP2, B
        MOVLW   0x16
        MOVWF   UEP0, B
        banksel ep0ostat
USBReset_1:
        BTFSC   TRNIF 
        BRA     USBReset_1
        BCF     PKTDIS 
        RCALL   USBPrepareForNextSetupTrf
        MOVLW   DEFAULT_STATE
        MOVWF   BANKMASK(usb_device_state), B
        return
;*******************************************************************************
USBCtrlEPService:
        MOVF    USTAT, W, A
        andlw   0x7c
        bz      USBCtrlEPService_out_setup  ; OUT or SETUP token
        andlw   0x78
        bz      USBCtrlTrfInHandler         ; IN token
        return
USBCtrlEPService_out_setup:
        btfss   PKTDIS                     ; SETUP token ?
        bra     USBCtrlTrfOutHandler        ; OUT token
USBCtrlTrfSetupHandler:
        clrf    ctrl_trf_state, A              ; WAIT_SETUP
        clrf    usb_status, A                  ; MUID_NULL, FLASH
        clrf    count, A
        rcall   USBCheckStdRequest
        btfss   usb_status, 1, A
        RCALL   USBCheckCdcRequest
        BRA     USBCtrlEPServiceComplete
;*******************************************************************************
USBCtrlTrfOutHandler:
        movlw   CTRL_TRF_RX      ; 2
        subwf   ctrl_trf_state, W, A
        bnz     USBPrepareForNextSetupTrf
        rcall   USBCtrlTrfRxService
        movlw   0xc8
        btfsc   BANKMASK(ep0ostat), __DTS, B
        movlw   0x88
        movwf   BANKMASK(ep0ostat), B
        return

USBPrepareForNextSetupTrf:
        CLRF    ctrl_trf_state, A
        MOVLW   CDC_EP_SIZE
        MOVWF   BANKMASK(ep0ocnt), B
        MOVLW   low(ep0buf)
        MOVWF   BANKMASK(ep0oadr), B
        MOVLW   high(ep0buf)
        MOVWF   BANKMASK(ep0oadr+1), B
        MOVLW   0x88 ;_USIE|_DTSEN
        MOVWF   BANKMASK(ep0ostat), B
        CLRF    BANKMASK(ep0istat), B
        RETURN
;*******************************************************************************
USBCtrlTrfInHandler:
        MOVLW   ADR_PENDING_STATE
        SUBWF   BANKMASK(usb_device_state), W, B
        BNZ     USBCtrlTrfInHandler_2
        MOVF    BANKMASK(ep0buf+2), W, B
        banksel UADDR
        movwf   UADDR, B
        MOVLW   DEFAULT_STATE
        BTFSS   ZERO
        MOVLW   ADDRESS_STATE
        banksel ep0ostat
        MOVWF   BANKMASK(usb_device_state), B
USBCtrlTrfInHandler_2:
        DECF    ctrl_trf_state, W, A
        BNZ     USBPrepareForNextSetupTrf
        bra     USBCtrlTrfTxService
;*******************************************************************************
USBCtrlTrfRxService:
        movf    BANKMASK(ep0ocnt), W, B
        movwf   moveLen, A
        movff   FSR0L, PREINC2
        movff   FSR0H, PREINC2
        lfsr    0, ep0buf      ; SRC ptr
        movff   dPtr, FSR1L    ; DST ptr
        movff   dPtr+1, FSR1H
;**********************************************
ramcp:
        bra     ramcp2
ramcp1:
        movf    POSTINC0, W, A
        movwf   POSTINC1, A
ramcp2:
        decf    moveLen, F, A
        bc      ramcp1
        movff   POSTDEC2, FSR0H
        movff   POSTDEC2, FSR0L
        bcf     usb_status, 0, A
        return
ramcp_tx:
        movff   FSR0L, PREINC2
        movff   FSR0H, PREINC2
        movff   dPtr, FSR0L
        movff   dPtr+1, FSR0H
        bra     ramcp
;**********************************************
USBCtrlTrfTxService:
        movf    count, W, A
        sublw   CDC_EP_SIZE-1 ; CDC_EP_SIZE - count
        bnn     LT1
GTE:
        movlw   CDC_EP_SIZE
        movwf   moveLen, A    ; moveLen =  CDC_EP_SIZE
        bra     SUB
LT1:
        movf    count, W, A
        movwf   moveLen, A    ; moveLen = count
SUB:
        movwf   BANKMASK(ep0icnt), B
        subwf   count, F, A       ; count = count - moveLen
romcp:
        lfsr    1, ep0buf      ; DST ptr
        btfsc   usb_status, 0, A
        bra     ramcp_tx
        movff   dPtr, TBLPTRL
        movff   dPtr+1, TBLPTRH
        bra     romcp2
romcp1:
        TBLRD*+
        movf    TABLAT, W, A
        movwf   POSTINC1, A
romcp2:
        decf    moveLen, F, A
        bc      romcp1
        movff   TBLPTRL, dPtr
        movff   TBLPTRH, dPtr+1

        MOVLW   0x88;USIE|DTSEN;0x88
        BTFSS   BANKMASK(ep0istat), __DTS, B
        MOVLW   0xc8;_DAT1|_USIE|_DTSEN;0xC8
        MOVWF   BANKMASK(ep0istat), B
        return

;*******************************************************************************
USBCtrlEPServiceComplete:
        MOVLW   CDC_EP_SIZE
        MOVWF   BANKMASK(ep0ocnt), B
        MOVLW   low(ep0buf)
        MOVWF   BANKMASK(ep0oadr), B
        MOVLW   high(ep0buf)
        MOVWF   BANKMASK(ep0oadr+1), B
        MOVWF   BANKMASK(ep0iadr+1), B

        btfsc   usb_status, 1, A
        bra     DATADIR_CHECK
        MOVLW   0x84; _USIE|_BSTALL
        MOVWF   BANKMASK(ep0ostat), B
        MOVWF   BANKMASK(ep0istat), B
        BRA     USBCtrlEPServiceComplete_4
DATADIR_CHECK:
        BTFSS   BANKMASK(ep0buf), 7, B
        BRA     DATADIR_HOST_TO_DEV
        MOVF    BANKMASK(ep0buf+7), W, B
        bnz     DATADIR_DEV_TO_HOST
        MOVF    count, W, A
        SUBWF   BANKMASK(ep0buf+6), W, B  ; F - W
        BC      DATADIR_DEV_TO_HOST
        MOVF    BANKMASK(ep0buf+6), W, B
        MOVWF   count, A
DATADIR_DEV_TO_HOST:
        RCALL   USBCtrlTrfTxService
        MOVLW   CTRL_TRF_TX
        MOVWF   ctrl_trf_state, A
        MOVLW   low(ep0buf)
        MOVWF   BANKMASK(ep0iadr), B
        MOVLW   0x80; _USIE; 0x80
        MOVWF   BANKMASK(ep0ostat), B
        MOVLW   0xc8;_DAT1|_USIE|_DTSEN;0xC8
        MOVWF   BANKMASK(ep0istat), B
        BRA     USBCtrlEPServiceComplete_4
DATADIR_HOST_TO_DEV:
        MOVLW   CTRL_TRF_RX
        MOVWF   ctrl_trf_state, A
        CLRF    BANKMASK(ep0icnt), B
        MOVLW   0xc8;_DAT1|_USIE|_DTSEN;0xC8
        MOVWF   BANKMASK(ep0istat), B
        MOVWF   BANKMASK(ep0ostat), B
        MOVLW   low(ep0buf)
        MOVWF   BANKMASK(ep0oadr), B
USBCtrlEPServiceComplete_4:
        BCF     PKTDIS 
        RETURN

;*******************************************************************************
USBCheckStdRequest:
        MOVLW   0x60
        ANDWF   BANKMASK(ep0buf), W, B
        BNZ     RETURN__
        MOVF    BANKMASK(ep0buf+1), W, B
        XORLW   0x9
        BZ      SET_CFG   ; 9(J) == SET_CFG
        XORLW   0xF
        BZ      GET_DSC   ; 6(G) == GET_DSC
        XORLW   0x3       ; 5(F) == SET_ADDR
        BNZ     RETURN__
SET_ADR:
        MOVLW   ADR_PENDING_STATE
        MOVWF   BANKMASK(usb_device_state), B
SESSION_OWNER_USB9:
        bsf     usb_status, 1, A
RETURN__:
        return
SET_CFG:
        MOVLW   ADDRESS_STATE
        TSTFSZ  BANKMASK(ep0buf+2), B
        MOVLW   CONFIGURED_STATE      ; USB ACTIVE CFG != 0
        MOVWF   BANKMASK(usb_device_state), B
        MOVF    BANKMASK(ep0buf+2), W, B
        BZ      SESSION_OWNER_USB9
;*******************************************************************************
CDCInitEP:
        banksel UEP2
        MOVLW   0x1A
        MOVWF   UEP1, B
        MOVLW   0x1E
        MOVWF   UEP2, B
        banksel ep0ostat
        CLRF    BANKMASK(ep1istat), B ; CDC notification end point not used
        MOVLW   8
        MOVWF   BANKMASK(ep2ocnt), B
        MOVWF   BANKMASK(ep2icnt), B
        MOVLW   low(cdc_data_rx)
        MOVWF   BANKMASK(ep2oadr), B
        MOVLW   low(cdc_data_tx)
        MOVWF   BANKMASK(ep2iadr), B
        MOVLW   high(cdc_data_rx)
        MOVWF   BANKMASK(ep2oadr+1), B
        MOVWF   BANKMASK(ep2iadr+1), B
        MOVLW   0x88;_USIE|_DTSEN
        MOVWF   BANKMASK(ep2ostat), B
        CLRF    BANKMASK(ep2istat), B
        clrf    ep2optr, A
        clrf    BANKMASK(cdc_data_tx), B
        clrf    ep2icount, A
        movlw   1
        call    TX0_SEND2
        bra     SESSION_OWNER_USB9
;*******************************************************************************
GET_DSC:
        MOVLW   0x80
        SUBWF   BANKMASK(ep0buf), W, B
        BNZ     GetDsc_6
        MOVF    BANKMASK(ep0buf+3), W, B
        XORLW   0x3
        BZ      GET_DSC_STR
        XORLW   0x1
        BZ      GET_DSC_CFG
        XORLW   0x3
        BNZ     GetDsc_6
GET_DSC_DEV:
        MOVLW   low(device_dsc)
        MOVWF   dPtr, A
        MOVLW   high(device_dsc)
        MOVWF   dPtr+1, A
        MOVLW   0x12
        BRA     GetDsc_4
GET_DSC_CFG:
        MOVLW   low(USB_CFG)
        MOVWF   dPtr, A
        MOVLW   high(USB_CFG)
        MOVWF   dPtr+1, A
        MOVLW   0x43
        BRA     GetDsc_4
GET_DSC_STR:
        tstfsz  BANKMASK(ep0buf+2), B
        bra     GET01
GET00:
        MOVLW   low(SD000)
        MOVWF   dPtr, A
        MOVLW   high(SD000)
        MOVWF   dPtr+1, A
        MOVLW   0x4
        BRA     GetDsc_4  
GET01:
        MOVLW   low(SD001)
        MOVWF   dPtr, A
        MOVLW   high(SD001)
        MOVWF   dPtr+1, A
        MOVLW   0xa
GetDsc_4:
        MOVWF   count, A
GetDsc_6:
        bsf     usb_status, 1, A
        RETURN

USBCheckCdcRequest:
        movf    BANKMASK(ep0buf), W, B     ; IF INTF & CLASS &  TO_DEVICE
        andlw   0x7f                  ;
        xorlw   0x21
        bnz     return6
        movf    BANKMASK(ep0buf+4), W, B
        sublw   1       ;1 - w        ; IF COMM_INTF || DATA_INTF
        bn      return6
        MOVF    BANKMASK(ep0buf+1), W, B
        addlw   -21                   ; GET_LINE_CODING 0x21
        bz      GET_LINE_CODING
        addlw   -1                    ; SET_CONTROL_LINE_STATE 0x22
        bz      SET_CONTROL_LINE_STATE
        addlw   2                     ; SET_LINE_CODING 0x20
        bnz     return6
SET_LINE_CODING:
        movlw   low(line_coding)
        movwf   dPtr, A
        movlw   high(line_coding)
        movwf   dPtr+1, A
SET_CONTROL_LINE_STATE:
SET_MUID_CDC:
        bsf     usb_status, 1, A
return6:
        return
GET_LINE_CODING:
        movlw   7
        movwf   count, A
        bsf     usb_status, 0, A
        bra     SET_MUID_CDC
dpusb:
        