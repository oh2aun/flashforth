;**********************************************************************
;                                                                     *
;    Filename:      usbcdc.asm                                        *
;    Date:          27.01.2019                                        *
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
; NOTE!
; Vendor ID 0xfaf0 and Product ID 0xfaf0 are used as temporary IDs for TESTING ONLY
; For a commercial product you MUST obtain your own Vendor ID and Product ID !
#define U_PID 0xfaf0  ; Product ID used for testing FlashForth
#define U_VID 0xfaf0  ; Vendor ID used for testing FlashForth


#ifdef __18F2455
    #include p18f2455.inc
#endif
#ifdef __18F2550
    #include p18f2550.inc
#endif
#ifdef __18F4455
    #include p18f4455.inc
#endif
#ifdef __18F4550
    #include p18f4550.inc
#endif
#ifdef __18F2458
    #include p18f2458.inc
#endif
#ifdef __18F2553
    #include p18f2553.inc
#endif
#ifdef __18F4458
    #include p18f4458.inc
endif
#ifdef __18F4553
    #include p18f4553.inc
#endif
#ifdef __18F14K50
    #include p18f14k50.inc
#endif
#ifdef __18LF14K50
    #include p18lf14k50.inc
#endif
#ifdef __18F24K50
    #include p18f24k50.inc
#endif
#ifdef __18F25K50
    #include p18f25k50.inc
#endif
#ifdef __18F45K50
    #include p18f45k50.inc
#endif

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

        global USBDriverService
        global cdc_data_tx
        global cdc_data_rx
        global usb_device_state
        global ep2istat
        global ep2icnt
        global ep2ostat
        global ep2ocnt
        global ep2itmo
        global ep2icount
        global ep2optr
        extern TX0_SEND2

USB_EP udata
ep0ostat	res 1
ep0ocnt  	res 1
ep0oadr		res 2
ep0istat	res 1
ep0icnt  	res 1
ep0iadr		res 2

ep1ostat	res 1
ep1ocnt  	res 1
ep1oadr		res 2
ep1istat	res 1
ep1icnt  	res 1
ep1iadr		res 2

ep2ostat	res 1
ep2ocnt  	res 1
ep2oadr		res 2
ep2istat	res 1
ep2icnt  	res 1
ep2iadr		res 2


FORTH_VARS      udata_acs
count           res 1
moveLen         equ PRODL           ; local variable
dPtr            res 2
ep2icount       res 1
ep2itmo         res 1
ep2optr         res 1

; Control transfer session owner
usb_status      res 1
#define MEM         0       ; 0 = FLASH, 1 = RAM
#define MUID_USB9   1       ; Control transfer session owner

; Control Transfer States
ctrl_trf_state res 1
#define WAIT_SETUP          0
#define CTRL_TRF_TX         1
#define CTRL_TRF_RX         2

USB_VARS udata

; USB Device States - To be used with usb_device_state
usb_device_state res 1
#define DETACHED_STATE          0
#define ATTACHED_STATE          0
#define POWERED_STATE           0
#define DEFAULT_STATE           1
#define ADR_PENDING_STATE       3
#define ADDRESS_STATE           h'07'
#define ADDRESS_STATE2          h'83'  ; Shifted to h'07'
#define CONFIGURED_STATE        h'0f'

; BUFFERS
ep0buf      res 8
cdc_data_rx res 8
cdc_data_tx res 8
line_coding res 7

;*******************************************************************************
FF_CODE code_pack
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

USB_SD:
        dw   SD000
        dw   SD001

USB_CFG:
        db   0x09     ; length
        db   0x02     ; configuration descriptor
        dw   0x0043   ; total length
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
        db   0x05,0x24,0x06,0x00,0x01 ; interface Union FD
        db   0x05,0x24,0x01,0x00,0x01 ; interface Call Management FD
        db   0x07,0x05,0x81,0x03,0x08,0x00,0x10 ; endpoint notification
        db   0x09,0x04,0x01,0x00,0x02,0x0a,0x00,0x00,0x00 ; interface data
        db   0x07,0x05,0x02,0x02,0x08,0x00,0x00 ; endpoint data out
        db   0x07,0x05,0x82,0x02,0x08,0x00,0x00 ; endpoint data in
        code
;*******************************************************************************
USBDriverService:
        banksel ep0ostat
        btfsc   UIE, ACTVIE
        rcall   USBWake
        btfsc   UCON, SUSPND
        return
        BTFSC   UIR, URSTIF 
        rcall   USBReset
        btfsc   UIR, IDLEIF
        rcall   USBSuspend
        btfss   usb_device_state, 0, BANKED
        return
USBDriverService_2:
        BTFSS   UIR, TRNIF 
        return
        RCALL   USBCtrlEPService
        BCF     UIR, TRNIF 
        return
;*******************************************************************************
USBSuspend:
        bsf     UIE, ACTVIE
        bcf     UIR, IDLEIF
        bsf     UCON, SUSPND
        return
USBWake:
        btfss   UIR, ACTVIF
        return
        bcf     UCON, SUSPND
        bcf     UIE, ACTVIE
        bcf     UIR, ACTVIF
        return
USBReset:
        banksel UEIR
        CLRF    UIR 
        CLRF    UADDR, BANKED
        CLRF    UEP1, BANKED
        CLRF    UEP2, BANKED
        MOVLW   0x16
        MOVWF   UEP0, BANKED
        banksel ep0ostat
USBReset_1:
        BTFSC   UIR, TRNIF 
        BRA     USBReset_1
        BCF     UCON, PKTDIS 
        RCALL   USBPrepareForNextSetupTrf
        MOVLW   DEFAULT_STATE
        MOVWF   usb_device_state, BANKED
        return
return1:
        return
;*******************************************************************************
USBCtrlEPService:
        banksel ep0ostat
        MOVF    USTAT, W 
        andlw   0x7c
        bz      USBCtrlEPService_out        ; USTAT == EP00_OUT
        andlw   0x78
        bnz     return1
        bra     USBCtrlTrfInHandler
USBCtrlEPService_out:
        MOVF    ep0ostat, W, BANKED         ; USTAT == EP00_OUT
        andlw   0x3C
        xorlw   0x34; _KEN|_INCDIS|_BSTALL        ; 0x34  SETUP_TOKEN
        bz      USBCtrlTrfSetupHandler
        bra     USBCtrlTrfOutHandler
USBCtrlTrfSetupHandler:
        clrf    ctrl_trf_state,             ; WAIT_SETUP
        clrf    usb_status                  ; MUID_NULL, FLASH
        clrf    count
        rcall   USBCheckStdRequest
        btfss   usb_status, MUID_USB9
        RCALL   USBCheckCdcRequest
        BRA     USBCtrlEPServiceComplete
;*******************************************************************************
USBCtrlTrfOutHandler:
        movlw   CTRL_TRF_RX      ; 2
        subwf   ctrl_trf_state, W
        bnz     USBPrepareForNextSetupTrf
        rcall   USBCtrlTrfRxService
        movlw   0xc8
        btfsc   ep0ostat, __DTS, BANKED
        movlw   0x88
        movwf   ep0ostat, BANKED
        return

USBPrepareForNextSetupTrf:
        CLRF    ctrl_trf_state
        MOVLW   CDC_EP_SIZE
        MOVWF   ep0ocnt, BANKED
        MOVLW   low(ep0buf)
        MOVWF   ep0oadr, BANKED
        MOVLW   high(ep0buf)
        MOVWF   ep0oadr+1, BANKED
        MOVLW   _USIE|_DTSEN      ;0x88
        MOVWF   ep0ostat, BANKED
        CLRF    ep0istat, BANKED
        RETURN
;*******************************************************************************
USBCtrlTrfInHandler:
        MOVLW   ADR_PENDING_STATE
        SUBWF   usb_device_state, W, BANKED
        BNZ     USBCtrlTrfInHandler_2
        MOVF    ep0buf+2, W, BANKED
        banksel UADDR
        movwf   UADDR, BANKED
        MOVLW   DEFAULT_STATE
        BTFSS   STATUS, Z
        MOVLW   ADDRESS_STATE
        banksel ep0ostat
        MOVWF   usb_device_state, BANKED
USBCtrlTrfInHandler_2:
        DECF    ctrl_trf_state, W
        BNZ     USBPrepareForNextSetupTrf
        RCALL   USBCtrlTrfTxService
        MOVLW   _USIE|_DTSEN;0x88
        BTFSS   ep0istat, __DTS, BANKED
        MOVLW   _DAT1|_USIE|_DTSEN;0xC8
        MOVWF   ep0istat, BANKED
        return
;*******************************************************************************
USBCtrlTrfRxService:
        movf    ep0ocnt, W, BANKED
        movwf   moveLen
        movff   FSR0L, PREINC2
        movff   FSR0H, PREINC2
        lfsr    0, ep0buf   ; SRC ptr
        movff   dPtr, FSR1L   ; DST ptr
        movff   dPtr+1, FSR1H
;**********************************************
ramcp:
        bra     ramcp2
ramcp1:
        movf    POSTINC0, W
        movwf   POSTINC1
ramcp2:
        decf    moveLen, F
        bc      ramcp1
        movff   POSTDEC2, FSR0H
        movff   POSTDEC2, FSR0L
        bcf     usb_status, MEM
        return
ramcp_tx:
        movff   FSR0L, PREINC2
        movff   FSR0H, PREINC2
        movff   dPtr, FSR0L
        movff   dPtr+1, FSR0H
        bra     ramcp
;**********************************************
USBCtrlTrfTxService:
        movf    count, W
        sublw   CDC_EP_SIZE-1 ; CDC_EP_SIZE - count
        bnn     LT
GTE:
        movlw   CDC_EP_SIZE
        movwf   moveLen        ; moveLen =  CDC_EP_SIZE
        bra     SUB
LT:
        movf    count, W
        movwf   moveLen        ; moveLen = count
SUB:
        movwf   ep0icnt, BANKED
        subwf   count, F       ; count = count - moveLen
romcp:
        lfsr    1, ep0buf;         ; DST ptr
        btfsc   usb_status, MEM
        bra     ramcp_tx
        movff   dPtr, TBLPTRL
        movff   dPtr+1, TBLPTRH
        bra     romcp2
romcp1:
        TBLRD*+
        movf    TABLAT, W
        movwf   POSTINC1
romcp2:
        decf    moveLen, F
        bc      romcp1
        movff   TBLPTRL, dPtr
        movff   TBLPTRH, dPtr+1
        return

;*******************************************************************************
USBCtrlEPServiceComplete:
        MOVLW   CDC_EP_SIZE
        MOVWF   ep0ocnt, BANKED
        MOVLW   low(ep0buf)
        MOVWF   ep0oadr, BANKED
        MOVLW   high(ep0buf)
        MOVWF   ep0oadr+1, BANKED
        MOVWF   ep0iadr+1, BANKED

        btfsc   usb_status, MUID_USB9
        bra     DATADIR_CHECK
        MOVLW   _USIE|_BSTALL ;0x84
        MOVWF   ep0ostat, BANKED
        MOVWF   ep0istat, BANKED
        BRA     USBCtrlEPServiceComplete_4
DATADIR_CHECK:
        BTFSS   ep0buf, 7, BANKED
        BRA     DATADIR_HOST_TO_DEV
        MOVF    ep0buf+7, W, BANKED
        bnz     DATADIR_DEV_TO_HOST
        MOVF    count, W
        SUBWF   ep0buf+6, W, BANKED  ; F - W
        BC      DATADIR_DEV_TO_HOST
        MOVF    ep0buf+6, W, BANKED
        MOVWF   count
DATADIR_DEV_TO_HOST:
        RCALL   USBCtrlTrfTxService
        MOVLW   CTRL_TRF_TX
        MOVWF   ctrl_trf_state
        MOVLW   low(ep0buf)
        MOVWF   ep0iadr, BANKED
        MOVLW   _USIE; 0x80
        MOVWF   ep0ostat, BANKED
        MOVLW   _DAT1|_USIE|_DTSEN;0xC8
        MOVWF   ep0istat, BANKED
        BRA     USBCtrlEPServiceComplete_4
DATADIR_HOST_TO_DEV:
        MOVLW   CTRL_TRF_RX
        MOVWF   ctrl_trf_state
        CLRF    ep0icnt, BANKED
        MOVLW   _DAT1|_USIE|_DTSEN;0xC8
        MOVWF   ep0istat, BANKED
        MOVWF   ep0ostat, BANKED
        MOVLW   low(ep0buf)
        MOVWF   ep0oadr, BANKED
USBCtrlEPServiceComplete_4:
        BCF     UCON, PKTDIS 
        RETURN

;*******************************************************************************
USBCheckStdRequest:
        MOVLW   0x60
        ANDWF   ep0buf, W, BANKED
        BNZ     RETURN__
        MOVF    ep0buf+1, W, BANKED
        XORLW   0x9
        BZ      SET_CFG   ; 9(J) == SET_CFG
        XORLW   0xF
        BZ      GET_DSC   ; 6(G) == GET_DSC
        XORLW   0x3       ; 5(F) == SET_ADDR
        BNZ     RETURN__
SET_ADR:
        MOVLW   ADR_PENDING_STATE
        MOVWF   usb_device_state, BANKED
SESSION_OWNER_USB9:
        bsf     usb_status, MUID_USB9
RETURN__:
        return
SET_CFG:
        MOVLW   ADDRESS_STATE
        TSTFSZ  ep0buf+2, BANKED
        MOVLW   CONFIGURED_STATE      ; USB ACTIVE CFG != 0
        MOVWF   usb_device_state, BANKED
        MOVF    ep0buf+2, W, BANKED
        BZ      SESSION_OWNER_USB9
;*******************************************************************************
CDCInitEP:
        banksel UEP2
        MOVLW   0x1A
        MOVWF   UEP1, BANKED
        MOVLW   0x1E
        MOVWF   UEP2, BANKED
        banksel ep0ostat
        CLRF    ep1istat, BANKED ; CDC notification end point not used
        MOVLW   8
        MOVWF   ep2ocnt, BANKED
        MOVWF   ep2icnt, BANKED
        MOVLW   low(cdc_data_rx)
        MOVWF   ep2oadr, BANKED
        MOVLW   low(cdc_data_tx)
        MOVWF   ep2iadr, BANKED
        MOVLW   high(cdc_data_rx)
        MOVWF   ep2oadr+1, BANKED
        MOVWF   ep2iadr+1, BANKED
        MOVLW   _USIE|_DTSEN ;0x88
        MOVWF   ep2ostat, BANKED
        CLRF    ep2istat, BANKED
        clrf    ep2optr
        clrf    cdc_data_tx, BANKED
        clrf    ep2icount
        movlw   1
        call    TX0_SEND2
        bra     SESSION_OWNER_USB9
;*******************************************************************************
GET_DSC:
        MOVLW   0x80
        SUBWF   ep0buf, W, BANKED
        BNZ     GetDsc_6
        MOVF    ep0buf+3, W, BANKED
        XORLW   0x3
        BZ      GET_DSC_STR
        XORLW   0x1
        BZ      GET_DSC_CFG
        XORLW   0x3
        BNZ     GetDsc_6
GET_DSC_DEV:
        MOVLW   low(device_dsc)
        MOVWF   dPtr
        MOVLW   high(device_dsc)
        MOVWF   dPtr+1
        MOVLW   0x12
        BRA     GetDsc_4
GET_DSC_CFG:
        MOVLW   low(USB_CFG)
        MOVWF   dPtr
        MOVLW   high(USB_CFG)
        MOVWF   dPtr+1
        MOVLW   0x43
        BRA     GetDsc_4
GET_DSC_STR:
        MOVF    ep0buf+2, W, BANKED
        MOVWF   TBLPTR 
        ADDWF   TBLPTR, F 
        MOVLW   low(USB_SD)
        ADDWF   TBLPTR, F  
        CLRF    TBLPTRH  
        MOVLW   high(USB_SD)
        ADDWFC  TBLPTRH, F  

        TBLRD*+
        MOVF    TABLAT, W 
        MOVWF   dPtr
        TBLRD*
        MOVWF   TBLPTR 
        MOVF    TABLAT, W 
        movwf   dPtr+1
        MOVWF   TBLPTRH 

        TBLRD*
        MOVF    TABLAT, W 
GetDsc_4:
        MOVWF   count
GetDsc_6:
        bsf     usb_status, MUID_USB9
        RETURN

USBCheckCdcRequest:
        movf    ep0buf, W, BANKED     ; IF INTF & CLASS &  TO_DEVICE
        andlw   0x7f                  ;
        xorlw   0x21
        bnz     return6
        movf    ep0buf+4, W, BANKED
        sublw   1       ;1 - w        ; IF COMM_INTF || DATA_INTF
        bn      return6
        MOVF    ep0buf+1, W, BANKED
        addlw   -0x20                 ; SET_LINE_CODING 0x20
        bz      SET_LINE_CODING
        addlw   -1                    ;  GET_LINE_CODING 0x21
        bz      GET_LINE_CODING
        addlw   -1                    ; SET_CONTROL_LINE_STATE 0x22
        bnz     return6
SET_CONTROL_LINE_STATE:
SET_LINE_CODING:
SET_MUID_CDC:
        movlw   low(line_coding)
        movwf   dPtr
        movlw   high(line_coding)
        movwf   dPtr+1
        bsf     usb_status, MUID_USB9
return6:
        return
GET_LINE_CODING:
        movlw 7
        movwf   count
        bsf     usb_status, MEM
        bra     SET_MUID_CDC
        END