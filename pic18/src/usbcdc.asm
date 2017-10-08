;**********************************************************************
;                                                                     *
;    Filename:      usbcdc.asm                                        *
;    Date:          07.10.2017                                        *
;    File Version:  5.0                                               *
;    Copyright:     Mikael Nordman                                    *
;    Author:        Mikael Nordman                                    *
;                                                                     *
;**********************************************************************
; FlashForth is a standalone Forth system for microcontrollers that
; can flash their own flash memory.
;
; Copyright (C) 2017  Mikael Nordman
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

; Control Transfer States
#define WAIT_SETUP          0
#define CTRL_TRF_TX         1
#define CTRL_TRF_RX         2

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

; USB Device States - To be used with usb_device_state
#define DETACHED_STATE          0
#define ATTACHED_STATE          1
#define POWERED_STATE           2
#define DEFAULT_STATE           3
#define ADR_PENDING_STATE       4
#define ADDRESS_STATE           5
#define CONFIGURED_STATE        8

#define CDC_INT_EP_SIZE         8
#define CDC_BULK_OUT_EP_SIZE    1
#define CDC_BULK_IN_EP_SIZE     0x10

#define MUID_NULL               0
#define MUID_USB9               1

        global USBDriverService
        global cdc_data_tx
        global cdc_data_rx
        global cdc_notice
        global usb_device_state
        global ep3istat
        global ep3ostat

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

ep3ostat	res 1
ep3ocnt  	res 1
ep3oadr		res 2
ep3istat	res 1
ep3icnt  	res 1
ep3iadr		res 2

setupPkt    res CDC_INT_EP_SIZE
ep0buf		res CDC_INT_EP_SIZE


C_VARS udata
moveLen res 1
count  res 1
pSrc    res 2
ctrl_trf_session_owner res 1
ctrl_trf_state res 1
usb_device_state res 1

USB_BUF udata
cdc_notice res CDC_INT_EP_SIZE
cdc_data_rx res CDC_BULK_OUT_EP_SIZE
cdc_data_tx res CDC_BULK_IN_EP_SIZE


;*******************************************************************************
FF_INTERRUPT code
device_dsc:
        dw      0x0112    ; Size of this descriptor in bytes
                          ; DEVICE descriptor type
        dw      0x0200    ; USB Spec Release Number in BCD format
        dw      0x0002;   ; Class Code CDC device
                          ; Subclass code = 00
        dw      0x0800    ; Protocol code = 00
                          ; EP0 packet size = 8
        dw      U_VID     ; Vendor ID
        dw      U_PID     ; Product ID
        dw      0x0000    ; Device release Number in BCD
        dw      0x0201    ; Manufacturer string index
                          ; Product string Index
        dw      0x0100    ; Device serial number string index
                          ; Number of possible configurations

SD000:
        dw      0x0304    ; sizeof descriptor in bytes
                          ; STRING descriptor type
        dw      0x0409

; Manufacturer string
SD001:
        dw      0x0304    ; sizeof descriptor in bytes
                          ; STRING descriptor type
        dw      'M'

; Product string
SD002:
        dw      0x030a    ; sizeof descriptor in bytes
                          ; STRING descriptor type
        dw      'F','F','5','0'

USB_SD:
        dw      SD000
        dw      SD001
        dw      SD002
USB_CFG:
        db  0x09,0x02,0x43,0x00,0x02,0x01,0x00,0x80,0x32,0x09,0x04,0x00,0x00,0x01,0x02,0x02
        db  0x01,0x00,0x05,0x24,0x00,0x10,0x01,0x04,0x24,0x02,0x02,0x05,0x24,0x06,0x00,0x01
        db  0x05,0x24,0x01,0x00,0x01,0x07,0x05,0x82,0x03,0x08,0x00,0x02,0x09,0x04,0x01,0x00
        db  0x02,0x0a,0x00,0x00,0x00,0x07,0x05,0x03,0x02,0x01,0x00,0x00,0x07,0x05,0x83,0x02
        db  0x10,0x00,0x00

;*******************************************************************************
USBDriverService:
        movlb   ep0ostat
USBCheckBusStatus:
        BTFSC UCON, USBEN, ACCESS
        BRA USBCheckBusStatus_1
        CLRF UCON, ACCESS
        CLRF UIE, ACCESS
        BSF UCON, USBEN, ACCESS
        MOVLW ATTACHED_STATE
        MOVWF usb_device_state, BANKED
USBCheckBusStatus_1:
        DECF usb_device_state, W, BANKED
        BNZ USBCheckBusStatus_2
        BTFSC UCON, SE0, ACCESS
        BRA USBCheckBusStatus_2
        CLRF UIR, ACCESS
        CLRF UIE, ACCESS
        BSF UIE, URSTIE, ACCESS
        BSF UIE, IDLEIE, ACCESS
        MOVLW POWERED_STATE
        MOVWF usb_device_state, BANKED
USBCheckBusStatus_2:
        MOVF    usb_device_state, W, BANKED
        bnz     USBDriverService_1
        return
USBDriverService_1:
        BTFSC   UCON, SUSPND, ACCESS
        return
        BTFSC   UIR, URSTIF, ACCESS
        RCALL   USBProtocolResetHandler
        BTFSC   UIR, SOFIF, ACCESS
        BCF     UIR, SOFIF, ACCESS
        BTFSC   UIR, STALLIF, ACCESS
        RCALL   USBStallHandler
        BTFSC   UIR, UERRIF, ACCESS
        BCF     UIR, UERRIF, ACCESS
        MOVLW   DEFAULT_STATE
        SUBWF   usb_device_state, W, BANKED
        BC  USBDriverService_2
        return
USBDriverService_2:
        BTFSS UIR, TRNIF, ACCESS
        return
        RCALL USBCtrlEPService
        BCF UIR, TRNIF, ACCESS
        return
;*******************************************************************************
USBStallHandler:
        movlb high(UEP0)
        BTFSS UEP0, EPSTALL, BANKED
        BRA USBStallHandler_1
        movlb ep0ostat
        rcall USBPrepareForNextSetupTrf
        movlb high(UEP0)
        BCF UEP0, EPSTALL, BANKED
USBStallHandler_1:
        BCF UIR, STALLIF, ACCESS
        movlb ep0ostat
        return
;*******************************************************************************
USBProtocolResetHandler:
        movlb high(UEIR)
        CLRF UEIR, BANKED
        CLRF UIR, ACCESS
        MOVLW 0x9F
        MOVWF UEIE, BANKED
        MOVLW 0x7B
        MOVWF UIE, ACCESS
        CLRF UADDR, BANKED
        CLRF UEP1, BANKED
        CLRF UEP2, BANKED
        CLRF UEP3, BANKED
        MOVLW 0x16
        MOVWF UEP0, BANKED
        movlb ep0ostat
USBProtocolResetHandler_1:
        BTFSC UIR, TRNIF, ACCESS
        BRA USBProtocolResetHandler_1
        BCF UCON, PKTDIS, ACCESS
        rCALL USBPrepareForNextSetupTrf
        MOVLW DEFAULT_STATE
        MOVWF usb_device_state, BANKED
        return

;*******************************************************************************
USBCtrlEPService:
        MOVF USTAT, W, ACCESS
        BNZ USBCtrlEPService_3
        MOVF ep0ostat, W, BANKED
        ANDLW 0x3C
        SUBLW _KEN|_INCDIS|_BSTALL;0x34
        bnz USBPrepareForNextSetupTrf
USBCtrlTrfSetupHandler:
        CLRF ctrl_trf_state, BANKED
        CLRF ctrl_trf_session_owner, BANKED ; MUID_NULL
        CLRF count, BANKED
        RCALL USBCheckStdRequest
        BRA   USBCtrlEPServiceComplete
USBCtrlEPService_3:
        MOVLW 0x0
        MOVF USTAT, F, ACCESS
        BTFSS STATUS, Z, A
        MOVLW 0x1
        IORLW 0x4
        BNZ USBCtrlTrfInHandler
        return
;*******************************************************************************
USBPrepareForNextSetupTrf:
        CLRF ctrl_trf_state, BANKED
        MOVLW CDC_INT_EP_SIZE
        MOVWF ep0ocnt, BANKED
        MOVLW low(setupPkt)
        MOVWF ep0oadr, BANKED
        MOVLW high(setupPkt)
        MOVWF ep0oadr+1, BANKED
        MOVLW _USIE|_DTSEN;0x88
        MOVWF ep0ostat, BANKED
        CLRF ep0istat, BANKED
        RETURN
;*******************************************************************************
USBCtrlTrfInHandler:
        MOVLW   ADR_PENDING_STATE
        SUBWF   usb_device_state, W, BANKED
        BNZ     USBCtrlTrfInHandler_2
        MOVF    setupPkt+2, W, BANKED
        movlb   high(UADDR)
        movwf   UADDR, BANKED
        MOVLW   DEFAULT_STATE
        BTFSS   STATUS, Z, A
        MOVLW   ADDRESS_STATE
        movlb   ep0ostat
        MOVWF   usb_device_state, BANKED
USBCtrlTrfInHandler_2:
        DECF    ctrl_trf_state, W, BANKED
        BNZ     USBPrepareForNextSetupTrf
        RCALL   USBCtrlTrfTxService

        MOVLW   _USIE|_DTSEN;0x88
        BTFSS   ep0istat, 6, BANKED
        MOVLW   _DAT1|_USIE|_DTSEN;0xC8
        MOVWF   ep0istat, BANKED
        return
;*******************************************************************************
USBCtrlTrfTxService:
        MOVLW   CDC_INT_EP_SIZE
        MOVWF   moveLen, BANKED
        SUBWF   count, W, BANKED
        BC      USBCtrlTrfTxService_1
        MOVFF   count, moveLen
USBCtrlTrfTxService_1:
        MOVFF   moveLen, ep0icnt
        MOVF    moveLen, W, BANKED
        SUBWF   count, W, BANKED
        MOVWF   count, BANKED
        lfsr    1, ep0buf
        movff   pSrc, TBLPTRL
        movff   pSrc+1, TBLPTRH
        bra     romcp2
romcp1:
        TBLRD*+
        movf    TABLAT, W, A
        movwf   POSTINC1, A
romcp2:
        decf    moveLen, F, BANKED
        bc      romcp1
        movff   TBLPTRL, pSrc
        movff   TBLPTRH, pSrc+1
        RETURN
;*******************************************************************************
USBCtrlEPServiceComplete:
        MOVLW CDC_INT_EP_SIZE
        MOVWF ep0ocnt, BANKED
        MOVLW low(setupPkt)
        MOVWF ep0oadr, BANKED
        MOVLW high(setupPkt)
        MOVWF ep0oadr+1, BANKED

        MOVF ctrl_trf_session_owner, W, BANKED
        BNZ DATADIR_DEV_TO_HOST
        MOVLW _USIE|_BSTALL ;0x84
        MOVWF ep0ostat, BANKED
        MOVWF ep0istat, BANKED
        BRA USBCtrlEPServiceComplete_4
DATADIR_DEV_TO_HOST:
        BTFSS setupPkt, 7, BANKED
        BRA USBCtrlEPServiceComplete_3
        MOVF setupPkt+7, W, BANKED
        bnz  USBCtrlEPServiceComplete_2
        MOVF count, W, BANKED
        SUBWF setupPkt+6, W, BANKED
        BC USBCtrlEPServiceComplete_2
        MOVF setupPkt+6, W, BANKED
        MOVWF count, BANKED
USBCtrlEPServiceComplete_2:
        RCALL USBCtrlTrfTxService
        MOVLW CTRL_TRF_TX
        MOVWF ctrl_trf_state, BANKED
        MOVLW _USIE; 0x80
        MOVWF ep0ostat, BANKED
        MOVLW low(ep0buf)
        MOVWF ep0iadr, BANKED
        MOVLW high(ep0buf)
        MOVWF ep0iadr+1, BANKED
        MOVLW _DAT1|_USIE|_DTSEN;0xC8
        MOVWF ep0istat, BANKED
        BRA USBCtrlEPServiceComplete_4
USBCtrlEPServiceComplete_3:
        MOVLW CTRL_TRF_RX
        MOVWF ctrl_trf_state, BANKED
        CLRF ep0icnt, BANKED
        MOVLW _DAT1|_USIE|_DTSEN;0xC8
        MOVWF ep0istat, BANKED
        MOVLW 0x8
        MOVWF ep0ocnt, BANKED
        MOVLW low(ep0buf)
        MOVWF ep0oadr, BANKED
        MOVLW high(ep0buf)
        MOVWF ep0oadr+1, BANKED
        MOVLW _DAT1|_USIE|_DTSEN;0xC8
        MOVWF ep0ostat, BANKED
USBCtrlEPServiceComplete_4:
        BCF UCON, PKTDIS, ACCESS
        RETURN

;*******************************************************************************
USBCheckStdRequest:
        MOVLW 0x60
        ANDWF setupPkt, W, BANKED
        BNZ   RETURN__
        MOVLW MUID_USB9
        MOVWF ctrl_trf_session_owner, BANKED
        MOVF setupPkt+1, W, BANKED
        XORLW 0x9
        BZ SET_CFG
        XORLW 0xF
        BZ GET_DSC
        XORLW 0x3
        BNZ RETURN__
SET_ADR:
        MOVLW ADR_PENDING_STATE
        MOVWF usb_device_state, BANKED
RETURN__:
        return
GET_DSC:
        BRA GetDsc
SET_CFG:
;*******************************************************************************
SetCfg:
        movlb high(UEP1)
        CLRF UEP1, BANKED
        CLRF UEP2, BANKED
        CLRF UEP3, BANKED
        movlb ep0ostat
        MOVLW ADDRESS_STATE
        TSTFSZ setupPkt+2, BANKED ; USB ACTIVE CFG != 0
        MOVLW CONFIGURED_STATE
        MOVWF usb_device_state, BANKED
;*******************************************************************************
CDCInitEP:
        movlb high(UEP2)
        MOVLW 0x1A
        MOVWF UEP2, BANKED
        MOVLW 0x1E
        MOVWF UEP3, BANKED
        movlb ep0ostat

        MOVLW low(cdc_notice)
        MOVWF ep2iadr, BANKED
        MOVLW high(cdc_notice)
        MOVWF ep2iadr+1, BANKED
        MOVLW _DAT1 ;0x40
        MOVWF ep2istat, BANKED

        MOVLW CDC_BULK_OUT_EP_SIZE
        MOVWF ep3ocnt, BANKED
        MOVLW low(cdc_data_rx)
        MOVWF ep3oadr, BANKED
        MOVLW high(cdc_data_rx)
        MOVWF ep3oadr+1, BANKED
        MOVLW _USIE|_DTSEN ;0x88
        MOVWF ep3ostat, BANKED

        MOVLW low(cdc_data_tx)
        MOVWF ep3iadr, BANKED
        MOVLW high(cdc_data_tx)
        MOVWF ep3iadr+1, BANKED
        MOVLW _DAT1 ;0x40
        MOVWF ep3istat, BANKED
        RETURN
;*******************************************************************************
GetDsc:
        MOVLW 0x80
        SUBWF setupPkt, W, BANKED
        BNZ GetDsc_6
        MOVF setupPkt+3, W, BANKED
        XORLW 0x3
        BZ GET_DSC_STR
        XORLW 0x1
        BZ GET_DSC_CFG
        XORLW 0x3
        BNZ GetDsc_6
GET_DSC_DEV:
        MOVLW low(device_dsc)
        MOVWF pSrc, BANKED
        MOVLW high(device_dsc)
        MOVWF pSrc+1, BANKED
        MOVLW 0x12 
        BRA  GetDsc_4
GET_DSC_CFG:
        MOVLW low(USB_CFG)
        MOVWF pSrc, BANKED
        MOVLW high(USB_CFG)
        MOVWF pSrc+1, BANKED
        MOVLW 0x43
        BRA GetDsc_4
GET_DSC_STR:
        CLRF TBLPTRH, ACCESS
        MOVF setupPkt+2, W, BANKED
        MOVWF TBLPTR, ACCESS
        ADDWF TBLPTR, F, ACCESS
        MOVLW low(USB_SD)
        ADDWF TBLPTR, F, ACCESS
        MOVLW high(USB_SD)
        ADDWFC TBLPTRH, F, ACCESS

        TBLRD*+
        MOVF  TABLAT, W, A
        MOVWF pSrc, BANKED
        TBLRD*-
        MOVFF TABLAT, pSrc+1
        MOVWF TBLPTR, A
        MOVFF pSrc+1, TBLPTRH

        TBLRD*
        MOVF  TABLAT, W, A
GetDsc_4:
        MOVWF count, BANKED
GetDsc_6:
        RETURN
        END