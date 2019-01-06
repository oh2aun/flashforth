;**********************************************************************
;                                                                     *
;    Filename:      cdc-bss.s                                         *
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
; NOTE!
; Vendor ID 0xfaf0 and Product ID 0xfaf0 are used as temporary IDs for TESTING ONLY
; For a commercial product you MUST obtain your own Vendor ID and Product ID !
.equ U_PID, 0xfaf0  ; Product ID used for testing FlashForth
.equ U_VID, 0xfaf0  ; Vendor ID used for testing FlashForth

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
.equ CDC_BULK_OUT_EP_SIZE,    8
.equ CDC_BULK_IN_EP_SIZE,     8

.bss
.align 512
bdt_base:
ep0ocnt:  	.space 1
ep0ostat:	.space 1
ep0oadr:	.space 2
ep0icnt:  	.space 1
ep0istat:	.space 1
ep0iadr:	.space 2
ep1ocnt:  	.space 1
ep1ostat:	.space 1
ep1oadr:	.space 2
ep1icnt:  	.space 1
ep1istat:	.space 1
ep1iadr:	.space 2
ep2ocnt:  	.space 1
ep2ostat:	.space 1
ep2oadr:	.space 2
ep2icnt:  	.space 1
ep2istat:	.space 1
ep2iadr:	.space 2

ep0buf:         .space CDC_INT_EP_SIZE
cdc_data_rx:    .space CDC_BULK_OUT_EP_SIZE
cdc_data_tx:    .space CDC_BULK_IN_EP_SIZE

; Control transfer session owner
usb_status:     .space 1
.equ MEM,           0
.equ MUID_USB9,     1

; Control Transfer States
ctrl_trf_state:     .space 1
.equ WAIT_SETUP,    0
.equ CTRL_TRF_TX,   1
.equ CTRL_TRF_RX,   2

; USB Device States - To be used with usb_device_state
usb_device_state:   .space 1
.equ DETACHED_STATE,    0
.equ ATTACHED_STATE,    0
.equ POWERED_STATE,     0
.equ DEFAULT_STATE,     1
.equ ADR_PENDING_STATE, 3
.equ ADDRESS_STATE,     0x07
.equ ADDRESS_STATE2,    0x83  ; Shifted to h'07'
.equ CONFIGURED_STATE,  0x0f
count:          .space 1
dPtr:           .space 2
line_coding:    .space 8
mem:            .space 2           ; 0 = flash ; 0xff = ram
ep2optr:        .space 2
ep2iptr:        .space 2
ep2icount:      .space 2
ep2itmo:        .space 2
           