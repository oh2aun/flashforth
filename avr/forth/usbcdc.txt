\ *********************************************************************
\    Filename:      usbcdc.txt                                        *
\    Date:          28.10.2020                                        *
\    FF Version:    5.0                                               *
\    MCU:           Atmega 32u4                                       *
\    Copyright:     Mikael Nordman                                    *
\    Author:        Mikael Nordman                                    *
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Compile this file on atmega32u4 over the serial port.
\ usb.terminal will configure USB interface as a serial port
\ and use it for the operator task.

\ needs forth/jumptable.txt
-usbcdc
marker -usbcdc

\ In order to save some memory we are using generic bitmasks
0 constant 0
%0000.0001 constant bit0
%0000.0010 constant bit1
%0000.0100 constant bit2
%0000.1000 constant bit3
%0001.0000 constant bit4
%0010.0000 constant bit5
%0100.0000 constant bit6
%1000.0000 constant bit7

$49 constant  PLLCSR
$52 constant	PLLFRQ
$f4 constant	UEINT
\ $f3 constant	UEBCHX
$f2 constant	UEBCLX
$f1 constant	UEDATX
$f0 constant	UEIENX
\ $ef constant	UESTA1X
\ $ee constant	UESTA0X
$ed constant	UECFG1X
$ec constant	UECFG0X
$eb constant	UECONX
$ea constant	UERST
$e9 constant	UENUM
$e8 constant	UEINTX
\ $e6 constant	UDMFN
\ $e5 constant	UDFNUMH
\ $e4 constant	UDFNUML
$e3 constant	UDADDR
$e2 constant	UDIEN
$e1 constant	UDINT
$e0 constant	UDCON
\ $da constant	USBINT
\ $d9 constant	USBSTA
$d8   constant	USBCON
$d7   constant	UHWCON

flash create usb.config.d hex 
0209 , 003e , 0102 , 8000 , 0932 , 0004 , 0100 , 0202 , 0001 ,
2405 , 0100 , 10 c,               \ interface header FD
2404 , 0202 ,                     \ interface ACM FD
2405 , 0006 , 01 c,               \ interface Union FD
0507 , 0382 , 0008 , 10 c,        \ endpoint notification 2
0409 , 0001 , 0a02 , 0000 , 00 c, \ interface data
0507 , 0203 , 0008 , 00 c,        \ OUT bulk endpoint 3
0507 , 0284 , 0008 , 00 c,        \ IN bulk endpoint  4 
$3e constant usb.config.l

flash create usb.device.d hex 
0112 , 0110 , 0002 , 2000 , faf0 , faf0 , 0000 , 0100 , 0100 ,
#18 constant usb.device.l

flash create usb.str.0 \ language code
hex 0304 , 0409 ,
flash create usb.str.1 \ Product name
hex 030a , char F , char F , char 5 , char 0 ,
decimal

ram create usb.ctl.buf 8 allot
usb.ctl.buf constant usb.request.typ
usb.ctl.buf 1 + constant usb.request
usb.ctl.buf 2 + constant usb.value
usb.ctl.buf 4 + constant usb.index
usb.ctl.buf 6 + constant usb.length

ram variable usb.conf
ram create usb.linecoding 8 allot

: usb.device
  bit3 UDINT mtst     \ EORSTI Reset the USB controller
  if
    0 UENUM c!
    bit0 UECONX mset   \ EPEN
    0 UECFG0X c!
    $22 UECFG1X c!
    0 usb.conf !
    bit0 UERST c!
    0 UERST c!
    bit3 UEIENX mset   \ RXSTPI
  then  0 UDINT c!
;i

: c!+ ( a n  -- a' ) swap dup 1+ >r c! r> ;
: usb@ UEDATX c@ ;
: usb! UEDATX c! ;
: usb? UEBCLX c@ ;
: usb>> ( a n ) for usb@ c!+ next drop ; 
: usb<< ( a n ) for c@+ usb! next drop ;
: usb.stall   bit5 bit0 or UECONX c! ;  \ STALLRQ EPEN
: usb.wait.tx begin ( TXINI) bit0 UEINTX mtst until ;
: usb.tx      ( TXINI) bit0 UEINTX mclr ;
: usb.d.d   usb.device.d usb.device.l ;
: usb.d.c   usb.config.d usb.length c@ ;
: usb.d.s0  usb.str.0 #4 ;
: usb.d.s1  usb.str.1 #10 ;
: usb.d.0   usb.stall 0 ;

jumptable: usb.desc?    ]
[   $0100 | usb.d.d     ]
[   $0200 | usb.d.c     ]
[   $0300 | usb.d.s0    ]
[   $0301 | usb.d.s1    ]
[  default| usb.d.0

: usb.get.desc
  usb.value @ usb.desc?
  dup if 
      usb.wait.tx
      for 
        c@+ usb! usb? 32 = if usb.tx usb.wait.tx then 
      next
      usb? if usb.tx then
  then
  drop
;

: usb.set.config
  bit1 UENUM c! bit0 UECONX c! $c1 UECFG0X c! bit1 UECFG1X c! \ Interrupt
  3    UENUM c! bit0 UECONX c! $80 UECFG0X c! bit1 UECFG1X c! \ OUT Bulk
  bit2 UENUM c! bit0 UECONX c! $81 UECFG0X c! bit1 UECFG1X c! \ IN Bulk
  $7e UERST c! 0 UERST c!
  usb.value c@ usb.conf c!
;
: usb.set.addr usb.value c@ bit7 or UDADDR c! ;
: usb.get.config usb.conf c@ usb! usb.tx ;
: usb.get.status 0 usb! 0 usb! usb.tx ;
: usb.get.lineco usb.linecoding 7 usb<< usb.tx ;
: usb.set.lineco usb.linecoding 7 usb>> ;
: usb.set.ctline ;

jumptable: usb.req?          ]
[      06  | usb.get.desc    ]
[      09  | usb.set.config  ]
[      05  | usb.set.addr    ]
[      08  | usb.get.config  ]
[      00  | usb.get.status  ]
[      $20 | usb.set.lineco  ]
[      $21 | usb.get.lineco  ]
[      $22 | usb.set.ctline  ]
[   default| usb.stall

: usb.endpoint
  UENUM c@
  0 UENUM c! 
  bit3 bit2 or UEINTX mtst if                      \ RXSTPI RXOUTI 
    \ SETUP PACKET 
    usb.ctl.buf 8 usb>>
    bit3 bit2 bit0 or or UEINTX mclr               \ RXSTPI RXOUTI TXINI 
    bit7 usb.request.typ mtst 0= if usb.tx usb.wait.tx then
    usb.request c@ usb.req?
  then
  UENUM c!
;i

: usb.init
  di
  bit0 UHWCON mset               \ UVREGE
  bit1 bit4 or PLLCSR mset       \ PLLE PINDIV 
  begin bit0 PLLCSR mtst until   \ PLOCK
  bit7 bit4 or USBCON mset       \ USBE OTGPADE 
  bit5 USBCON mclr               \ FRZCLK
  0 UDCON c!                     \ ATTACH, fullspeed mode
  0 usb.conf !
  ['] usb.device #11 int!
  ['] usb.endpoint #12 int!
  bit3 UDIEN mset               \ EORSTIE
  ei
;

: txu.ready begin pause bit0 ( TXINI) UEINTX mtst until usb.tx ;
: txu 4 UENUM c! txu.ready usb!  bit7 ( FIFOCON) UEINTX mclr ;

: rxu? 3 UENUM c! bit7 ( FIFOCON) UEINTX mtst ;
: rxu.ready begin pause rxu? until bit2 ( RXOUTI) UEINTX mclr ;
: rxu 
  rxu.ready
  usb? if usb@ then 
  usb? 0= if bit7 ( FIFOCON) UEINTX mclr then
;

: usb.terminal
  usb.init 
  begin usb.conf c@ until
  ['] txu 'emit !
  ['] rxu 'key !
  ['] rxu? 'key? !
;

