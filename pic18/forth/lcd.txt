\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ LCD Driver
\ 
\ This project uses FlashForth to write text
\ to the LCD display on the Microchip PICDEM
\ PIC18 Explorer Demo Board, running on a
\ PIC18F8722 microcontroller.
\
\ John Earnest
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 
marker -lcd-driver
 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ First we'll define some words to configure
\ MSSP1 for SPI and write data through it:
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 
$ff94 con trisc
$ffc6 con ssp1con1
$ffc7 con ssp1stat
$ffc9 con ssp1buf
$ff9e con pir1
 
: spi-init ( -- )
  %00101000 trisc    mclr \ make sdo and sck output
    %00100010 ssp1con1 c!   \ enable ssp
    %01000000 ssp1stat mset \ configure clock select
;
 
: spi-write ( 8b -- )
    %1000 pir1 mclr \ clear transmit flag
    ssp1buf c!      \ write data
    begin           \ wait for transmit
        %1000 pir1 mtst
    until
;
 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Using our previously defined SPI communication
\ vocabulary, define some words for writing
\ to the ports of an MCP23S17 port expander:
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 
$ff92 con trisa
$ff89 con lata
 
: cs-up %100 lata mset ; ( -- )
: cs-dn %100 lata mclr ; ( -- )
 
: mcp-write ( 8b reg -- )
    cs-dn
    %01000000 spi-write \ write control
              spi-write \ write register
              spi-write \ write value
    cs-up
;
 
: mcp-init ( -- )
    spi-init
    %100 trisa mclr \ make ra2 output
    0 0 mcp-write   \ init port a
    0 1 mcp-write   \ init port b
    cs-up
;
 
: mcp-a  $12 mcp-write ; ( 8b -- )
: mcp-b  $13 mcp-write ; ( 8b -- )
 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Now that we can talk to the HD44780
\ display driver, define a vocabulary for
\ interacting with it:
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 
: lcd-i ( op -- )
    $00 mcp-a
        mcp-b
    $40 mcp-a \ clock
    $00 mcp-a
;
 
: lcd-emit ( char -- )
    $80 mcp-a \ register select
        mcp-b
    $c0 mcp-a \ clock, register select
    $00 mcp-a
;
 
: lcd-init ( -- )
    mcp-init
    $3c lcd-i \ 0011NFxx
    $0c lcd-i \ display off
    $01 lcd-i \ display clear
    $06 lcd-i \ entry mode
;
 
: lcd-line1  $80 lcd-i ;
: lcd-line2  $c0 lcd-i ;
: lcd-clear  $01 lcd-i ;
: lcd-type   for dup @ lcd-emit 1+ next drop ;
 
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ Finally, a simple test application:
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 
: hello ( -- )
    lcd-init
    lcd-line1 s" Hello," lcd-type
    lcd-line2 s" World!" lcd-type
;