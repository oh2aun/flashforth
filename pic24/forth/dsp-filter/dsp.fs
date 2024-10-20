\ DSP tests for dsPIC33fj128gp802

\ Fcy = 16384000 Hz
\ 16 times oversampling
\ sampling rate = 16000*16 = 256000 Hz
\ sampling clocks = 32 (14 conversion, 18 aquisition)
\ Tad = 122.07 ns (8192KHz) adcs=1 (/2)

$10 iec0 mclr
-dsp
marker -dsp

$0770 constant pmd1
$0772 constant pmd2
$0774 constant pmd3
$03f0 constant dac1con
$03f2 constant dac1stat
$03f4 constant dac1dflt
$03f6 constant dac1rdat inlined
$03f8 constant dac1ldat inlined
$074a constant aclkcon

\ Interrupt vectors
\ #86   constant irqdac1r
\ #87   constant irqdac1l

\ Interrupt status register and bits 
\ $008c constant ifs4
\ #15   constant dac1lif
\ #14   constant dac1rif
\ Interrupt enable register and bits 
\ $009c constant iec4
\ #15   constant dac1lie
\ #14   constant dac1rie

: dac1/
  %111.1011.1111 pmd3 !
  -1 pmd2 !
  %0000.0100.1000.0000 aclkcon !
  %1000.0000.0000.0000 dac1stat !
  %0001.0001.0000.0000 dac1con ! \ 16384/4 = 4096 KHz 16 KBits/s output
  %1001.0001.0000.0000 dac1con ! \ 16384/4 = 4096 KHz 16 KBits/s output
  $0000 dac1dflt !
;

\ FCY = 16384 KHz 

\ adc1
$0300 constant adc1buf0
$0320 constant ad1con1
$0322 constant ad1con2
$0324 constant ad1con3
\ $0326 constant ad1chs123
$0328 constant ad1chs0
$032c constant ad1pcfgl
$0330 constant ad1cssl
$0332 constant ad1con4

: ad1on [ #15 ad1con1 bset, ] ;
: ad1off [ #15 ad1con1 bclr, ] ;

$4000 constant dmabufa inlined  \ DMA dual port memory
$4020 constant dmabufb inlined  \ DMA dual port memory
$0380 constant dma0con
$0382 constant dma0req
$0384 constant dma0sta
$0386 constant dma0stb
$0388 constant dma0pad
$038a constant dma0cnt
$03e0 constant dmacs0
$03e2 constant dmacs1
$03e4 constant dsadr
$0094 constant iec0
$0084 constant ifs0
$0004 constant dma0i
#0012 constant irqdma0

ram variable filter

\ Sum 16 samples, run the fir filter and output to left DA channel
: dma0irq
  [i
  [ 0 dmacs1 btst, z, if, ]
    dmabufa
  [ else, ]
    dmabufb
  [ then, ]
  #16 inline sum[n]
  filter @ if  firContext  fir then dac1ldat ! 
  [ dma0i ifs0 bclr, ]
  i]
;i

\ Transfer 16 AD0 samples to alternate buffers and interrupt
: dma0/
  ['] dma0irq irqdma0 int!
  aivt
  [ dma0i iec0 bset, ]
  0 dma0con !
  dmabufa dma0sta !
  dmabufb dma0stb !
  #13 dma0req !       \ AD interrupt vector number
  adc1buf0 dma0pad !  \ datat source
  #15 dma0cnt !       \ Transfer 16 words
  %1000.0000.0000.0010 dma0con ! \ continous ping-pong mode
  0 filter !   \ Fir filter off
;

\ Init 12-bit ADC @256Ksamples/s, DMA 16 samples as a chunk.
: adc1/
  %1101.0111.1101.1110 pmd1 !
  $0400 ad1con1 !   \ 12-bit ad off
\ Vreflo=GND Vrefhi=Vdd noscan convertCH0
  %0000.0000.0000.0000 ad1con2 !
  %0001.0010.0000.0001 ad1con3 ! \ 18 Tad sample time, Tad = 122.07 ns (16384KHz/2)
  %0000.0000.0000.0100 ad1con4 ! \ 16 words of buffer
  %0000.0101.0000.0101 ad1chs0 ! \ AN5
  %1111.1111.1101.1111 ad1pcfgl ! \ AN5 analog
  %0001.0101.1110.0100 ad1con1 ! \ auto-start, auto convert, integer, 12-bit
  ad1on
;

: init ( filterTaps -- ) initFilter dma0/ adc1/ dac1/ ;

lp2.5KHz init

