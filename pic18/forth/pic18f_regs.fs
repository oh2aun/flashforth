\ **********************************************************************
\                                                                      *
\     Filename:      pic18f_regs.txt                                   *
\     Date:          06.01.2014                                        *
\     FF Version:    5.0                                               *
\     MCU:           PIC18                                             *
\     Copyright:     Mikael Nordman                                    *
\     Author:        Mikael Nordman                                    *
\ **********************************************************************
\     FlashForth is licensed acording to the GNU General Public License*
\ **********************************************************************
\ 
\ special function registers

\ pic18f4525 pic18f4620 pic18f2525 pic18f2620
\ pic18f4525 pic18f4620
\ These should be (about?) the same and all 18fxxxx pics

$ff80 constant porta     
$ff81 constant portb     
$ff82 constant portc     
$ff83 constant portd     
$ff84 constant porte     
$ff89 constant lata      
$ff8a constant latb      
$ff8b constant latc      
$ff8c constant latd      
$ff8d constant late      
$ff92 constant trisa     
$ff93 constant trisb     
$ff94 constant trisc     
$ff95 constant trisd     
$ff96 constant trise
$ff9b constant osctune   
$ff9d constant pie1      
$ff9e constant pir1      
$ff9f constant ipr1      
$ffa0 constant pie2      
$ffa1 constant pir2      
$ffa2 constant ipr2      
$ffa6 constant eecon1    
$ffa7 constant eecon2    
$ffa8 constant eedata    
$ffa9 constant eeadr     
$ffaa constant eeadrh    
$ffab constant rcsta     
$ffac constant txsta     
$ffad constant txreg     
$ffae constant rcreg     
$ffaf constant spbrg     
$ffb0 constant spbrgh    
$ffb1 constant t3con     
$ffb2 constant tmr3l     
$ffb3 constant tmr3h     
$ffb4 constant cmcon     
$ffb5 constant cvrcon    
$ffb6 constant eccp1as
$ffb7 constant pwm1con
$ffb8 constant baudcon   
$ffba constant ccp2con   
$ffbb constant ccpr2l    
$ffbc constant ccpr2h    
$ffbd constant ccp1con   
$ffbe constant ccpr1l    
$ffbf constant ccpr1h    
$ffc0 constant adcon2    
$ffc1 constant adcon1    
$ffc2 constant adcon0    
$ffc3 constant adresl    
$ffc4 constant adresh    
$ffc5 constant sspcon2   
$ffc6 constant sspcon1   
$ffc7 constant sspstat   
$ffc8 constant sspadd    
$ffc9 constant sspbuf    
$ffca constant t2con     
$ffcb constant pr2       
$ffcc constant tmr2      
$ffcd constant t1con     
$ffce constant tmr1l     
$ffcf constant tmr1h     
$ffd0 constant rcon      
$ffd1 constant wdtcon    
$ffd2 constant hlvdcon   
$ffd3 constant osccon    
$ffd5 constant t0con     
$ffd6 constant tmr0l     
$ffd7 constant tmr0h     
$ffd8 constant status    
$ffd9 constant fsr2l     
$ffda constant fsr2h     
$ffdb constant plusw2    
$ffdc constant preinc2   
$ffdd constant postdec2  
$ffde constant postinc2
$ffdf constant indf2
$ffe0 constant bsr
$ffe1 constant fsr1l
$ffe2 constant fsr1h
$ffe3 constant plusw1
$ffe4 constant preinc1
$ffe5 constant postdec1
$ffe6 constant postinc1
$ffe7 constant indf1
$ffe8 constant wreg
$ffe9 constant fsr0l
$ffea constant fsr0h
$ffeb constant plusw0
$ffec constant preinc0
$ffed constant postdec0
$ffee constant postinc0
$ffef constant indf0
$fff0 constant intcon3
$fff1 constant intcon2
$fff2 constant intcon
$fff3 constant prodl
$fff4 constant prodh
$fff5 constant tablat
$fff6 constant tblptrl
$fff7 constant tblptrh
$fff8 constant tblptru
$fff9 constant pcl
$fffa constant pclath
$fffb constant pclatu
$fffc constant stkptr
$fffd constant tosl
$fffe constant tosh
$ffff constant tosu
