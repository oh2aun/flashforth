;
; ff24.inc
; FlashForth configuration includes for PIC24, dsPIC30, and dsPIC33
; See also configs.c
.ifdef __24EP128GP202
.endif

.ifdef __24EP128GP204
.endif

.ifdef __24EP128GP206
.endif

.ifdef __24EP128MC202
.endif

.ifdef __24EP128MC204
.endif

.ifdef __24EP128MC206
.endif

.ifdef __24EP256GP202
.equ FLASH_SIZE,     0x2a800
.equ RAM_SIZE,       0x8000
.include "p24e_config.inc"
.endif

.ifdef __24EP256GP204
.endif

.ifdef __24EP256GP206
.endif

.ifdef __24EP256GU810
.endif

.ifdef __24EP256GU814
.endif

.ifdef __24EP256MC202
.endif

.ifdef __24EP256MC204
.endif

.ifdef __24EP256MC206
.endif

.ifdef __24EP32GP202
.endif

.ifdef __24EP32GP203
.endif

.ifdef __24EP32GP204
.endif

.ifdef __24EP32MC202
.endif

.ifdef __24EP32MC203
.endif

.ifdef __24EP32MC204
.endif

.ifdef __24EP512GP202
.endif

.ifdef __24EP512GP204
.endif

.ifdef __24EP512GP206
.endif

.ifdef __24EP512GP806
.endif

.ifdef __24EP512GU810
.endif

.ifdef __24EP512GU814
.endif

.ifdef __24EP512MC202
.endif

.ifdef __24EP512MC204
.endif

.ifdef __24EP512MC206
.endif

.ifdef __24EP64GP202
.endif

.ifdef __24EP64GP203
.endif

.ifdef __24EP64GP204
.endif

.ifdef __24EP64GP206

.endif

.ifdef __24EP64MC202
.endif

.ifdef __24EP64MC203
.endif

.ifdef __24EP64MC204
.endif

.ifdef __24EP64MC206
.endif

.ifdef __24F32KA301
.equ FLASH_SIZE,     0x5800
.equ EEPROM_SIZE,    0x0200
.equ RAM_SIZE,       0x0800
.include "p24fk_config.inc"
.endif

.ifdef __24F32KA302
.equ FLASH_SIZE,     0x5800
.equ EEPROM_SIZE,    0x0200
.equ RAM_SIZE,       0x0800
.include "p24fk_config.inc"
.endif

.ifdef __24F32KA304
.equ FLASH_SIZE,     0x5800
.equ EEPROM_SIZE,    0x0200
.equ RAM_SIZE,       0x0800
.include "p24fk_config.inc"
.endif

.ifdef __24FJ1024GA606
.endif

.ifdef __24FJ1024GA610
.endif

.ifdef __24FJ1024GB606
.endif

.ifdef __24FJ1024GB610
.endif

.ifdef __24FJ128DA106
.endif

.ifdef __24FJ128DA110
.endif

.ifdef __24FJ128DA206
.endif

.ifdef __24FJ128DA210
.endif

.ifdef __24FJ128GA006
.endif

.ifdef __24FJ128GA008
.endif

.ifdef __24FJ128GA010
.endif

.ifdef __24FJ128GA106
.endif

.ifdef __24FJ128GA108
.endif

.ifdef __24FJ128GA110
.endif

.ifdef __24FJ128GA202
.equ FLASH_SIZE,     0x15800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA204
.equ FLASH_SIZE,     0x15800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA306
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA308
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA310
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA406
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA410
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA412
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GA606
.endif

.ifdef __24FJ128GA610
.endif

.ifdef __24FJ128GA702
.equ FLASH_SIZE,     0x15800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ128GA704
.equ FLASH_SIZE,     0x15800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ128GA705
.equ FLASH_SIZE,     0x15800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ128GB106
.include "p24FJ128GB106.inc"

.endif

.ifdef __24FJ128GB108
.include "p24FJ128GB108.inc"

.endif

.ifdef __24FJ128GB110
.include "p24FJ128GB110.inc"

.endif

.ifdef __24FJ128GB202
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GB204
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GB206
.endif

.ifdef __24FJ128GB210
.endif

.ifdef __24FJ128GB406
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GB410
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GB412
.equ FLASH_SIZE,     0x15400
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ128GB606
.endif

.ifdef __24FJ128GB610
.endif

.ifdef __24FJ128GC006
.endif

.ifdef __24FJ128GC010
.endif

.ifdef __24FJ128GL302
.endif

.ifdef __24FJ128GL303
.endif

.ifdef __24FJ128GL305
.endif

.ifdef __24FJ128GL306
.endif

.ifdef __24FJ128GL405
.endif

.ifdef __24FJ128GL406
.endif

.ifdef __24FJ128GL408
.endif

.ifdef __24FJ128GL410
.endif

.ifdef __24FJ128GU405
.endif

.ifdef __24FJ128GU406
.endif

.ifdef __24FJ128GU408
.endif

.ifdef __24FJ128GU410
.endif

.ifdef __24FJ192GA106
.endif

.ifdef __24FJ192GA108
.endif

.ifdef __24FJ192GA110
.endif

.ifdef __24FJ192GB106
.endif

.ifdef __24FJ192GB108
.endif

.ifdef __24FJ192GB110
.endif

.ifdef __24FJ256DA106
.endif

.ifdef __24FJ256DA110
.endif

.ifdef __24FJ256DA206
.endif

.ifdef __24FJ256DA210
.endif

.ifdef __24FJ256GA106
.endif

.ifdef __24FJ256GA108
.endif

.ifdef __24FJ256GA110
.endif

.ifdef __24FJ256GA406
.equ FLASH_SIZE,     0x2AC00
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ256GA410
.equ FLASH_SIZE,     0x2AC00
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ256GA412
.equ FLASH_SIZE,     0x2AC00
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ256GA606
.endif

.ifdef __24FJ256GA610
.endif

.ifdef __24FJ256GA702
.equ FLASH_SIZE,     0x2a800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ256GA704
.equ FLASH_SIZE,     0x2a800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ256GA705
.equ FLASH_SIZE,     0x2a800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ256GB106
.endif

.ifdef __24FJ256GB108
.endif

.ifdef __24FJ256GB110
.endif

.ifdef __24FJ256GB206
.endif

.ifdef __24FJ256GB210
.endif

.ifdef __24FJ256GB406
.equ FLASH_SIZE,     0x2AC00
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ256GB410
.equ FLASH_SIZE,     0x2AC00
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ256GB412
.equ FLASH_SIZE,     0x2AC00
.equ RAM_SIZE,       0x4000
.include "p24f_config.inc"
.endif

.ifdef __24FJ256GB606
.endif

.ifdef __24FJ256GB610
.endif

.ifdef __24FJ256GL405
.endif

.ifdef __24FJ256GL406
.endif

.ifdef __24FJ256GL408
.endif

.ifdef __24FJ256GL410
.endif

.ifdef __24FJ256GU405
.endif

.ifdef __24FJ256GU406
.endif

.ifdef __24FJ256GU408
.endif

.ifdef __24FJ256GU410
.endif

.ifdef __24FJ32GA002
.endif

.ifdef __24FJ32GA004
.endif

.ifdef __24FJ32GA102
.endif

.ifdef __24FJ32GA104
.endif

.ifdef __24FJ32GB002
.equ FLASH_SIZE,     0x5400
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ32GB004
.equ FLASH_SIZE,     0x5400
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ32MC101
.endif

.ifdef __24FJ32MC102
.endif

.ifdef __24FJ32MC104
.endif

.ifdef __24FJ48GA002
.endif

.ifdef __24FJ48GA004
.endif

.ifdef __24FJ512GA606
.endif

.ifdef __24FJ512GA610
.endif

.ifdef __24FJ512GB606
.endif

.ifdef __24FJ512GB610
.endif

.ifdef __24FJ512GL405
.endif

.ifdef __24FJ512GL406
.endif

.ifdef __24FJ512GL408
.endif

.ifdef __24FJ512GL410
.endif

.ifdef __24FJ512GU405
.endif

.ifdef __24FJ512GU406
.endif

.ifdef __24FJ512GU408
.endif

.ifdef __24FJ512GU410
.endif

.ifdef __24FJ64GA002
.endif

.ifdef __24FJ64GA004
.endif

.ifdef __24FJ64GA006
.endif

.ifdef __24FJ64GA008
.endif

.ifdef __24FJ64GA010
.endif

.ifdef __24FJ64GA102
.endif

.ifdef __24FJ64GA104
.endif

.ifdef __24FJ64GA106
.endif

.ifdef __24FJ64GA108
.endif

.ifdef __24FJ64GA110
.endif

.ifdef __24FJ64GA202
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA204
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA306
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA308
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA310
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA406
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA410
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA412
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GA702
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ64GA704
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ64GA705
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x4000
.include "p24fj_ga_705.inc"
.endif

.ifdef __24FJ64GB002
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GB004
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GB106
.include "p24FJ64GB106.inc"

.endif

.ifdef __24FJ64GB108
.include "p24FJ64GB108.inc"

.endif

.ifdef __24FJ64GB110
.include "p24FJ64GB110.inc"

.endif

.ifdef __24FJ64GB202
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GB204
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GB406
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GB410
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GB412
.equ FLASH_SIZE,     0xa800
.equ RAM_SIZE,       0x2000
.include "p24f_config.inc"
.endif

.ifdef __24FJ64GC006
.endif

.ifdef __24FJ64GC010
.endif

.ifdef __24FJ64GL302
.endif

.ifdef __24FJ64GL303
.endif

.ifdef __24FJ64GL305
.endif

.ifdef __24FJ64GL306
.endif

.ifdef __24FJ96GA006
.endif

.ifdef __24FJ96GA008
.endif

.ifdef __24FJ96GA010
.endif

.ifdef __24FV32KA301
.equ FLASH_SIZE,     0x5800
.equ EEPROM_SIZE,    0x0200
.equ RAM_SIZE,       0x0800
.include "p24fk_config.inc"
.endif

.ifdef __24FV32KA302
.equ FLASH_SIZE,     0x5800
.equ EEPROM_SIZE,    0x0200
.equ RAM_SIZE,       0x0800
.include "p24fk_config.inc"
.endif

.ifdef __24FV32KA304
.equ FLASH_SIZE,     0x5800
.equ EEPROM_SIZE,    0x0200
.equ RAM_SIZE,       0x0800
.include "p24fk_config.inc"
.endif

.ifdef __24HJ128GP202
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP204
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP206
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP206A
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP210
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP210A
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP306
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP306A
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP310
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP310A
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP502
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP504
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP506
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP506A
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP510
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ128GP510A
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x2000
.include "p24hj_config.inc"
.endif

.ifdef __24HJ256GP206
.endif

.ifdef __24HJ256GP206A
.endif

.ifdef __24HJ256GP210
.endif

.ifdef __24HJ256GP210A
.endif

.ifdef __24HJ256GP610
.endif

.ifdef __24HJ256GP610A
.endif

.ifdef __24HJ32GP202
.endif

.ifdef __24HJ32GP204
.endif

.ifdef __24HJ32GP302
.endif

.ifdef __24HJ32GP304
.endif

.ifdef __24HJ64GP202
.endif

.ifdef __24HJ64GP204
.endif

.ifdef __24HJ64GP206
.endif

.ifdef __24HJ64GP206A
.endif

.ifdef __24HJ64GP210
.endif

.ifdef __24HJ64GP210A
.endif

.ifdef __24HJ64GP502
.endif

.ifdef __24HJ64GP504
.endif

.ifdef __24HJ64GP506
.endif

.ifdef __24HJ64GP506A
.endif

.ifdef __24HJ64GP510
.endif

.ifdef __24HJ64GP510A
.endif

.ifdef __30F1010
.endif

.ifdef __30F2010
.endif

.ifdef __30F2011
.endif

.ifdef __30F2012
.endif

.ifdef __30F2020
.endif

.ifdef __30F2023
.endif

.ifdef __30F3010
.endif

.ifdef __30F3011
.endif

.ifdef __30F3012
.endif

.ifdef __30F3013
.endif

.ifdef __30F3014
.endif

.ifdef __30F4011
.endif

.ifdef __30F4012
.equ FLASH_SIZE,     0x8000
.equ EEPROM_SIZE,    0x0400
.equ RAM_SIZE,       0x0800
.include "p30f_config.inc"
.endif

.ifdef __30F4013
.endif

.ifdef __30F5011
.endif

.ifdef __30F5013
.endif

.ifdef __30F5015
.endif

.ifdef __30F5016
.endif

.ifdef __30F6010
.endif

.ifdef __30F6010A
.endif

.ifdef __30F6011
.endif

.ifdef __30F6011A
.endif

.ifdef __30F6012
.endif

.ifdef __30F6012A
.endif

.ifdef __30F6013
.endif

.ifdef __30F6013A
.endif

.ifdef __30F6014
.endif

.ifdef __30F6014A
.endif

.ifdef __30F6015
.endif

.ifdef __33CH128MP202
.endif

.ifdef __33CH128MP202S1
.endif

.ifdef __33CH128MP203
.endif

.ifdef __33CH128MP203S1
.endif

.ifdef __33CH128MP205
.endif

.ifdef __33CH128MP205S1
.endif

.ifdef __33CH128MP206
.endif

.ifdef __33CH128MP206S1
.endif

.ifdef __33CH128MP208
.endif

.ifdef __33CH128MP208S1
.endif

.ifdef __33CH128MP502
.endif

.ifdef __33CH128MP502S1
.endif

.ifdef __33CH128MP503
.endif

.ifdef __33CH128MP503S1
.endif

.ifdef __33CH128MP505
.endif

.ifdef __33CH128MP505S1
.endif

.ifdef __33CH128MP506
.endif

.ifdef __33CH128MP506S1
.endif

.ifdef __33CH128MP508
.endif

.ifdef __33CH128MP508S1
.endif

.ifdef __33CH256MP205
.endif

.ifdef __33CH256MP205S1
.endif

.ifdef __33CH256MP206
.endif

.ifdef __33CH256MP206S1
.endif

.ifdef __33CH256MP208
.endif

.ifdef __33CH256MP208S1
.endif

.ifdef __33CH256MP505
.endif

.ifdef __33CH256MP505S1
.endif

.ifdef __33CH256MP506
.endif

.ifdef __33CH256MP506S1
.endif

.ifdef __33CH256MP508
.endif

.ifdef __33CH256MP508S1
.endif

.ifdef __33CH512MP205
.endif

.ifdef __33CH512MP205S1
.endif

.ifdef __33CH512MP206
.endif

.ifdef __33CH512MP206S1
.endif

.ifdef __33CH512MP208
.endif

.ifdef __33CH512MP208S1
.endif

.ifdef __33CH512MP505
.endif

.ifdef __33CH512MP505S1
.endif

.ifdef __33CH512MP506
.endif

.ifdef __33CH512MP506S1
.endif

.ifdef __33CH512MP508
.endif

.ifdef __33CH512MP508S1
.endif

.ifdef __33CH64MP202
.endif

.ifdef __33CH64MP202S1
.endif

.ifdef __33CH64MP203
.endif

.ifdef __33CH64MP203S1
.endif

.ifdef __33CH64MP205
.endif

.ifdef __33CH64MP205S1
.endif

.ifdef __33CH64MP206
.endif

.ifdef __33CH64MP206S1
.endif

.ifdef __33CH64MP208
.endif

.ifdef __33CH64MP208S1
.endif

.ifdef __33CH64MP502
.endif

.ifdef __33CH64MP502S1
.endif

.ifdef __33CH64MP503
.endif

.ifdef __33CH64MP503S1
.endif

.ifdef __33CH64MP505
.endif

.ifdef __33CH64MP505S1
.endif

.ifdef __33CH64MP506
.endif

.ifdef __33CH64MP506S1
.endif

.ifdef __33CH64MP508
.endif

.ifdef __33CH64MP508S1
.endif

.ifdef __33CK128MP202
.endif

.ifdef __33CK128MP203
.endif

.ifdef __33CK128MP205
.endif

.ifdef __33CK128MP206
.endif

.ifdef __33CK128MP208
.endif

.ifdef __33CK128MP502
.endif

.ifdef __33CK128MP503
.endif

.ifdef __33CK128MP505
.endif

.ifdef __33CK128MP506
.endif

.ifdef __33CK128MP508
.endif

.ifdef __33CK256MP202
.endif

.ifdef __33CK256MP203
.endif

.ifdef __33CK256MP205
.endif

.ifdef __33CK256MP206
.endif

.ifdef __33CK256MP208
.endif

.ifdef __33CK256MP502
.endif

.ifdef __33CK256MP503
.endif

.ifdef __33CK256MP505
.endif

.ifdef __33CK256MP506
.endif

.ifdef __33CK256MP508
.endif

.ifdef __33CK32MC102
.endif

.ifdef __33CK32MC103
.endif

.ifdef __33CK32MC105
.endif

.ifdef __33CK32MP102
.endif

.ifdef __33CK32MP103
.endif

.ifdef __33CK32MP105
.endif

.ifdef __33CK32MP202
.endif

.ifdef __33CK32MP203
.endif

.ifdef __33CK32MP205
.endif

.ifdef __33CK32MP206
.endif

.ifdef __33CK32MP502
.endif

.ifdef __33CK32MP503
.endif

.ifdef __33CK32MP505
.endif

.ifdef __33CK32MP506
.endif

.ifdef __33CK64MC102
.endif

.ifdef __33CK64MC103
.endif

.ifdef __33CK64MC105
.endif

.ifdef __33CK64MP102
.endif

.ifdef __33CK64MP103
.endif

.ifdef __33CK64MP105
.endif

.ifdef __33CK64MP202
.endif

.ifdef __33CK64MP203
.endif

.ifdef __33CK64MP205
.endif

.ifdef __33CK64MP206
.endif

.ifdef __33CK64MP208
.endif

.ifdef __33CK64MP502
.endif

.ifdef __33CK64MP503
.endif

.ifdef __33CK64MP505
.endif

.ifdef __33CK64MP506
.endif

.ifdef __33CK64MP508
.endif

.ifdef __33EDV64MC205
.endif

.ifdef __33EP128GM304
.endif

.ifdef __33EP128GM306
.endif

.ifdef __33EP128GM310
.endif

.ifdef __33EP128GM604
.endif

.ifdef __33EP128GM706
.endif

.ifdef __33EP128GM710
.endif

.ifdef __33EP128GP502
.endif

.ifdef __33EP128GP504
.endif

.ifdef __33EP128GP506
.endif

.ifdef __33EP128GS702
.endif

.ifdef __33EP128GS704
.endif

.ifdef __33EP128GS705
.endif

.ifdef __33EP128GS706
.endif

.ifdef __33EP128GS708
.endif

.ifdef __33EP128GS804
.endif

.ifdef __33EP128GS805
.endif

.ifdef __33EP128GS806
.endif

.ifdef __33EP128GS808
.endif

.ifdef __33EP128MC202
.endif

.ifdef __33EP128MC204
.endif

.ifdef __33EP128MC206
.endif

.ifdef __33EP128MC502
.endif

.ifdef __33EP128MC504
.endif

.ifdef __33EP128MC506
.endif

.ifdef __33EP256GM304
.endif

.ifdef __33EP256GM306
.endif

.ifdef __33EP256GM310
.endif

.ifdef __33EP256GM604
.endif

.ifdef __33EP256GM706
.endif

.ifdef __33EP256GM710
.endif

.ifdef __33EP256GP502
.equ FLASH_SIZE,     0x2a800
.equ RAM_SIZE,       0x8000
.include "p33e_config.inc"
.endif

.ifdef __33EP256GP504
.endif

.ifdef __33EP256GP506
.endif

.ifdef __33EP256MC202
.endif

.ifdef __33EP256MC204
.endif

.ifdef __33EP256MC206
.endif

.ifdef __33EP256MC502
.endif

.ifdef __33EP256MC504
.endif

.ifdef __33EP256MC506
.endif

.ifdef __33EP256MU806
.endif

.ifdef __33EP256MU810
.endif

.ifdef __33EP256MU814
.endif

.ifdef __33EP32GP502
.endif

.ifdef __33EP32GP503
.endif

.ifdef __33EP32GP504
.endif

.ifdef __33EP32GS202
.endif

.ifdef __33EP32GS502
.endif

.ifdef __33EP32GS504
.endif

.ifdef __33EP32GS505
.endif

.ifdef __33EP32GS506
.endif

.ifdef __33EP32MC202
.endif

.ifdef __33EP32MC203
.endif

.ifdef __33EP32MC204
.endif

.ifdef __33EP32MC502
.endif

.ifdef __33EP32MC503
.endif

.ifdef __33EP32MC504
.endif

.ifdef __33EP512GM304
.endif

.ifdef __33EP512GM306
.endif

.ifdef __33EP512GM310
.endif

.ifdef __33EP512GM604
.endif

.ifdef __33EP512GM706
.endif

.ifdef __33EP512GM710
.endif

.ifdef __33EP512GP502
.endif

.ifdef __33EP512GP504
.endif

.ifdef __33EP512GP506
.endif

.ifdef __33EP512GP806
.endif

.ifdef __33EP512MC202
.endif

.ifdef __33EP512MC204
.endif

.ifdef __33EP512MC206
.endif

.ifdef __33EP512MC502
.endif

.ifdef __33EP512MC504
.endif

.ifdef __33EP512MC506
.endif

.ifdef __33EP512MC806
.endif

.ifdef __33EP512MU810
.endif

.ifdef __33EP512MU814
.endif

.ifdef __33EP64GP502
.endif

.ifdef __33EP64GP503
.endif

.ifdef __33EP64GP504
.endif

.ifdef __33EP64GP506
.endif

.ifdef __33EP64GS502
.endif

.ifdef __33EP64GS504
.endif

.ifdef __33EP64GS505
.endif

.ifdef __33EP64GS506
.endif

.ifdef __33EP64GS708
.endif

.ifdef __33EP64GS804
.endif

.ifdef __33EP64GS805
.endif

.ifdef __33EP64GS806
.endif

.ifdef __33EP64GS808
.endif

.ifdef __33EP64MC202
.endif

.ifdef __33EP64MC203
.endif

.ifdef __33EP64MC204
.endif

.ifdef __33EP64MC206
.endif

.ifdef __33EP64MC502
.endif

.ifdef __33EP64MC503
.endif

.ifdef __33EP64MC504
.endif

.ifdef __33EP64MC506
.endif

.ifdef __33EV128GM002
.endif

.ifdef __33EV128GM003
.endif

.ifdef __33EV128GM004
.endif

.ifdef __33EV128GM006
.endif

.ifdef __33EV128GM102
.endif

.ifdef __33EV128GM103
.endif

.ifdef __33EV128GM104
.endif

.ifdef __33EV128GM106
.endif

.ifdef __33EV256GM002
.endif

.ifdef __33EV256GM003
.endif

.ifdef __33EV256GM004
.endif

.ifdef __33EV256GM006
.endif

.ifdef __33EV256GM102
.endif

.ifdef __33EV256GM103
.endif

.ifdef __33EV256GM104
.include "p33EV256GM104.inc"

.endif

.ifdef __33EV256GM106
.include "p33EV256GM106.inc"

.endif

.ifdef __33EV32GM002
.include "p33EV32GM002.inc"

.endif

.ifdef __33EV32GM003
.include "p33EV32GM003.inc"

.endif

.ifdef __33EV32GM004
.include "p33EV32GM004.inc"

.endif

.ifdef __33EV32GM006
.include "p33EV32GM006.inc"

.endif

.ifdef __33EV32GM102
.include "p33EV32GM102.inc"

.endif

.ifdef __33EV32GM103
.include "p33EV32GM103.inc"

.endif

.ifdef __33EV32GM104
.include "p33EV32GM104.inc"

.endif

.ifdef __33EV32GM106
.include "p33EV32GM106.inc"

.endif

.ifdef __33EV64GM002
.include "p33EV64GM002.inc"

.endif

.ifdef __33EV64GM003
.include "p33EV64GM003.inc"

.endif

.ifdef __33EV64GM004
.include "p33EV64GM004.inc"

.endif

.ifdef __33EV64GM006
.include "p33EV64GM006.inc"

.endif

.ifdef __33EV64GM102
.include "p33EV64GM102.inc"

.endif

.ifdef __33EV64GM103
.include "p33EV64GM103.inc"

.endif

.ifdef __33EV64GM104
.include "p33EV64GM104.inc"

.endif

.ifdef __33EV64GM106
.include "p33EV64GM106.inc"

.endif

.ifdef __33FJ06GS001
.include "p33FJ06GS001.inc"

.endif

.ifdef __33FJ06GS101
.include "p33FJ06GS101.inc"

.endif

.ifdef __33FJ06GS101A
.include "p33FJ06GS101A.inc"

.endif

.ifdef __33FJ06GS102
.include "p33FJ06GS102.inc"

.endif

.ifdef __33FJ06GS102A
.include "p33FJ06GS102A.inc"

.endif

.ifdef __33FJ06GS202
.include "p33FJ06GS202.inc"

.endif

.ifdef __33FJ06GS202A
.include "p33FJ06GS202A.inc"

.endif

.ifdef __33FJ09GS302
.include "p33FJ09GS302.inc"

.endif

.ifdef __33FJ128GP202
.include "p33FJ128GP202.inc"

.endif

.ifdef __33FJ128GP204
.include "p33FJ128GP204.inc"

.endif

.ifdef __33FJ128GP206
.include "p33FJ128GP206.inc"

.endif

.ifdef __33FJ128GP206A
.include "p33FJ128GP206A.inc"

.endif

.ifdef __33FJ128GP306
.include "p33FJ128GP306.inc"

.endif

.ifdef __33FJ128GP306A
.include "p33FJ128GP306A.inc"

.endif

.ifdef __33FJ128GP310
.include "p33FJ128GP310.inc"

.endif

.ifdef __33FJ128GP310A
.include "p33FJ128GP310A.inc"

.endif

.ifdef __33FJ128GP706
.include "p33FJ128GP706.inc"

.endif

.ifdef __33FJ128GP706A
.include "p33FJ128GP706A.inc"

.endif

.ifdef __33FJ128GP708
.include "p33FJ128GP708.inc"

.endif

.ifdef __33FJ128GP708A
.include "p33FJ128GP708A.inc"

.endif

.ifdef __33FJ128GP710
.include "p33FJ128GP710.inc"

.endif

.ifdef __33FJ128GP710A
.include "p33FJ128GP710A.inc"

.endif

.ifdef __33FJ128GP802
.equ FLASH_SIZE,     0x15600
.equ RAM_SIZE,       0x4000
.include "p33_config.inc"
.equ VALID_CONF,1
.endif

.ifdef __33FJ128GP804
.include "p33FJ128GP804.inc"

.endif

.ifdef __33FJ128MC202
.include "p33FJ128MC202.inc"

.endif

.ifdef __33FJ128MC204
.include "p33FJ128MC204.inc"

.endif

.ifdef __33FJ128MC506
.include "p33FJ128MC506.inc"

.endif

.ifdef __33FJ128MC506A
.include "p33FJ128MC506A.inc"

.endif

.ifdef __33FJ128MC510
.include "p33FJ128MC510.inc"

.endif

.ifdef __33FJ128MC510A
.include "p33FJ128MC510A.inc"

.endif

.ifdef __33FJ128MC706
.include "p33FJ128MC706.inc"

.endif

.ifdef __33FJ128MC706A
.include "p33FJ128MC706A.inc"

.endif

.ifdef __33FJ128MC708
.include "p33FJ128MC708.inc"

.endif

.ifdef __33FJ128MC708A
.include "p33FJ128MC708A.inc"

.endif

.ifdef __33FJ128MC710
.include "p33FJ128MC710.inc"

.endif

.ifdef __33FJ128MC710A
.include "p33FJ128MC710A.inc"

.endif

.ifdef __33FJ128MC802
.include "p33FJ128MC802.inc"

.endif

.ifdef __33FJ128MC804
.include "p33FJ128MC804.inc"

.endif

.ifdef __33FJ256GP506
.include "p33FJ256GP506.inc"

.endif

.ifdef __33FJ256GP506A
.include "p33FJ256GP506A.inc"

.endif

.ifdef __33FJ256GP510
.include "p33FJ256GP510.inc"

.endif

.ifdef __33FJ256GP510A
.include "p33FJ256GP510A.inc"

.endif

.ifdef __33FJ256GP710
.include "p33FJ256GP710.inc"

.endif

.ifdef __33FJ256GP710A
.include "p33FJ256GP710A.inc"

.endif

.ifdef __33FJ256MC510
.include "p33FJ256MC510.inc"

.endif

.ifdef __33FJ256MC510A
.include "p33FJ256MC510A.inc"

.endif

.ifdef __33FJ256MC710
.include "p33FJ256MC710.inc"

.endif

.ifdef __33FJ256MC710A
.include "p33FJ256MC710A.inc"

.endif

.ifdef __33FJ32GP101
.include "p33FJ32GP101.inc"

.endif

.ifdef __33FJ32GP102
.include "p33FJ32GP102.inc"

.endif

.ifdef __33FJ32GP104
.include "p33FJ32GP104.inc"

.endif

.ifdef __33FJ32GP202
.include "p33FJ32GP202.inc"

.endif

.ifdef __33FJ32GP204
.include "p33FJ32GP204.inc"

.endif

.ifdef __33FJ32GP302
.include "p33FJ32GP302.inc"

.endif

.ifdef __33FJ32GP304
.include "p33FJ32GP304.inc"

.endif

.ifdef __33FJ32GS406
.include "p33FJ32GS406.inc"

.endif

.ifdef __33FJ32GS606
.include "p33FJ32GS606.inc"

.endif

.ifdef __33FJ32GS608
.include "p33FJ32GS608.inc"

.endif

.ifdef __33FJ32GS610
.include "p33FJ32GS610.inc"

.endif

.ifdef __33FJ32MC101
.include "p33FJ32MC101.inc"

.endif

.ifdef __33FJ32MC102
.include "p33FJ32MC102.inc"

.endif

.ifdef __33FJ32MC104
.include "p33FJ32MC104.inc"

.endif

.ifdef __33FJ32MC202
.include "p33FJ32MC202.inc"

.endif

.ifdef __33FJ32MC204
.include "p33FJ32MC204.inc"

.endif

.ifdef __33FJ32MC302
.include "p33FJ32MC302.inc"

.endif

.ifdef __33FJ32MC304
.include "p33FJ32MC304.inc"

.endif

.ifdef __33FJ64GP202
.include "p33FJ64GP202.inc"

.endif

.ifdef __33FJ64GP204
.include "p33FJ64GP204.inc"

.endif

.ifdef __33FJ64GP206
.include "p33FJ64GP206.inc"

.endif

.ifdef __33FJ64GP206A
.include "p33FJ64GP206A.inc"

.endif

.ifdef __33FJ64GP306
.include "p33FJ64GP306.inc"

.endif

.ifdef __33FJ64GP306A
.include "p33FJ64GP306A.inc"

.endif

.ifdef __33FJ64GP310
.include "p33FJ64GP310.inc"

.endif

.ifdef __33FJ64GP310A
.include "p33FJ64GP310A.inc"

.endif

.ifdef __33FJ64GP706
.include "p33FJ64GP706.inc"

.endif

.ifdef __33FJ64GP706A
.include "p33FJ64GP706A.inc"

.endif

.ifdef __33FJ64GP708
.include "p33FJ64GP708.inc"

.endif

.ifdef __33FJ64GP708A
.include "p33FJ64GP708A.inc"

.endif

.ifdef __33FJ64GP710
.include "p33FJ64GP710.inc"

.endif

.ifdef __33FJ64GP710A
.include "p33FJ64GP710A.inc"

.endif

.ifdef __33FJ64GP802
.include "p33FJ64GP802.inc"

.endif

.ifdef __33FJ64GP804
.include "p33FJ64GP804.inc"

.endif

.ifdef __33FJ64GS406
.include "p33FJ64GS406.inc"

.endif

.ifdef __33FJ64GS606
.include "p33FJ64GS606.inc"

.endif

.ifdef __33FJ64GS608
.include "p33FJ64GS608.inc"

.endif

.ifdef __33FJ64GS610
.include "p33FJ64GS610.inc"

.endif

.ifdef __33FJ64MC202
.include "p33FJ64MC202.inc"

.endif

.ifdef __33FJ64MC204
.include "p33FJ64MC204.inc"

.endif

.ifdef __33FJ64MC506
.include "p33FJ64MC506.inc"

.endif

.ifdef __33FJ64MC506A
.include "p33FJ64MC506A.inc"

.endif

.ifdef __33FJ64MC508
.include "p33FJ64MC508.inc"

.endif

.ifdef __33FJ64MC508A
.include "p33FJ64MC508A.inc"

.endif

.ifdef __33FJ64MC510
.include "p33FJ64MC510.inc"

.endif

.ifdef __33FJ64MC510A
.include "p33FJ64MC510A.inc"

.endif

.ifdef __33FJ64MC706
.include "p33FJ64MC706.inc"

.endif

.ifdef __33FJ64MC706A
.include "p33FJ64MC706A.inc"

.endif

.ifdef __33FJ64MC710
.include "p33FJ64MC710.inc"

.endif

.ifdef __33FJ64MC710A
.include "p33FJ64MC710A.inc"

.endif

.ifdef __33FJ64MC802
.include "p33FJ64MC802.inc"

.endif

.ifdef __33FJ64MC804
.include "p33FJ64MC804.inc"

.endif

.ifndef FLASH_SIZE
.error "processor configuration has not been defined"
.endif
