#!/bin/bash

wine asm2/avrasm2.exe -Dffm328 -Dop0 -I asm2/include/ avr/src/ff-atmega.asm -o avr/hex/328-16MHz-38400.hex -fI
wine asm2/avrasm2.exe -Dffm32u4 -Dop0 -I asm2/include/ avr/src/ff-atmega.asm -o avr/hex/32u4-16MHz-UART38400.hex -fI
wine asm2/avrasm2.exe -Dffm32u4 -Dop3 -I asm2/include/ avr/src/ff-atmega.asm -o avr/hex/32u4-16MHz-USB.hex -fI
wine asm2/avrasm2.exe -Dffm2560 -Dop0 -I asm2/include/ avr/src/ff-atmega.asm -o avr/hex/2560-16MHz-38400.hex -fI


