#!/bin/sh
# Add this XC8 global options in the MPLABX project
# -DOPERATOR_UART="${OPERATOR_UART}"

make clean all MP_PROCESSOR_OPTION=ATmega328  OPERATOR_UART=0
cp dist/default/production/FF-ATMEGA.X.production.hex  ../hex/328-16MHz-38400.hex
make clean all MP_PROCESSOR_OPTION=ATmega32U4 OPERATOR_UART=0
cp dist/default/production/FF-ATMEGA.X.production.hex  ../hex/32u4-16MHz-UART38400.hex
make clean all MP_PROCESSOR_OPTION=ATmega32U4 OPERATOR_UART=3
cp dist/default/production/FF-ATMEGA.X.production.hex  ../hex/32u4-16MHz-USB.hex
make clean all MP_PROCESSOR_OPTION=ATmega2560 OPERATOR_UART=0
cp dist/default/production/FF-ATMEGA.X.production.hex  ../hex/2560-16MHz-38400.hex
