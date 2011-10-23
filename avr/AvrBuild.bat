@ECHO OFF
"C:\Program Files\Atmel\AVR Tools\AvrAssembler2\avrasm2.exe" -S "C:\git\flashforth\avr\labels.tmp" -fI -W+ie -C V2E -o "C:\git\flashforth\avr\FlashForth.hex" -d "C:\git\flashforth\avr\FlashForth.obj" -e "C:\git\flashforth\avr\FlashForth.eep" -m "C:\git\flashforth\avr\FlashForth.map" "C:\git\flashforth\avr\FlashForth.asm"
