@ECHO OFF
"C:\Program Files\Atmel\AVR Tools\AvrAssembler2\avrasm2.exe" -S "D:\git\flashforth\avr\labels.tmp" -fI -W+ie -o "D:\git\flashforth\avr\FlashForth.hex" -d "D:\git\flashforth\avr\FlashForth.obj" -e "D:\git\flashforth\avr\FlashForth.eep" -m "D:\git\flashforth\avr\FlashForth.map" "D:\git\flashforth\avr\FlashForth.asm"
