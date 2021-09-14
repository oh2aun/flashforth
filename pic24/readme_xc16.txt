FF 5 has been tested on PIC24FJ256GA705 on MPLABX 5.50 and XC 1.70. Just open the UART or USB project from within MPLABX.

Steps:
======

1. Open project, test build once to ensure that it works without issues. Newer compilers need the c config file for setting the chip config bits.

2. Edit the .inc file for your chip. Update the frequency, usb_cdc (if using usb instead of uart), the baud rate and the RPI/RPO pins used for UART

3. Check the datasheet for the flash and ram memory sizes and update that as well in the inc file.
    3.1 The last portion of the flash memory is used for configuration bits, so you will not be able to use the full size. The datasheet will specify the upper limit of flash.

    3.2 The full RAM also can't be used (from emperical data). Even though the datasheet specfies the ram size after the SFRs, from experience it doesn't seem to be the case. Set RAM = Total ram - ~ 0x800h

4. Clean, build and download the hex to the chip.
