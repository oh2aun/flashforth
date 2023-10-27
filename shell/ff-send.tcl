#!/usr/bin/tclsh
# Simple shell script to upload forth source files to FlashForth.
# An arbitrary number of files to be sent can be given.
#
# You will need to run with sufficient privilege to access 
# the serial port.  
# On Ubuntu, add the user to the dialout group:
# $ sudo adduser <username> dialout
#
# Usage:
# $ ff-send.tcl filename1 filenameN
#
# Optionally, you may start the program with a specified
# serial-port device and speed.
# $ ff-send.tcl -device /dev/ttyACM0 -speed 38400 filename1 filenameN
# or
# $ ff-send.tcl -port /dev/ttyACM0 -baud 38400 filename1 filenameN
#
# Author
# ------
# Mikael Nordman
#
# Version
# -------
# 2023-10-27 Initial version based on ff-shell.tcl by P.A. Jacobs 
#
# Licence
# -------
# GPL, as per the rest of FlashForth.
#
# --------------------------------------------------------------
# Sending and receiving characters.
namespace eval serialPort {
    variable baudRate 38400
    variable parity "n"; # n=none e=even o=odd m=mark s=space
    variable dataBits 8; # 7 8
    variable stopBits 1; # 1 2
    variable parityAndBits "$parity,$dataBits,$stopBits"
    variable handShake none; # none xonxoff rtscts
    variable name /dev/ttyACM0
    if { [string equal $::tcl_platform(platform) windows] } {
	set name {\\.\com5}
    }; # end if
    variable state closed
    variable tty; # this will be our Tcl channel
    
    proc receive {} {
	# Receive and process characters that have arrived.
	variable tty
        variable recordingFile
        # First, unregister while we are busy servicing the incoming text.
        chan event $tty readable {}
	if { [chan eof $tty] } {
	    puts "Oops serial port read: eof"
            flush stdout
	    serialPort::stop
	    return
	}
	if { [catch {chan read $tty} incomingText] } {
	    puts "Oops serial port read: $incomingText"
            flush stdout
	    return
	}
	if { [string length $incomingText] == 0 } {
	    return
	}
	puts -nonewline $incomingText
       foreach character [split $incomingText {}] {
            # binary scan $character "c" charCode
            # puts [format "character: %s 0x%02x" $character $charCode]
            switch -regexp -- $character {
                \x0a { } 
                \x0d { smartSend::resetFlags}
            }; # end switch
        }; # end foreach

        # Finally, as we leave, re-register for the next event.
        chan event $tty readable [list ::serialPort::receive]
    }; # end receive

    proc send { character } {
	variable tty
	# send BS, TAB, CR, Control-O and ESC through but not LF
	switch -regexp -- $character {
	    \x0a {}
	    \x08 -
	    \x09 -
	    \x0d -
	    \x0f -
            \x1b -
	    [\x20-\x7e] { puts -nonewline $tty $character; flush $tty }
	}; # end switch
    }; # end send

    proc start {} {
	variable name
	variable baudRate
	variable parityAndBits
	variable handShake
	variable tty
	variable state
	if { [catch {::open $name r+} result] } {
	    puts "serialPort::open : $result"
            set tty none
	    set state closed
	} else {
	    set tty $result 
	    puts "Serial port is open as $tty"
	    # We allow a short timeout period to prevent the read function
	    # from stalling for too long.
	    chan configure $tty -mode $baudRate,$parityAndBits -timeout 10 \
		-encoding binary -translation binary -handshake $handShake \
		-buffering none -buffersize 8192 -blocking false
	    if { [string equal $::tcl_platform(platform) windows] } {
		chan configure $tty -sysbuffer 8192
	    }
	    chan event $tty readable [list ::serialPort::receive]
	    set state open
	}
    }; # end start

    proc stop {} {
	variable tty
        variable state
        if { [string equal $tty none] } {
            set state closed
            return
        }
	if { [catch {close $tty} err] } {
	    puts "Close serial port failed: $err"
	}
        set tty none
	set state closed
    }; # end stop

    proc restart {} {
        puts "Stop and then restart serial port."
        stop
        start
    };
}; # end namespace serialPort

namespace eval smartSend {
    # Send text only when the microcontroller is ready.
    variable waitingForCR false
    variable timerForCR {}
    variable timeOutForCR false
    variable pauseBetweenChar 0; # milliseconds

    proc resetFlags {} {
        variable waitingForCR
        variable timerForCR
        variable timeOutForCR
        set waitingForCR false
        if { [string length $timerForCR] > 0 } {
            after cancel $timerForCR
            set timerForCR {}
        }
        set timeOutForCR false
    }

    proc receivedCR {} {
        resetFlags
    }

    proc sendLine { line } {
        variable waitingForCR
        variable timerForCR
        variable timeOutForCR
        variable pauseBetweenChar
        if {$timeOutForCR} {
            serialPort::restart
            resetFlags
            return -code error "sendLine failed on entry: Timeout waiting for CR."
        }
        set retries 2000
        while {$waitingForCR && ($retries > 0)} {
            # puts "sendLine waiting for CR, retries remaining: $retries"
            update
            after 1
            incr retries -1
        }
        if {$waitingForCR} {
            serialPort::restart
            resetFlags
            return -code error "sendLine failed: Waited too long for CR."
        }
        # Send all characters with pauses between and a CR at end.
        foreach character [split $line {}] {
            serialPort::send $character
            after $pauseBetweenChar; # pause after each character
        }
        serialPort::send "\r"
        set waitingForCR true
        set timerForCR [after 2000 set timeOutForCR true]
    };
    
}; # end namespace smartSend


proc sendFile { filename } {
    if {[string bytelength $filename] > 0} {
        set fp [open $filename "r"]
        while {[gets $fp line] >= 0} {
            if [catch {smartSend::sendLine $line} result] {
                puts "sendLine failed: $result"
                break
            }
        }
        close $fp
    }
}; # end sendFile

# Command-line options processing, adapted from https://wiki.tcl.tk/1730
set files {}
while {[llength $argv]} {
    # Presume that the first item is a flag and pop it off the args list.
    set argv [lassign $argv flag]
    switch -glob $flag {
        -device -
        -port { set argv [lassign $argv serialPort::name] }
        -speed -
        -baud { set argv [lassign $argv serialPort::baudRate] }
        -- { break }
        -* {
            puts "Unknown option $flag"
            set argv [lassign $argv discardValue]
        }
        default {
            # If we encounter a non-option item,
            # just accumulate it and all of the remaining items,
            # breaking the while loop.
            set files [list $flag {*}$argv]
            puts $files
            break
        }
    }; # end switch
}; # end while

smartSend::resetFlags
serialPort::start
foreach name $files  { 
        sendFile $name
   };


