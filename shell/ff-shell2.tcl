#! /usr/bin/wish
#                    ff-shell.tcl
# The Manual
# ----------
# This simple shell is built around the Tcl/Tk text widget.
# Using a custom binding, key presses sent to the widget
# are redirected to the serial port and the FlashForth micro
# attached to that port.  Incoming characters from the micro
# are received from the serial port and are inserted into the
# widget at the end of the text.  An update to the GUI is
# triggered at the end of every line.
#
# A file is sent to the serial port, one line at a time.
# To allow the GUI to update smoothly in this single-threaded
# program, the lines to send are accumulated into a list and a
# procedure to send the first line is scheduled.
# For each line to be sent, the procedure checks if it is
# still waiting for a carriage-return from the microcontroller.
# If it is, the work of sending the line is rescheduled for 
# a later time.  If it is not waiting, the line is sent
# one character at a time.  Incoming characters are inserted 
# at the end of the text widget as they arrive.
#
# Copy-and-Paste insertions to the text widget are intercepted
# by the <<Paste>> binding and handled in a similar manner
# as sending lines from a file.
#
# At any point in time, the text from the widget may be saved
# to a file.  This might be good a way to save a session or
# collect large amounts of output from the microcontroller.
#
# You will need to run with sufficient privilege to access 
# the serial port.  
# On Ubuntu, this can be done by starting the program like so:
# $ sudo ./ff-shell.tcl
#
# Author
# ------
# P.A.Jacobs
# School of Engineering, Uni of Qld.
#
# Version
# -------
# 2015-04-03 
# Initial code cobbled together from a few examples, especially
# Rolf Schroedter's simple terminal at http://wiki.tcl.tk/3642
# and Mikael Nordman's ff-shell.py.
#
# Licence
# -------
# GPL, as per the rest of FlashForth.
#
# --------------------------------------------------------------
# Configuration
# Set defaults that suit your environment.

set ::baudRate 38400
set ::handShake xonxoff; # none xonxoff rtscts
if { [string equal $tcl_platform(platform) windows] } {
    console show
    set ::serialPortName "com2"
} else {
    set ::serialPortName "/dev/ttyACM0"
}; # end if

set ::textWidth 72
set ::textHeight 20
set ::textFont Courier

set ::afterLineMilliseconds 200

# --------------------------------------------------------------
# Sending and receiving characters, one at a time.

set ::waitingForCR false

proc serialIn { channel } {
    foreach character [split [read $channel] {}] {
	# scan $character "%c" asciiCode
	# puts -nonewline stdout [format " 0x%02x " $asciiCode]
	# flush stdout
	switch -regexp -- $character {
	    \x07 { bell }
	    \x08 { deleteLastChar }
	    \x09 { addToLogText $character }
	    \x0a { } 
	    \x0d { addToLogText "\n"; set ::waitingForCR false; update }
	    \x11 { puts -nonewline Xon }
	    \x13 { puts -nonewline Xoff }
	    [\x20-\x7e] { addToLogText $character }
	}; # end switch
    }
}; # end serialIn

proc serialOut { channel character } {
    # puts -nonewline $channel $character; flush $channel
    # send CR and BS through but not LF
    switch -regexp -- $character {
	\x0a {}
	\x08 -
	\x09 -
	\x0d -
	\x0f -
	[\x20-\x7e] { puts -nonewline $channel $character; flush $channel }
    }; # end switch
}; # end serialOut

proc openSerialPort {} {
    set ::tty [open $::serialPortName r+]
    puts "Serial channel is $::tty"
    # We allow a short timeout period to prevent the read function
    # from stalling for too long.
    chan configure $::tty -mode $::baudRate,n,8,1 -timeout 100 \
	-encoding binary -translation binary -handshake $::handShake \
	-buffering none -blocking true
    chan event $::tty readable [list serialIn $::tty]
}; # end openSerialPort

proc closeSerialPort {} {
    if { [catch {close $::tty} err] } {
	puts "Close serial port failed: $err"
    }
}; # end closeSerialPort

# --------------------------------------------------------------
# GUI elements
wm title . "FlashForth Shell"
# Main menu allow us a convenient way to exit.
menu .mb
. configure -menu .mb
menu .mb.file -tearoff 0
.mb.file add command -label "Send..." -command { sendFile }
.mb.file add command -label "Exit" -command { displayExitDialog }
.mb add cascade -label File -menu .mb.file
menu .mb.log -tearoff 0
.mb.log add command -label "Clear" -command { clearLogText }
.mb.log add command -label "Save..." -command { saveLogText }
.mb add cascade -label Log -menu .mb.log
menu .mb.micro -tearoff 0
.mb.micro add command -label "Warm restart" -command { warmRestart }
.mb add cascade -label Micro -menu .mb.micro
menu .mb.help -tearoff 0
.mb.help add command -label "About..." -command { displayAboutMessage }
.mb.help add command -label "Hints" -command { displayHints }
.mb add cascade -label Help -menu .mb.help

proc displayExitDialog {} {
    if [tk_messageBox -type yesno -icon question -message "Really exit?"] {
	closeSerialPort
	exit
    }
}

proc displayAboutMessage {} {
    tk_messageBox -type ok -icon info -parent . \
	-message "ff-shell in Tcl\nA simple shell for FlashForth.\n2015-04-04"
}

proc displayHints {} {
    set message {
	"\n----------------------------------------------------------"
	"\nType directly into the text window.  Characters will go to"
	"\nthe microcontroller, one at a time.  Incoming characters"
	"\nfrom the microcontroller will appear in the text window."
	"\n"
	"\nSending a file: For every line of the file, characters go"
	"\none at a time to the microcontroller, but the shell will"
	"\nwait for a carriage-return from the microcontroller before"
	"\nsending the next line."
	"\n"
	"\nPasting a selection of text works in a similar way to"
	"\nsending a file.  You should be able to paste large sections"
	"\nof text without overruns."
	"\n"
	"\nKeyboard short-cuts:"
	"\nControl-Shift-v  send selection to micro"
	"\nControl-Shift-s  save log"
	"\nControl-Shift-x  exit"
	"\nControl-Shift-o  warm restart of micro"
	"\n----------------------------------------------------------"
	"\n"
    }
    foreach line $message { addToLogText $line }
}

# A scrolling text window to log messages
set textFrame [frame .tf]
set ::logText [text .tf.t -height $::textHeight -width $::textWidth \
    -font $::textFont -wrap char \
    -yscrollcommand [list $textFrame.vsb set] ]
set textScrollBar [scrollbar .tf.vsb -orient vertical \
    -command {$::logText yview} ]
pack $::logText -side left -expand 1 -fill both
pack $textScrollBar -side left -fill y
pack $textFrame -fill both -expand 1

proc addToLogText { txt } {
    $::logText insert end "$txt"
    $::logText yview moveto 1.0
}

proc deleteLastChar {} {
    $::logText delete "end-2c"
    $::logText yview moveto 1.0
}

proc clearLogText {} {
    $::logText delete 1.0 end
}

set ::saveFileName {}

proc saveLogText {} {
    set ::saveFileName [tk_getSaveFile -initialfile $::saveFileName \
			   -title "Save log text to file"]
    if {[string length $::saveFileName] > 0} {
	set fp [open $::saveFileName "w"]
	puts $fp [$::logText get 1.0 end]
	close $fp
    }
}; # end saveLogText

proc sendTextChar { character } {
    # We use this function in the key-binding for the logText widget.
    # The break is to stop the default binding from inserting another
    # character into the widget.
    serialOut $::tty $character
    return -code break
}

set ::linesToSend {}

proc sendFirstLine {} {
    if {$::waitingForCR} {
	# reschedule the current work
	after $::afterLineMilliseconds sendFirstLine
	return
    }
    # Pop the first line from the list and send it.
    set line [lindex $::linesToSend 0]
    set ::linesToSend [lreplace $::linesToSend 0 0]
    foreach character [split $line {}] {
	serialOut $::tty $character
	after 1; # 1ms pause after each character
    }
    serialOut $::tty "\r"
    set ::waitingForCR true
    if {[llength $::linesToSend] > 0} {
	# there is more work to do
	after $::afterLineMilliseconds sendFirstLine
    }
}; # end sendLine

set ::sendFileName {}

proc sendFile {} {
    set ::sendFileName [tk_getOpenFile -initialfile $::sendFileName \
			   -title "Open file to send"]
    if {[string length $::sendFileName] > 0} {
	set fp [open $::sendFileName "r"]
	while {[gets $fp line] >= 0} { lappend ::linesToSend $line } 
	close $fp
	sendFirstLine
    }
}; # end sendFile

proc sendSelection { text } {
    foreach line [split $text "\n"] { lappend ::linesToSend $line }
    sendFirstLine
    return -code break
}; # end sendSelection

# The following binding redirects key presses in the text widget
# to the serial-port.
bind $::logText <Any-Key> [list sendTextChar %A]

# The following virtual event can be triggered with Control-Shift-v
# or with clicking the middle mouse button in X-Windows.
# The selected region of text is sent to the serial port.
event add <<PasteSelection>> <Control-V>
bind $::logText <<PasteSelection>> { sendSelection [selection get] }

# Keyboard short-cuts.
bind $::logText <Control-F> { sendFile }
bind $::logText <Control-S> { saveLogText }
bind $::logText <Control-X> { displayExitDialog }

proc warmRestart {} {
    serialOut $::tty "\x0f"; # Control-O
}

# --------------------------------------------------------------
# Initialize streams and hand control over to the event loop.
openSerialPort
update idletasks
focus $::logText
