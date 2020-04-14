#!/usr/bin/wish
#                    ff-shell.tcl
# The Manual
# ----------
# This simple shell is built around the Tcl/Tk text widget
# and a command-entry widget.
#
# Using a custom binding, key presses sent to the text widget
# are redirected to the serial port and the FlashForth micro
# attached to that port.  Incoming characters from the micro
# are received from the serial port and are inserted into the
# widget at the end of the text.  An update to the GUI is
# triggered at the end of every line.
#
# Alternatively, you may enter text onto the command-line widget
# and, on pressing enter, the text will be sent to the FF-micro.
# Incoming characters from the micro are again inserted into the
# text widget.  The command widget provides a simple history
# recall, using the Up and Down keys.  You can also paste a selection
# from the text widget into the command widget.
# On start-up the command history is read from a hidden file
# (.ff-shell-history, if it exists) in the current directory.
# On exit, the same hidden file is overwritten with the current
# history.
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
# Alternatively, add the user to the dialout group:
# $ sudo adduser <username> dialout
#
# Optionally, you may start the program with a specified
# serial-port device and speed.
# $ ./ff-shell.tcl -device /dev/ttyACM0 -speed 38400
# or
# $ ./ff-shell.tcl -port /dev/ttyACM0 -baud 38400
#
# Author
# ------
# P.A.Jacobs
# School of Mechanical and Mining Engineering,
# University of Queensland.
#
# Version
# -------
# 2015-04-03 
#   Initial code cobbled together from a few examples, especially
#   Rolf Schroedter's simple terminal at http://wiki.tcl.tk/3642
#   and Mikael Nordman's ff-shell.py.
# 2015-04-25, 26
#   Added status line and selection of speed, etc, from the GUI.
# 2016-09-15
#   Added command-line widget with command history recall.
# 2018-04-09, 10, 11
#   Receive more than one incoming character at a time, trim log.
#   Add events to start and stop recording to a file.
#   Command-line options to set serial-port device and speed.
# 2018-07-27
#   Add an action to get the attention of the microcontroller,
#   bypassing the turnkey, so that we get the interpreter listening.
# 2018-07-28
#   Put a time-out on the waiting for CR from microcontroller.
# 2018-08-15
#   Improve the the structure of the code by introducing more namespaces
#   to separate the layers of the code.
#   Also, improve the handling of the times that the microcontroller
#   doesn't answer, such that we don't lock-up the program so easily.
# 2018-12-29
#   Adjustable intercharacter pause.
# 2018-12-30
#   Put configuration variables (and their default values) into namespaces.
# 2019-04-25
#   Introduce combobox entries to give hints for speed and handshake.
#   
#
# Licence
# -------
# GPL, as per the rest of FlashForth.
#
# --------------------------------------------------------------

if { [string equal $::tcl_platform(platform) windows] } {
    console show
}; # end if

# --------------------------------------------------------------
# A buffer for a line of text and a place to record its history.

set ::textBuffer ""

namespace eval cmdHistory {
    variable textLines {}
    variable pointer 0
    variable maxLines 50
    variable fileName ".ff-shell-history"

    proc add { line } {
	variable textLines
	variable maxLines
	if {$line == ""} return
	if [string compare $line [lindex $textLines end]] {
	    lappend textLines $line
	}
	while {[llength $textLines] > $maxLines} {
	    set textLines [lreplace $textLines 0 0]
	}
	pointToEnd
    }

    proc pointToEnd {} {
	variable textLines
	variable pointer
	set pointer [llength $textLines]
    }

    proc save {} {
	variable fileName
	variable textLines
	catch {
	    set fp [open $fileName "w"]
	    foreach line $textLines {
		set line [string trim $line]
		if {[string length $line] > 0} {
		    puts $fp "$line"
		}
	    }
	    close $fp
	}
    }

    proc read {} {
	variable fileName
	variable textLines
	if [file exists $fileName] {
	    set fp [open $fileName "r"]
	    while {[gets $fp line] >= 0} { lappend textLines $line } 
	    close $fp
	}
    }

    proc getPrevious {} {
	variable pointer
	variable textLines
	incr pointer -1
	if {$pointer < 0} { set pointer 0 }
	return [lindex $textLines $pointer]
    }

    proc getNext {} {
	variable pointer
	variable textLines
	incr pointer
	if {$pointer > [llength $textLines]} {
	    set pointer [llength $textLines]
	}
	if {$pointer < [llength $textLines]} {
	    return [lindex $textLines $pointer]
	} else {
	    return ""
	}
    }
}; # end namespace cmdHistory

# --------------------------------------------------------------
# Sending and receiving characters.

namespace eval serialPort {
    variable baudRate 38400
    variable parity "n"; # n=none e=even o=odd m=mark s=space
    variable dataBits 8; # 7 8
    variable stopBits 1; # 1 2
    variable parityAndBits "$parity,$dataBits,$stopBits"
    variable handShake xonxoff; # none xonxoff rtscts
    variable name /dev/ttyUSB0
    if { [string equal $::tcl_platform(platform) windows] } {
	set name {\\.\com5}
    }; # end if
    variable state closed
    variable tty; # this will be our Tcl channel

    variable recordingFileName {}
    variable recordingFile {}
    
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
        if { [string length $recordingFile] > 0 } {
            puts -nonewline $recordingFile $incomingText
            flush stdout
        }
        foreach character [split $incomingText {}] {
            # binary scan $character "c" charCode
            # puts [format "character: %s 0x%02x" $character $charCode]
            switch -regexp -- $character {
                \x07 { bell }
                \x08 { logText::deleteLastChar }
                \x09 { logText::add $character }
                \x0a { } 
                \x0d { logText::add "\n"
                    smartSend::resetFlags
                    logText::showTail
                }
                \x11 { puts -nonewline Xon; flush stdout }
                \x13 { puts -nonewline Xoff; flush stdout }
                [\x20-\x7e] { logText::add $character }
            }; # end switch
        }; # end foreach
        if {$logText::doTrimTextLog} { logText::trim }
	# Sometimes, we get a long stream of characters without a carriage-return,
	# or new-line and we occasionally need to show that tail.
	logText::maybeShowTail
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
    }

    proc startRecording {} {
        variable recordingFileName
        variable recordingFile
        set recordingFileName [tk_getSaveFile -initialfile $recordingFileName \
                                   -title "Start saving text to file"]
        if {[string length $recordingFileName] > 0} {
            set recordingFile [open $recordingFileName w]
        }
    }; # end startRecording

    proc stopRecording {} {
        variable recordingFile
        if { [string length $recordingFile] > 0 } {
            flush $recordingFile
            close $recordingFile
            set recordingFile {}
        }
    }; # end stopRecording
}; # end namespace serialPort


namespace eval smartSend {
    # Send text only when the microcontroller is ready.
    variable waitingForCR false
    variable timerForCR {}
    variable timeOutForCR false
    variable pauseBetweenChar 1; # milliseconds

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
        set retries 10
        while {$waitingForCR && ($retries > 0)} {
            # puts "sendLine waiting for CR, retries remaining: $retries"
            update
            after 100
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
    }
}; # end namespace smartSend

# --------------------------------------------------------------
# GUI elements
wm title . "FlashForth Shell"
image create photo applicationIcon -file terminal.gif
wm iconphoto . -default applicationIcon
# Main menu
menu .mb
. configure -menu .mb
menu .mb.file -tearoff 0
.mb.file add command -label "Send..." -command { sendFile }
.mb.file add command -label "Start Recording..." -command { serialPort::startRecording }
.mb.file add command -label "Stop Recording" -command { serialPort::stopRecording }
.mb.file add command -label "Exit" -command { displayExitDialog }
.mb add cascade -label File -menu .mb.file
menu .mb.log -tearoff 0
.mb.log add command -label "Clear" -command { logText::clear }
.mb.log add command -label "Save..." -command { logText::save }
.mb add cascade -label Log -menu .mb.log
menu .mb.micro -tearoff 0
.mb.micro add command -label "Warm restart" -command { warmRestart }
.mb.micro add command -label "Bypass turnkey" -command { bypassTurnkey }
.mb.micro add command -label "Restart serial port" -command { serialPort::restart }
.mb add cascade -label Micro -menu .mb.micro
menu .mb.help -tearoff 0
.mb.help add command -label "About..." -command { displayAboutMessage }
.mb.help add command -label "Hints" -command { displayHints }
.mb add cascade -label Help -menu .mb.help

proc displayExitDialog {} {
    if [tk_messageBox -type yesno -icon question -message "Really exit?"] {
	::serialPort::stop
	::cmdHistory::save
	exit
    }
}

proc displayAboutMessage {} {
    tk_messageBox -type ok -icon info -parent . \
	-message "ff-shell in Tcl\nA simple shell for FlashForth.\n2019-04-26"
}

proc displayHints {} {
    set message {
	"\n----------------------------------------------------------"
	"\nType directly into the text window.  Characters will go to"
	"\nthe microcontroller, one at a time.  Incoming characters"
	"\nfrom the microcontroller will appear in the text window."
	"\n"
	"\nAlternatively, type into the command-line widget and press"
	"\nEnter to send the full line."
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
	"\nControl-V  send selection to micro"
	"\nControl-s  save log"
	"\nControl-f  send file"
	"\nControl-r  start recording (to file)"
	"\nControl-q  stop recording"
	"\nControl-x  exit"
	"\nControl-o  warm restart of micro"
        "\nControl-c  warm restart and bypass turnkey"
	"\nUp         recall previous command"
	"\nDown       following command"
	"\nControl-u  clear command buffer"
	"\n----------------------------------------------------------"
	"\n"
    }
    foreach line $message { logText::add $line }
    logText::showTail
}

namespace eval logText {
    # A scrolling text window to log messages
    variable textWidth 72
    variable textHeight 20
    variable textFont Courier
    variable doTrimTextLog 1
    variable textMaxLines 800
    variable textDeleteLines 50
    variable textFrame
    variable textWidget
    variable charCount 0
    variable saveFileName {}
    
    set textFrame [ttk::labelframe .tf -text " Response from Microcontroller "]
    set textWidget [text .tf.t -height $textHeight -width $textWidth \
                        -font $textFont -wrap char \
                        -yscrollcommand [list $textFrame.vsb set] ]
    set textScrollBar [ttk::scrollbar .tf.vsb -orient vertical \
                           -command [list $textWidget yview] ]
    pack $textWidget -side left -expand 1 -fill both
    pack $textScrollBar -side left -fill y

    proc add { txt } {
        variable textWidget
        variable charCount
        $textWidget insert end "$txt"
        set charCount [expr $charCount + [string length $txt]]
    }

    proc showTail {} {
        variable textWidget
        variable charCount
        $textWidget yview moveto 1.0
        update
        set charCount 0 
    }

    proc maybeShowTail {} {
        variable textWidget
        variable charCount
        if {[expr $charCount > int(0.8 * [$textWidget cget -width])]} {
            showTail
        }
    }

    proc deleteLastChar {} {
        variable textWidget
        variable charCount
        $textWidget delete "end-2c"
        $textWidget yview moveto 1.0
        set charCount 0
    }

    proc clear {} {
        variable textWidget
        variable charCount
        $textWidget delete 1.0 end
        set charCount 0
    }

    proc trim {} {
        variable textWidget
        variable textMaxLines
        variable textDeleteLines
        if [$textWidget compare end > ${textMaxLines}.0] {
            $textWidget delete 1.0 ${textDeleteLines}.0
        }
    }

    proc save {} {
        variable saveFileName
        variable textWidget
        set saveFileName [tk_getSaveFile -initialfile $saveFileName \
                                -title "Save log text to file"]
        if {[string length $saveFileName] > 0} {
            set fp [open $saveFileName "w"]
            puts $fp [$textWidget get 1.0 end]
            close $fp
        }
    }
}; # end logText

# Entry for collecting the outgoing text.
set entryFrame [ttk::labelframe .ef -text " Command to Send "]
set ::entryText [ttk::entry .ef.et -textvariable ::textBuffer -width $::logText::textWidth]
pack $::entryText -side left -fill x -expand 1

# A status line
set statusFrame [ttk::labelframe .sf -text " Serial Port "]
set lab1 [ttk::label .sf.lab1 -text "Device:"]
set deviceEntry [ttk::entry .sf.entr1 -width 15 -textvariable ::serialPort::name]
pack $lab1 $deviceEntry -side left
set lab2 [ttk::label .sf.lab2 -text "Speed:"]
set speedEntry [ttk::combobox .sf.entr2 -width 8 \
                    -textvariable ::serialPort::baudRate \
                    -values [list 9600 19200 38400 57600 115200]]
pack $lab2 $speedEntry -side left
set lab3 [ttk::label .sf.lab3 -text "ParityAndBits:"]
set parityBitsEntry [ttk::entry .sf.entr3 -width 6 -textvariable ::serialPort::parityAndBits]
pack $lab3 $parityBitsEntry -side left
set lab4 [ttk::label .sf.lab4 -text "Hand Shake:"]
set handShakeEntry [ttk::combobox .sf.entr4 -width 7 \
                        -textvariable ::serialPort::handShake \
                        -values [list xonxoff rtscts none]]
pack $lab4 $handShakeEntry -side left
set lab5 [ttk::label .sf.lab5 -text "State:"]
set entr5 [ttk::entry .sf.entr5 -width 6 -textvariable ::serialPort::state -state readonly]
pack $lab5 $entr5 -side left
set lab6 [ttk::label .sf.lab6 -text "Pause(ms):"]
set entr6 [ttk::entry .sf.entr6 -width 6 -textvariable ::smartSend::pauseBetweenChar]
pack $lab6 $entr6 -side left

# Pack the frames in order
pack $statusFrame -fill x -expand 0 -pady 5
pack $logText::textFrame -fill both -expand 1 -pady 5
pack $entryFrame -fill x -expand 0 -pady 5

bind $deviceEntry <Return> { serialPort::restart }
bind $speedEntry <Return> { serialPort::restart }
bind $parityBitsEntry <Return> { serialPort::restart }
bind $handShakeEntry <Return> { serialPort::restart }

proc sendTextChar { character } {
    # We use this function in the key-binding for the logText widget.
    # The break is to stop the default binding from inserting another
    # character into the widget.
    serialPort::send $character
    return -code break
}

# We build the procedures to send chunks of text on top of the basic
# smartSend::sendLine procedure.

set ::sendFileName {}

proc sendFile {} {
    set ::sendFileName [tk_getOpenFile -initialfile $::sendFileName \
			   -title "Open file to send"]
    if {[string length $::sendFileName] > 0} {
	set fp [open $::sendFileName "r"]
	while {[gets $fp line] >= 0} {
            if [catch {smartSend::sendLine $line} result] {
                puts "sendLine failed: $result"
                break
            }
        }
	close $fp
    }
}; # end sendFile

proc sendSelection { text } {
    # We use this function from <<PasteSelection>> binding to the logText widget.
    foreach line [split $text "\n"] {
        if [catch {smartSend::sendLine $line} result] {
            puts "sendSelection failed: $result"
            break
        }
    }
    return -code break
}; # end sendSelection

proc sendTextBuffer {} {
    # To be used from the entry widget binding.
    if {[string length $::textBuffer] == 0} {
        # send a CR only
        if [catch {smartSend::sendLine ""} result] {
            puts "sendTextBuffer failed: $result"
        }
    } else {
        foreach line [split $::textBuffer "\n"] {
            if [catch {smartSend::sendLine $line} result] {
                puts "sendTextBuffer failed: $result"
                break
            }
            cmdHistory::add $line
        }
    }
    set ::textBuffer ""
    cmdHistory::pointToEnd
    return -code break
}; # end sendTextBuffer

# The following binding redirects key presses in the text widget
# to the serial-port.
bind $logText::textWidget <Any-Key> [list sendTextChar %A]

# The following virtual event can be triggered with Control-Shift-v
# or with clicking the middle mouse button in X-Windows.
# The selected region of text is sent to the serial port.
event add <<PasteSelection>> <Control-V>

event add <<SendFile>> <Control-F> <Control-f>
event add <<SaveLogText>> <Control-S> <Control-s>
event add <<StartRecording>> <Control-R> <Control-r>
event add <<StopRecording>> <Control-Q> <Control-q>
event add <<Exit>> <Control-X> <Control-x>
event add <<EraseLine>> <Control-U> <Control-u>
event add <<WarmRestart>> <Control-O> <Control-o>
event add <<BypassTurnkey>> <Control-C> <Control-c>

bind $logText::textWidget <<PasteSelection>> { sendSelection [selection get] }
bind $logText::textWidget <<SendFile>> { sendFile }
bind $logText::textWidget <<SaveLogText>> { logText::save }
bind $logText::textWidget <<StartRecording>> { serialPort::startRecording }
bind $logText::textWidget <<StopRecording>> { serialPort::stopRecording }
bind $logText::textWidget <<Exit>> { displayExitDialog }
bind $logText::textWidget <<WarmRestart>> { warmRestart }
bind $logText::textWidget <<BypassTurnkey>> { bypassTurnkey }

bind $::entryText <Return> { sendTextBuffer }
bind $::entryText <<SendFile>> { sendFile }
bind $::entryText <<StartRecording>> { serialPort::startRecording }
bind $::entryText <<StopRecording>> { serialPort::stopRecording }
bind $::entryText <<Exit>> { displayExitDialog }
bind $::entryText <Up> {
    set ::textBuffer [::cmdHistory::getPrevious];
    $::entryText icursor end
}
bind $::entryText <Down> {
    set ::textBuffer [::cmdHistory::getNext];
    $::entryText icursor end
}
bind $::entryText <<EraseLine>> { $::entryText delete 0 end }
bind $::entryText <<WarmRestart>> { warmRestart }
bind $::entryText <<BypassTurnkey>> { bypassTurnkey }

proc warmRestart {} {
    serialPort::send "\x0f"; # Control-O
}

proc bypassTurnkey {} {
    puts "Try to bypass turnkey"
    serialPort::send "\x0f"; # Control-O
    after 500 { serialPort::send "\x1b" }; # ESC
    after 500 { serialPort::send "\x0d" }; # CR
}

# ---------------------------------------------------------------
# Command-line options processing, adapted from https://wiki.tcl.tk/1730
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
            set argv [list $flag {*}$argv]
            break
        }
    }; # end switch
}; # end while

# --------------------------------------------------------------
# Initialize streams and hand control over to the event loop.
update idletasks
focus $::entryText
cmdHistory::read
cmdHistory::pointToEnd
smartSend::resetFlags
# Customize configuration before starting up the serial port.
# For slow devices without flow control, we might want to pause
# for significant periods between characters.
# set smartSend::pauseBetweenChar 10;
# set serialPort::handShake none
# set serialPort::handShake rtscts
# set serialPort::name {\\.\com5}
serialPort::start
