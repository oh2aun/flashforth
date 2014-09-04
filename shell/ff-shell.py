#!/usr/bin/python
#
# Upload & interpreter shell for FlashForth.
#
# Copyright 2014 Mikael Nordman (oh2aun@gmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

import os
import sys
import serial
import readline
import rlcompleter
import atexit
import logging
import signal
from thread import start_new_thread, allocate_lock
from time import *

# Threading stuff
running = True
RECVR_STARTED = False
THR_LOCK = allocate_lock()
waitForOK = 0
uploadMode = 0

com_port  = '/dev/ttyACM0'
baud_rate = '9600'
try:
  ser = serial.Serial(com_port, baud_rate, timeout=0.1, writeTimeout=1.0, rtscts=1)
except serial.SerialException as e:
  logging.debug("Could not open serial port '{}': {}".format(com_port, e))
  raise e
 
def send_line(line):
  try:
    bytes = ser.write(line)
  except serial.SerialTimeoutException as e:
    logging.error("Write timeout on serial port '{}': {}".format(com_port, e))
  ser.flush()       # Send the output buffer

# receive_thr() receives chars from FlashForth
def receive_thr():
  # Cause it's not atomic operation
  global RECVR_STARTED, running, waitForOK, uploadMode
  THR_LOCK.acquire()
  RECVR_STARTED = True
  THR_LOCK.release()
  OK0 = ' '
  OK1 = ' '
  OK2 = ' '
  while running==True:
    while ser.inWaiting() > 0:
      try:
        THR_LOCK.acquire()
        char = ser.read()
        OK0 = OK1
        OK1 = OK2
        OK2 = char
        if waitForOK < 2:
          sys.stdout.write(char)
          sys.stdout.flush()
        if uploadMode > 0:
          if OK0==' ' and OK1=='o' and OK2=='k':
            sys.stdout.write(OK0)
            sys.stdout.write(OK1)
            sys.stdout.write(OK2)
            sys.stdout.flush()
            waitForOK = 1
          if OK1==' ' and OK2=='?':
            #sys.stdout.write(OK0)
            sys.stdout.write(OK1)
            sys.stdout.write(OK2)
            sys.stdout.flush()
            waitForOK = 1
          if char=='\n':
            waitForOK = 0
        THR_LOCK.release()
      except KeyboardInterrupt:
        running = False
        #print "Receive thread exception"     
  #print "End of receive thread"
  THR_LOCK.acquire()
  RECVR_STARTED = False
  THR_LOCK.release()

#main loop for sending and receiving
def main():
  global running, waitForOK, uploadMode
  # Nice to see how python supports function pointer in such an easy way
  start_new_thread(receive_thr, ())

  # Wait for the thread to start
  while not RECVR_STARTED:
    pass
  # 
  readline.parse_and_bind("tab: complete")
  histfn = os.path.join(os.path.expanduser("~"), ".ff.history")
  print histfn
  try:
    readline.read_history_file(histfn)
  except IOError, e:
    pass
  atexit.register(readline.write_history_file, histfn)
  running = True
  waitForOK = 0
  uploadMode = 0
  while running:
    try:
      while waitForOK > 0:
        pass
      if uploadMode == 0:
        line = raw_input()
        sys.stdout.write('\r')
        sys.stdout.write('\033')
        sys.stdout.write('\133')
        sys.stdout.write('\101')
        sys.stdout.flush()
      else:
        line = file.readline()
        if line == "":
          file.close()
          uploadMode = 0
        else:
          line = line.rstrip('\n')
          line = line.rstrip('\r')
          sys.stdout.write(">>"+line)
      if line[:6] == "#send ":
        pathfile = line[6:]
        print pathfile
        file = open(pathfile, "r")
        line = ""
        uploadMode = 1
      if uploadMode == 1:
        waitForOK = 2

      THR_LOCK.acquire()
      send_line(line+"\n")
      THR_LOCK.release()

    except KeyboardInterrupt:
      running = False
      # print "Transmission thread exception"     

  while RECVR_STARTED:
    pass
  ser.close()
  print "Exiting ff-shell.py, goodbye..."
 
sys.exit(main())

