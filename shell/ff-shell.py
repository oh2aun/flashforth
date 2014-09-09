#!/usr/bin/python
#
# Upload & interpreter shell for FlashForth.
# Written for python 2.7
#
# Copyright 9.9.2014 Mikael Nordman (oh2aun@gmail.com)
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
import argparse
import serial
import readline
import rlcompleter
import atexit
import logging
import signal
from thread import start_new_thread, allocate_lock
from time import *

# Threading stuff, global flags
running = True
RECVR_STARTED = False
THR_LOCK = allocate_lock()
waitForOK = 0
uploadMode = 0

class Config(object):
  def __init__(self):
    self.serial_port  = '/dev/ttyACM0'
    self.rate = '9600'
    self.rtscts = False
    self.xonxoff = False

def serial_open(config):
  print "Port:"+config.port+" Speed:"+config.rate+" rtscts:"+str(config.rtscts)+" xonxoff:"+str(config.xonxoff)
  try:
    config.ser = serial.Serial(config.port, config.rate, timeout=0.1, writeTimeout=1.0, rtscts=config.rtscts, xonxoff=config.xonxoff)
  except serial.SerialException as e:
    print("Could not open serial port '{}': {}".format(com_port, e))
    raise e
  
def send_line(config, line):
  try:
    bytes = config.ser.write(line)
  except serial.SerialTimeoutException as e:
    logging.error("Write timeout on serial port '{}': {}".format(com_port, e))
  config.ser.flush()       # Send the output buffer

# receive_thr() receives chars from FlashForth
def receive_thr(config, *args):
  global RECVR_STARTED, running, waitForOK, uploadMode
  THR_LOCK.acquire()
  RECVR_STARTED = True
  THR_LOCK.release()
  OK0 = ' '
  OK1 = ' '
  OK2 = ' '
  while running==True:
    while config.ser.inWaiting() > 0:
      try:
        THR_LOCK.acquire()
        char = config.ser.read()
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
            sys.stdout.write(OK1)
            sys.stdout.write(OK2)
            sys.stdout.flush()
            waitForOK = 1
          if char == '\n':
            if waitForOK == 2:
              sys.stdout.write('\n')
              sys.stdout.flush()
            waitForOK = 0
        THR_LOCK.release()
      except KeyboardInterrupt:
        running = False
        #print "Receive thread exception"     
  #print "End of receive thread"
  THR_LOCK.acquire()
  RECVR_STARTED = False
  THR_LOCK.release()

def parse_arg(config):
  parser = argparse.ArgumentParser(description="Small shell for FlashForth", 
           epilog="""Interact with FlashForth using commmand line editing and history. Send files to FlashForth with #send path/filename. Warm start with #warm.""")
  parser.add_argument("--port", "-p", action="store",
         type=str, default="/dev/ttyACM0", help="Serial port name")
  parser.add_argument("--rtscts", action="store_true",
         default=False, help="Serial port RTS/CTS enable")
  parser.add_argument("--xonxoff", action="store_true",
         default=False, help="Serial port XON/XOFF enable")
  parser.add_argument("--speed", "-s", action="store",
         type=int, default=38400, help="Serial port speed")
  arg = parser.parse_args()
  config.port = arg.port
  config.rtscts = arg.rtscts
  config.xonxoff = arg.xonxoff
  config.speed = arg.speed

#main loop for sending and receiving
def main():
  global running, waitForOK, uploadMode
  
  config = Config() 
  parse_arg(config)
  serial_open(config)
  start_new_thread(receive_thr, (config, 1))
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
        sys.stdout.write('\r\033\133\101')
        sys.stdout.flush()
      else:
        line = file.readline()
        if line == "":
          file.close()
          uploadMode = 0
        else:
          line = line.rstrip('\n')
          line = line.rstrip('\r')
          sys.stdout.write("> "+line)
      if line[:6] == "#send ":
        pathfile = line[6:]
        print pathfile
        file = open(pathfile, "r")
        line = ""
        uploadMode = 1
      if uploadMode == 1:
        waitForOK = 2
      if line[:5] == "#warm":
        line = '\017'
      THR_LOCK.acquire()
      send_line(config, line+"\n")
      THR_LOCK.release()

    except KeyboardInterrupt:
      running = False
      # print "Transmission thread exception"     

  while RECVR_STARTED:
    pass
  config.ser.close()
  print "Exiting ff-shell.py, goodbye..."

sys.exit(main())

