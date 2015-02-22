#!/usr/bin/python
#
# Upload & interpreter shell for FlashForth.
# Written for python 2.7
#
# Copyright 25.01.2015 Mikael Nordman (oh2aun@gmail.com)
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
waitForNL = 0
uploadMode = 0

class Config(object):
  def __init__(self):
    self.serial_port  = '/dev/ttyACM0'
    self.rate = '38400'
    self.rtscts = False
    self.xonxoff = False

def serial_open(config):
  print "Port:"+config.port+" Speed:"+config.rate+" rtscts:"+str(config.rtscts)+" xonxoff:"+str(config.xonxoff)
  try:
    config.ser = serial.Serial(config.port, config.rate, timeout=0.1, writeTimeout=1.0, rtscts=config.rtscts, xonxoff=config.xonxoff)
  except serial.SerialException as e:
    print("Could not open serial port '{}': {}".format(com_port, e))
    raise e
  

# receive_thr() receives chars from FlashForth
def receive_thr(config, *args):
  global RECVR_STARTED, running, waitForNL, uploadMode
  RECVR_STARTED = True
  while running==True:
    try:
      while config.ser.inWaiting() > 0:
        try:
          THR_LOCK.acquire()
          char = config.ser.read()
          sys.stdout.write(char)
          sys.stdout.flush()
          if char == '\n':
            waitForNL = 0
          THR_LOCK.release()
        except Exception as e:
          THR_LOCK.release()
          running = True
          # print "Receive thread exception {0}".format(e)

    except Exception as e:
      print "Serial exception {0}".format(e)
      RECVR_STARTED = False
      running = False
      os.kill(os.getpid(), signal.SIGINT)      

  print "End of receive thread"
  RECVR_STARTED = False
  running = False

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
         type=str, default=38400, help="Serial port speed")
  arg = parser.parse_args()
  config.port = arg.port
  config.rtscts = arg.rtscts
  config.xonxoff = arg.xonxoff
  config.rate = arg.speed
  print arg.speed

#main loop for sending and receiving
def main():
  global running, waitForNL, uploadMode
  
  config = Config() 
  parse_arg(config)
  serial_open(config)
  start_new_thread(receive_thr, (config, 1))
  # Wait for the thread to start
  while not RECVR_STARTED:
    pass
  # 
  # readline.parse_and_bind("tab: complete")
  histfn = os.path.join(os.path.expanduser("~"), ".ff.history")
  print histfn
  try:
    readline.read_history_file(histfn)
  except IOError, e:
    pass
  atexit.register(readline.write_history_file, histfn)
  running = True
  waitForNL = 0
  uploadMode = 0
  while running:
    try:
      if uploadMode == 0:
        try:
          line = raw_input()
        except KeyboardInterrupt:
          print "KeyboardInterrupt"
          raise Exception
        sys.stdout.write('\r\033\133\101')
        sys.stdout.flush()
      else:
        while waitForNL > 0:
          pass
        line = file.readline()
        if line == "":
          file.close()
          uploadMode = 0
          waitForNL = 0
          pass
        else:
          line = line.rstrip('\n')
          line = line.rstrip('\r')
          sys.stdout.write("> ") 
      if line[:6] == "#send ":
        pathfile = line[6:]
        line = ""
        try:
          file = open(pathfile, "r")
          uploadMode = 1
        except IOError, e:
          print "\nFile not found: "+pathfile
      if line[:5] == "#warm":
        line = '\017'           # CTRL-O
      THR_LOCK.acquire()
      try:
        waitForNL = 1
        bytes = config.ser.write(line+'\n')
        config.ser.flush()       # Send the output buffer
      except Exception as e:
        THR_LOCK.release()
        print("Write error on serial port {0}, {1}".format(com_port, e))
        running = False
      THR_LOCK.release()

    except Exception as e:
      print "Transmission thread exception {0}".format(e) 
      running = False

  while RECVR_STARTED:
    pass
  config.ser.close()
  print "Exiting ff-shell.py, goodbye..."

try:
  sys.exit(main())
except Exception as e:
  print "sys.exit {0}".format(e)
