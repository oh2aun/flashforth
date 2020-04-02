#!/usr/bin/python
#
# Upload & interpreter shell for FlashForth.
# Written for python 2.7
#
# Copyright 12.09.2019 Mikael Nordman (oh2aun@gmail.com)
# 12.09.2019 - Updated for nonblocking I/O
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
import string
from thread import start_new_thread
from time import *

# Threading stuff, global flags
running = True
waitForNL = 0
uploadMode = 0
waitForChar = 'idle'

class Config(object):
  def __init__(self):
    self.serial_port  = '/dev/ttyACM0'
    self.rate = '38400'
    self.hw = False
    self.sw = False
    self.chardelay = '0'
    self.newlinedelay = '0'
    self.charflowcontrol = False

def serial_open(config):
  print "Port:"+str(config.port)+" Speed:"+str(config.rate)+" hw:"+str(config.hw)+" sw:"+str(config.sw)+" newlinedelay:"+str(config.chardelay)+" chardelay:"+str(config.chardelay)+" charfc:"+str(config.charflowcontrol)
  try:
    config.ser = serial.Serial(config.port, config.rate, timeout=0.5, writeTimeout=1.0, rtscts=config.hw, xonxoff=config.sw)
  except serial.SerialException as e:
    print("Could not open serial port '{}': {}".format(config.serial_port, e))
    raise e
  

# receive_thr() receives chars from FlashForth
def receive_thr(config, *args):
  global running, waitForNL, uploadMode, waitForChar
  while running==True:
    try:
      char = config.ser.read()
      sys.stdout.write(char)
      sys.stdout.flush()
      if char == '\n':
        waitForNL = 0
      if config.charflowcontrol == True:
        if char == waitForChar:
          waitForChar = 'idle'

    except Exception as e:
      print "Serial exception {0}".format(e)
      running = False

  print "End of receive thread. Press enter to exit."
  exit()

def parse_arg(config):
  parser = argparse.ArgumentParser(description="Small shell for FlashForth", 
           epilog="""Interact with FlashForth using commmand line editing and history. Send files to FlashForth with #send path/filename. Warm start with #warm.""")
  parser.add_argument("--port", "-p", action="store",
         type=str, default="/dev/ttyACM0", help="Serial port name")
  parser.add_argument("--hw", action="store_true",
         default=False, help="Serial port RTS/CTS enable")
  parser.add_argument("--sw", action="store_true",
         default=False, help="Serial port XON/XOFF enable")
  parser.add_argument("--speed", "-s", action="store",
         type=str, default=38400, help="Serial port speed")
  parser.add_argument("--chardelay", "-d", action="store",
         type=str, default=0, help="Character delay(milliseconds)")
  parser.add_argument("--newlinedelay", "-n", action="store",
         type=str, default=0, help="Newline delay(milliseconds)")
  parser.add_argument("--cc", "-c", action="store_true",
         default=False, help="Character by character flow control")
  arg = parser.parse_args()
  config.port = arg.port
  config.hw = arg.hw
  config.sw = arg.sw
  config.rate = arg.speed
  config.chardelay = arg.chardelay
  config.newlinedelay = arg.newlinedelay
  config.charflowcontrol = arg.cc

#main loop for sending and receiving
def main():
  global running, waitForNL, uploadMode, waitForChar
  
  config = Config() 
  parse_arg(config)
  serial_open(config)
  start_new_thread(receive_thr, (config, 1))
 
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
          sleep(0.001)
          waitForNL = waitForNL - 1
          continue
        line = file.readline()
        if line == "":
          file.close()
          uploadMode = 0
          waitForNL = 0
          continue
        else:
          line = line.rstrip('\n')
          line = line.rstrip('\r')
 
      if line[:6] == "#send ":
        pathfile = line[6:]

        if pathfile.endswith(".txt") == False:
          pathfile = pathfile + ".txt"
        line = ""
        try:
          file = open(pathfile, "r")
          uploadMode = 1
        except IOError, e:
          print "\nFile not found: "+pathfile
      if line[:5] == "#warm":
        line = '\017'           # CTRL-O
      if line[:5] == "#esc":
        line = '\033'           # Escape
      try:
        waitForNL = 1000
        for c in line:
          sleep(float(config.chardelay)/1000)
          if config.charflowcontrol == True:
            while waitForChar <> 'idle':
              sleep(0.001)
            waitForChar = c
          config.ser.write(c)
          config.ser.flush()       # Send the output buffer
        sleep(float(config.newlinedelay)/1000)
        config.ser.write('\n')
        config.ser.flush()       # Send the output buffer

      except Exception as e:
        print("Write error on serial port {0}, {1}".format(config.serial_port, e))
        running = False

    except Exception as e:
      print "Transmission thread exception {0}".format(e) 
      running = False

  config.ser.close()
  print "Exiting ff-shell.py, goodbye..."

try:
  sys.exit(main())
except Exception as e:
  print "sys.exit {0}".format(e)
