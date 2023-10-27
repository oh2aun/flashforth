#!/usr/bin/python3
#
# Upload & interpreter shell for FlashForth.
# Written for python 3
#
# Copyright 2022 Mikael Nordman (oh2aun@gmail.com)
# 12.09.2019 - Updated for nonblocking I/O
# 26.1.2021  - Change default suffix to '.fs'
# 5.6.2023   - Better error handling
# 27.10.2023 - #sendm Send many files
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
import _thread
from time import *
from base_dictionary import base_dictionary
from user_dictionary import user_dictionary
# Threading stuff, global flags
running = True
waitForNL = 0
uploadMode = 0
waitForChar = 'idle'
errorCount = 0
lineLength = 0
openfiles = []
sendfileCount = 0
sendfileIndex = 0

cmd_dictionary = {
  "#help filter   ": "Print filtered help text",
  "#help          ": "Print help text for all words",
  "#warm          ": "Send ctrl-o to FF",
  "##             ": "Send ESC(0x27) to disable the turnkey",
  "#send filename {startstring {stopstring}}" : "Send a file optonally starting at line with startstring and optionally stop at line with stopstring", 
  "#sendm file1..fileN" : "Send multiple files",
  "#pwd           ": "Print working directory",
  "#ls {path}     ": "List files in working directory or in path",
  "#cd path       ": "Change working directory",
  "#cat file      ": "Show the file contents",
  "#history filter": "Show the history"
}

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
  print("port:"+str(config.port)+" speed:"+str(config.rate)+" hw:"+str(config.hw)+" sw:"+str(config.sw)+" newlinedelay:"+str(config.newlinedelay)+" chardelay:"+str(config.chardelay)+" cc:"+str(config.charflowcontrol)+" nl:"+str(config.newlineflowcontrol))
  try:
    config.ser = serial.Serial(config.port, config.rate, timeout=0.5, writeTimeout=1.0, rtscts=config.hw, xonxoff=config.sw)
  except Exception as e:
    raise e
  

# receive_thr() receives chars from FlashForth
def receive_thr(config, *args):
  global running, waitForNL, uploadMode, waitForChar, errorCount, lineLength
  count = 0
  while running == True:
    try:
      char = config.ser.read()
      if len(char) == 0:
        continue
      if lineLength < 1 or uploadMode > 0:
        sys.stdout.buffer.write(char)
        sys.stdout.flush()
      lineLength = max(0, lineLength - 1)
      count = count + 1
      if char == b'\x15' and uploadMode > 0:
        errorCount = errorCount + 1
        if errorCount > 3:
          uploadMode = 0
          errorCount = 0
          print("\nCODE UPLOAD INTERRUPTED\n")
      if char == b'\n':
        waitForNL = 0
        count = 0
      if count > 80:
        count = 0
        sys.stdout.flush()
      if config.charflowcontrol == True:
        if char == waitForChar:
          waitForChar = 'idle'
    except Exception as e:
      running = False
  os.kill(os.getpid(), signal.SIGINT)

def parse_arg(config):
  parser = argparse.ArgumentParser(description="Small shell for FlashForth.")
           #epilog="""End-of-line flow control with a 1 second timeout is always active.""")
  parser.add_argument("--port", "-p", action="store",
         type=str, default="/dev/ttyACM0", help="Serial port name, default = /dev/ttyACM0")
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
  parser.add_argument("--nl", action="store_false",
         default=True, help="Newline flow control disable")

  arg = parser.parse_args()
  config.port = arg.port
  config.hw = arg.hw
  config.sw = arg.sw
  config.rate = arg.speed
  config.chardelay = arg.chardelay
  config.newlinedelay = arg.newlinedelay
  config.charflowcontrol = arg.cc
  config.newlineflowcontrol = arg.nl

#main loop for sending and receiving
def main():
  global running, waitForNL, uploadMode, waitForChar, errorCount, lineLength
  
  config = Config() 
  parse_arg(config)
  serial_open(config)
  _thread.start_new_thread(receive_thr, (config,))
 
  # readline.parse_and_bind("tab: complete")
  histfn = os.path.join(os.path.expanduser("~"), ".ff.history")
  try:
    readline.set_history_length(500)
    readline.read_history_file(histfn)
  except(IOError, e):
    print(e)
    pass
  atexit.register(readline.write_history_file, histfn)

  dictionary = base_dictionary.copy()
  dictionary.update(user_dictionary.copy())
  dictionary.update(cmd_dictionary.copy())
  
  print("Shell directives:")
  for cmd in sorted(cmd_dictionary):
    print(cmd + "\t" + cmd_dictionary[cmd])
  print("\n")

  running = True
  waitForNL = 0
  uploadMode = 0
  errorCount = 0
  openfiles = []
  while running:
    try:
      if uploadMode == 0:
        for openfile in openfiles:
          openfile.close()
        openfiles = []
        errorCount = 0
        try:
          line = input()
        except KeyboardInterrupt:
          raise Exception
        args = line.split()
        if len(args) > 1 and args[0] == "#send":
          pathfile = args[1]
          startString = ""
          stopString = ""
          if len(args) > 2:
            startString = args[2]
          if len(args) > 3:
            stopString = args[3]
          if pathfile.endswith(".fs") == False:
            pathfile = pathfile + ".fs"
          line = ""
          try:
            file = open(pathfile, "r")
            uploadMode = 1
          except Exception as e:
            print(format(e))
            continue
        if len(args) > 1 and args[0] == "#sendm":
           uploadMode = 3
           sendfiles = args[-(len(args)-1):]
           for openfile in sendfiles:
             try:
               if openfile.endswith(".fs") == False:
                 openfile = openfile + ".fs"
               openfiles.append(open(openfile, "r"))
             except Exception as e:
               print(format(e))
               uploadMode = 0
               continue
           sendfileCount = len(openfiles)
           sendfileIndex = 0;
           continue
        if len(args) == 1 and args[0] == "#warm":
          line = '\017'           # CTRL-O
        if len(args) == 1 and args[0] == "##":
          line = '\033'           # Escape
        if len(args) == 2 and args[0] == "#help":
          filter = args[1]
          try:
            for cmd in sorted(dictionary):
              if filter in cmd:
                print(cmd + "\t" + dictionary[cmd])
          except:
            print("\n" + args[1] + "\t" + "not found")
          continue
        if len(args) == 1 and args[0] == "#help":
          for name in sorted(dictionary):
            print(name + "\t" + dictionary[name])
          continue
        if len(args) == 1 and args[0] == "#pwd":
          print("dir : " + os.getcwd())
          continue
        if len(args) == 2 and args[0] == "#cd":
          os.chdir(args[1])
          print("dir : " + os.getcwd())
          continue
        if len(args) >= 1 and args[0] == "#ls":
          print("dir : " + os.getcwd())
          lspath = '.'
          if len(args) == 2:
            lspath = args[1]
          files = os.listdir(lspath)
          for file in sorted(files):
            print(file)
          continue
        if len(args) == 2 and args[0] == "#cat":
          try:
            catfile = open(args[1], "r")
            for catline in catfile:
              catline = catline.rstrip('\n')
              catline = catline.rstrip('\r')
              print(catline)
          except Exception as e:
            print(format(e))
          continue
        if len(args) >= 1 and args[0] == "#history":
          if len(args) == 2:
            filter = args[1]
          else:
            filter = ""
          for index in range (1, readline.get_current_history_length()):
            historyline = readline.get_history_item(index) 
            if filter == "":
              print(historyline)
            else:
              if filter in historyline:
                print(historyline)
          continue
      elif uploadMode < 3:                       # Send file
        while waitForNL > 0:
          sleep(0.001)
          waitForNL = waitForNL - 1
          continue
        line = file.readline()
        if line.startswith("\\ "):
          continue
        if uploadMode == 1 and startString == "":
          uploadMode = 2
        else:
          if uploadMode == 1 and line.find(startString) >= 0:
            uploadMode = 2

        if uploadMode == 2 and stopString != "":
          if line.find(stopString) >= 0:
            line = ""

        if line == "":
          file.close()
          uploadMode = 0
          waitForNL = 0
          errorCount = 0
          continue
        else:
          line = line.rstrip('\n')
          line = line.rstrip('\r')

        if uploadMode < 2:
          continue
      elif uploadMode == 3:                     # send multiple files
        while waitForNL > 0:
          sleep(0.001)
          waitForNL = waitForNL - 1
          continue
        line = openfiles[sendfileIndex].readline()
        if line.startswith("\\ "):
          continue
        if line == "":
          waitForNL = 0
          errorCount = 0
          sendfileIndex += 1
          if sendfileIndex == sendfileCount:
            uploadMode = 0
          continue
        else:
          line = line.rstrip('\n')
          line = line.rstrip('\r')
      else:
        continue
      try:
        if config.newlineflowcontrol:
          waitForNL = 2000
        lineLength = len(line)
        for c in line:
          sleep(float(config.chardelay)/1000)
          if config.charflowcontrol:
            while waitForChar != 'idle':
              sleep(0.001)
            waitForChar = c
          config.ser.write(bytes(c, 'UTF-8'))
          if config.charflowcontrol:
            config.ser.flush()       # Send the output buffer
        config.ser.write(bytes('\n', 'UTF-8'))
        config.ser.flush()       # Send the output buffer
        sleep(float(config.newlinedelay)/1000)

      except Exception as e:
        print(format(config.serial_port, e))
        running = False

    except Exception as e:
      print(format(e)) 
      running = False

  config.ser.close()
  print("Exiting ff-shell, goodbye...")

try:
  sys.exit(main())
except Exception as e:
  print(format(e))
  
