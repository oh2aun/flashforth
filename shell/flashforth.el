;;; flashforth.el --- Connect to the FlashForth serial shell
;;; Copyright © 2021 宋文武 <iyzsong@member.fsf.org>
;;;
;;; This file is NOT part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.

;;; FlashForth <https://flashforth.com/> is a Forth stamp system
;;; implemented on the Microchip 8-bit PIC18F and 16-bit PIC24, 30, 33
;;; and the Atmel Atmega microcontroller families.  This package
;;; provides the `connect-to-flashforth' function to create a
;;; `*flashforth*' buffer, which works well for multiple lines paste
;;; and `forth-send-region' (from gforth.el).

(setq *flashforth-writer-thread* nil)
(defvar flashforth-port-name "/dev/ttyACM0"
  "Serial port name for FlashForth.")
(defvar flashforth-port-speed 38400
  "Serial port speed for FlashForth.")

(defun comint-flashforth-send (proc string)
  (dolist (line (split-string string "\n"))
    (setq *flashforth-writer-thread*
          (make-thread
           `(lambda ()
              (let ((prev ,*flashforth-writer-thread*))
                (when prev (thread-join prev)))
              (comint-send-string ,proc (concat ,line "\n"))
              ;; The simplest flow control solution: sleep 300ms after
              ;; the end of each line...
              (sleep-for 0.3))))))

(define-advice comint-send-region
    (:around (func process start end) "flashforth")
  (if (string-equal (buffer-name (process-buffer process)) "*flashforth*")
      (comint-flashforth-send process (buffer-substring-no-properties start end))
    (funcall func process start end)))

(defun connect-to-flashforth (port speed)
  "Run a FlashForth serial process, input and output via buffer
*flashforth*."
  (interactive
   (if current-prefix-arg
       (list
        (read-file-name "Serial port: " "/dev" flashforth-port-name)
        (read-number "Serial speed: " flashforth-port-speed))
     (list flashforth-port-name flashforth-port-speed)))
  (with-current-buffer (make-comint-in-buffer "flashforth" "*flashforth*" nil)
    (setq-local comint-process-echoes nil)
    (setq-local comint-input-sender #'comint-flashforth-send))
  (when (not (string-equal
              "serial"
              (process-type (get-buffer-process "*flashforth*"))))
    (make-serial-process
     :buffer "*flashforth*"
     :port port
     :speed speed
     :noquery t))
  (setq forth-process-buffer "*flashforth*") ; for 'forth-mode'
  (pop-to-buffer "*flashforth*"))

(provide 'flashforth)
