;;; Wifi formatter for the mode-line
;;;
;;; Copyright 2013 Russell Sim <russell.sim@gmail.com>
;;; Copyright 2008 John Li
;;;
;;; This module is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.
;;;
;;; This module is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, see
;;; <http://www.gnu.org/licenses/>.


(in-package :stumpwm.contrib.wifi)

(defvar *iw-path* "/sbin/iw"
  "Location of iw, defaults to /sbin/iw.")

(defvar *rfkill-path* "/usr/sbin/rfkill"
  "Location of rfkill, defaults to /usr/sbin/rfkill.")

(defvar *wireless-device* nil
  "Set to the name of the wireless device you want to monitor. If set
  to NIL, try to guess.")

(defvar *physical-device* nil)

(defvar *wireless-cache-interval* 5
  "The number of seconds that can pass before refreshing.")

(defmacro defun-cached (name interval arglist &body body)
  "Creates a function that does simple caching. The body must be
written in a functional style - the value returned is set as the
prev-val."
  (let ((prev-time (gensym "PREV-TIME"))
        (prev-val (gensym "PREV-VAL"))
        (now (gensym "NOW"))
        (docstring (when (stringp (car body))
                     (pop body))))
    `(let ((,prev-time 0)
           (,prev-val nil))
       (defun ,name ,arglist
         ;; if no docstring, return nothing (not even nil)
         ,@(when docstring (list docstring))
         (let ((,now (get-internal-real-time)))
           (when (>= (- ,now ,prev-time)
                     (* ,interval internal-time-units-per-second))
             (setf ,prev-time ,now)
             (setf ,prev-val (locally ,@body)))
           ,prev-val)))))

(defun guess-wireless-device ()
  (let ((iw-output (run-shell-command (format nil "~A dev 2>&1" *iw-path*) t)))
    (aif (car
          (mapcar #'list
                  (multiple-value-bind (match? sub)
                      (cl-ppcre:scan-to-strings "Interface (.*)" iw-output)
                    (when match?
                      (loop :for i :below (array-dimension sub 0)
                            :collect (aref sub i))))
                  (multiple-value-bind (match? sub)
                      (cl-ppcre:scan-to-strings "phy#(.*)" iw-output)
                    (when match?
                      (loop :for i :below (array-dimension sub 0)
                            :collect (format nil "phy~A" (aref sub i)))))))
         (apply #'values it)
         (error "No Wifi."))))

(defun wifi-device ()
  (or *wireless-device* (guess-wireless-device)))

(defun phy-device ()
  (or *physical-device*
      (multiple-value-bind (wifi-dev phy-dev)
          (guess-wireless-device)
        (declare (ignore wifi-dev))
        phy-dev)))

(defun string-to-keyword (string)
    (intern (string-upcase string)
            (find-package 'keyword)))

(defun read-rfhardware-info ()
  (flet ((parse-field (line field-name)
           (multiple-value-bind (match? sub)
               (cl-ppcre:scan-to-strings (format nil "\\s+~A: (.*)" field-name) line)
             (when match? (aref sub 0))))
         (parse-boolean (value)
           (when (equal value "yes") t)))
    (let ((output (run-shell-command (format nil "~A list" *rfkill-path*) t))
          hardware-info
          current-hardware)
      (with-input-from-string (stream output)
        (do ((line (read-line stream nil)
                   (read-line stream nil)))
            ((null line))
          (acond
            ((multiple-value-bind (match? sub)
                 (cl-ppcre:scan-to-strings "\\d+: (.*): .*" line)
               (if match? (aref sub 0) nil))
             (setf current-hardware (string-to-keyword it)))
            ((parse-field line "Soft blocked")
             (setf (getf (getf hardware-info current-hardware) :soft)
                   (parse-boolean it)))
            ((parse-field line "Hard blocked")
             (setf (getf (getf hardware-info current-hardware) :hard)
                   (parse-boolean it))))))
      hardware-info)))

(defun format-rfinfo (info device)
  (case (rfinfo-off-p info device)
    (:soft "Soft Off")
    (:hard "Hard Off")))

(defun rfinfo-off-p (info device)
  (car
   (loop :for (type disabled) :on (getf info (string-to-keyword device))
           :by #'cddr
         :when disabled
           :collect type)))

(defun read-wifi-info (device)
  (let ((info (run-shell-command (format nil "~A dev ~A link" *iw-path* device) t)))
    (list
     :ssid
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "SSID: (.*)" info)
       (if match? (aref sub 0) nil))
     :freq
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "freq: (.*)" info)
       (if match? (parse-integer (aref sub 0)) nil))
     :signal
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "signal: (.*) dBm" info)
       (if match? (parse-integer (aref sub 0)) nil))
     :tx-bitrate
     (multiple-value-bind (match? sub)
         (cl-ppcre:scan-to-strings "tx bitrate: (.*)" info)
       (if match? (aref sub 0) nil)))))


(defun color-signal-strength (signal-strength)
  (bar-zone-color
   (if (numberp signal-strength)
       signal-strength
       0)
   -50 -70 -80 t))

(defun-cached fmt-wifi *wireless-cache-interval* (ml)
  "Formatter for wifi status. Displays the ESSID of the access point
you're connected to as well as the signal strength. When no valid data
is found, just displays nil."
  (declare (ignore ml))
  (handler-case
      (destructuring-bind (&key ssid freq signal tx-bitrate)
          (read-wifi-info (wifi-device))
        (cond
          (signal
           (format nil "~A ^[~A~D dBm^]" ssid (color-signal-strength signal) signal))
          ((rfinfo-off-p (read-rfhardware-info) (phy-device))
           (format-rfinfo (read-rfhardware-info) (phy-device)))))
    ;; CLISP has annoying newlines in their error messages... Just
    ;; print a string showing our confusion.
    (t (c) (format nil "~A" c))))

(defun-cached fmt-wifi-short *wireless-cache-interval* (ml)
  "Formatter for wifi status. Displays the signal strength. When no
valid data is found, just displays nil."
  (declare (ignore ml))
  (handler-case
      (destructuring-bind (&key ssid freq signal tx-bitrate)
          (read-wifi-info (wifi-device))
        (cond
          (signal
           (format nil "^[~A~D dBm^]" (color-signal-strength signal) signal))
          ((rfinfo-off-p (read-rfhardware-info) (phy-device))
           (format-rfinfo (read-rfhardware-info) (phy-device)))))
    ;; CLISP has annoying newlines in their error messages... Just
    ;; print a string showing our confusion.
    (t (c) (format nil "~A" c))))

;;; Add mode-line formatter

(add-screen-mode-line-formatter #\I #'fmt-wifi)

(add-screen-mode-line-formatter #\i #'fmt-wifi-short)
