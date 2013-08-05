;;; udisksctl.el --- interface to udisksctl

;; Copyright (C) 2013 Toni Schmidbauer

;; Author: Toni Schmidbauer <toni@stderr.at>
;; Created: 11 June 2013
;; Version: 0.1 (11 June 2013)
;; Keywords: udisksctl, emacs

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; This is a major mode to interact with udisksctl. It allows you to
;; see devices, unlock/lock encrypted device and mounting/unmounting
;; devices via udisksctl

;; Operating Systems:
;; Developped under Linux. Should work on all OS'es
;; that support the udisksctl command.


;;; Code:

;;; Customizable variables

;;; Todo
;; - excute-cmd should use a tmp buffer an not a filter function maybe
;; - we should filter the udisksctl output through a filter function
;; - if a device is unlocked save the unlocked device name and the
;;   dm-? device name created
;; - if a device gets mounted save the device name
;; - maybe the functions to mount/unmount/lock/unlock could be
;;   defined via defmacro?

(require 'comint)

(defvar udisksctl-buffer-name "*udisksctl*")
(defvar udisksctl-status-cmd "status")
(defvar udisksctl-mount-cmd "mount")
(defvar udisksctl-unmount-cmd "unmount")
(defvar udisksctl-unlock-cmd "unlock")
(defvar udisksctl-lock-cmd "lock")
(defvar udisksctl-output nil)

(defvar udisksctl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'udisksctl-unlock)
    (define-key map "l" 'udisksctl-lock)
    (define-key map "m" 'udisksctl-mount)
    (define-key map "U" 'udisksctl-unmount)
    (define-key map "q" 'kill-buffer)
    map)
  "Keymap for `udisksctl-mode'.")

(define-derived-mode udisksctl-mode special-mode
  "Udisksctl"
  "Major mode for udisksctl. Shows status information about disks
via udisksctl.

Keybindings:
\\{udisksctl-mode-map}"
  (kill-all-local-variables)
  (use-local-map udisksctl-mode-map)
  (setq major-mode 'udisksctl-mode
	buffer-read-only t))

(defun udisksctl-execute-cmd-comint (cmd device)
  "execute cmd on device, cmd requires user input"
  (let (params)
    (setq params (append (list device) params))
    (setq params (append (list "-b") params))
    (setq params (append (list cmd) params))
    (udisksctl-comint params)))

(defun udisksctl-comint(params)
  (let ((old-buf (current-buffer))
	(udisksctl-comint-buffer-name "*udisksctl-comint*"))
    (with-current-buffer (get-buffer-create udisksctl-comint-buffer-name)
      (apply 'make-comint-in-buffer "udiskctl-comint" udisksctl-comint-buffer-name "udisksctl" nil params))))

(defvar udisksctl-process-buffer-name "*udisksctl-process*")
(defvar udisksctl-process nil)

(defun udisksctl-execute-cmd (cmd device)
  "execute cmd on device, does not require user input"
  (let ((process-connection-type t)
	(buf (get-buffer-create udisksctl-process-buffer-name)))
    (if (get-buffer udisksctl-process-buffer-name)
	(kill-buffer udisksctl-process-buffer-name))
    (setq udisksctl-output nil)
    (setq udisksctl-process
	  (start-process "udisksctl" udisksctl-process-buffer-name "udisksctl" cmd "-b" device))
    (set-process-filter udisksctl-process 'udisksctl-process-filter)
    (while (equal (process-status udisksctl-process) 'run)
      (sit-for 0.1 t))
    (or (equal (process-exit-status udisksctl-process) 0)
	(error "%s" (or (with-current-buffer (get-buffer udisksctl-process-buffer-name)
			  (re-search-backward (concat "^\\(Error.*\\)" paragraph-separate) nil t)
			  (match-string 1))
			"udiskctl failed"
			)))))

(defun udisksctl-process-filter(proc string)
  "filter udisksctl output for a password prompt"
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (if (string-match "^Passphrase: " string)
	(process-send-string proc (read-passwd "Passphrase: " nil)))))

(defun udisksctl-parse-output(udisksctl-proc)
  (save-current-buffer
    (set-buffer (process-buffer udisksctl-proc))))


(defun udisksctl-unlock(&optional device)
  (interactive)
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to unlock: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-unlock-cmd udisksctl-device))

(defun udisksctl-lock(&optional device)
  (interactive)
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to lock: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-lock-cmd udisksctl-device))

(defun udisksctl-mount(&optional device)
  (interactive)
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to mount: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-mount-cmd udisksctl-device))

(defun udisksctl-unmount(&optional device)
  (interactive)
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to unmount: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-unmount-cmd udisksctl-device))


(defun udisksctl-read-device(&optional message)
"read a device name to work on (mount, unlock ...)"
  (interactive)
  (if (boundp 'message)
      (read-string message)
    (read-string "Enter device name: ")))


(defun udisksctl-status()
  (call-process "udisksctl" nil udisksctl-buffer-name nil
		udisksctl-status-cmd))

(defun udisksctl-buffer()
"creates the udisksctl buffer"
  (get-buffer-create udisksctl-buffer-name)
  (switch-to-buffer udisksctl-buffer-name)
  (udisksctl-mode))

(defun udisksctl()
  "run udiskctl status"
  (interactive)
  (progn
    (udisksctl-buffer)
    (udisksctl-status)))

(provide 'udisksctl-mode)
