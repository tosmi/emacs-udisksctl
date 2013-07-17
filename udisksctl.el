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

(require 'comint)

(defvar udisksctl-buffer-name "*udisksctl*")
(defvar udisksctl-status-cmd "status")
(defvar udisksctl-mount-cmd "mount")
(defvar udisksctl-unmount-cmd "unmount")
(defvar udisksctl-unlock-cmd "unlock")
(defvar udisksctl-lock-cmd "lock")

(defvar udisksctl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'udisksctl-unlock)
    (define-key map "l" 'udisksctl-lock)
    (define-key map "m" 'udisksctl-mount)
    (define-key map "u" 'udisksctl-unmount)
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

(defun udisksctl-read-passphrase()
  "read the passphrase for an encrypted device"
  (interactive)
  (read-passwd "Enter passphrase: "))

(defun udisksctl-execute-cmd-comint (cmd device)
  "execute cmd on device, cmd requires user input"
  (let (params)
    (setq params (append (list device) params))
    (setq params (append (list "-b") params))
    (setq params (append (list cmd) params))
    (udisksctl-comint params)))

(defun udisksctl-execute-cmd (cmd device)
  "execute cmd on device, does not require user input"
  (if (boundp 'device )
      (call-process "udisksctl" nil nil nil cmd "-b" device)
    (call-process "udisksctl" nil nil nil cmd "-b" device)))

(defun udisksctl-unlock()
  (interactive)
  (setq udisksctl-device (udisksctl-read-device))
  (udisksctl-execute-cmd-comint udisksctl-unlock-cmd udisksctl-device))

(defun udisksctl-lock()
  (interactive)
  (setq udisksctl-device (udisksctl-read-device))
  (udisksctl-execute-cmd-comint udisksctl-lock-cmd udisksctl-device))

(defun udisksctl-mount()
  (interactive)
  (setq udisksctl-device (udisksctl-read-device))
  (udisksctl-execute-cmd udisksctl-mount-cmd udisksctl-device))

(defun udisksctl-unmount()
  (interactive)
  (setq udisksctl-device (udisksctl-read-device))
  (udisksctl-execute-cmd udisksctl-unmount-cmd udisksctl-device))

(defun udisksctl-comint(params)
  (apply 'make-comint "*udiskctl-comint*" "udisksctl" nil params))

(defun udisksctl-read-device()
"read a device name to work on (mount, unlock ...)"
  (interactive)
  (read-string "Enter device name: "))

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
