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
;; This is a major mode to interact with udisksctl.  It allows you to
;; see devices, unlock/lock encrypted device and mounting/unmounting
;; devices via udisksctl

;; Operating Systems:
;; Developped under Linux.  Should work on all OS'es
;; that support the udisksctl command.


;;; Code:

;;; Customizable variables

;;; Todo
;; - maybe the functions to mount/unmount/lock/unlock could be
;;   defined via defmacro?

(defvar udisksctl-buffer-name "*udisksctl*")
(defvar udisksctl-status-cmd "status")
(defvar udisksctl-mount-cmd "mount")
(defvar udisksctl-unmount-cmd "unmount")
(defvar udisksctl-unlock-cmd "unlock")
(defvar udisksctl-lock-cmd "lock")
(defvar udisksctl-output nil)
(defvar udisksctl-process-buffer-name "*udisksctl-process*")
(defvar udisksctl-process nil)
(defvar udisksctl-device nil)
(defvar udisksctl-mapped-devices nil)
(defvar udisksctl-mounted-devices nil)

(defvar udisksctl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'udisksctl-unlock)
    (define-key map "l" 'udisksctl-lock)
    (define-key map "m" 'udisksctl-mount)
    (define-key map "U" 'udisksctl-unmount)
    (define-key map "g" 'udisksctl-refresh-buffer)
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

(setq debug-on-error nil)

(defun udisksctl-execute-cmd (cmd device &optional noerase)
  "Execute CMD on DEVICE, does not require user input.
If NOERASE is specified the output buffer will not be erased."
  (let ((process-connection-type t))
    (get-buffer-create  udisksctl-process-buffer-name)
    (with-current-buffer udisksctl-process-buffer-name
      (if noerase
	  (goto-char (point-max))
	(erase-buffer))
      (setq udisksctl-output nil)
      (setq udisksctl-process
	    (start-process "udisksctl" udisksctl-process-buffer-name "udisksctl" cmd "-b" device)))
    (set-process-filter udisksctl-process 'udisksctl-process-filter)
    (set-process-sentinel udisksctl-process 'udisksctl-process-sentinel)
    (while (equal (process-status udisksctl-process) 'run)
      (sit-for 0.1 t))
    (if (not (equal (process-exit-status udisksctl-process) 0))
	(udisksctl-error-message)
      (progn
	(udisksctl-success-message)
	(udisksctl-remember-mounts-and-mappings)))
    (udisksctl-refresh-buffer)))

(defun udisksctl-error-message()
  (error "%s" (or (with-current-buffer (get-buffer udisksctl-process-buffer-name)
		    (goto-char (point-min))
		    (or
		     (re-search-forward "^\\([^[:space:]].*?\\)\\.$" nil t))
;;		     (re-search-forward "\\(^[^[:space:]].*$\\)" nil t))
;;		     (re-search-forward "\\(No key available with this passphrase\\)" nil t)
;;		     (re-search-forward "\\(Device [^[:space:]]+ is not unlocked\\)" nil t)
;;		     (re-search-forward (concat "^\\(Error.*\\)" paragraph-separate) nil t))
		    (match-string 1))
		  "udiskctl failed"
		  )))

(defun udisksctl-success-message()
  (message "%s" (with-current-buffer (get-buffer udisksctl-process-buffer-name)
		  (goto-char (point-min))
		  (re-search-forward "^\\(.*\\)$" nil t)
		  (match-string 1))))

(defvar udisksctl-status-list nil)
(defun udisksctl-remember-mounts-and-mappings()
  (with-current-buffer (get-buffer udisksctl-process-buffer-name)
    (goto-char (point-min))
    (cond ((or
	    (re-search-forward "Unlocked \\([^[:space:]]+\\) as \\([^[:space:]]+\\)\." nil t)
	    (re-search-forward "Mounted \\([^[:space:]]+\\) at \\([^[:space:]]+\\)\.?" nil t))
	   (add-to-list 'udisksctl-status-list (cons (match-string 1) (match-string 2)))))
    (cond ((or
	    (re-search-forward "Unmounted \\([^[:space:]]+\\)\." nil t))
	   (udisksctl-remove-mapping (match-string 1))))))

(defun udisksctl-remove-mapping (searchkey)
  "Search for SEARCHKEY in the status list."
  (let((key (assoc searchkey udisksctl-status-list)))
    (setq udisksctl-status-list (assq-delete-all (car key) udisksctl-status-list))))

(defun udisksctl-print-alist (list format)
  "Print the assoc LIST with the FORMAT specified."
  (when list
    (let ((device (car (car list)))
	  (dmdevice (cdr (car list))))
      (insert (format format device dmdevice))
      (udisksctl-print-alist (cdr list) format))))

(defun udisksctl-process-filter(proc string)
  "filter udisksctl output for a password prompt"
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (if (string-match "^Passphrase: " string)
	(process-send-string proc (concat (read-passwd "Passphrase: " nil) "\n"))
      (save-excursion
	(goto-char (process-mark proc))
	(insert string)
	(set-marker (process-mark proc) (point))
	(goto-char (process-mark proc))))))

(defun udisksctl-process-sentinel(proc event)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert event "\n"))))

(defun udisksctl-parse-output(udisksctl-proc)
  (save-current-buffer
    (set-buffer (process-buffer udisksctl-proc))))

;; (defmacro udiskctl-cmd (cmd &optional device)
;;   "Run the given udisksctl CMD on DEVICE."
;;   `(interactive)
;;   `(if (not device)
;;        `(setq udisksctl-device (udisksctl-read-device "Enter device name to lock: "))
;;      `(setq udisksctl-device 'device))
;;   `(udisksctl-execute-cmd udisksctl-(cmd)-cmd udisksctl-device))

;; (macroexpand '(udiskctl-cmd "unlock" "/dev/sde1"))

;; (udiskctl-cmd )

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
  (if (boundp 'message)
      (read-string message)
    (read-string "Enter device name: ")))

(defun udisksctl-status-cmd()
  (call-process "udisksctl" nil udisksctl-buffer-name nil
		udisksctl-status-cmd)
  (udisksctl-print-status))

(defun udisksctl-print-status()
  (with-current-buffer (get-buffer udisksctl-buffer-name)
    (goto-char (point-max))
    (if (not (equal udisksctl-status-list nil))
	(progn
	  (insert "\nDevice mappings\n---------------\n")
	  (udisksctl-print-alist udisksctl-status-list "%s mapped to %s\n")))))

(defun udisksctl-refresh-buffer()
""
  (interactive)
  (with-current-buffer (get-buffer udisksctl-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (udisksctl-status-cmd))))

(defun udisksctl-create-buffer()
  "creates the udisksctl buffer"
  (if (not (buffer-live-p (get-buffer udisksctl-buffer-name)))
      (progn
	(get-buffer-create udisksctl-buffer-name)
	(udisksctl-refresh-buffer)))
  (switch-to-buffer udisksctl-buffer-name)
  (udisksctl-mode))

;;;###autoload
(defun udisksctl-status()
  "run udiskctl status"
  (interactive)
  (progn
    (udisksctl-create-buffer)))

(provide 'udisksctl)
;;; udisksctl.el ends here
