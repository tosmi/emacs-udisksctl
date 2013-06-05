;; emacs interface to udisksctl

(setq udisksctl-buffer-name "*udisksctl*")

(setq udisksctl-status-cmd "status")

(defun udisksctl-status()
  (call-process "udisksctl" nil udisksctl-buffer-name nil udisksctl-status-cmd))

(defun udisksctl-buffer()
  ()
  )

(defun udisksctl()
  "run udiskctl status"
  (interactive)
  (progn
    (udisksctl-status)
    (switch-to-buffer udisksctl-buffer-name)
    (read-only-mode))
  )
