;; emacs interface to udisksctl

(setq udisksctl-buffer-name "*udisksctl*")
(setq udisksctl-status-cmd "status")

(defvar udisksctl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Keymap for `udisksctl-mode'.")

(define-derived-mode udisksctl-mode special-mode
  "Udisksctl"
  "Major mode for udisksctl. Shows status information about disk
via udisksctl."
)

(defun udisksctl-status()
  (call-process "udisksctl" nil udisksctl-buffer-name nil udisksctl-status-cmd))

(defun udisksctl-buffer()
"creates the udisksctl buffer"
  (get-buffer-create udisksctl-buffer-name)
  (switch-to-buffer udisksctl-buffer-name)
  (read-only-mode)
  )

(defun udisksctl()
  "run udiskctl status"
  (interactive)
  (progn
    (udisksctl-buffer)
    (udisksctl-status)
  ))

(provide 'udisksctl-mode)
