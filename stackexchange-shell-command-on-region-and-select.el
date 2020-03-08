;;; stackexchange-shell-command-on-region-and-select.el --- Run command on region, keeping the selection -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/34900',
;;   'replace': (
;;     (r'\bmy-shell-command-on-region-and-select\b', 'stackexchange-shell-command-on-region-and-select'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; While Emacs `shell-command-on-region' operates on the selection,
;; the resulting text isn't selected.
;; This is a utility command that keeps the selection for the
;; resulting output. With support for `evil-mode' (when in use).

;;; Code:

;; Wrapper for 'shell-command-on-region', keeps the selection.
;;;###autoload
(defun stackexchange-shell-command-on-region-and-select
    (start
     end
     command
     &optional
     output-buffer
     replace
     error-buffer
     display-error-buffer
     region-noncontiguous-p)
  "Wrapper for 'shell-command-on-region', re-selecting the output.

Useful when called with a selection, so it can be modified in-place."
  (interactive)
  (let ((buffer-size-init (buffer-size)))
    (shell-command-on-region
     start
     end
     command
     output-buffer
     replace
     error-buffer
     display-error-buffer
     region-noncontiguous-p)
    (setq deactivate-mark nil)
    (setq end (+ end (- (buffer-size) buffer-size-init)))
    (set-mark start)
    (goto-char end)
    (activate-mark)
    ;; needed for evil line mode
    (when (and (boundp evil-state) (string= evil-state "visual"))
      (when (eq (evil-visual-type) evil-visual-line)
        (evil-visual-select start end 'line)))))

(provide 'stackexchange-shell-command-on-region-and-select)
;;; stackexchange-shell-command-on-region-and-select.el ends here
