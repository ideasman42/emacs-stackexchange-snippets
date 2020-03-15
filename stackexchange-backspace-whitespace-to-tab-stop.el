;;; stackexchange-backspace-whitespace-to-tab-stop.el --- Backspace to tab-stop or single character -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/56084',
;;   'author': ('trey-jackson', ),
;;   'replace': (
;;     (r'\bmy-', 'stackexchange-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Detect blank lines, using back-space to tab-stop,
;; otherwise operate on single characters.

;;; Code:

;;;###autoload
(defun stackexchange-backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode (region-active-p)
          (> (point)
             (save-excursion
               (back-to-indentation)
               (point))))
      (call-interactively 'backward-delete-char)
    (let ((step (% (current-column) tab-width))
          (pt (point)))
      (when (zerop step)
        (setq step tab-width))
      ;; Account for edge case near beginning of buffer.
      (setq step (min (- pt 1) step))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$"
                          (buffer-substring-no-properties
                           (- pt step) pt))
            (backward-delete-char (- (match-end 1)
                                     (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(provide 'stackexchange-backspace-whitespace-to-tab-stop)
;;; stackexchange-backspace-whitespace-to-tab-stop.el ends here
