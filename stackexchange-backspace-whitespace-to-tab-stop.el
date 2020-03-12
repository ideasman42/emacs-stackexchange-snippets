;;; stackexchange-backspace-whitespace-to-tab-stop.el --- Backspace to tab-stop or single character -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/56084',
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
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0)
        (setq movement tab-width))
      ;; Account for edge case near beginning of buffer.
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$"
                          (buffer-substring-no-properties
                           (- p movement) p))
            (backward-delete-char (- (match-end 1)
                                     (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(provide 'stackexchange-backspace-whitespace-to-tab-stop)
;;; stackexchange-backspace-whitespace-to-tab-stop.el ends here
