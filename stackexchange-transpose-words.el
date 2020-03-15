;;; stackexchange-transpose-words.el --- Transpose words, keeping the cursor location -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/54055',
;;   'author': ('ideasman42', ),
;;   'replace': (
;;     (r'\btranspose-words-', 'stackexchange-transpose-words-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; This has some usability improvements over Emacs transpose:
;;
;; - Transpose words with adjustments to keep the cursor in-place
;;   relative to the word.
;; - Doesn't move the cursor when transposing fails.
;; - Avoids strange behavior where being on the first character
;;   of a word enforces transpose backwards.
;; - Expose forward/backward transpose for easy key bindings.

;;; Code:

(defun stackexchange-transpose-words--impl (arg)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (let* ((pt-init (point))
             (pt-end-of-word (cdr bounds))
             (pt-offset (- pt-init pt-end-of-word))
             (pt-transpose
              (save-excursion
                ;; Without this, the first character will move the previous word.
                (goto-char pt-end-of-word)
                (if
                    (condition-case err
                        (progn
                          (transpose-words arg)
                          t)
                      (message err))
                    (+ (point) pt-offset)
                  nil))))

        (when pt-transpose
          (goto-char pt-transpose))))))

;;;###autoload
(defun stackexchange-transpose-words-backward ()
  (interactive)
  (stackexchange-transpose-words--impl -1))

;;;###autoload
(defun stackexchange-transpose-words-forward ()
  (interactive)
  (stackexchange-transpose-words--impl 1))

(provide 'stackexchange-transpose-words)
;;; stackexchange-transpose-words.el ends here
