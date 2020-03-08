;;; stackexchange-c-transpose-words.el --- Transpose comma separated arguments (C family languages) -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/47934',
;;   'replace': (
;;     (r'\bmy-c-transpose-args-', 'stackexchange-c-transpose-args-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Utility to move function arguments in C-family languages forwards
;; and backwards.
;; Note that even though this is intended for C,
;; it works well in other languages that comma separate arguments
;; (Python, Lua ... etc).
;;
;; Transpose Arguments
;;
;; Public functions:
;;   (c-transpose-args-forward)
;;   (c-transpose-args-backward)

;;; Code:

(defun stackexchange-c-transpose-args--forward-to-argsep ()
  "Move to the end of the current c function argument.
Returns point."
  (interactive)
  (while (progn
           (comment-forward most-positive-fixnum)
           (looking-at "[^,)]"))
    (forward-sexp))
  (point))

(defun stackexchange-c-transpose-args--backward-to-argsep ()
  "Move to the beginning of the current c function argument.
Returns point."
  (interactive)
  (let ((pt (point))
        cur)
    (up-list -1)
    (forward-char)
    (while (progn
             (setq cur (point))
             (> pt (stackexchange-c-transpose-args--forward-to-argsep)))
      (forward-char))
    (goto-char cur)))

(defun stackexchange-c-transpose-args--direction (is_forward)
  "Transpose two arguments of a c-function.
The first arg is the one with point in it."
  (interactive)
  (let* ((pt-original (point)) ;; only different to pt when not 'is_forward'
         (pt (progn
               (when (not is_forward)
                 (goto-char (- (stackexchange-c-transpose-args--backward-to-argsep) 1))
                 (unless (looking-at ",")
                   (goto-char pt-original)
                   (user-error "Argument separator not found")))
               (point)))
         (b (stackexchange-c-transpose-args--backward-to-argsep))
         (sep (progn
                (goto-char pt)
                (stackexchange-c-transpose-args--forward-to-argsep)))
         (e (progn
              (unless (looking-at ",")
                (goto-char pt-original)
                (user-error "Argument separator not found"))
              (forward-char)
              (stackexchange-c-transpose-args--forward-to-argsep)))
         (ws-first (buffer-substring-no-properties
                    (goto-char b)
                    (progn
                      (skip-chars-forward "[[:space:]\n]")
                      (point))))
         (first (buffer-substring-no-properties (point) sep))
         (ws-second (buffer-substring-no-properties
                     (goto-char (1+ sep))
                     (progn
                       (skip-chars-forward "[[:space:]\n]")
                       (point))))
         (second (buffer-substring-no-properties (point) e)))

    (delete-region b e)
    (insert ws-first second "," ws-second first)

    ;; Correct the cursor location to be on the same character.
    (if is_forward
        (goto-char
         (+
          ;; word start.
          (- (point) (length first))
          ;; Apply initial offset within the word.
          (- pt b (length ws-first))))
      (goto-char
       (+
        b (length ws-first)
        ;; Apply initial offset within the word.
        (- pt-original (+ pt 1 (length ws-second))))))))

;;;###autoload
(defun stackexchange-c-transpose-args-forward ()
  (interactive)
  (stackexchange-c-transpose-args--direction t))
;;;###autoload
(defun stackexchange-c-transpose-args-backward ()
  (interactive)
  (stackexchange-c-transpose-args--direction nil))

(provide 'stackexchange-c-transpose-args)
;;; stackexchange-c-transpose-args.el ends here
