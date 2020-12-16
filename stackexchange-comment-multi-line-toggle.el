;;; stackexchange-comment-multi-line-toggle.el --- Toggle multi-line comments around the selection -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/56285',
;;   'author': ('ideasman42', ),
;;   'replace': (
;;     (r'\bmy-', 'stackexchange-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Toggles multi-line comment using the selection to define the region.
;;
;; TODO:
;;
;; Other popular languages.

;;; Code:

(defun stackexchange-comment-multi-line-toggle--impl (head tail head-regex tail-regex)
  "Utility for toggling multi-line comments.
Argument HEAD Start of multi-line comment.
Argument TAIL End of multi-line comment.
Argument HEAD-REGEX Match the start of a multi-line comment.
Argument TAIL-REGEX Match the end of a multi-line comment."
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((beg (region-beginning))
        (end (region-end))
        (match-head nil)
        (match-tail nil)
        ;; Prevent local language settings
        ;; impacting basic space stepping.
        (syntax-table (make-syntax-table)))
    (save-excursion
      (with-syntax-table syntax-table
        ;; True when we have multi-line comments.
        (if (and (setq match-head
                       (save-match-data
                         (goto-char beg)
                         (skip-chars-forward "[:blank:]")
                         (when (looking-at head-regex)
                           (match-data))))
                 (setq match-tail
                       (save-match-data
                         (goto-char end)
                         (skip-chars-backward "[:blank:]")
                         (when (looking-back tail-regex)
                           (match-data))))
                 ;; Double check there is no overlap.
                 (<= (save-match-data
                       (set-match-data match-head)
                       (match-end 0))
                     (save-match-data
                       (set-match-data match-tail)
                       (match-beginning 0))))

            ;; Remove multi-line comment.
            (dolist (match (list match-tail match-head))
              (save-match-data
                (set-match-data match)
                (goto-char (match-beginning 0))
                (replace-match "" t nil nil))
              (beginning-of-line)
              (when (looking-at-p "[[:blank:]]*$")
                (kill-whole-line)))

          ;; Add multi-line comment.
          (goto-char end)
          (insert tail)
          (indent-according-to-mode)
          (insert "\n")

          (goto-char beg)
          (insert head)
          (indent-according-to-mode)
          (insert "\n"))))))

;;;###autoload
(defun stackexchange-comment-multi-line-toggle (&optional head tail head-regex tail-regex)
  "Toggle multi-line comments for various languages."
  (interactive)
  (cond
   ;; Support optionally passing in arguments.
   ((and head tail head-regex tail-regex)
    (stackexchange-comment-multi-line-toggle--impl
     head tail head-regex tail-regex))
   ((member major-mode '(c-mode c++-mode glsl-mode))
    (stackexchange-comment-multi-line-toggle--impl
     "#if 0"
     "#endif"
     "^[[:blank:]]*#[[:blank:]]*if[[:blank:]]+0[[:blank:]]*$"
     "^[[:blank:]]*#[[:blank:]]*endif[[:blank:]]*$"))
   ((member major-mode '(lua-mode))
    (stackexchange-comment-multi-line-toggle--impl
     "--[["
     "]]"
     "^[[:blank:]]*\\-\\-\\[\\[[[:blank:]]*$"
     "^[[:blank:]]*\\]\\][[:blank:]]*$"))
   ((member major-mode '(cmake-mode))
    (stackexchange-comment-multi-line-toggle--impl
     "#[["
     "]]"
     "^[[:blank:]]*#\\[\\[[[:blank:]]*$"
     "^[[:blank:]]*\\]\\][[:blank:]]*$"))
   ((member major-mode '(python-mode))
    (stackexchange-comment-multi-line-toggle--impl
     "'''"
     "'''"
     "^[[:blank:]]*\\('''\\|\"\"\"\\)[[:blank:]]*$"
     "^[[:blank:]]*\\('''\\|\"\"\"\\)[[:blank:]]*$"))
   (t
    (user-error "Multi-line comment unsupported major mode '%s'" major-mode))))

(provide 'stackexchange-comment-multi-line-toggle)
;;; stackexchange-comment-multi-line-toggle.el ends here
