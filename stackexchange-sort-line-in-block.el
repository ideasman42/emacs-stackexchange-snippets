;;; stackexchange-sort-line-in-block.el --- Sort a single line within a block -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/54023',
;;   'replace': (
;;     (r'\bsort-line-in-block\b', 'stackexchange-sort-line-in-block'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; This function is intended as a quick convenience function
;; to use when adding an item to a list which is already sorted
;; or partially sorted.
;;
;; Without this, it's often necessary to select a block of text,
;; then run the sort command on it - which ends up involving quite a
;; few steps.
;;
;; With this command, you can sort the current line relative to it's
;; surrounding text (at the same level of indentation)
;; with a single command.

;;; Code:

;;;###autoload
(defun stackexchange-sort-line-in-block ()
  "Sort the current line relative to surrounding lines in the same block.

Blocks are delimited by empty lines.
"
  (interactive)
  (let* ((pt-init (point))
         (indent-init (current-indentation))
         (line-init-begin (line-beginning-position))
         (line-init-end (line-end-position))
         (line-init-length (- line-init-end line-init-begin))
         (line-init
          (buffer-substring-no-properties
           line-init-begin line-init-end))
         (pt-init-indent (- pt-init line-init-begin))
         (pt-found nil))

    (if (string-blank-p line-init)
        (message "Line empty, not sorting")

      (dolist (dir '(1 -1))
        (save-excursion
          (beginning-of-line)
          (let ((keep-looking (null pt-found)))
            (while keep-looking
              (setq keep-looking nil)
              (forward-line dir)
              (when (eq indent-init (current-indentation))
                (let ((line-cmp
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))

                  (when (not (string-blank-p line-cmp))
                    (when
                        (if (eq dir 1)
                            (string< line-cmp line-init)
                          (string< line-init line-cmp))
                      (setq pt-found
                            (if (eq dir 1)
                                (line-end-position)
                              (line-beginning-position)))
                      (setq keep-looking t)))))))))

      (if (not pt-found)
          (message "Line already sorted!")

        (goto-char pt-found)
        (if (< pt-init pt-found)
            (insert "\n" line-init)
          (insert line-init "\n"))
        (forward-char
         (+
          ;; Offset based on position of new line.
          (if (< pt-init pt-found) 0 -1)
          ;; Back to the start of the line.
          (- line-init-length)
          ;; Match the initial indent level.
          pt-init-indent))

        (if (< pt-init pt-found)
            (delete-region
             line-init-begin
             (+ 1 line-init-end))
          (delete-region
           (+ line-init-length line-init-begin)
           (+ line-init-length 1 line-init-end)))))))

(provide 'stackexchange-sort-line-in-block)
;;; stackexchange-sort-line-in-block.el ends here
