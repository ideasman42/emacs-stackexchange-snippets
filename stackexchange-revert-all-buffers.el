;;; stackexchange-revert-all-buffers.el --- Revert all buffers, with progress output -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/50730',
;;   'replace': (
;;     (r'\brevert-all-buffers\b', 'stackexchange-revert-all-buffers'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; When Emacs has many files open, changes on-disk can cause many
;; prompts to reload the file.
;; When switching branches or applying patches outside of Emacs
;; it can be handy to reload all files.
;;
;; This command does just this, reporting progress since this
;; can take some time when there are many files open.

;;; Code:

;;;###autoload
(defun stackexchange-revert-all-buffers ()
  "Refresh all open buffers from their respective files.

Buffers which no longer exist are closed.

This can be useful when updating or checking out branches outside of Emacs."
  (interactive)
  (let* ((filename-and-buffer-list ;; pairs of (filename, buf)
          (mapcar
           (lambda (buf) (list (buffer-file-name buf) buf))
           (seq-filter 'buffer-file-name (buffer-list))))

         (count (length filename-and-buffer-list))
         (count-final 0)
         (count-close 0)
         (count-error 0)
         ;; Keep text at a fixed width when redrawing.
         (format-count
          (format "%%%dd" (length (number-to-string count))))
         (format-text
          (concat "Reverting [" format-count " of " format-count "] %3d%%: %s"))
         (index 1))

    (message "Begin reverting %d buffers..." count)
    (while filename-and-buffer-list
      (pcase-let ((`(,filename ,buf) (pop filename-and-buffer-list)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers such as '*Messages*'.
        (message format-text index count (round (* 100 (/ (float index) count))) filename)
        (if (file-exists-p filename)
            ;; If the file exists, revert the buffer.
            (if (with-demoted-errors "Error: %S"
                  (with-current-buffer buf
                    (let ((no-undo (eq buffer-undo-list t)))

                      ;; Disable during revert.
                      (unless no-undo
                        (setq buffer-undo-list t)
                        (setq pending-undo-list nil))

                      (unwind-protect
                          (revert-buffer :ignore-auto :noconfirm)

                        ;; Enable again (always run).
                        (unless no-undo
                          ;; It's possible a plugin loads undo data from disk,
                          ;; check if this is still unset.
                          (when (and (eq buffer-undo-list t) (null pending-undo-list))
                            (setq buffer-undo-list nil))))))
                  t)
                (setq count-final (1+ count-final))
              (setq count-error (1+ count-error)))

          ;; If the file doesn't exist, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer.
            (message "Closing non-existing file buffer: %s" buf)
            (kill-buffer buf)
            (setq count-close (1+ count-close))))
        (setq index (1+ index))))
    (message
     (concat
      "Finished Revert All: " (format "%d buffer(s)" count-final)
      (if (zerop count-close)
          ""
        (format ", %d closed" count-close))
      (if (zerop count-error)
          ""
        (format ", %d error (see message buffer)" count-error))))))

(provide 'stackexchange-revert-all-buffers)
;;; stackexchange-revert-all-buffers.el ends here
