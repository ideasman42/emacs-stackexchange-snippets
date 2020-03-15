;;; stackexchange-scratch-buffer-from-file.el --- Use a file for the scratch buffer on startup -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/38709',
;;   'author': ('ideasman42', ),
;;   'replace': (
;;     (r'\bmy-scratch-buffer-from-file\b', 'stackexchange-scratch-buffer-from-file'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; See code comments...

;;; Code:

;; Load startup text when available.
;;
;; Example usage:
;;
;;   (my-startup-buffer-from-file)
;;
;; Or if you like to use an org-mode scratch buffer,
;; an option file path can be passed in.
;; The file extension is used to set the mode:
;;
;;   (stackexchange-scratch-buffer-from-file (concat user-emacs-directory "scratch.org"))
;;

(defvar stackexchange-scratch-buffer-from-file--value nil)
;;;###autoload
(defun stackexchange-scratch-buffer-from-file (&optional scratch-file)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (when scratch-file
    (setq stackexchange-scratch-buffer-from-file--value scratch-file))

  (setq
   initial-buffer-choice
   (lambda ()
     (if (buffer-file-name)
         (current-buffer) ;; leave as-is
       (let ((original-buffer (current-buffer))
             (filename
              (or stackexchange-scratch-buffer-from-file--value
                  (concat user-emacs-directory "scratch.txt")))
             ;; Not essential, just gives some handy startup info.
             (startup-info
              (format
               "Emacs: %d.%d, time: %.2f, packages: %d"
               emacs-major-version
               emacs-minor-version
               (float-time (time-subtract after-init-time before-init-time))
               (length package-activated-list))))
         (with-current-buffer (get-buffer-create "*scratch*")
           ;; Don't track undo.
           (buffer-disable-undo)

           ;; Set the mode based on the filename, users may use filenames that infer modes.
           (let ((buffer-file-name filename))
             (set-auto-mode t))

           ;; Use the comment character set by the mode where possible.
           (let ((comment-start-or-empty
                  (if comment-start
                      ;; Ensure one trailing space (some comments include space).
                      (concat
                       (replace-regexp-in-string "[[:space:]]*$" "" comment-start)
                       " ")
                    "")))
             (if (file-exists-p filename)
                 (insert-file-contents filename)
               (insert
                comment-start-or-empty
                (format "Scratch buffer, create '%s' to replace this text on startup."
                        filename))
               (goto-char (point-min)))
             ;; Add some startup info above the static text.
             (insert comment-start-or-empty startup-info "\n\n"))
           (buffer-enable-undo)
           (set-buffer original-buffer)))))))

(provide 'stackexchange-scratch-buffer-from-file)
;;; stackexchange-scratch-buffer-from-file.el ends here
