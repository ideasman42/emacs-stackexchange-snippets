;;; stackexchange-ispell-word-immediate.el --- Correct spelling without prompting -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/55545',
;;   'author': ('ideasman42', ),
;;   'replace': (
;;     (r'\bispell-word-immediate\b', 'stackexchange-ispell-word-immediate'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;;   'index': 1,
;; }

;;; Usage:

;; Correct the spelling of a word `ispell-word',
;; without prompting for which word, cycling through it's results on
;; successive calls.

;;; Code:

(defmacro stackexchange-ispell-word-immediate--with-messages-as-list (message-list &rest body)
  "Run BODY adding any message call to the MESSAGE-LIST list."
  (declare (indent 1))
  `
  (let ((temp-message-list (list)))
    (cl-letf
        (((symbol-function 'message)
          (lambda (&rest args)
            ;; Only check if non-null because this is a signal not to log at all.
            (when message-log-max
              (push (apply 'format-message args) temp-message-list)))))
      (unwind-protect
          (progn
            ,@body)
        ;; Protected.
        (setq ,message-list (append ,message-list (reverse temp-message-list)))))))

(defvar-local stackexchange-ispell-word-immediate--alist nil
  "Internal properties for repeated `stackexchange-ispell-word-immediate'")

(defun stackexchange-ispell-word-immediate--impl (cycle-direction)
  "Run `ispell-word', using the first suggestion.
Argument CYCLE-DIRECTION The offset for cycling words, 1 or -1 for forwards/backwards."
  (let ((message-list (list))
        (index 0)
        (point-init (point))
        (display-text nil))

    ;; Roll-back and cycle through corrections.
    (when
        (and
         stackexchange-ispell-word-immediate--alist
         (or
          (eq last-command 'stackexchange-ispell-word-immediate-forwards)
          (eq last-command 'stackexchange-ispell-word-immediate-backwards)))

      ;; Roll-back correction.
      (let ((alist stackexchange-ispell-word-immediate--alist))

        ;; Roll back the edit.
        (delete-region (alist-get 'start alist) (alist-get 'end alist))
        (insert (alist-get 'word alist))

        ;; Update vars from previous state.
        (setq point-init (alist-get 'point alist))
        (setq index (+ cycle-direction (cdr (assq 'index alist))))

        ;; Roll back the buffer state.
        (setq buffer-undo-list (alist-get 'buffer-undo-list alist))
        (setq pending-undo-list (alist-get 'pending-undo-list alist))
        (goto-char point-init)))

    ;; Clear every time, ensures stale data is never used.
    (setq stackexchange-ispell-word-immediate--alist nil)

    (cl-letf
        (((symbol-function 'ispell-command-loop)
          (lambda (miss _guess word start end)
            ;; Wrap around in either direction.
            (setq index (mod index (length miss)))
            (let ((word-at-index (nth index miss)))

              ;; Generate display text.
              (setq display-text
                    (string-join
                     (mapcar
                      (lambda (word-iter)
                        (if (eq word-at-index word-iter)
                            (format "[%s]" (propertize word-iter 'face 'match))
                          (format " %s " word-iter)))
                      miss)
                     ""))

              ;; Set the state for redoing the correction.
              (setq stackexchange-ispell-word-immediate--alist
                    (list
                     ;; Tricky! but nicer usability.
                     (cons 'buffer-undo-list buffer-undo-list)
                     (cons 'pending-undo-list pending-undo-list)
                     (cons 'point point-init)

                     (cons 'index index)
                     (cons 'word word)
                     (cons 'start (marker-position start))
                     (cons 'end
                           (+ (marker-position end)
                              (- (length word-at-index) (length word))))))

              word-at-index))))

      ;; Run quietly so message output doesn't flicker.
      (prog1 (stackexchange-ispell-word-immediate--with-messages-as-list message-list (ispell-word))

        ;; Log the message, only display if we don't have 'display-text'
        ;; This avoids flickering message output.
        (let ((inhibit-message (not (null display-text))))
          (dolist (message-text message-list)
            (message "%s" message-text)))

        ;; Run last so we can ensure it's the last text in the message buffer.
        ;; Don't log because it's not useful to keep the selection.
        (when display-text
          (let ((message-log-max nil))
            (message "%s" display-text)))))))

;; Public functions.
;;;###autoload
(defun stackexchange-ispell-word-immediate-forwards ()
  "Run `ispell-word', using the first suggestion, or cycle forwards."
  (interactive)
  (stackexchange-ispell-word-immediate--impl 1))

;;;###autoload
(defun stackexchange-ispell-word-immediate-backwards ()
  "Run `ispell-word', using the first suggestion, or cycle backwards."
  (interactive)
  (stackexchange-ispell-word-immediate--impl -1))

(provide 'stackexchange-ispell-word-immediate)
;;; stackexchange-ispell-word-immediate.el ends here
