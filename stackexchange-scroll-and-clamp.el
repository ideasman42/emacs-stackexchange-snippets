;;; stackexchange-scroll-and-clamp.el --- Scroll, clamping to screen bounds -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/57639',
;;   'author': ('ideasman42', ),
;;   'replace': (
;;     (r'\bmy-', 'stackexchange-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Bind the up/down scroll commands to page up/down keys, eg:
;;
;;    (global-set-key (kbd "<next>") 'stackexchange-scroll-and-clamp-up-command)
;;    (global-set-key (kbd "<prior>") 'stackexchange-scroll-and-clamp-down-command)
;;

;;; Code:

;; Scrolling that re-centers, keeping the cursor vertically centered.
;; Also clamps window top when scrolling down,
;; so the text doesn't scroll off-screen.

(defun stackexchange-scroll-and-clamp--forward-line (n)
  "Wrap `forward-line', supporting Emacs built-in goal column.
Argument N the number of lines, passed to `forward-line'."
  (let ((next-column
         (or goal-column
             (and (memq last-command
                        '(next-line previous-line line-move))
                  (if (consp temporary-goal-column)
                      (car temporary-goal-column)
                    temporary-goal-column)))))
    (unless next-column
      (setq temporary-goal-column (current-column))
      (setq next-column temporary-goal-column))
    (forward-line n)
    (move-to-column next-column))
  ;; Needed so `temporary-goal-column' is respected in the future.
  (setq this-command 'line-move))

(defmacro stackexchange-scroll-and-clamp--with-evil-visual-mode-hack (&rest body)
  "Execute BODY with the point not restricted to line limits.

This is needed so the point is not forced to line bounds
even when in evil visual line mode."
  `(let ((mark-found nil))
     (when (and (fboundp 'evil-visual-state-p)
                (funcall 'evil-visual-state-p)
                (fboundp 'evil-visual-type)
                (eq (funcall 'evil-visual-type) 'line)
                (boundp 'evil-visual-point))
       (let ((mark (symbol-value 'evil-visual-point)))
         (when (markerp mark)
           (setq mark-found mark))))
     (unwind-protect
         (progn
           (when mark-found
             (goto-char (marker-position mark-found)))
           ,@body)
       (when mark-found
         (set-marker mark-found (point))))))


;;;###autoload
(defun stackexchange-scroll-and-clamp-up-command ()
  (interactive)
  (stackexchange-scroll-and-clamp--with-evil-visual-mode-hack
   (let ((height (window-height)))

     ;; Move point.
     (stackexchange-scroll-and-clamp--forward-line height)

     ;; Move window.
     (set-window-start
      (selected-window)
      (min
       (save-excursion ;; new point.
         (forward-line (- (/ height 2)))
         (point))
       (save-excursion ;; max point.
         (goto-char (point-max))
         (beginning-of-line)
         (forward-line (- (- height (+ 1 (* 2 scroll-margin)))))
         (point))))))
  (redisplay))

;;;###autoload
(defun stackexchange-scroll-and-clamp-down-command ()
  (interactive)
  (stackexchange-scroll-and-clamp--with-evil-visual-mode-hack
   (let* ((height (window-height)))

     ;; Move point.
     (stackexchange-scroll-and-clamp--forward-line (- height))
     (setq this-command 'line-move)

     ;; Move window.
     (set-window-start
      (selected-window)
      (save-excursion ;; new point.
        (forward-line (- (/ height 2)))
        (point)))))
  (redisplay))

(provide 'stackexchange-scroll-and-clamp)
;;; stackexchange-scroll-and-clamp.el ends here
