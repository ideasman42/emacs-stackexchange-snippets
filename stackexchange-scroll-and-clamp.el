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

;; Scrolling that re-centers, keeping the cursor centered.
;; Also clamps window top when scrolling down, so the text doesn't scroll off-screen.
(defvar-local stackexchange-scroll-and-clamp--column nil)

(defun stackexchange-scroll-and-clamp--column-update ()
  (unless (member last-command '(stackexchange-scroll-and-clamp-up-command stackexchange-scroll-and-clamp-down-command))
    (setq stackexchange-scroll-and-clamp--column nil))
  (unless stackexchange-scroll-and-clamp--column
    (setq stackexchange-scroll-and-clamp--column (current-column))))

;;;###autoload
(defun stackexchange-scroll-and-clamp-up-command ()
  (interactive)
  (stackexchange-scroll-and-clamp--column-update)
  (let ((height (window-height)))

    ;; Move point.
    (forward-line height)
    (move-to-column stackexchange-scroll-and-clamp--column)

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
          (point)))))
  (redisplay))

;;;###autoload
(defun stackexchange-scroll-and-clamp-down-command ()
  (interactive)
  (stackexchange-scroll-and-clamp--column-update)
  (let* ((height (window-height)))

    ;; Move point.
    (forward-line (- height))
    (move-to-column stackexchange-scroll-and-clamp--column)

    ;; Move window.
    (set-window-start
      (selected-window)
      (save-excursion ;; new point.
        (forward-line (- (/ height 2)))
        (point))))
  (redisplay))

(provide 'stackexchange-scroll-and-clamp)
;;; stackexchange-scroll-and-clamp.el ends here
