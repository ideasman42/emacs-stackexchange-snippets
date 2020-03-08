;;; stackexchange-frame-urgent-hint-set.el --- Visual bell using mode-line -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/56037',
;;   'replace': (
;;     (r'\bmy-', 'stackexchange-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Set the window urgent hint.
;; Note: currently only supports X11.

;;; Code:

(defun stackexchange-frame-urgent-hint-set--for-x11 (frame arg &optional source)
  "Set the x11-urgency hint for the frame to arg:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear."
  (let* ((wm-prop "WM_HINTS")  ;; Constants.
         (wm-flag-urgent #x100)

         (wm-hints (append (x-window-property wm-prop frame wm-prop source nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints
            (if arg
                (logior flags wm-flag-urgent)
              (logand flags (lognot wm-flag-urgent))))
    (x-change-window-property wm-prop wm-hints frame wm-prop 32 t)))

;;;###autoload
(defun stackexchange-frame-urgent-hint-set (&optional arg)
  "Mark the current Emacs frame as requiring urgent attention.

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag
(which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let*
      (
       (frame (selected-frame))
       (win-system (window-system frame)))
    (cond
     ((eq win-system 'x)
      (stackexchange-frame-urgent-hint-set--for-x11 frame (not arg)))
     ;; TODO, other platforms.
     (t
      (message "Urgent hint for window system %S unsupported" win-system)))))

(provide 'stackexchange-frame-urgent-hint-set)
;;; stackexchange-frame-urgent-hint-set.el ends here
