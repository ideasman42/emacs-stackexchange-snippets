;;; stackexchange-mode-line-visual-bell.el --- Visual bell using mode line -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/55988',
;;   'replace': (
;;     (r'\bmode-line-visual-bell\b', 'stackexchange-mode-line-visual-bell'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Temporarily highlight the mode-line & header
;; instead of making an audible beep.
;;
;; Add this to your configuration to enable this:
;;
;; (stackexchange-mode-line-visual-bell)

;;; Code:

;;;###autoload
(defun stackexchange-mode-line-visual-bell ()
  (setq visible-bell nil)
  (setq ring-bell-function 'stackexchange-mode-line-visual-bell--flash))

(defun stackexchange-mode-line-visual-bell--flash ()
  (let ((frame (selected-frame)))
    (invert-face 'header-line frame)
    (invert-face 'header-line-highlight frame)
    (invert-face 'mode-line frame)
    (invert-face 'mode-line-inactive frame)
    (run-with-timer
     0.1 nil
     #'(lambda (frame)
         (invert-face 'header-line frame)
         (invert-face 'header-line-highlight frame)
         (invert-face 'mode-line frame)
         (invert-face 'mode-line-inactive frame))
     frame)))

(provide 'stackexchange-mode-line-visual-bell)
;;; stackexchange-mode-line-visual-bell.el ends here
