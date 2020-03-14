;;; stackexchange-imenu-goto.el --- Go to next/previous imenu item -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/33747',
;;   'replace': (
;;     (r'\bmy-', 'stackexchange-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Use this in a mode that supports imenu to navigate to
;; next/previous imenu items.

;;; Code:

(defun stackexchange-imenu-goto--closest-dir (direction)
  "Jump to the closest imenu item on the current buffer.
If direction is 1, jump to next imenu item.
If direction is -1, jump to previous imenu item.
See https://emacs.stackexchange.com/questions/30673
Adapted from `which-function' in::
https://github.com/typester/emacs/blob/master/lisp/progmodes/which-func.el"
  ;; Ensure `imenu--index-alist' is populated.
  (imenu--make-index-alist)

  (let ((alist imenu--index-alist)
        (minoffset (point-max))
        offset pair mark imstack destination)
    ;; Elements of alist are either ("name" . marker), or
    ;; ("submenu" ("name" . marker) ... ). The list can be
    ;; Arbitrarily nested.
    (while (or alist imstack)
      (if alist
          (progn
            (setq pair (car-safe alist)
                  alist (cdr-safe alist))
            (cond
             ((atom pair)) ;; Skip anything not a cons.

             ((imenu--subalist-p pair)
              (setq imstack   (cons alist imstack)
                    alist     (cdr pair)))

             ((number-or-marker-p (setq mark (cdr pair)))
              (when (> (setq offset (* (- mark (point)) direction)) 0)
                (when (< offset minoffset) ;; Find the closest item.
                  (setq minoffset offset
                        destination mark))))))

        (setq alist   (car imstack)
              imstack (cdr imstack))))
    (when destination
      (imenu-default-goto-function "" destination ""))))

;;;###autoload
(defun stackexchange-imenu-goto-next ()
  (interactive)
  (unless (stackexchange-imenu-goto--closest-dir 1)
    (goto-char (point-max))))

;;;###autoload
(defun stackexchange-imenu-goto-prev ()
  (interactive)
  (unless (stackexchange-imenu-goto--closest-dir -1)
    (goto-char (point-min))))

(provide 'stackexchange-imenu-goto)
;;; stackexchange-imenu-goto.el ends here
