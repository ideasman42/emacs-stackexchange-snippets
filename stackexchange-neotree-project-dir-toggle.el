;;; stackexchange-neotree-project-dir-toggle.el --- Toggle neotree in the project root -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/29500',
;;   'author': ('xakz:github', 'ideasman42'),
;;   'replace': (
;;     (r'\bmy-', 'stackexchange-'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; When working in a project it's useful to show only the project
;; files in the directory browser (not the entire file-system).
;; This command toggles NeoTree using the project root as detected by
;; projectile, find-file-in-project when they're in use or the
;; version control root as a fall-back.
;;
;; Bind this command to a key to toggle the directory. e.g:
;;
;; (define-key global-map (kbd "<f9>")
;;   'stackexchange-neotree-project-dir-toggle)
;;

;;; Code:

;;;###autoload
(defun stackexchange-neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using projectile, find-file-in-project,
or the current buffer directory."
  (interactive)
  (let* ((filepath (buffer-file-name))
         (project-dir
          (with-demoted-errors
              (cond
               ((featurep 'projectile)
                (projectile-project-root))
               ((featurep 'find-file-in-project)
                (ffip-project-root))
               (t ;; Fall back to version control root.
                (if filepath
                    (vc-call-backend
                     (vc-responsible-backend filepath) 'root filepath)
                  nil)))))
         (neo-smart-open t))

    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (neotree-show)
      (when project-dir
        (neotree-dir project-dir))
      (when filepath
        (neotree-find filepath)))))

(provide 'stackexchange-neotree-project-dir-toggle)
;;; stackexchange-neotree-project-dir-toggle.el ends here
