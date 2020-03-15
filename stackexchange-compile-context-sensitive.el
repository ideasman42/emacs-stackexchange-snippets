;;; stackexchange-compile-context-sensitive.el --- Context sensitive build command -*- lexical-binding: t -*-

;;; Recipe:

;; {
;;   'url': 'https://emacs.stackexchange.com/a/56008',
;;   'author': ('ideasman42', ),
;;   'replace': (
;;     (r'\bmy-compile-context-sensitive\b', 'stackexchange-compile-context-sensitive'),
;;     # Auto-load functions without a '--'.
;;     (r'(\(defun\b)(((?!(\-\-)).)*)\n', r';;;###autoload\n\1\2\n'),
;;   ),
;; }

;;; Usage:

;; Detect the build system based on file filename
;; of the current buffer and run the appropriate build command.
;;
;; Useful when switching between projects which don't
;; have a single build-system that can be assosiated with the mode.
;; As well as non-code projects which have a make-files to
;; generate output.
;;
;; TODO:
;;
;; Other popular build-systems Cargo, Meson...?

;;; Code:

(defun stackexchange-compile-context-sensitive--locate-dominating-file-multi (dir compilation-filenames)
  "Search for the compilation file traversing up the directory tree.

DIR the base directory to search.
COMPILATION-FILENAMES a list pairs (id, list-of-names).
Note that the id can be any object, this is intended to identify the kind of group.

Returns a triplet (dir, filename, id) or nil if nothing is found.
"
  ;; Ensure 'test-dir' has a trailing slash.
  (let ((test-dir (file-name-as-directory dir))
        (parent-dir (file-name-directory (directory-file-name dir))))

    (catch 'mk-result
      (while (not (string= test-dir parent-dir))
        (dolist (test-id-and-filenames compilation-filenames)
          (pcase-let ((`(,test-id ,test-filenames) test-id-and-filenames))
            (dolist (test-filename test-filenames)
              (when (file-readable-p (concat test-dir test-filename))
                (throw 'mk-result (list test-dir test-filename test-id))))))
        (setq test-dir parent-dir)
        (setq parent-dir (file-name-directory (directory-file-name parent-dir)))))))

;;;###autoload
(defun stackexchange-compile-context-sensitive ()
  (interactive)
  (let* ((mk-reference-dir (expand-file-name "."))
         (mk-dir-file-id
          (stackexchange-compile-context-sensitive--locate-dominating-file-multi
           (directory-file-name mk-reference-dir)
           (list
            '("make" ("Makefile" "makefile" "GNUmakefile"))
            '("ninja" ("build.ninja"))
            '("scons" ("SConstruct"))))))

    (if mk-dir-file-id
        (pcase-let ((`(,dir ,file ,id) mk-dir-file-id))
          ;; ensure 'compile-command' is used.
          (let ((compilation-read-command nil)
                (compile-command
                 (cond
                  ((string= id "make")
                   (concat "make -C " (shell-quote-argument dir)))
                  ((string= id "ninja")
                   (concat "ninja -C " (shell-quote-argument dir)))
                  ((string= id "scons")
                   (concat "scons -C " (shell-quote-argument dir)))
                  (t
                   (error "Unhandled type (internal error)")))))

            (call-interactively 'compile)))
      (message "No makefile found in %S" mk-reference-dir))))

(provide 'stackexchange-compile-context-sensitive)
;;; stackexchange-compile-context-sensitive.el ends here
