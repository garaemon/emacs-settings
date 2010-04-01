;; emacs-settings bootstrap emacs lisp

(require 'cl)

(defun load-emacs-settings (dir)
  (let ((installed-file (format "%s/emacs.d/installed" dir)))
    (let ((f (find-file-noselect installed-file)))
      (let ((sexp (read f)))
        (let ((bootstrap-library
               (remove-if-not #'(lambda (x)
				  (destructuring-bind
				      (package . type)
				      x
				    (eq type 'bootstrap)))
			      sexp)))
	  (message "boostrap-library -> %s" bootstrap-library)
          (dolist (l bootstrap-library)
            (load-all-elisp-file l dir))))
      (kill-buffer f))))

(defun load-all-elisp-file (pkg-name root)
  (let ((pkg-directory (format "%s/emacs.d/%s" root (car pkg-name))))
    (let ((fs (remove-duplicates (directory-files
                                  (expand-file-name pkg-directory)
                                  t ".*\.\\(el\\|elc\\)$")
                                 :test #'string=
                                 )))
      (dolist (f fs)
        (load (file-name-sans-extension f)))
      t)))

(require 'cl)

