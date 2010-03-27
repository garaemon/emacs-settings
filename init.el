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
    (let ((fs (directory-files (expand-file-name pkg-directory) t ".*\.el")))
      (dolist (f fs)
        (load f))
      t)))

(require 'cl)

;; utility functions for compatibility on some emacs implementations
(defun cocoa-emacs-p ()
  (and (= emacs-major-version 23) (eq window-system 'ns)))

(defun carbon-emacs-p ()
  (and (= emacs-major-version 22) (eq window-system 'mac)))

(defun meadowp ()
  (eq system-type 'windows-nt))

(defun cygwinp ()
  (eq system-type 'cygwin))

(defun emacs22p ()
  (= emacs-major-version 22))

(defun emacs23p ()
  (= emacs-major-version 23))

(defmacro when-gui (&rest bodies)
  `(when window-system
     ,@bodies))

(defmacro when-meadow (&rest bodies)
  `(when (meadowp)
     ,@bodies))

(defmacro when-darwin (&rest bodies)
  `(when (eq system-type 'darwin)
     ,@bodies))

(defmacro when-cygwin (&rest bodies)
  `(when (cygwinp)
     ,@bodies))

(defmacro when-carbon (&rest bodies)
  `(when (carbon-emacs-p)
     ,@bodies))

(defmacro when-cocoa (&rest bodies)
  `(when (cocoa-emacs-p)
     ,@bodies))

(defmacro when-emacs22 (&rest bodies)
  `(when (emacs22p)
     ,@bodies))

(defmacro when-emacs23 (&rest bodies)
  `(when (emacs23p)
     ,@bodies))

(defmacro unless-gui (&rest bodies)
  `(unless window-system
     ,@bodies))

(defmacro unless-meadow (&rest bodies)
  `(unless (meadowp)
     ,@bodies))

(defmacro unless-cygwin (&rest bodies)
  `(unless (cygwinp)
     ,@bodies))

(defmacro unless-carbon (&rest bodies)
  `(unless (carbon-emacs-p)
     ,@bodies))

(defmacro unless-cocoa (&rest bodies)
  `(unless (cocoa-emacs-p)
     ,@bodies))

(defmacro unless-emacs22 (&rest bodies)
  `(unless (emacs22p)
     ,@bodies))

(defmacro unless-emacs23 (&rest bodies)
  `(unless (emacs23p)
     ,@bodies))
