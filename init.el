;; emacs-settings bootstrap emacs lisp

(defun load-emacs-settings (dir)
  (let ((installed-file (format "%s/emacs.d/installed")))
    (let ((f (find-file-noselect installed-file)))
      (let ((sexp (read f)))
        (let ((bootstrap-library
               (remove-if #'(lambda (x)
                              (destructuring-bind
                                  (package . type)
                                  x
                                (eq type 'library)))
                          sexp)))
          (dolist (l bootstrap-library)
            (load-all-elisp-file l))))
      (kill-buffer f))))
    


