;; -*- mode: emacs-lisp -*-
;; emacs-settings is a utility script to setup your
;; emacs running on emacs batch mode.
;;
;; written by R.Ueda (garaemon)

(require 'cl)                           ;use common lisp like mode

;; globals. these globals are changed in `setup' function
(defvar *emacs-settings-source-dir* "~/prog/emacs-settings/sources")
(defvar *emacs-settings-site-dir* "~/prog/emacs-settings/emacs.d")
(defvar *emacs-settings-debug-p* nil)

(defun setup (basedir debug-mode)
  "this function is always called by emacs-settings shell script."
  ;; setup
  (setq *emacs-settings-debug-p* (string= debug-mode "true"))
  (setq *emacs-setting-source-dir* (format "%s/sources" basedir))
  (setq *emacs-setting-site-dir* (format "%s/emacs.d" basedir))
  )

(defmacro* with-open-file ((f fname) &rest bodies)
  ;; common lisp like with-open-file macro
  `(let ((,f (find-file-noselect ,fname)))
     (prog1 (progn ,@bodies)
       (kill-buffer ,f))))

(defun format* (str &rest args)
  "this function works as common lisp's (format t ...)"
  (format-string-to-stdout (apply #'format str args)))

(defun format-string-to-stdout (str)
  "print str to standard-output"
  (let ((len (length str)))
    (dotimes (i len)
      (write-char (aref str i))))
  ;; returns nil like common lisp format function
  nil)

(defun wget (url dir)
  "download `url' to `dir' using wget command."
  (format* "now downloading %s to %s...\n" url dir)
  (call-process "wget" nil t t url "-P" dir))

;; in emacs-settings.el, all the packages are represented
;; in associated list.
(defun make-package-alist (list)
  "`list' is an element of a source file. This function make an
associated list from the element list. If you want to access
the associated list, use package-accessors defined by `defpackage-accessor'."
  (destructuring-bind (name type sources &optional doc depend install)
      list
    (list (cons :name name)
          (cons :type type)
          (cons :sources
                (cond ((null sources) nil)
                      ((atom sources) (list sources))
                      (t sources)))
          (cons :documentation doc)
          (cons :depend depend)
          (cons :install install))))

(defmacro defpackage-accessor (funcname slot)
  ;; define an accessor function to package slot
  `(defun ,funcname (a) (assoc-ref ,slot a)))

(defpackage-accessor name-of :name)
(defpackage-accessor type-of* :type)    ;type-of collides with embedded function
(defpackage-accessor sources-of :sources)
(defpackage-accessor documentation-of :documentation)
(defpackage-accessor depend-of :depend)
(defpackage-accessor install-of :install)

(defun install-package-with-dependencies-from-name (name)
  "install the package whose name is `name' and the
other packages it depends on. "
  (let ((all-packages (get-all-packages)))
    (let ((pkg (find name all-packages
                     :key #'(lambda (x) (symbol->string (name-of x)))
                     :test #'string=)))
      (if pkg
          (progn
            (if *emacs-settings-debug-p*
                (format* "find package %s\n" pkg))
            (install-package-with-dependencies pkg all-packages))
        (progn
          (if *emacs-settings-debug-p*
              (format* "cannot find package %s\n" pkg)))))))

(defun install-package-with-dependencies (pkg all-packages)
  "this function install pkg and its dependent packages."
  ;; currently does not consider about dependency
  (let ((resolved-packages (list pkg)))
    (dolist (p resolved-packages)
      (install-package p))))

(defun package-directory (pkg)
  ""
  (expand-file-name (format "%s/%s" *emacs-settings-site-dir*
                            (name-of pkg))))

(defun installed-file ()
  (format "%s/installed" *emacs-settings-site-dir*))

(defun installed-package-from-installed-file ()
  (if (file-exists-p (installed-file))
      (with-open-file (f (installed-file)) (read f))))

(defun add-to-installed (pkg)
  "write package name and directory to *emacs-settings-site-dir*/installed"
  (let ((installed-file (installed-file))
        (installed (installed-package-from-installed-file)))
    (with-temp-file installed-file
      (print (cons (cons (name-of pkg) (type-of* pkg))
                   installed)
             (current-buffer)))))

(defun remove-from-installed (name)
  (let ((installed-file (format "%s/installed" *emacs-settings-site-dir*)))
    (when (file-exists-p installed-file)
      (let ((installed (installed-package-from-installed-file)))
        (with-temp-file installed-file
          (print (remove name installed :key #'car)
                 (current-buffer)))))))


(defun delete-directory-recursive (dir)
  "this function works like `rm -rf dir'"
  (let ((files (directory-files "." nil "[^\.*]")))
    (dolist (f files)
      (if (file-directory-p f)
          ;; if f is a directory, call delete-directory-recursive recursively
          (delete-directory-recursive f)
        ;; if f is not a directory, call delete-file to remove f
        (delete-file f)))
    files))

(defun uninstall-package-from-name (name)
  "This function is called from "
  (let ((installed (installed-package-from-installed-file)))
    (let ((target (find name installed :key #'car)))
      (if target
          (progn
            ;; remove the entry from emacs.d/installed
            (remove-from-installed name)
            ;; remove installed
            
            )
        ))))

(defun install-package (pkg)
  (let ((sources (sources-of pkg))
        (dir (package-directory pkg)))
    (unless (file-exists-p dir)
      (make-directory dir)
      (dolist (s sources) (%install-package s pkg))
      ;; we need to add to load path
      (add-to-installed pkg))))

(defun %install-package (source pkg)
  (cond
   ((or (stringp source) (symbolp source)) ;url
    (wget (symbol->string source) (package-directory pkg)))
   (t (error "not supported source %s" source)))
  )

(defun update-sources ()
  "Update the all of source files.
Each source file has its URL in car."
  )

(defun parse-source (fname)
  "make the package associated lists of a source file `fname'."
  (with-open-file (str f)
    (mapcar #'make-package-alist (cdr (read str)))))

(defun assoc-ref (key list)
  "take a value of `key' in an associated list `list'"
  (cdr (assoc key list)))

(defun all-source-files ()
  "Return the all of source files.
Search .el file in emacs-settings/sources directory"
  (directory-files (expand-file-name  "~/prog/emacs-settings/sources")
                   t ".*\.el"))

(defun get-all-packages ()
  (let ((source-files (all-source-files)))
    (let ((packages nil))
      (dolist (f source-files)
        (setq packages (append packages (parse-source f))))
      packages)))

(defun symbol->string (sym)
  (format "%s" sym))

(defun enumerate-packages ()
  "called in `packages' commend"
  (if *emacs-settings-debug-p* (format* "enumerating packages...\n"))
  (let ((packages (get-all-packages)))
    (dolist (pkg packages)
      (format* "%s\n" (name-of pkg))
      (format* "   %s\n" (documentation-of pkg))
    )))
