;; -*- mode: emacs-lisp -*-
;; emacs-settings is a utility script to setup your
;; emacs running on emacs batch mode.
;;
;; written by R.Ueda (garaemon)
(setq max-lisp-eval-depth 1000)
(require 'cl)                           ;use common lisp like mode

;; globals. these globals are changed in `setup' function
(defvar *emacs-settings-source-dir* "~/prog/emacs-settings/sources")
(defvar *emacs-settings-site-dir* "~/prog/emacs-settings/emacs.d")
(defvar *emacs-path* "/usr/bin/emacs")
(defvar *emacs-settings-debug-p* nil)


(defun setup (basedir debug-mode emacs-path)
  "this function is always called by emacs-settings shell script."
  ;; setup
  (setq *emacs-settings-debug-p* (string= debug-mode "true"))
  (setq *emacs-settings-source-dir* (format "%s/sources" basedir))
  (setq *emacs-settings-site-dir* (format "%s/emacs.d" basedir))
  (setq *emacs-path* emacs-path)
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
  (call-process "wget" nil t t "-N" url "-P" dir))

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
          (cons :sources sources)
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

(defun resolve-package-dependencies (pkg all-packages)
  "resoleve dependencies of `pkg' and returns the packages need to be installed"
  (%resolve-package-dependencies nil (list pkg) all-packages))

(defun %resolve-package-dependencies (already-searched targets all-packages)
  (if (null targets)
      already-searched
      (let ((target (car targets)))
        (let ((dependent-packages (depend-of target)))
          (let ((new-packages
                 (mapcan #'(lambda (x)
                             (if (member x already-searched)
                                 (progn
                                   (if *emacs-settings-debug-p*
                                       (format* "%s already enumerated\n" x))
                                   nil)
                                 (progn
                                   ;; error check, x is in all-package or not
                                   (unless (find-package x all-packages)
                                     (error "%s is not in sources file" x))
                                   (if *emacs-settings-debug-p*
                                       (format* "%s add to list\n" x))
                                   (list (find-package x all-packages)))))
                         dependent-packages)))
            (%resolve-package-dependencies
             (if (eq (type-of* target) 'virtual)
                 already-searched
               (append (list target) already-searched))
             (append (cdr targets) new-packages)
             all-packages))))))

(defun install-package-with-dependencies (pkg all-packages)
  "this function install pkg and its dependent packages."
  ;; currently does not consider about dependency
  (let ((resolved-packages (resolve-package-dependencies pkg all-packages)))
    (if *emacs-settings-debug-p*
        (format* "resolve dependencies -> %s¥n" resolved-packages))
    (dolist (p resolved-packages)
      (unless (eq (type-of* p) 'virtual)
	(install-package p)))               ;NB: rename to download
    (update-emacs-settings-site-dir *emacs-settings-site-dir*)
    (if *emacs-settings-debug-p*
        (format* "current load-path -> %s\n" load-path))
    (dolist (p resolved-packages)
      (exec-install-commands p))
    ))

(defun package-directory (pkg)
  "returns the path that pkg is installed"
  (expand-file-name (format "%s/%s" *emacs-settings-site-dir* (name-of pkg))))

(defun installed-file ()
  "returns the `installed' file path"
  (format "%s/installed" *emacs-settings-site-dir*))

(defun installed-package-from-installed-file ()
  "returns a list of the package alists which are described in
emacs.d/installed"
  (if (file-exists-p (installed-file))
      (with-open-file (f (installed-file))
        (mapcar #'(lambda (x)
                    (destructuring-bind (name . type) x
                      ;; find package from source file...
                      (find-package-alist-from-source-files name)))
                (read f)))))

(defun add-to-installed (pkg)
  "write package name and directory to *emacs-settings-site-dir*/installed"
  (let ((installed-file (installed-file))
        (installed (installed-package-from-installed-file)))
    (with-temp-file installed-file
      (print (cons (cons (name-of pkg) (type-of* pkg))
                   (mapcar #'(lambda (x)
                               (cons (name-of x) (type-of* x)))
                           installed))
             (current-buffer)))))

(defun remove-from-installed (pkg)
  "remove `pkg' from emacs.d/installed"
  (let ((installed-file (installed-file))
        (name (name-of pkg)))
    (when (file-exists-p installed-file) ;check emacs.d/installed existing
      (let ((installed (installed-package-from-installed-file)))
        (with-temp-file installed-file
          (print
           (mapcar #'(lambda (x)
                       (cons (name-of x) (type-of* x)))
                   (remove-if #'(lambda (x) (eq name (name-of x))) installed))
           (current-buffer)))))))

(defun delete-directory-recursive (dir)
  "this function works like `rm -rf dir'"
  (if *emacs-settings-debug-p* (format* "delete redursive %s \n" dir))
  (let ((files (directory-files dir t "[^\.*]")))
    (dolist (f files)
      (if (file-directory-p f)
          (progn
            ;; if f is a directory, call delete-directory-recursive recursively
            (if *emacs-settings-debug-p* (format* "%s is a directory\n" f))
            (delete-directory-recursive f))
        (progn
          ;; if f is not a directory, call delete-file to remove f
          (if *emacs-settings-debug-p* (format* "%s is a file\n" f))
          (delete-file f))))
    (delete-directory dir)
    files))

(defun find-package (name packages)
  "`packages' is a list of package alists. This function find a package alist
from `packages' whose name is `name'."
  (cond ((symbolp name)
         (find name packages :key #'name-of))
        ((stringp name)
         (find name packages :key #'name-of
               :test #'(lambda (x y) (string= x (symbol->string y)))))))

(defun uninstall-package-from-name (name)
  "This function is called by emacs-setting `uninstall' command.
`uninstall' comamnd removes the package entry from emacs.d/installed
and the directory from emacs.d/"
  (let ((installed (installed-package-from-installed-file)))
    (let ((target (find-package name installed)))
      (if target
          (progn
            (if *emacs-settings-debug-p* 
                (format* "%s is found %s\n" name target))
            ;; remove the entry from emacs.d/installed
            (remove-from-installed target)
            ;; remove directory from emacs.d/
            (delete-directory-recursive (package-directory target)))
        (error "%s is not found" name)))))

(defun exec-install-commands (pkg)
  "run install commands specified in sources file"
  (let ((installs (install-of pkg)))
    (dolist (install installs)
      (run-install-command install pkg))
    ))

(defun run-install-command (command pkg)
  "execute a install command `command'."
  (cond ((stringp command) (run-install-shell-comamnd command pkg))
        ((keywordp command) (run-install-keyword-command command pkg))
        (t (eval command))))            ;just eval it!

(defun update-emacs-load-path ()
  (let ((path (apply #'concatenate 'string (mapcan #'(lambda (x) (list x ":"))
                                                   load-path))))
    (setenv "EMACSLOADPATH" (substring path 0 (1- (length path))))
    ))

(defun run-install-shell-comamnd (command pkg)
  "execute a shell command `command'"
  (update-emacs-load-path)
  (let ((default-directory (package-directory pkg)))
    (let ((%command (replace-regexp-in-string
                     "$EMACS" *emacs-path* command t))) 
      (if *emacs-settings-debug-p* (format* "now exec '%s'\n" %command))
      (if (= (shell-command
              (format "cd %s && %s" default-directory %command)) 0)
          t
          (error "error has occurred")))))

(defun search-all-elisp-files (root)
  (let ((all (directory-files root t "[^\.*]")))
    (mapcan #'(lambda (x)
                (cond ((file-directory-p x)
                       (search-all-elisp-files x))
                      ((string= (file-name-extension x) "el")
                       (list x))
                      (t nil)))
            all)))

(defun run-install-keyword-command (command pkg)
  (case command
    (:byte-compile
     (let ((default-directory (package-directory pkg)))
       ;; compile the all files which has .el in suffix
       (let ((files (search-all-elisp-files (package-directory pkg))))
	 (dolist (f files)
	   (unless (ignore-errors (byte-compile-file f t))
	     (format* "Error: compiling %s is failed\n" f))))))
    (t (error "%s is not supported" command))))

(defun update-emacs-settings-site-dir (dir)
  "add `dir' and subdirectories of it to load-path"
  (let ((dirs (remove-if-not #'file-directory-p
                             (directory-files dir t "[^\.*]"))))
    (dolist (d dirs)
      (update-emacs-settings-site-dir d))
    (setq load-path (cons dir load-path))))

(defun install-package (pkg)
  "install a package `pkg'. First of all, make a directory
whose name is (name-of pkg), "
  (let ((sources (sources-of pkg))
        (dir (package-directory pkg)))
    (unless (file-exists-p dir)
      (make-directory dir)              ;first of all, make directory
      (%install-package sources pkg)    ;download the source codes
      
      ;; (exec-install-commands pkg)
      ;; we need to add to load path
      (add-to-installed pkg))))         ;add a package to emacs.d/installed

(defun tar-xvjf (tar-path dir)
  "call tar -xvjf `tar-path' -C `dir'"
  (format* "now expanding %s to %s...\n" tar-path dir)
  (call-process "tar" nil t t "xvjf" tar-path "-C" dir))

(defun tar-xvzf (tar-path dir)
  "call tar -xvzf `tar-path' -C `dir'"
  (format* "now expanding %s to %s...\n" tar-path dir)
  (call-process "tar" nil t t "xvzf" tar-path "-C" dir))

(defun wget-and-expand-tar-ball (url fname pkg)
  (wget url (package-directory pkg))
  (if (string= (file-name-extension url) "bz2")
      (tar-xvjf (format "%s/%s" (package-directory pkg) fname)
                (package-directory pkg))
      (tar-xvzf (format "%s/%s" (package-directory pkg) fname)
                (package-directory pkg))))

(defun cvs-checkout (cvs-root module-name pkg)
  "call cvs -d `cvs-root' co -d `package-directory' `module-name'"
  (if *emacs-settings-debug-p*
      (format* "cvs -d %s co -d %s %s\n" cvs-root
               (absolute-path->relative-path (package-directory pkg))
               module-name))
  (call-process "cvs" nil t t "-d" cvs-root
                "co" "-d"
                (absolute-path->relative-path (package-directory pkg))
                module-name))

(defun svn-checkout (svn-path pkg)
  "call svn co `svn-path' `package-directory'"
  (call-process "svn" nil t t "co" svn-path (package-directory pkg)))

(defun git-clone (git-repo pkg)
  "call git clone `git-repo' `package-directory'"
  (call-process "git" nil t t "clone" git-repo (package-directory pkg)))

;; install-xxx takes source list and package alist
(defun install-tar-ball (source pkg)
  (destructuring-bind (tar-ball url &optional file-name) source
    (let* ((%url (symbol->string url))
           (%file-name (if file-name
                           ;; if file-name is specified
                           (symbol->string file-name)
                         ;; unless file-name is specified, use basename of url
                         (file-name-nondirectory %url))))
      (wget-and-expand-tar-ball %url %file-name pkg))))

(defun install-cvs (source pkg)
  (destructuring-bind (cvs cvs-root module) source
    (let ((%cvs-root (symbol->string cvs-root))
          (%module (symbol->string module)))
      (cvs-checkout %cvs-root %module pkg))))

(defun install-svn (source pkg)
  (destructuring-bind (svn svn-path) source
    (let ((%svn-path (symbol->string svn-path)))
      (svn-checkout %svn-path pkg))))

(defun install-git (source pkg)
  (destructuring-bind (git git-repo) source
    (let ((%git-repo (symbol->string git-repo)))
      (git-clone %git-repo pkg))))

(defun %install-package (source pkg)
  (if *emacs-settings-debug-p* (format* "parsing %s\n" source))
  (cond
   ((or (stringp source) (symbolp source)) ;url, http://.*\.el
    (wget (symbol->string source) (package-directory pkg)))
   ((listp source)
    (case (car source)
      (tar-ball (install-tar-ball source pkg))
      (cvs (install-cvs source pkg))
      (svn (install-svn source pkg))
      (git (install-git source pkg))
      (t ;; probably list of <source>
       (dolist (s source)
         (%install-package s pkg)
         ))))
   (t (error "not supported source %s" source))))

(defun update-sources ()
  "Update the all of source files. Each source file has its URL in car."
  (error "not supported"))

(defun parse-source (fname)
  "make the package associated lists of a source file `fname'."
  (with-open-file (str fname)
    (mapcar #'make-package-alist (cdr (read str)))))

(defun all-source-files ()
  "Return the all of source files.
Search .el file in emacs-settings/sources directory"
  (directory-files (expand-file-name  *emacs-settings-source-dir*)
                   t ".*\.el$"))

(defun get-all-packages ()
  "read all of the source files and return a list of package-alist"
  (let ((source-files (all-source-files))) ;all file name
    (mapcan #'parse-source source-files)))

(defun find-package-alist-from-source-files (name)
  "find a package alist whose name is `name' from source files"
  (let ((all-packages (get-all-packages)))
    (find name all-packages :key #'name-of)))

;; utlity
(defun assoc-ref (key list)
  "take a value of `key' in an associated list `list'"
  (cdr (assoc key list)))

(defun symbol->string (sym)
  "convert symbol to string."
  (symbol-name sym))

(defun absolute-path->relative-path (abs-path)
  (file-relative-name abs-path (expand-file-name default-directory)))

(defun enumerate-packages (command)
  "called in `packages' commend. print out name and description of packages
to standard out."
  (if *emacs-settings-debug-p* (format* "enumerating packages...\n"))
  (let ((packages
         (cond ((string= command "installed")
                (installed-package-from-installed-file))
               (t (get-all-packages)))))
    (dolist (pkg packages)
      (format* "%s\n" (name-of pkg))
      (format* "   %s\n" (documentation-of pkg)))
    nil))


