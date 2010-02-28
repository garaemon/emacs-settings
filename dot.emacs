;; -*- mode: emacs-lisp -*-

;;========================================
;;.emacs
;;   written by R.Ueda (garaemon)
;;========================================

(require 'cl)
;; parameters
(defvar *save-window-size-p* nil)
(defvar *eshell-setup-p* nil)
(defvar *migemo-setup-p* t)
(defvar *pretty-pambda-p* nil)

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

;;============================================
;;             for Meadow
;;============================================
(when-meadow
 (setq inhibit-default-init t))

;; ordinary settings...
;;============================================
;;              display-time
;;============================================
(display-time)

;;============================================
;;             C-h -> BackSpace
;;============================================
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-h" 'help-for-help)

;;===========================================
;;             M-g -> goto-line
;;===========================================
(global-set-key "\M-g" 'goto-line)

;;============================================
;;                Japanese Settings
;;===========================================
(defun setup-coding-system (code)
  (set-terminal-coding-system code)
  (set-buffer-file-coding-system code)
  (set-buffer-file-coding-system code))
(setup-coding-system 'utf-8)

(global-unset-key "\C-\\")
(when-carbon
 (mac-input-method-mode 1))

;; font setting
;;==============================================
;; font setting for meadow using ARISAKA and monaco
;;==============================================
(when-meadow
 (let ((make-spec
	(function
	 (lambda (size charset fontname &optional windows-charset)
	   (setq size (- size))
	   (if (not windows-charset)
	       (setq windows-charset
		     (cadr (assq charset
				 mw32-charset-windows-font-info-alist))))
	   `(((:char-spec ,charset :height any)
	      strict
	      (w32-logfont ,fontname 0 ,size 400 0
			   nil nil nil ,windows-charset 1 3 0))
	     ((:char-spec ,charset :height any :weight bold)
	      strict
	      (w32-logfont ,fontname 0 ,size 700 0
			   nil nil nil ,windows-charset 1 3 0)
	      ((spacing . -1)))
	     ((:char-spec ,charset :height any :slant italic)
	      strict
	      (w32-logfont ,fontname 0 ,size 400 0
			   t nil nil ,windows-charset 1 3 0))
	     ((:char-spec ,charset :height any :weight bold :slant italic)
	      strict
	      (w32-logfont ,fontname 0 ,size 700 0
			   t nil nil ,windows-charset 1 3 0)
	      ((spacing . -1)))))))
       (make-spec-list
	(function
	 (lambda (size params-list)
           (list
	    (cons 'spec
		  (apply 'append
			 (mapcar (lambda (params)
				   (apply make-spec (cons size params)))
				 params-list)))))))
       (define-fontset
	 (function
	  (lambda (fontname size fontset-list)
	    (let ((spec (funcall make-spec-list size fontset-list)))
	      (if (w32-list-fonts fontname)
		  (w32-change-font fontname spec)
		(w32-add-font fontname spec))))))
       (arisaka-fontset-list
	'((ascii "monaco")
	  (katakana-jisx0201 "ARISAKA-等幅")
	  (japanese-jisx0208 "ARISAKA-等幅")))
       )
   (funcall define-fontset "Arisaka 10" 10 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 12" 12 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 14" 14 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 16" 16 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 18" 18 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 20" 20 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 22" 22 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 24" 24 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 36" 36 arisaka-fontset-list)
   (funcall define-fontset "Arisaka 48" 48 arisaka-fontset-list)
   )
 ;; initial frame setting
 (setq default-frame-alist
       (append (list '(font . "Arisaka 12")
		     '(ime-font . (w32-logfont "Arisaka"
					       0 16 400 0 nil nil nil
					       128 1 3 49))) ; only TrueType
	       default-frame-alist))
 )

;;==============================================
;;            font setting for Carbon Emacs
;;==============================================
(when-carbon
 (require 'carbon-font)
 (setq mac-allow-anti-aliasing t)
 (fixed-width-set-fontset "hiramaru" 12))

;;==============================================
;;            font設定 for Cocoa Emacs
;;==============================================
(when-cocoa
 ;; フォントフェースの設定
 ;; see http://d.hatena.ne.jp/kazu-yamamoto/20090122/1232589385
 (set-face-attribute 'default nil
		     :family "monaco"
		     :height 120)
 ;; 日本語フォント: ヒラギノ丸ゴシック 
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0208
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'katakana-jisx0201
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0212
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 )

;;==============================================
;;            emacs23 mac keybind
;;==============================================
(when-cocoa
 (setq ns-command-modifier (quote meta))
 (setq ns-alternate-modifier (quote super)))

;;==============================================
;;            meadowでマウスカーソルを消す
;;==============================================
(when-meadow
 (setq w32-hide-mouse-on-key t))

;;============================================
;;           起動時の画面をなくす
;;============================================
(setq inhibit-startup-message t)

;;============================================
;;      ~バックアップファイルを作成しない
;;============================================
(setq make-backup-files nil)

;;=============================================
;;       from "yes or no" to "y or n"
;;=============================================
(fset 'yes-or-no-p 'y-or-n-p)

;;=============================================
;;             画像を表示する
;;=============================================
(auto-image-file-mode)

;;=============================================
;;         補間時に大文字小文字を無視
;;=============================================
(setq completion-ignore-case t)

;;=============================================
;;          regionを反転で強調表示
;;=============================================
(setq-default transient-mark-mode t)

;;=============================================
;;           スクロールバーを消去
;;=============================================
(unless-cygwin
 (scroll-bar-mode -1))

;;=============================================
;;            改行時にインデント
;;=============================================
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;;=============================================
;;      comment-region/uncommentregion
;;=============================================
;; C-x;でコメント, C-x:でアンコメント
(global-set-key "\C-x;" 'comment-region)
(fset 'uncomment-region "\C-u\C-[xcomment-region\C-m")
(global-set-key "\C-x:" 'uncomment-region)

;;=============================================
;;                 行数の表示
;;=============================================
(line-number-mode 1)
(column-number-mode 1)
(when-emacs23
 (global-linum-mode t))

;;=============================================
;;                   音無し
;;=============================================
(setq visible-bell t)
(put 'upcase-region 'disabled nil)

;;=============================================
;;              括弧の対応付け
;;=============================================
(show-paren-mode t)
(setq show-paren-style 'mixed)

;;=============================================
;;                一行スクロール
;; M-n, M-pで一行だけスクロールする.
;; カーソル位置はそのまま.
;;=============================================
(global-unset-key "\M-p")
(global-unset-key "\M-n")

(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(global-set-key "\M-p" 'scroll-up-in-place)
(global-set-key [M-up] 'scroll-up-in-place)
(global-set-key "\M-n" 'scroll-down-in-place)
(global-set-key [M-down] 'scroll-down-in-place)

;;=============================================
;;              save window size
;; 終了時の画面サイズを~/.framesize.elに記録
;; しておき, 起動時にそのサイズにそろえる.
;;=============================================
(when *save-window-size-p*
  (defun my-window-size-save ()
    (let* ((rlist (frame-parameters (selected-frame)))
	   (ilist initial-frame-alist)
	   (nCHeight (frame-height))
	   (nCWidth (frame-width))
	   (tMargin (if (integerp (cdr (assoc 'top rlist)))
			(cdr (assoc 'top rlist)) 0))
	   (lMargin (if (integerp (cdr (assoc 'left rlist)))
			(cdr (assoc 'left rlist)) 0))
	   buf
	   (file "~/.framesize.el"))
      (if (get-file-buffer (expand-file-name file))
	  (setq buf (get-file-buffer (expand-file-name file)))
	(setq buf (find-file-noselect file)))
      (set-buffer buf)
      (erase-buffer)
      (insert (concat
	       "(delete 'width initial-frame-alist)\n"
	       "(delete 'height initial-frame-alist)\n"
	       "(delete 'top initial-frame-alist)\n"
	       "(delete 'left initial-frame-alist)\n"
	       "(setq initial-frame-alist (append (list\n"
	       "'(width . " (int-to-string nCWidth) ")\n"
	       "'(height . " (int-to-string nCHeight) ")\n"
	       "'(top . " (int-to-string tMargin) ")\n"
	       "'(left . " (int-to-string lMargin) "))\n"
	       "initial-frame-alist))\n"
	       ))
      (save-buffer)
      ))

  (defun my-window-size-load ()
    (let* ((file "~/.framesize.el"))
      (if (file-exists-p file)
	  (load file))))

  (my-window-size-load)

  ;; Call the function above at C-x C-c.
  (defadvice save-buffers-kill-emacs
    (before save-frame-size activate)
    (my-window-size-save))
  )
;;=============================================
;;                 透明化
;;=============================================
(cond ((carbon-emacs-p)
       (setq default-frame-alist
	     (append (list '(alpha . (90 90))) default-frame-alist)))
      ((meadowp)
       (modify-all-frames-parameters
        (list (cons 'alpha  '(nil nil nil nil)))))
      )

;;=============================================
;;                  色の設定
;;=============================================
(when-gui
 ;; 文字の色を設定します。
 (set-foreground-color "white")
 ;; 背景色を設定します。
 (set-background-color "gray15")
 ;; モードラインの文字の色を設定します。
 (set-face-foreground 'modeline "white")
 ;; モードラインの背景色を設定します。
 (set-face-background 'modeline "gray40")
 ;; カーソルの色を設定します。
 (set-cursor-color "yellow")
 ;; マウスポインタの色を設定します。
 (set-mouse-color  "yellow")
 )

;;============================================
;;         インデントスタイル
;;============================================
(setq c-default-style "stroustrup")

;;============================================
;;    インデント時のタブをソフトタブに
;;============================================
(defmacro soft-tab-mode (mode)
  `(add-hook ,mode (lambda () (setq indent-tabs-mode nil))))

(dolist (l '(c-mode-common-hook
             lisp-mode-hook
             euslisp-mode-hook
             emacs-lisp-mode-hook
             slime-mode-hook
             shell-script-mode
             scheme-mode-hook
             html-mode-hook
             yatex-mode-hook
             python-mode-hook))
  (soft-tab-mode l))

;;============================================
;;    長い行は折り返して表示する
;;============================================
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;;=============================================
;; lambdaの表示
;;=============================================
(defun set-pretty-patterns (patterns)
  (loop for (glyph . pairs) in patterns do
	(loop for (regexp . major-modes) in pairs do
	      (loop for major-mode in major-modes do
		    (let ((major-mode (intern
				       (concat (symbol-name major-mode)
					       "-mode")))
			  (n (if (string-match "\\\\([^?]" regexp) 1 0)))
		      (font-lock-add-keywords
		       major-mode
		       `((,regexp (0 (prog1 ()
				       (compose-region (match-beginning ,n)
						       (match-end ,n)
						       ,glyph)))))))))))
(when *pretty-pambda-p*
  (set-pretty-patterns
   '((?λ ("\\<lambda\\>" lisp lisp-interaction emacs-lisp scheme)))))

;; 各種メジャーモードの設定
;;=============================================
;;                Dabbrev
;; キーバインドはC-o
;;=============================================
(global-set-key "\C-o" 'dabbrev-expand)

;;=============================================
;;                shell-mode
;;=============================================
(cond
 ((meadowp)
  (setq shell-file-name "c:/cygwin/bin/zsh.exe")
  (modify-coding-system-alist 'process ".*sh\\.exe" 'undecided-unix))
 ((cygwinp)
  (setq shell-file-name "/bin/zsh")))
(setq explicit-shell-file-name shell-file-name)
(setq shell-command-option "-c")
(setq system-uses-terminfo nil)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
;; C-lでシェルをクリアするようにする
(defun shell-clear()
  "In shell-mode,clear all display and move cursor top of the buffer."
  (interactive)
  (recenter 0))
(eval-after-load "shell"
  '(define-key shell-mode-map "\C-l" 'shell-clear))

;;shell-modeでlsしたとき色を表示
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;=============================================
;;                eshell-mode
;; eshellはelispで書かれたshell.
;;=============================================
(when *eshell-setup-p*
  (modify-coding-system-alist 'process "\\*eshell\\*" 'undecided-unix)
  (require 'eshell)
  (defun eshell-scroll-to-bottom (window display-start)
    (if (and window (window-live-p window))
	(let ((resize-mini-windows nil))
	  (save-selected-window
	    (select-window window)
	    (save-restriction
	      (widen)
	      (when (> (point) eshell-last-output-start)
		;; we're editing a line. Scroll.
		(save-excursion
		  (recenter -1)
		  (sit-for 0))))))))
  (defun eshell-add-scroll-to-bottom ()
    (interactive)
    (make-local-hook 'window-scroll-functions)
    (add-hook 'window-scroll-functions 'eshell-scroll-to-bottom nil t))

  (add-hook 'eshell-mode-hook 'eshell-add-scroll-to-bottom)
  ;; ;;clear
  (defun eshell/clear ()
    "Clear the current buffer, leaving one prompt at the top."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(eshell-ask-to-save-history (quote always))
   '(eshell-history-size 1000)
   '(eshell-ls-dired-initial-args (quote ("-h")))
   '(eshell-ls-exclude-regexp "~\\'")
   '(eshell-ls-use-in-dired t nil (em-ls))
   '(eshell-modules-list (quote (eshell-alias
				 eshell-basic
				 eshell-cmpl
				 eshell-dirs
				 eshell-glob
				 eshell-ls
				 eshell-pred
				 eshell-prompt
				 eshell-rebind
				 eshell-script
				 eshell-smart
				 eshell-term
				 eshell-unix
				 eshell-xtra)))
   '(eshell-prefer-o-shell t nil (eshell))
   '(eshell-stringify-t nil)
   '(eshell-term-name "ansi")
   '(eshell-visual-commands (quote ("vi" "top" "screen" "less" "lynx" "ssh" "rlogin" "telnet"))))

  ;; ;;current directory
  (defun eshell-cd-default-directory ()
    (interactive)
    (let ((dir default-directory))
      (eshell)
      (cd dir)
      (eshell-interactive-print (concat "cd " dir "\n"))
      (eshell-emit-prompt)))

  ;; ;;prompt
  (setq eshell-prompt-function
	(lambda ()
	  (concat "[Yes,Master?] "
		  (eshell/pwd)
		  (if (= (user-uid) 0) "]# " "]$ ")
		  )))
  (setq eshell-prompt-regexp "^[^#$]*[$#] ")

  ;;less
  (defun eshell/less (&rest args)
    "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	  (let* ((line (string-to-number (match-string 1 (pop args))))
		 (file (pop args)))
	    (view-file file)
	    (goto-line line))
	(view-file (pop args)))))
  )

;;=============================================
;;                Python mode
;;=============================================
(autoload 'python-mode "python-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'python-mode-hook
          '(lambda()
             (require 'pycomplete)))
             
;;=============================================
;;               lisp-mode
;;=============================================
(font-lock-add-keywords 'lisp-mode
			(list
			 ;; *hoge*に色を付ける
			 (list "\\(\\*\\w\+\\*\\)\\>"
			       '(1 font-lock-constant-face nil t))
			 ;; +hoge+に色を付ける
			 (list "\\(\\+\\w\+\\+\\)\\>"
			       '(1 font-lock-constant-face nil t))
			 ;; <hoge>に色を付ける
			 (list "\\(<\\w\+>\\)\\>"
			       '(1 font-lock-constant-face nil t))
			 ;; defclass*に色を付ける
			 (list "\\(defclass\\*\\)"
			       '(1 font-lock-keyword-face nil t))
			 ;; defvirtualmethodに色を付ける
			 ))
(defun cl-indent (sym indent)
  (put sym 'common-lisp-indent-function
       (if (symbolp indent)
	   (get indent 'common-lisp-indent-function)
	 indent)))
(cl-indent 'iterate 'let)
(cl-indent 'collect 'progn)

;;=============================================
;;               euslisp-mode
;;=============================================
(setq auto-mode-alist (cons (cons "\\.l$" 'euslisp-mode) auto-mode-alist))
 (defun lisp-other-window ()
   "Run lisp on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*inferior-lisp*"))
  (run-lisp inferior-euslisp-program))
 (set-variable 'inferior-euslisp-program "jskrbeusgl")
 (global-set-key "\C-cE" 'lisp-other-window)

;;=============================================
;;                Haskell mode
;;=============================================
(setq auto-mode-alist
      (append auto-mode-alist
	      '(("\\.[hg]s$"  . haskell-mode)
		("\\.hi$"     . haskell-mode)
		("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(setq haskell-literate-default 'latex)
(setq haskell-doc-idle-delay 0)

;;=============================================
;;                   Migemo
;;=============================================
(when *migemo-setup-p*
  (cond
   ((meadowp)
    (setq migemo-directory "c:/cygwin/usr/local/share/migemo")
    (load "migemo"))
   ((cygwinp)
    (setq migemo-directory "/usr/local/share/migemo")
    (load "migemo"))
   ((or (cocoa-emacs-p) (carbon-emacs-p))
    (setq load-path (cons "~/elisp/migemo" load-path))
    (load "migemo.el")
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs" "-i" "\a"))
    (setq migemo-dictionary "/usr/local/share/migemo/euc-jp/migemo-dict")
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil))
   (t					;Linux
    ;;(setq migemo-directory "/usr/local/share/migemo")
    (setq migemo-directory "/usr/share/migemo")
    (load "migemo")
    (migemo-init))
   )
  )

;;=============================================
;;                 .h->c++-mode
;;=============================================
;; (setq auto-mode-alist
;;       (cons (cons "\\.h$" 'c++-mode) auto-mode-alist))

;;=============================================
;;                  navi2ch
;;=============================================
(setq load-path (cons "~/elisp/navi2ch" load-path))
(setq navi2ch-article-auto-range nil)
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
(setq navi2ch-list-bbstable-url
      "http://azlucky.s25.xrea.com/2chboard/bbsmenu2.html")

;;=============================================
;;                cygwin-mount
;;=============================================
(when (eq window-system 'windows-nt)
  (require 'cygwin-mount-mw32)
  (cygwin-mount-activate))

;;=============================================
;; anything
;;=============================================
(require 'anything-config)
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-file-name-history
            anything-c-source-imenu
            anything-c-source-recentf
            ;;anything-c-source-man-pages
            ;;anything-c-source-info-pages
            anything-c-source-calculation-result
            anything-c-source-kill-ring
            ;;anything-c-source-bookmarks
            anything-c-source-locate))

(global-set-key "\C-xb" 'anything)
(global-set-key "\M-y" 'anything-show-kill-ring)
;;(global-set-key [?\C-;] 'iswitchb-buffer)
(anything-iswitchb-setup)

(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

;;=============================================
;; anything-grep
;; http://d.hatena.ne.jp/rubikitch/20090106/anythinggrep
;;=============================================
(require 'anything-grep)
(setq anything-grep-alist
      ;; 全バッファのファイル名においてegrepをかける。moccurの代わり。
      '(("buffers" ("egrep -Hin %s $buffers" "/"))
	;; ~/memo 以下から再帰的にegrepをかける。不要なファイルは除かれる。
	("memo" ("ack-grep -af | xargs egrep -Hin %s" "~/memo"))
	;; ~/doc/postgresql-74 から *.txt に対してegrepをかける。
	("PostgreSQL" ("egrep -Hin %s *.txt" "~/doc/postgresql-74/"))
	;; ~/prog以下のファイルをまとめて検索する。
	("~/prog"
	 ("ack-grep -af | xargs egrep -Hin %s" "~/prog"))
	))
;;
(global-set-key "\C-c;" 'anything-grep)
(global-set-key "\C-c:" 'anything-grep-by-name)

;;=============================================
;;                imenu
;;=============================================
(global-set-key "\C-cg" 'imenu)
;; モードラインに編集の関数名を表示
(require 'which-func)
(setq which-func-modes
      (append which-func-modes
	      '(lisp-mode slime-mode)))
(which-func-mode t)

;;=============================================
;;                iswitch
;;=============================================
(iswitchb-mode 1)

;;=============================================
;;               nxml-mode
;; xmlの表示をいい感じにする.
;;=============================================
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|kml\\|gpx\\)\\'" . nxml-mode)
	    auto-mode-alist))

;;=============================================
;;                 wanderlust
;; emacsのmailer.
;; IMAPが使えるのでGmailジャンキーにもってこい.
;;=============================================
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(setq auto-mode-alist
      (cons (cons ".wl$" 'emacs-lisp-mode) auto-mode-alist))

;;=============================================
;;                 thumbs
;; ディレクトリ内にある画像のサムネイルを表示する.
;; M-x thumbsで起動
;;=============================================
(setq thumbs-thumbsdir
      (expand-file-name "~/.emacs-thumbs"))
(setq thumbs-temp-dir "/tmp")

;;==============================================
;;                 keisen-mule
;; カーソル移動およびマウス移動で表を書く.
;; M-x keisen-modeで起動.
;;==============================================
(when-gui
 (autoload 'keisen-mode "keisen-mouse" "MULE 版罫線モード + マウス" t)
 (autoload 'keisen-mode "keisen-mule" "MULE 版罫線モード" t))

;;============================================
;;         scheme-mode for gauche
;;============================================
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "gosh")
(require 'cmuscheme)
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)
(setq gosh-program-name "/usr/bin/env gosh -i")
(setq scheme-program-name "gosh -i")

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(put 'if 'scheme-indent-function 2)
(put 'dotimes 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'instance 'scheme-indent-function 1)
(put 'set! 'scheme-indent-function 1)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'defun 'scheme-indent-function 2)
(put 'defclass 'scheme-indent-function 2)
(put 'defmethod 'scheme-indent-function 2)
(put 'define-method* 'scheme-indent-function 2)
(put 'define-class* 'scheme-indent-function 2)
(put 'define-function* 'scheme-indent-function 1)
(put 'let-keywords 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-optionals 'scheme-indent-function 2)
(put 'let-values 'scheme-indent-function 2)
(put 'receive 'scheme-indent-function 1)
(put 'mutex-block 'scheme-indent-function 2)
(put 'unless 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'defmethod 'scheme-indent-function 1)

;;font-lock
(font-lock-add-keywords 'scheme-mode
                        (list
                         (list (concat
                                "(" (regexp-opt '("use") t) "\\>") '(1 font-lock-keyword-face nil t))
                         (list "\\(self\\)\\>" '(1 font-lock-constant-face nil t))
                         (list "\\(\\*\\w\+\\*\\)\\>" '(1 font-lock-constant-face nil t))
                         (list "\\(#\\(\\+\\|\\-\\)\.\*\\)" '(1 font-lock-variable-name-face))
                         (cons "\\(dotimes\\|unless\\|when\\|dolist\\|while\\)\\>" 1)
                         (cons "\\(let-\\(keywords\\|optionals\\|values\\|keywords\\*\\|optionals\\*\\|values\\*\\)\\)\\>" 1)
                         (list "\\(warn\\)\\>" '(1 font-lock-warning-face))
                         (list "\\(#t\\|#f\\)\\>" '(1 font-lock-constant-face))
                         (cons "\\(defclass\\|defmethod\\)\\>" 1)
			 (cons "\\(define-\\(function\\*\\|class\\*\\|method\\*\\)\\)\\>" 1)
                         )
                        )

;;============================================
;;              speedbar
;; 別ウィンドウでディレクトリツリーなどが表示される.
;; F4にバインド.
;;============================================
(defun my-speedbar-expand-line ()
  (interactive)
  (if (= (point-max) (progn (speedbar-expand-line) (point-max)))
      (save-current-buffer
        (speedbar-edit-line))))

(when (locate-library "speedbar")
  (require 'speedbar)
  ;; "a" で無視ファイル表示/非表示のトグル
  (define-key speedbar-file-key-map "a" 'speedbar-toggle-show-all-files)
  ;; ← や → でもディレクトリを開閉 ;;デフォルト: "=" "+", "-"
  (define-key speedbar-file-key-map [right] 'my-speedbar-expand-line)
  (define-key speedbar-file-key-map "\C-f" 'my-speedbar-expand-line)
  (define-key speedbar-file-key-map [left] 'speedbar-contract-line)
  (define-key speedbar-file-key-map "\C-b" 'speedbar-contract-line)
  ;; BS でも上位ディレクトリへ ;;デフォルト: "U"
  (define-key speedbar-file-key-map [backspace] 'speedbar-up-directory)
  (define-key speedbar-file-key-map "\C-h" 'speedbar-up-directory)
  ;; 起動位置を直接指定する
  (setq speedbar-frame-parameters
        (append (list '(top . 40)

                      '(left . 780)
                      '(width . 25))
                speedbar-frame-parameters))
  ;; Speedbar で表示するファイルタイプ
  (setq speedbar-supported-extension-expressions
        (append '(".el" ".[ch]\\(pp\\|\\+\\+\\)?"
		  ".java"
		  ".tex\\(i\\(nfo\\)?\\)?"
                  ".s?html?" ".xml" ".dtd" ".css" ".js" 
		  ".l" ".lisp" ".asd"
		  ".c" ".h" ".cpp"
                  ".gif" ".jpe?g" ".png")))
  ) ;; end of speedbar

;; F4 で Speedbar
(global-set-key [f4] 'speedbar-get-focus)

;;============================================
;;               clmemo
;; ChangeLog Memo
;; http://pop-club.hp.infoseek.co.jp/emacs/clmemo.html
;;
;; M-x clmemoで起動
;;============================================
(setq load-path (cons "~/elisp/clmemo" load-path))
(autoload 'clmemo "clmemo" "ChangeLog memo mode." t)
;; あなたの ChangeLog メモファイルへのパス
(setq clmemo-file-name "~/Documents/changelog/ChangeLog")
;; 好きなキーへバインド
(global-set-key "\C-xM" 'clmemo)
;; タイトルの補完
(setq clmemo-title-list '("idea" "bookmark" "Emacs" "EusLisp" "Research"))

;;============================================
;;               auto-insert
;; 新規ファイル作成時に, 自動でテンプレートを
;; 挿入する.
;;============================================
(require 'autoinsert)
;; テンプレートのディレクトリ
(setq auto-insert-directory "~/elisp/templates/")
;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(("\\.cpp$" . ["template.cpp" my-template])
	       ("\\.c$" .   ["template.c" my-template])
	       ("\\.h$"   . ["template.h" my-template])
	       ("\\.lisp$"   . ["template.lisp" my-template])
	       ("\\.muse$" . ["template.muse" my-template]))
	     auto-insert-alist))
(require 'cl)
;; ここが腕の見せ所
(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"    . (lambda () (format "__%s_H__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
	    (progn
	      (goto-char (point-min))
	      (replace-string (car c) (funcall (cdr c)) nil)))
	template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;;============================================
;;               anthy
;; cygwinとlinuxではIMEにanthyを利用.
;;============================================
(when (or (eq system-type 'cygwin)
	  (eq system-type 'gnu/linux))
  (setq load-path (append '("/usr/share/emacs/site-lisp/anthy/")
			  load-path))
  (load-library "anthy")
  (global-unset-key "\C-\\")
  (setq default-input-method "japanese-anthy")
  (global-set-key "\C-\\" 'anthy-mode))


;;============================================
;;              html mode
;; *.htmlにはhtmlモード
;;============================================
(setq auto-mode-alist (cons (cons "\\.html$" 'html-mode)
			    auto-mode-alist))

;;============================================
;;              goby
;; Emacs上で動くWYSIWYGなプレゼンツール
;;============================================
(add-to-list 'load-path "~/elisp/goby")
(autoload 'goby "goby" nil t)

;;============================================
;;              SLIME
;; The Superior Lisp Interaction Mode for Emacs
;;============================================
(defun slime-other-window ()
  "Run slime on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*inferior-lisp*"))
  (slime))
(eval-when-compile 
  (require 'cl))
(add-to-list 'load-path "~/elisp/slime")
(defun slime-sbcl ()
  (interactive)
  (slime-connect "localhost" 4005))
(defun slime-clisp ()
  (interactive)
  (slime-connect "localhost" 4006))
(defun slime-cmu ()
  (interactive)
  (slime-connect "localhost" 4007))
(defun slime-ecl ()
  (interactive)
  (slime-connect "localhost" 4008))
(defun slime-allegro ()
  (interactive)
  (slime-connect "localhost" 4009))
(defun slime-starlisp-allegro ()
  (interactive)
  (slime-connect "localhost" 4010))
(defun slime-lw ()
  (interactive)
  (slime-connect "localhost" 4011))
(require 'slime)

(setq slime-lisp-implementations
      (case system-type
	(windows-nt `((sbcl ("sbcl") :coding-system utf-8-unix)))
	;;(otherwise  `((sbcl ("/opt/local/bin/sbcl") :coding-system utf-8-unix)
	(otherwise  `((allegro ("alisp"))
		      (sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
		      (clisp ("/usr/local/bin/clisp"))
		      ;;(starlisp ("~/bin/slime-starlisp"))
		      (ecl ("/usr/local/bin/ecl"))
		      (cmucl ("cmucl") :coding-system utf-8-unix)))))
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup)
(add-hook 'lisp-mode-hook 
	  (lambda () 
	    (slime-mode t)
	    (local-set-key [(control ?c) ?\;]        'slime-insert-balanced-comments)
	    (local-set-key [(control ?c) (meta ?\;)] 'slime-remove-balanced-comments)
	    (show-paren-mode t)
	    ))

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy
		    slime-asdf
		    slime-banner 
		    ;;slime-highlight-edits
		    ))
     (setq slime-complete-symbol*-fancy t
           browse-url-firefox-program (if (memq system-type '(darwin)) "open" "clhs")
	   slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	   common-lisp-hyperspec-root (concat "file://"
					      (expand-file-name "~/Documents/hyperspec/HyperSpec/"))
	   browse-url-browser-function 'browse-url-firefox)
     ))

(add-hook 'slime-mode-hook
	  (lambda ()
	    (setq lisp-indent-function 'common-lisp-indent-function)
	    (cl-indent 'iterate 'let)
	    (cl-indent 'collect 'progn)
	    (cl-indent 'mapping 'let)
	    (cl-indent 'mapping 'let)))
(slime-autodoc-mode)
(global-set-key "\C-cC" 'slime-other-window)
(defvar ac-slime-modes
  '(lisp-mode))

(defun ac-slime-candidates ()
  "Complete candidates of the symbol at point."
  (if (memq major-mode ac-slime-modes)
      (let* ((end (point))
	     (beg (slime-symbol-start-pos))
	     (prefix (buffer-substring-no-properties beg end))
	     (result (slime-simple-completions prefix)))
	(destructuring-bind (completions partial) result
	  completions))))

(defvar ac-source-slime
  '((candidates . ac-slime-candidates)
    (requires-num . 3)))



;;=============================================
;;                Twittering mode
;; http://twmode.sourceforge.net/
;; http://www.emacswiki.org/emacs/TwitteringMode
;;=============================================
(with-local-elisp-package
 (twittering-mode twittering-mode)
 (if (file-exists-p "~/.twittering")
     (load "~/.twittering")
   (message "there is no ~/.twittering")))

;;=============================================
;;                org-mode
;; いわゆるアウトライナーというやつ.
;;=============================================
(setq load-path (cons "~/elisp/org" load-path))
(setq load-path (cons "~/elisp/remember" load-path))
(require 'org)
(require 'org-remember)
(require 'remember)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate)
(setq org-directory "~/memo/")
(setq org-default-notes-file (concat org-directory "agenda.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ))
(defvar org-code-reading-software-name nil)
;; ~/memo/code-reading.org に記録する
(defvar org-code-reading-file "code-reading.org")
(defun org-code-reading-read-software-name ()
  (set (make-local-variable 'org-code-reading-software-name)
       (read-string "Code Reading Software: " 
                    (or org-code-reading-software-name
                        (file-name-nondirectory
                         (buffer-file-name))))))

(defun org-code-reading-get-prefix (lang)
  (concat "[" lang "]"
          "[" (org-code-reading-read-software-name) "]"))

(defun org-remember-code-reading ()
  (interactive)
  (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (org-remember-templates
          `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t"
             ,org-code-reading-file "Memo"))))
    (org-remember)))

(setq org-log-done t)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list "~/memo/agenda.org"
			     "~/memo/software.org"))

;;=============================================
;;                tramp
;; ssh越しのファイル操作
;;=============================================
(require 'tramp)
(setq tramp-default-method "sshx")
(add-hook 'lisp-mode-hook
          (lambda ()
	    (hs-minor-mode t)))

(global-set-key "\C-c(" 'hs-hide-block)
(global-set-key "\C-c)" 'hs-show-block)
(global-set-key "\C-c{" 'hs-hide-all)
(global-set-key "\C-c}" 'hs-show-all)

;;=============================================
;;               NVidia Cg
;;---------------------------------------------
;; cgファイルをc-modeで開く
;;=============================================
(setq auto-mode-alist (cons (cons "\\.cg?$" 'c-mode) auto-mode-alist))

;;=============================================
;;               NVidia CUDA
;;---------------------------------------------
;; cuファイルをc-modeで開く
;;=============================================
(setq auto-mode-alist (cons (cons "\\.cu?$" 'c-mode) auto-mode-alist))

;;=============================================
;;               ispell
;;---------------------------------------------
;; 日本語を無視する
;;=============================================
(eval-after-load "ispell"
  '(setq ispell-skip-region-alist
	 (cons '("[^¥000-¥377]")
	       ispell-skip-region-alist)))

;;=============================================
;;               SDIC
;;---------------------------------------------
;; http://www.namazu.org/~tsuchiya/sdic/
;; Emacsで和英/英和をつかう. C-c wにバインド.
;;=============================================
(setq load-path (cons "~/elisp/sdic" load-path))
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point
  "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;;=============================================
;;               mode-info
;;---------------------------------------------
;; http://www.namazu.org/~tsuchiya/elisp/mode-info.html
;; M-x mode-info-make-all-indicesでインデックス作成
;; M-x help-for-help f setqとかするらしいがうまくうごかん.
;; M-.でもつかえるとか
;;=============================================
(setq load-path (cons "~/elisp/mode-info" load-path))
(require 'mi-config)
(setq mode-info-index-directory "~/Documents/info/index")
(setq Info-directory-list
      (append
       Info-default-directory-list
       (list
	(expand-file-name "~/Documents/info")
	(expand-file-name "~/Documents/info/glibc-2.3.2"))))
(define-key global-map "\C-chf" 'mode-info-describe-function)
(define-key global-map "\C-chv" 'mode-info-describe-variable)
(define-key global-map "\M-." 'mode-info-find-tag)
(require 'mi-fontify)
(setq mode-info-class-alist
      '((elisp  emacs-lisp-mode lisp-interaction-mode)
	(libc   c-mode c++-mode)
	(make   makefile-mode)
	(perl   perl-mode cperl-mode eperl-mode)
	(ruby   ruby-mode)
	(gauche scheme-mode scheme-interaction-mode inferior-scheme-mode)))

;;=============================================
;;             flyspell-mode
;; M-x flyspell-modeで起動
;;=============================================
(add-hook
 'tex-mode-hook
 '(lambda()
    (flyspell-mode)
    (local-set-key [(control .)] 'flyspell-auto-correct-word)))

;;=============================================
;; gcode-lookup
;;=============================================
(defun gcode-lookup ()
  "カーソル位置のシンボルをGoogle Codeで検索(lisp決め打ち)"
  (interactive)
  (browse-url
   (format
    "http://www.google.com/codesearch?q=%s+lang:%s+file:\\.%s$&hl=ja&num=20"
    (thing-at-point 'symbol) "lisp" "lisp")))

(global-set-key "\C-cs" 'gcode-lookup)


;;=============================================
;; muse
;;=============================================
(setq load-path (cons "~/elisp/muse-latest/lisp" load-path))
(require 'muse-mode)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-book)
(require 'muse-wiki)
(require 'muse-project)
(add-to-list 'muse-project-alist
             '("Default"
               ("~/muse/default" :default "index")
               (:base "html" :path "~/muse/default/html")))
(add-to-list 'muse-ignored-extensions "DS_store")
(defvar *muse-root* (expand-file-name "~/muse/default"))

;;=============================================
;; emacs-wiki
;; http://www.apollostar.com/k-ishii/EmacsWiki.html
;;=============================================
(when-emacs22
 (setq load-path (cons "~/elisp/emacs-wiki" load-path))
 (setq load-path (cons "~/elisp/planner" load-path))
 (require 'emacs-wiki)

 (defun emacs-wiki-get-modtime (file)
   (format-time-string " %Y/%m/%d %h:%M:%S " (nth 4 (file-attributes file))))

 (defun emacs-wiki-get-filesize (file)
   (format "<center> %d </center>" (nth 7 (file-attributes file))))
;;; emacs-wiki-publishing-{header, footer} 用

 (defun emacs-wiki-project-insert-header (project)
   "project 名を prefix とした header を挿入"
   (emacs-wiki-project-insert-templete (symbol-name project) "-header"))

 (defun emacs-wiki-project-insert-footer (project)
   "project 名を prefix とした footer を挿入"
   (emacs-wiki-project-insert-templete (symbol-name project) "-footer"))

 (defun emacs-wiki-project-insert-templete (project templete)
   "project 名 + テンプレート名 のファイル内容を挿入"
   (let ((file (concat "~/Wiki/wiki/" project templete)))
     (when (file-readable-p file)
       (ignore (insert-file-contents file)))))

;;; <lisp> ... </lisp> 用
 (defun emacs-wiki-figure-with-class (class url width height caption)
   "caption 付きの図を挿入する関数 (クラス指定付き)"
   (format (concat "<nowiki><div class=\"%s\"><a href=\"%s\">"
		   "<img width=\"%s\" height=\"%s\" src=\"%s\" /></a>\n"
		   "<p class=\"caption\">%s</p></div></nowiki>")
	   class url width height url caption))

 (defun emacs-wiki-figure (url width height caption)
   "caption 付きの図を挿入"
   (emacs-wiki-figure-with-class "fig" url width height caption))

 (defun emacs-wiki-figure-right (url width height caption)
   "caption 付きの図を挿入 (float: right)"
   (emacs-wiki-figure-with-class "figright" url width height caption))

 (defun emacs-wiki-figure-left (url width height caption)
   "caption 付きの図を挿入 (float: left)"
   (emacs-wiki-figure-with-class "figleft" url width height caption))

 (require 'planner)
 (require 'emacs-wiki-menu)

 (setq emacs-wiki-directories '("~/ewiki/wiki"))
 (setq planner-directory "~/plans")
 (setq emacs-wiki-publishing-directory "~/ewiki/www")
 (setq emacs-wiki-maintainer "garaemon@gmail.com")
 (setq emacs-wiki-inline-images t)
 (setq emacs-wiki-publishing-transforms '(("WelcomePage" . "index")))
 (setq emacs-wiki-meta-content-coding "UTF-8")
 (setq emacs-wiki-charset-default "UTF-8")

 (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(emacs-wiki-footer-date-format "%Y-%m-%d %T")
  '(emacs-wiki-link-face ((t (:foreground "blue"))))
  '(emacs-wiki-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">")
  '(safe-local-variable-values (quote ((Syntax . Common-lisp)
				       (Package . XLIB)
				       (Lowercase . Yes)
				       (Syntax . ANSI-Common-Lisp)
				       (Base . 10)
				       (Package . CLIM-CLX)
				       (Package JPEG :use (common-lisp))
				       (Syntax . COMMON-LISP)))))
 )

;;=============================================
;; ibuffer
;;=============================================
(global-set-key "\C-x\C-b" 'ibuffer)

;;=============================================
;; yasnippet
;;=============================================
(let ((emacs-major-version 22))	;yasnippet does not work on emacs 23.0
  (unintern 'locate-dominating-file)
  (unintern 'locate-dominating-stop-dir-regexp)
  (with-local-elisp-package
   (yasnippet yasnippet)
   (setq yas/trigger-key (kbd "C-;"))
   (yas/initialize)
   (yas/load-directory "~/elisp/yasnippet/snippets")))

;;=============================================
;; auto-complete-mode
;;=============================================
;;(setq load-path (cons "~/elisp/auto-complete" load-path))
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (define-key ac-complete-mode-map "\t" 'ac-expand)
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (setq ac-auto-start 2)
  (setq ac-dwim t)
  (set-default 'ac-sources '(ac-source-yasnippet
			     ac-source-abbrev
			     ac-source-words-in-buffer))
  (setq ac-modes
	(append ac-modes
		'(emacs-lisp-mode
		  lisp-mode
		  ;;org-mode
		  )))
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (setq ac-sources '(ac-source-yasnippet
				 ac-source-abbrev
				 ac-source-words-in-buffer
				 ac-source-symbols))))
  )

;;=============================================
;; ac-anything.el
;; auto-completeの結果をanythingで絞り込む
;; http://d.hatena.ne.jp/rubikitch/20090210/auto_complete_anything
;;=============================================
;; (require 'ac-anything)
;; (define-key ac-complete-mode-map (kbd "C-:")
;;   'ac-complete-with-anything)

;;=============================================
;; shell-pop
;;---------------------------------------------
;; shell modeをポップアップする. 微妙
;;=============================================
(with-local-elisp-package
 (shell-pop shell-pop)
 ;;(shell-pop-set-internal-mode "ansi-term")
 (shell-pop-set-internal-mode-shell "/bin/zsh")
 (defvar ansi-term-after-hook nil)
 (add-hook 'ansi-term-after-hook
	   (function
	    (lambda ()
	      (define-key term-raw-map "\C-t" 'shell-pop))))
 (defadvice ansi-term (after ansi-term-after-advice (arg))
   "run hook as after advice"
   (run-hooks 'ansi-term-after-hook))
 (ad-activate 'ansi-term))

;;=============================================
;; apel
;;=============================================
(setq load-path (cons "~/elisp/apel" load-path))

;;=============================================
;; ddskk
;;---------------------------------------------
;; SKK
;;=============================================
(with-local-elisp-package
 (ddskk skk-autoloads)
 (setq mac-pass-control-to-system nil)
 (global-set-key "\C-x\C-j" 'skk-mode)
 (global-set-key "\C-xj" 'skk-auto-fill-mode)
 (global-set-key "\C-xt" 'skk-tutorial)
 (when-darwin
  (setq skk-server-host "localhost")
  (setq skk-jisyo-code 'utf-8-unix)
  (setq skk-server-portnum 1178))
 
 (setq skk-henkan-show-candidates-keys '(?a ?o ?e ?u ?h ?t ?n))
 (setq skk-kutouten-type 'en))

;;=============================================
;; go-mode
;;---------------------------------------------
;; Googleのプログラミング言語Goに付属しているもの
;;=============================================
(with-local-elisp-package
 (go go-mode-load)
 (define-key global-map [165] nil)
 (define-key global-map [67109029] nil)
 (define-key global-map [134217893] nil)
 (define-key global-map [201326757] nil)
 (define-key function-key-map [165] [?\\])
 (define-key function-key-map [67109029] [?\C-\\])
 (define-key function-key-map [134217893] [?\M-\\])
 (define-key function-key-map [201326757] [?\C-\M-\\]))

;;=============================================
;; icicles
;;---------------------------------------------
;; 使いかたわからん
;;=============================================
(with-local-elisp-package
 (icicles icicles)
 )

;;=============================================
;; yatex
;;---------------------------------------------
;; prefix = C-c
;; end補完 = C-c e
;;
;;
;;=============================================
(with-local-elisp-package
 (yatex yatex)
 (setq load-path (cons "~/elisp/auctex" load-path))
 (setq auto-mode-alist
       (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
 (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t) 
 (setq tex-command "platex-utf8")
 (setq YaTeX-kanji-code 4)
 (setq YaTeX-use-LaTeX2e t)
 (setq YaTeX-use-AMS-LaTeX t)
 (setq YaTeX-use-hilit19 nil
       YaTeX-use-font-lock t)
 )

;;=============================================
;; uniquify
;;---------------------------------------------
;; バッファ名をディレクトリ名にする
;;=============================================
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; screen saver!
(require 'zone)
(zone-when-idle 10000)

;;=============================================
;;              メニュー無し
;;=============================================
(tool-bar-mode nil)
(tool-bar-mode nil)
(menu-bar-mode nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp) (Package DATABASE :USE LISP) (Base . 10)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; lisp mode
(add-hook 'lisp-mode-hook
          (lambda ()
	    (global-unset-key "\M-p")
	    (global-unset-key "\M-n")
	    (global-set-key "\M-p" 'scroll-up-in-place)
	    (global-set-key [M-up] 'scroll-up-in-place)
	    (global-set-key "\M-n" 'scroll-down-in-place)
	    (global-set-key [M-down] 'scroll-down-in-place)
	    ))


;; slime時には別に設定する必要あり
(add-hook 'slime-mode-hook
          (lambda ()
	    (push 'ac-source-slime ac-sources)
	    (auto-complete-mode)
	    (global-unset-key "\M-p")
	    (global-unset-key "\M-n")
	    (global-set-key "\M-p" 'scroll-up-in-place)
	    (global-set-key [M-up] 'scroll-up-in-place)
	    (global-set-key "\M-n" 'scroll-down-in-place)
	    (global-set-key [M-down] 'scroll-down-in-place)
	    ))


