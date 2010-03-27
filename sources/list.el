;; sources.el written by R.Ueda (garaemon)
;; the list of emacs packages.
;;
;; <all>          := (<url> <def> <def> ...)
;; <def>          := (<package-name> <type> <source-list>
;;                     &optional <documentation> <dependent-packages>
;;                               <install-commands>)
;; <package-name> := <symbol>
;; <type>         := library | bootstrap
;; <source-list>  := <source> | (<source> <source> ...)
;; <source>       := <url> | <cvs-url> | <svn-url> | <git-url> | <local-path>
;;                        | <tar-ball>
;; <url>          := <symbol> | <string>
;; <cvs-url>      := (cvs <url> <module-name>)
;; <module-name>  := <symbol> | <string>
;; <svn-url>      := (svn <url>)
;; <local-path>   := (local <path>)
;; <path>         := <url>
;; <path>         := <string>
;; <tar-ball>     := (tar-ball <url> &optional <tar-ball-file-name>)
;; <dependent-packages> := (<package-name>)
;; <package-name> := <symbol> | <string>
;; <install-commands> := (<install-command> <install-command> ...)
;; <install-command>  := <lisp-command> | <shell-command> | <install-keyword>
;; <install-keyword>  := :byte-compile
;; <lisp-command>     := <S-expression>
;; <shell-command>    := <string>
;; You can use some keywords in <shell-command>.
;; Currently supported keywords are:
;;   $EMACS -> path-to-emacs
;;   $PACKAGE -> path-to-package
(http://github.com/garaemon/emacs-settings/raw/master/sources/list.el ;URL of this source file
 (navi-2ch                              ;package name
  library                               ;type of package
  (tar-ball http://sourceforge.net/projects/navi2ch/files/navi2ch/navi2ch-1.8.3/navi2ch-1.8.3.tar.gz/download navi2ch-1.8.3.tar.gz) ;source location
  "2ch viewer"                          ;description
  nil                                   ;dependency
  ("cd navi2ch-1.8.3 && EMACS=$EMACS ./configure --with-lispdir=. && make")) ;install command   
 (clmemo library
         (tar-ball
          http://isweb22.infoseek.co.jp/computer/pop-club/emacs/clmemo-1.0rc3.tar.gz)
         "ChangeLog Memo"
         nil
         (:byte-compile))
 (goby library
       (tar-ball http://www.mew.org/~kazu/proj/goby/goby-1.0.tar.gz)
       "Presentation Mode"
       nil
       ("cd goby-1.0; make"))
 (slime library
        (cvs :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot slime)
        "Common Lisp IDE")
 (twittering library
             (git git://github.com/hayamiz/twittering-mode.git)
             "Post to twitter and get your time line"
             nil
             (:byte-compile))
 (org library
      (tar-ball http://orgmode.org/org-6.34c.tar.gz)
      "an Emacs Mode for Notes, Project Planning, and Authoring"
      (remember))
 (remember library
           (tar-ball http://download.gna.org/remember-el/remember-2.0.tar.gz)
           "quickly jotting down things to remember")
 (muse library
       (tar-ball http://mwolson.org/static/dist/muse-latest.tar.gz)
       "a publishing environment for Emacs.")
 (emacs-wiki library
             (tar-ball http://www.mwolson.org/static/dist/emacs-wiki-latest.tar.gz)
             "Implementation of a Wiki by JohnWiegley"
             (planner))
 (planner library
          (tar-ball http://download.gna.org/planner-el/planner-3.42.tar.gz)
          "PersonalInformationManager (PIM) by JohnWiegley")
 (yasnippet library
            (tar-ball http://yasnippet.googlecode.com/files/yasnippet-bundle-0.6.1c.el.tgz)
            "yet another snippet extension for Emacs")
 (auto-complete library
                nil
                "")
 (go-lang library
          nil
          "Major mode for Go Language")
 (yatex library
        nil
        "Yet Another Tex Mode")
 (anything library
           (http://www.emacswiki.org/emacs/download/anything.el
            http://www.emacswiki.org/emacs/download/anything-config.el
            http://www.emacswiki.org/emacs/download/anything-match-plugin.el
            http://www.emacswiki.org/emacs/download/anything-migemo.el
            http://www.emacswiki.org/emacs/download/anything-complete.el
            http://www.emacswiki.org/emacs/download/anything-show-completion.el
            http://www.emacswiki.org/emacs/download/anything-auto-install.el
            http://www.emacswiki.org/emacs/download/descbinds-anything.el
            http://www.emacswiki.org/emacs/download/anything-grep.el
            http://www.emacswiki.org/emacs/download/anything-startup.el)
           "Integrated inteface to anything from emacs"
           (auto-install)
           (:byte-compile))
 (icicles library
          (http://www.emacswiki.org/emacs/download/icicles.el
           http://www.emacswiki.org/emacs/download/icicles-chg.el
           http://www.emacswiki.org/emacs/download/icicles-cmd1.el
           http://www.emacswiki.org/emacs/download/icicles-cmd2.el
           http://www.emacswiki.org/emacs/download/icicles-doc1.el
           http://www.emacswiki.org/emacs/download/icicles-doc2.el
           http://www.emacswiki.org/emacs/download/icicles-face.el
           http://www.emacswiki.org/emacs/download/icicles-fn.el
           http://www.emacswiki.org/emacs/download/icicles-mac.el
           http://www.emacswiki.org/emacs/download/icicles-mcmd.el
           http://www.emacswiki.org/emacs/download/icicles-mode.el
           http://www.emacswiki.org/emacs/download/icicles-opt.el
           http://www.emacswiki.org/emacs/download/icicles-var.el
           http://www.emacswiki.org/emacs/download/lacarte.el
           http://www.emacswiki.org/emacs/download/icomplete+.el
           http://www.emacswiki.org/emacs/download/hexrgb.el
           http://www.emacswiki.org/emacs/download/synonyms.el)
          "smart complementation library"
          nil
          (:byte-compile))
 (ddskk library
        (tar-ball http://openlab.ring.gr.jp/skk/maintrunk/ddskk-14.0.91.tar.gz
                  ddskk-14.0.91.tar.gz)
        "IME for Japanese. SKK(Super Kanji Kanzen) server and client.")
 (shell-pop library
            http://www.emacswiki.org/emacs/download/shell-pop.el
            "popping up shell-mode buffer"
            nil
            (:byte-compile))
 (dabbrev-ja library
             http://namazu.org/~tsuchiya/elisp/dabbrev-ja.el
             "dabbrev mode for japanese characters"
             nil
             (:byte-compile))
 )
