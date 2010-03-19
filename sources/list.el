;; sources.el written by R.Ueda (garaemon)
;; the list of emacs packages.
;; <all> := (<def>...)
;; <def> := (<package-name> <type> <source-list>
;;           &optional <documentation> <dependent-packages>
;;                     <install-commands>)
;; <type> := library | bootstrap
;; <source-list> := <source> | (<source> ...)
;; <source>      := <url> | <cvs-url> | <svn-url> | <git-url> | <local-path>
;;                        | <tar-ball>
;; <url> := symbol
;; <cvs-url> := (cvs <url> module-name)
;; <svn-url> := (svn <url>)
;; <local-path> := (local <path>)
;; <path>       := <string>
;; <tar-ball>   := (tar-ball <url>)
(http://github.com/garaemon/emacs-settings/raw/master/sources/list.el
 (navi-2ch library
           (tar-ball http://sourceforge.net/projects/navi2ch/files/navi2ch/navi2ch-1.8.3/navi2ch-1.8.3.tar.gz/download navi2ch-1.8.3.tar.gz)
           "2ch viewer")
 (navi-2ch-setting bootstrap
                   nil
                   "setting for 2ch viewer"
                   (navi-2ch))
 (clmemo library
         (tar-ball
          http://isweb22.infoseek.co.jp/computer/pop-club/emacs/clmemo-1.0rc3.tar.gz
          clmemo-1.0rc3.tar.gz)
  "ChangeLog Memo")
 (goby library
       (tar-ball http://www.mew.org/~kazu/proj/goby/goby-1.0.tar.gz
                 goby-1.0.tar.gz)
       "Presentation Mode")
 (slime library
        (cvs :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot slime)
        "Common Lisp IDE")
 (twittering library
             (git git://github.com/hayamiz/twittering-mode.git)
             "Post to twitter and get your time line")
 (org library
  nil
  ""
  (remember))
 (remember library
  nil
  "")
 (muse library
  nil
  "")
 (emacs-wiki library
  nil
  "wiki system"
  (planner))
 (planner library
  nil
  "")
 (yasnippet library
  nil
  "")
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
  "Integrated inteface to anything")
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
  "")
 (ddskk library
        (tar-ball http://openlab.ring.gr.jp/skk/maintrunk/ddskk-14.0.91.tar.gz
                  ddskk-14.0.91.tar.gz)
        "IME for Japanese. SKK(Super Kanji Kanzen) server and client.")
 (shell-pop library
            http://www.emacswiki.org/emacs/download/shell-pop.el
            "popping up shell-mode buffer")
 (dabbrev-ja library
             http://namazu.org/~tsuchiya/elisp/dabbrev-ja.el
             "dabbrev mode for japanese")
 (dabbrev-ja-setting bootstrap
                     nil
                     "settings for dabbrev-ja"
                     (dabbrev-ja))
 (shell-pop-setting bootstrap
                    nil
                    "settings for shell-pop written by garaemon"
                    (shell-pop))
 )
