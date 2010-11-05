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
         ("cd clmemo-1.0rc3 && EMACS=$EMACS make"))
 (goby library
       (tar-ball http://www.mew.org/~kazu/proj/goby/goby-1.0.tar.gz)
       "Presentation Mode"
       nil
       ("cd goby-1.0; EMACS=$EMACS make"))
 (slime library
        (cvs :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot slime)
        "Common Lisp IDE"
        nil
        (:byte-compile))
 (twittering library
             (git git://github.com/hayamiz/twittering-mode.git)
             "Twitter Client on Emacs."
             nil
             (:byte-compile))
 (org library
      (tar-ball http://orgmode.org/org-7.01h.tar.gz)
      "an Emacs Mode for Notes, Project Planning, and Authoring"
      (remember)
      (:byte-compile))
 (org-info-js library
              (git git://github.com/SebastianRose/org-info-js.git)
              "implements part of Emacs Org-mode in it's XHTML-exported files,\
 allowing them to be rendered and browsed in a linuxdoc/texinfo style."
              (org))
 (remember library
           (tar-ball http://download.gna.org/remember-el/remember-2.0.tar.gz)
           "quickly jotting down things to remember"
           nil
           ("cd remember-2.0 && EMACS=$EMACS make"))
 (muse library
       (tar-ball 
	http://download.gna.org/muse-el/muse-3.20.tar.gz)
       "a publishing environment for Emacs."
       nil
       ("cd muse-3.20 && EMACS=$EMACS make"))
 (emacs-wiki library
             (tar-ball http://mwolson.org/static/dist/emacs-wiki/emacs-wiki-2.72.tar.gz)
             "Implementation of a Wiki by JohnWiegley"
             (planner)
             ("cd emacs-wiki-2.72 && EMACS=$EMACS make"))
 (planner library
          (tar-ball http://download.gna.org/planner-el/planner-3.42.tar.gz)
          "PersonalInformationManager (PIM) by JohnWiegley"
          (muse)
          ("cd planner-3.42 && EMACS=$EMACS make"))
 (yasnippet library
            (tar-ball http://yasnippet.googlecode.com/files/yasnippet-bundle-0.6.1c.el.tgz)
            "yet another snippet extension for Emacs"
            nil
            (:byte-compile))
 (auto-complete library
                (tar-ball http://cx4a.org/pub/auto-complete/auto-complete-1.2.tar.bz2)
                "Auto completion with popup menu"
                nil
                (:byte-compile))
 (yatex library
        (tar-ball http://www.yatex.org/yatex1.74.tar.gz)
        "Yet Another Tex Mode"
        nil
        (:byte-compile))
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
        "IME for Japanese. SKK(Super Kanji Kanzen) server and client."
        nil
        ("cd ddskk-14.0.91 && EMACS=$EMACS make")) ;failed...
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
 (haskell library
          (tar-ball http://projects.haskell.org/haskellmode-emacs/haskell-mode-2.7.0.tar.gz)
          "major mode for programming language haskell."
          nil
          (:byte-compile))
 (magit library
        (git git://gitorious.org/magit/mainline.git)
        "emacs interface to git"
        nil
        (:byte-compile))
 (mode-info 
  library
  (tar-ball http://www.namazu.org/~tsuchiya/elisp/mode-info-0.8.5.tar.gz)
  "emacs interface to info file. 
http://www.namazu.org/~tsuchiya/elisp/mode-info.html"
  nil
  ("cd mode-info-0.8.5 && ./configure && make"))
 (auto-install
  library
  http://www.emacswiki.org/emacs/download/auto-install.el
  "Effortlessly download, install, and update Elisp files \
from the web or from a local buffer"
  nil
  (:byte-compile))
 (usage-memo
  library
  http://www.emacswiki.org/cgi-bin/emacs/download/usage-memo.el
  "Integration of Emacs help system and memo"
  nil (:byte-compile))
 (wanderlust
  library
  (cvs :pserver:anonymous@cvs.m17n.org:/cvs/root wanderlust)
  "Wanderlust is a mail/news reader supporting IMAP4rev1 for emacsen."
  nil
  )
 (apel
  library
  (cvs :pserver:anonymous@cvs.m17n.org:/cvs/root apel)
  "APEL (A Portable Emacs Library) is a library to support \
to write portable Emacs Lisp programs."
  nil
  ("EMACS=$EMACS make"))
;; sorry url contains space does not supported
;; (python-mode-extension
;;  library
;;  (tar-ball "http://sourceforge.net/projects/page/files/Python Mode Extrensions/Python-Mode-Extension-1.0/py-mode-ext-1.0.tgz/download py-mode-ext-1.0.tgz")
;;  "python mode extensions"
;;  nil
;;  (:byte-compile))
 (python-mode
  library
  http://launchpadlibrarian.net/21781107/python-mode.el
  "python-mode.el is a major mode for editing, debugging, \
and developing Python programs"
  nil
  (:byte-compile))
 (ipython
  library
  http://ipython.scipy.org/dist/ipython.el
  "ipython is yet another python shell. ipython.el adds support ipython to
python-mode"
  (python-mode))
 (shell-toggle
  library
  http://www.linux-france.org/article/appli/emacs/bibliotheque.html/shell-toggle-patched.el
  "Toggle to and from the *shell* buffer"
  nil
  (:byte-compile))
 (rosemacs
  Library
  (svn http://code.ros.org/svn/ros/stacks/ros/tags/cturtle/tools/rosemacs)
  "emacs utilities for ROS(Robot Operating System)"
  nil
  (:byte-compile))
 (jsk-rosemacs
  library
  (svn http://jsk-ros-pkg.svn.sourceforge.net/svnroot/jsk-ros-pkg/trunk/jsk_ros_tools/jsk-rosemacs)
  "emacs utilities for ROS(Robot Operating System) written by R.Ueda(garaemon)."
  (anything auto-complete)              ;for popup.el
  (:byte-compile))
 (bm
  library
  (cvs :pserver:anonymous@cvs.sv.gnu.org:/sources/bm bm)
  "visible, buffer local, bookmarks"
  nil
  (:byte-compile))
 (svn-clients
  library
  (svn
   http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs)
  "vc-svn, dsvn and psvn for svn client on emacs"
  nil
  (:byte-compile))
  (svn-clients-patched
  library
  (git
   git://github.com/garaemon/svn-client.git)
  "vc-svn, dsvn and psvn for svn client on emacs"
  nil
  (:byte-compile))
 (yaml-mode
  library
  (git http://github.com/yoshiki/yaml-mode.git)
  "simple major mode ro edit YAML file for emacs"
  nil
  (:byte-compile))
 (ac-slime
  library
  (git git://github.com/purcell/ac-slime.git)
  "Slime completion source for Emacs auto-complete package"
  (auto-complete)
  (:byte-compile))
 )
