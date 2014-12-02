(defvar emacs-root (if (or (eq system-type 'cygwin32)
			   (eq system-type 'gnu/linux)
			   (eq system-type 'linux)
			   (eq system-type 'usg-unix-v)
			   (eq system-type 'darwin)
			   )
		       (concat (getenv "HOME") "/dotemacs/")
		     "c:/home/stucchio/")
  "My home directory, the root of my personal emacs load-path.")

(defun add-path (p)
  (add-to-list 'load-path (concat emacs-root p)))
(add-path "lisp")            ;; Simple lisp files I've found, not big enough to need a whole directory
(add-path "my_lisp")         ;; My own personal lisp files, mostly used for customizing particular modes.
(add-path "site-lisp")       ;; elisp stuff I find on the 'net
(add-path "site-lisp/org-mode/lisp") ;;Latest org-mode
(add-path "site-lisp/org-mode/contrib/lisp") ;;Latest org-mode contrib
(add-path "site-lisp/reftex") ;;reftex, support for references
(add-path "site-lisp/gnuserv") ;;gnuserv
(add-path "site-lisp/erlang") ;;Erlang mode
(add-path "site-lisp/tuareg")  ;; OCaml support
(add-path "site-lisp/gnuplot-mode") ;;Gnuplot support
(add-path "site-lisp/python-mode") ;;Python mode
(add-path "site-lisp/slime-cvs") ;;Slime lisp mode

(add-path "site-lisp/s.el") ;;String manipulation lib, dependency of projectile
(add-path "site-lisp/dash.el") ;;list manipulation lib, dependency of projectile
(add-path "site-lisp/pkg-info.el") ;;list manipulation lib, dependency of projectile
(add-path "site-lisp/epl") ;;list manipulation lib, dependency of projectile
(add-path "site-lisp/projectile") ;;projectile
(add-path "site-lisp/google-maps") ;;Google maps
(add-path "site-lisp/git-emacs") ;;git emacs
(add-path "site-lisp/color-theme") ;;Color themes
(add-path "site-lisp/notmuch") ;;Notmuch mail mode

(transient-mark-mode 1)
(add-hook 'write-file-functions 'delete-trailing-whitespace)
(setq truncate-partial-width-windows 'nil) ;;Makes word wrapping work even after split horizontally

;;Load my basic customizations

;;******** wrap-region ********
(require 'wrap-region)
;;******** thrift-mode ********
(require 'thrift-mode)
;;******** yaml-mode ********
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;********* multi-shell stuff
(require 'multi-eshell)
(defalias 'emacs 'find-file) ;;This binds the command emacs to find-file. Useful in eshell.
(defalias 'more 'find-file-temporary) ;;Binds command more to find-file-temporary. Opens file in split window.
(add-hook 'eshell-mode-hook (lambda () (interactive) (setenv "PYTHONPATH" "/home/stucchio/lib/python2.6/site-packages/") ))

;;******** tempbuf ********
;; Kill buffers after a while, if I don't really want them
(require 'tempbuf)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)              ;;Kill man pages after a while
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)             ;;kill view pages after some time
(add-hook 'apropos-mode-hook 'turn-on-tempbuf-mode)          ;;Kill apropos buffers
(add-hook 'fundamental-mode-hook 'turn-on-tempbuf-mode)      ;;Kill LaTeX-mode's old buffers
(add-hook 'ps-mode-hook 'turn-on-tempbuf-mode)               ;;Kill ps-mode
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)            ;;Kill old dired buffers
(add-hook 'reftex-toc-mode-hook 'turn-on-tempbuf-mode)       ;;Kill old reftex table-of-contents
(add-hook 'bibtex-mode-hook 'turn-on-tempbuf-mode)           ;;Kill old bibtex; probably just opened by reftex
(add-hook 'ediff-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'ediff-meta-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'darcsum-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'compilation-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'compile-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'slime-macroexpansion-minor-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'org-agenda-mode-hook 'turn-on-tempbuf-mode)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -src")
 '(LaTeX-enable-toolbar nil)
 '(TeX-electric-sub-and-superscript t)
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi -bg white -fg black -editor \"gnuclient -q +%l %f\" -offsets -0.6in -expert -s 7 -geometry +0+0 -paper 16x24%dS %d") ("^pdf$" "." "xpdf %o") ("^html?$" "." "netscape %o"))))
 '(TeX-view-style (quote (("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^landscape$" "%(o?)xdvi %dS -paper a4r -s 0 %d") ("." "%(o?)xdvi -editor \"gnuclient -q +%l %f\" %dS %d"))))
 '(eshell-directory-name (concat emacs-root "eshell-custom/"))
 '(eshell-save-history-on-exit t)
 '(git--use-ido nil)
 '(hl-delay 0.05)
 '(lazy-lock-mode nil t (lazy-lock))
; '(multi-eshell-name "*ansi-term*")
 '(multi-eshell-shell-function (quote (ansi-term (getenv "SHELL"))))
 '(nxhtml-skip-welcome t)
 '(org-agenda-files (quote ("~/org/papers.org" "~/org/tasks.org")))
 '(paren-mode (quote sexp) nil (paren))
 '(popcmp-popup-completion nil)
 '(reftex-toc-split-windows-horizontally t)
 '(regex-tool-new-frame nil)
 '(st-shell-shell-function (quote (ansi-term (getenv "SHELL"))))
 '(tempbuf-minimum-timeout 60)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;;******** eshell custom ********
(load-file (concat emacs-root "my_lisp/eshell-custom.el"))

;;******** regex-tool ********
(load-file (concat emacs-root "lisp/regex-tool.el"))

;;********* Org mode ********
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-replace-disputed-keys t)
(global-set-key "\C-ca" 'org-agenda)
(setq org-tab-follows-link t)
(setq org-default-notes-file "~/org/notes/capture.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/notes/tasks.org" "Tasks") "* TODO %?\n  %i\n  %a")
        ("i" "Idea" entry (file+datetree "~/org/notes/ideas.org" "Ideas") "* %?\nEntered on %U\n  %i\n  %a")
        ("r" "Toread" entry (file+datetree "~/org/notes/toread.org" "Stuff to read") "* %?\nEntered on %U\n  %i\n  %a")
        )
      )
(setq org-log-done 'time)
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "LightSalmon" :strike-through t)))))

;;Load local customizations
(load-file (concat emacs-root "local-customizations.el"))

;; start gnuserv, so apps can talk to us (e.g. p4, browsers)
;(autoload 'gnuserv-start "gnuserv-compat"
;             "Allow this Emacs process to be a server for client processes."
;             t)
;(setq gnuserv-frame (selected-frame))
;(condition-case nil
;	   (gnuserv-start)
;	   (error (message "Warning! Failed to start gnuserv!"))
;	   )


;Turn on scroll bars, turn off menu-bar and toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-font-lock-mode t)
(setq inhibit-startup-message t)

;;********* Uniquify buffer names *********
(require 'uniquify)

;;******** google-maps ********
(require 'google-maps)

;;******** git-emacs ********
(require 'git-emacs)
(require 'git-status)


;;******** rainbow-mode ********
;; http://julien.danjou.info/rainbow-mode.html
(require 'rainbow-mode)

;;******** Helm ********
(add-path "site-lisp/helm")
(add-path "site-lisp/emacs-async")
(require 'helm-config)
(helm-mode)

;;******** projectile ********
(require 'projectile)
(projectile-global-mode)
(require 'helm-projectile)
;(setq projectile-enable-caching t)

;******** bookmarking ********
(load "bm")

;;********* C-mode *********
(load "c-custom")

(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)

;;********* Mathematica mode *********
(require 'mathematica)

;;******** Erlang mode ********
(setq erlang-root-dir "/opt/local/lib/erlang")
(setq exec-path (cons "/opt/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

;;********* AUCTEX *********
;; For auctex

;Sets sentence end to be .?!, but with 1-space after sentence terminator rather than 2. I have no idea why 2 spaces is the default.
(setq sentence-end "[.?!][]\"')}]*\\($\\| $\\|	\\| \\)[ 	]*")

;Make latex-mode default for *.tex files
(setq auto-mode-alist (cons '("\\.tex" . latex-mode) auto-mode-alist))
;Load my personal customizations
(load "auctex.el" nil t t)

(require 'xdvi-search)
(require 'bibtex)
(add-hook 'LaTeX-mode-hook (lambda () (ispell-minor-mode) ))
(add-hook 'LaTeX-mode-hook (lambda () (load "reftex-custom")))
(load-file (concat emacs-root "my_lisp/auctex-custom.el"))

;Load misc stuff
(load-file (concat emacs-root "my_lisp/misc.el"))
(load-file (concat emacs-root "my_lisp/fc-eval-and-replace.el"))
(load-file (concat emacs-root "my_lisp/gpl.el"))


;********* Darcsum *********
(load-file (concat emacs-root "lisp/darcsum.el"))
(load-file (concat emacs-root "my_lisp/darcsum-custom.el"))
;********* Mercurial *********
(require 'mercurial)


;********* Gnuplot mode *********
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;******** Markdown mode ********
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;********* Tuared Mode *********

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;********* Quack *********
(require 'quack)
(load-file (concat emacs-root "my_lisp/quack-custom.el"))

;;********* Python Mode **********
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(add-hook 'python-mode-hook (lambda () (load "python-custom")))
(add-hook 'python-mode-hook (lambda () (setq font-lock-maximum-decoration t)))

(require 'doctest-mode)

;;******** julia mode ********
(require 'julia-mode)

;;******** nxml mode ********
(load (concat emacs-root "site-lisp/nxml/autostart.el"))
(load (concat emacs-root "my_lisp/nxml-custom.el"))

;;******** Haskell Mode *********
(load (concat emacs-root "site-lisp/haskell-mode-2.1/haskell-site-file.el"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;;******** Clojure Mode ********
(add-path "lisp/clojure-mode")
(require 'clojure-mode)
(load-file (concat emacs-root "my_lisp/paredit-custom.el"))

;;******** Scala Mode ********
(add-path "site-lisp/scala-mode2")
(require 'scala-mode2)


;;********* Javascript mode *********
;(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

;;******** Java mode ********
(load-file (concat emacs-root "lisp/java-abbrev.el"))


;;******** Global-sexp highlighting *********
;(global-hl-line-mode 1)

(require 'hl-sexp)

(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)

;;******** Color theme ********
; Standard theme
;(set-face-background 'hl-sexp-face "#ffe0e0")
;(set-face-background 'hl-line "#f3f3f3")

; Billw
(load-file (concat emacs-root "site-lisp/color-theme/themes/color-theme-library.el"))
(color-theme-billw)
;(set-face-background 'hl-line "#060606")
(set-face-background 'hl-sexp-face "#250606")


;;******** Notmuch mail mode ********
(require 'notmuch)

;;******** Show parenthesis ********
(show-paren-mode t)

;;******** Highlight Parenthesis ********
(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)

;;******** Spaces instead of tabs ********
(setq-default indent-tabs-mode nil)

;;********** Desktop mode **********
;(custom-set-variables '(desktop-path emacs-root))
;(desktop-save-mode 1)
;(setq history-length 250)
;(add-to-list 'desktop-globals-to-save 'file-name-history)
;(add-to-list 'desktop-modes-not-to-save 'dired-mode)
;(add-to-list 'desktop-modes-not-to-save 'Info-mode)
;(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
;(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;;********** dired **********
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files "^\\.?#\\|^\\.$\\$" )


;;********** Turn on session ********
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning)) ;;Make sure session doesn't clobber desktop

;'******** Save location of point in files
;(require 'saveplace)
;(setq-default save-place t)

;;******** Save list of files recently opened
(require 'recentf)
(recentf-mode 1)

;;******** Revert open files, if file on disk changed ********
(global-auto-revert-mode 1) ;; If file attached to unmodified buffer is changed, revert it transparently.

;;******** Ediff mode ********
(if window-system
    (ediff-toggle-multiframe) ;;Don't make *Ediff Control Panel* open in new frame, just new window.
  )

;;******** Window numbering ********
(require 'window-numbering)
(window-numbering-mode 1)

;;******** winner ********
;; Binds C-c left or C-c right to move through past window configurations
(winner-mode)

;;******** Window-config-ring ********
(require 'window-config-ring)

;;******** windmove ********
(when (fboundp 'windmove-default-keybindings) ;;Turns on windmove mode: shift+arrow keys move between windows.
  (windmove-default-keybindings))

;;******** make scripts executable ********
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;******** TRAMP ********
(setq tramp-default-method "ssh")

;;******** Save buffers without asking about killing processes ********
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;********* Smooth scrolling *********
(require 'smooth-scrolling)

;;******** Undo-tree ********
(require 'undo-tree)
(global-undo-tree-mode)

;;******** Mozrepl ********
(require 'moz)

;;******** Perspective mode ********
(require 'perspective)
(persp-mode)

(autoload 'inferior-moz-mode "moz" "MozRepl Inferior Mode" t)
(autoload 'moz-minor-mode "moz" "MozRepl Minor Mode" t)
(defun web-moz-setup ()
  (progn
    (run-mozilla)
    (moz-minor-mode 1)))

(add-hook 'js2-mode-hook 'web-moz-setup)
(add-hook 'css-mode-hook 'web-moz-setup)
(add-hook 'nxhtml-mode-hook 'web-moz-setup)

;;******** Git environment variables ********
(setenv "PAGER" "cat") ;;Allows git to work in eshell
(setenv "EDITOR" "emacsclient -c ") ;;Allows git to work in eshell

;;******** Finally, set key bindings ********
(load-file (concat emacs-root "keys.el"))

;;********* Byte-compile local lisp directory, if necessary *********
(byte-recompile-directory (concat emacs-root "lisp") 0)

;;******** Turn on emacsclient server ********
(server-start)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;******** Activate org mode ********
(find-file "~/org/tasks.org")
(find-file "~/org/papers.org")
(org-agenda nil "t")
