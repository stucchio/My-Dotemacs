(defvar emacs-root (if (or (eq system-type 'cygwin32)
			   (eq system-type 'gnu/linux)
			   (eq system-type 'linux)
			   (eq system-type 'usg-unix-v)
			   (eq system-type 'darwin)
			   )
		       (concat (getenv "HOME") "/emacs/")
		     "c:/home/stucchio/")
  "My home directory, the root of my personal emacs load-path.")

(defun add-path (p)
		 (add-to-list 'load-path (concat emacs-root p)))
(add-path "lisp")            ;; Simple lisp files I've found, not big enough to need a whole directory
(add-path "my_lisp")         ;; My own personal lisp files, mostly used for customizing particular modes.
(add-path "site-lisp")       ;; elisp stuff I find on the 'net
(add-path "site-lisp/reftex") ;;reftex, support for references
(add-path "site-lisp/gnuserv") ;;gnuserv
(add-path "site-lisp/erlang") ;;Erlang mode
(add-path "site-lisp/tuareg")  ;; OCaml support
(add-path "site-lisp/gnuplot-mode") ;;Gnuplot support
(add-path "site-lisp/python-mode") ;;Python mode
;(add-path "site-lisp/icicles") ;;Icicles, autocompletion
(add-path "site-lisp/slime-cvs") ;;Slime lisp mode
(add-path "site-lisp/eproject") ;;Eproject

(transient-mark-mode 1)
(add-hook 'write-file-functions 'delete-trailing-whitespace)
(setq truncate-partial-width-windows 'nil) ;;Makes word wrapping work even after split horizontally

;;Load my basic customizations

;;******** bubble-buffer ********
(require 'bubble-buffer)
;;******** wrap-region ********
(require 'wrap-region)

;;********* multi-shell stuff
(require 'multi-eshell)
(defalias 'emacs 'find-file) ;;This binds the command emacs to find-file. Useful in eshell.
(defalias 'more 'find-file-temporary) ;;Binds command more to find-file-temporary. Opens file in split window.

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
 '(eshell-directory-name (concat emacs-root "eshell-custom/"))
 '(eshell-save-history-on-exit t)
 '(icicle-reminder-prompt-flag 0)
 '(lazy-lock-mode nil t (lazy-lock))
 '(nxhtml-skip-welcome t)
 '(org-agenda-files (quote ("~/notes/projects.org")))
 '(paren-mode (quote sexp) nil (paren))
 '(popcmp-popup-completion nil)
 '(reftex-toc-split-windows-horizontally t)
 '(regex-tool-new-frame nil)
 '(multi-eshell-name "*eshell*")
 '(multi-eshell-shell-function (quote (eshell)))
 '(tempbuf-minimum-timeout 60)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;;******** eshell custom ********
(load-file (concat emacs-root "my_lisp/eshell-custom.el"))

;;******** regex-tool ********
(load-file (concat emacs-root "lisp/regex-tool.el"))

;;******** tabbar ********

(require 'tabbar)
(tabbar-mode 1)


;;********* Org mode ********
(setq org-replace-disputed-keys t)
(global-set-key "\C-ca" 'org-agenda)
(setq org-tab-follows-link t)


;;******** htmlize ********
(require 'htmlize)

;;Set tabbar faces
(progn
  (set-face-attribute
   'tabbar-default-face nil
   :background "gray60")
  (set-face-attribute
   'tabbar-unselected-face nil
   :background "gray85"
   :foreground "gray30"
   :box nil)
  (set-face-attribute
   'tabbar-selected-face nil
   :background "#f2f2f6"
   :foreground "black"
   :box nil)
(set-face-attribute
 'tabbar-button-face nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator-face nil
   :height 0.7)
)

;;Load local customizations
(load-file (concat emacs-root "local-customizations.el"))

;; start gnuserv, so apps can talk to us (e.g. p4, browsers)
(autoload 'gnuserv-start "gnuserv-compat"
             "Allow this Emacs process to be a server for client processes."
             t)
(setq gnuserv-frame (selected-frame))
(condition-case nil
	   (gnuserv-start)
	   (error (message "Warning! Failed to start gnuserv!"))
	   )


;Turn on scroll bars, turn off menu-bar and toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-font-lock-mode t)
(setq inhibit-startup-message t)

;;********* Uniquify buffer names *********
(require 'uniquify)

;;******** eproject ********
(require 'eproject)
(require 'eproject-extras)
(load-file (concat emacs-root "my_lisp/eproject-custom.el"))

;; ;; Icicles
;; (load "icicles-custom")
;; ;;Build a gigantic regexp to match a bunch of buffers I don't care about.
;; (setq icicle-buffer-no-match-regexp (mapconcat 'identity  ;; We will take a list of sub-regexp's, and \| or them together
;; 					       '("\*Messages\*" ;;A list of
;; 						 "\*Completions\*"
;; 						 "\*xdvi output\*"
;; 						 "\*.*+ output\*"
;; 						 )
;; 					       "\\|" ))


(require 'anything-config)
(require 'anything)
;Load bm.el, bookmarking facility.
(load "bm")

;;********* C-mode *********
(load "c-custom")

(autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)

;;********* Mathematica mode *********
(require 'mathematica)

;;******** Lush ********
(load (concat emacs-root "lisp/lush.el"))

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

;;******** nxml mode ********
(load "~/emacs/site-lisp/nxml/autostart.el")
(load "~/emacs/my_lisp/nxml-custom.el")

;;******** Haskell Mode *********
(load (concat emacs-root "site-lisp/haskell-mode-2.1/haskell-site-file.el"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;********* Clojure mode *********
;; Clojure mode
(add-path "lisp/clojure/clojure-mode")
(add-path "lisp/clojure/swank-clojure")
(require 'clojure-mode)
;; (require 'clojure-paredit) ; Uncomment if you use Paredit

;; Slime
;;(add-to-list 'load-path "/path/to/slime/")
(require 'slime)
(slime-setup)

;; clojure swank
(setq swank-clojure-jar-path (concat (getenv "HOME") "/src/clojure/clojure.jar"))
;alternatively, you can set up the clojure wrapper script and use that:
;(setq swank-clojure-binary "/path/to/cljwrapper")

; you can also set up extra classpaths, such as the classes/ directory used by AOT compilation
(setq swank-clojure-extra-classpaths
      (list (concat (getenv "HOME") "/src/clojure/src/clj/clojure")
	    ))

(add-path "lisp/clojure/swank-clojure")
(require 'swank-clojure-autoload)

(load-file (concat emacs-root "my_lisp/paredit-custom.el"))

;; The following function runs Slime with Clojure, even if Slime defaults to another Lisp.
;; The above configuration alone, however, will make Clojure the default, so all that is necessary
;; to run Slime with Clojure is M-x slime.
(defun run-clojure ()
  "Starts clojure in Slime"
  (interactive)
  (slime 'clojure))

;; To use other Lisps...
;; Incidentally, you can then choose different Lisps with
;;   M-- M-x slime <tab>
;; (add-to-list 'slime-lisp-implementations
;;             '(sbcl   ("/path/to/bin/sbcl")))

;;********* Javascript mode *********
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;******** Global-sexp highlighting *********
(global-hl-line-mode 1)
(set-face-background 'hl-line "#f3f3f3")

(require 'hl-sexp)
(set-face-background 'hl-sexp-face "#ffe0e0")
(custom-set-variables
 '(hl-delay 0.05)
 )

(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)


;;******** Show parenthesis ********
(show-paren-mode t)

;;******** Highlight Parenthesis ********
(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)

;;********** Desktop mode **********
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;;********** Turn on session ********
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning)) ;;Make sure session doesn't clobber desktop

;'******** Save location of point in files
(require 'saveplace)
(setq-default save-place t)

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

;;******** Load browse-kill-ring ********
;(require 'browse-kill-ring)

;;******** far-search ********
(require 'far-search)

;;******** TRAMP ********
(setq tramp-default-method "ssh")

;;******** Save buffers without asking about killing processes ********
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;********* Smooth scrolling *********
(require 'smooth-scrolling)

;;******** Mozrepl ********
(require 'moz)

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