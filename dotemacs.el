(defvar emacs-root (if (or (eq system-type 'cygwin32)
			   (eq system-type 'gnu/linux)
			   (eq system-type 'linux))
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
(add-path "site-lisp/tuareg")  ;; OCaml support
(add-path "site-lisp/gnuplot-mode") ;;Gnuplot support
(add-path "site-lisp/python-mode") ;;Python mode
(add-path "site-lisp/icicles") ;;Icicles, autocompletion

(transient-mark-mode 1)

;;Load my basic customizations

;;******** bubble-buffer ********
(require 'bubble-buffer)

;;********* st-shell stuff
(require 'st-shell)

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

(custom-set-variables
 '(LaTeX-command "latex -src")
 '(LaTeX-enable-toolbar nil)
 '(TeX-electric-sub-and-superscript t)
 '(icicle-reminder-prompt-flag 0)
 '(lazy-lock-mode nil t (lazy-lock))
 '(paren-mode (quote sexp) nil (paren))
 '(popcmp-popup-completion nil)
 '(reftex-toc-split-windows-horizontally t)
 '(st-shell-shell-function '(eshell)) 
 '(st-shell-name "*eshell*")
 '(paren-mode (quote sexp) nil (paren))
 '(lazy-lock-mode nil nil (lazy-lock))
 '(tempbuf-minimum-timeout 60)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;;******** tabbar ********

(require 'tabbar)
(tabbar-mode 1)

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
(gnuserv-start)


;Turn on scroll bars, turn off menu-bar and toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-font-lock-mode t)

;;********* Uniquify buffer names *********
(require 'uniquify)

;; Icicles
(load "icicles-custom")

;Load bm.el, bookmarking facility.
(load "bm")

;;********* C-mode *********
(load "c-custom")

;;********* AUCTEX *********
;; For auctex

;Sets sentence end to be .?!, but with 1-space after sentence terminator rather than 2. I have no idea why 2 spaces is the default.
(setq sentence-end "[.?!][]\"')}]*\\($\\| $\\|	\\| \\)[ 	]*")

;Make latex-mode default for *.tex files
(setq auto-mode-alist (cons '("\\.tex" . latex-mode) auto-mode-alist))
;Load my personal customizations
(load "auctex.el" nil t t)

(require 'xdvi-search)
(load-file (concat emacs-root "my_lisp/auctex-custom.el"))
(require 'bibtex)
(add-hook 'LaTeX-mode-hook (lambda () (ispell-minor-mode) ))
(add-hook 'LaTeX-mode-hook (lambda () (load "reftex-custom")))

;Load misc stuff
(load-file (concat emacs-root "my_lisp/misc.el"))
(load-file (concat emacs-root "my_lisp/fc-eval-and-replace.el"))
(load-file (concat emacs-root "my_lisp/gpl.el"))


;********* Darcsum *********
(load-file (concat emacs-root "lisp/darcsum.el"))
(load-file (concat emacs-root "my_lisp/darcsum-custom.el"))


;********* Gnuplot mode *********
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))


;;********* Tuared Mode *********

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

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

;;******** Global-sexp highlighting *********
(require 'hl-sexp)
(global-hl-sexp-mode)

;;******** Show parenthesis ********
(show-paren-mode t)

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
(ediff-toggle-multiframe) ;;Don't make *Ediff Control Panel* open in new frame, just new window. 

;;******** windmove ********
(when (fboundp 'windmove-default-keybindings) ;;Turns on windmove mode: shift+arrow keys move between windows.
  (windmove-default-keybindings))

;;******** make scripts executable ********
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;******** Finally, set key bindings ********
(load-file (concat emacs-root "keys.el"))