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

;;Load my basic customizations

;;********* st-shell stuff
(require 'st-shell)


;;******** Basic configurations ********
(transient-mark-mode t)

(load-file (concat emacs-root "local-customizations.el"))

(custom-set-variables
 '(LaTeX-command "latex -src")
 '(LaTeX-enable-toolbar nil)
 '(TeX-electric-sub-and-superscript t)
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi -bg white -fg black -editor \"gnuclient -q +%l %f\" -offsets -0.6in -expert -s 7 -geometry +0+0 -paper 16x24%dS %d") ("^pdf$" "." "xpdf %o") ("^html?$" "." "netscape %o"))))
 '(TeX-view-style (quote (("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^landscape$" "%(o?)xdvi %dS -paper a4r -s 0 %d") ("." "%(o?)xdvi -editor \"gnuclient -q +%l %f\" %dS %d"))))
  '(paren-mode (quote sexp) nil (paren))
 '(lazy-lock-mode nil nil (lazy-lock))
 '(column-number-mode t)
 '(row-number-mode t)
 '(st-shell-shell-function '(ansi-term (getenv "SHELL")))
 '(gnuserv-frame t)
 '(gnuserv-visit-hook (lambda () (raise-frame) (recenter)))
)

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

(custom-set-variables
 '(uniquify-buffer-name-style (quote forward))
)

;;********* icomplete *********
(require 'icomplete)
(icomplete-mode)

;Load bm.el, bookmarking facility.
(load "bm")

;;********* ido *********
;; This mode makes the minibuffer behave very nicely with regards to autocompletion
;; when looking for files or buffers. Just type a substring, it will find buffers 
;; associated to it. Makes a nice list of completions in the minibuffer, as opposed
;; to irritating completion buffer.
;(require 'ido)
;(ido-mode t)
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
(load-file (concat emacs-root "my_lisp/reftex-custom.el"))
(require 'bibtex)
(add-hook 'LaTeX-mode-hook (lambda () (ispell-minor-mode) ))
(add-hook 'LaTeX-mode-hook (lambda () (load "reftex-custom")))

;Load misc stuff
(load-file (concat emacs-root "my_lisp/misc.el"))
(load-file (concat emacs-root "my_lisp/fc-eval-and-replace.el"))


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

(load-file (concat emacs-root "keys.el"))

;;******** Haskell Mode *********
(load (concat emacs-root "site-lisp/haskell-mode-2.1/haskell-site-file.el"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;******** Global-sexp highlighting *********
(require 'hl-sexp)
(global-hl-sexp-mode)