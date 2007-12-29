;;; regex-tool --- A regular expression evaluation tool for programmers

;; Copyright (C) 2007 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Created: 29 Oct 2007
;; Modified: 17 Nov 2007
;; Version: 1.2
;; Keywords: regex languages programming development
;; X-URL: http://www.newartisans.com/

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This program currently uses frames only.
;;
;; After you type M-x regex-tool, you will see three buffers: *Regex*, *Text*
;; and *Groups*.  The *Regex* buffer contains your regular expression.  By
;; default, this tool uses Emacs regular expressions.  If you customize the
;; variable `regex-tool-backend', you can switch to using full Perl regular
;; expressions.
;;
;; The *Text* buffer contains the sample text you want to match against.
;; Change this however you like.
;;
;; The *Groups* buffer will list out any regular expression groups that match.
;; Your regular expression is searched for as many times as it appears in the
;; buffer, and any groups that match will be repeated.
;;
;; The results are updated as you type in either the *Regex* or *Text* buffer.
;; Use C-c C-c to force an update.  Use C-c C-k to quit all the regex-tool
;; buffers and remove the frame.

;;; Version History:

;; 1.1 - Don't die horribly if the user simply types '^' or '$'
;; 1.2 - Include cl.el at compile time

(eval-when-compile
  (require 'cl))

(defgroup regex-tool nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'programming)

(defvar regex-tool-mode-map (make-sparse-keymap))
(defvar regex-tool-mode-abbrev-table)
(defvar regex-tool-old-window-configuration-temp nil)

(define-derived-mode regex-tool-mode text-mode "Regex Tool"
  "This is regex-tool mode."
  (define-key regex-tool-mode-map [(control ?c) (control ?c)]
    'regex-tool-markup-text)
  (define-key regex-tool-mode-map [(control ?c) (control ?k)]
    'regex-tool-quit)
  (define-key regex-tool-mode-map [(control ?c) (control ?i)]
    'regex-tool-insert-old-buffer-contents)
  (define-key regex-tool-mode-map [(control ?c) (control ?n)]
    'regex-tool-next-match)
  (define-key regex-tool-mode-map [(control ?c) (control ?p)]
    'regex-tool-prev-match)
  (add-hook 'after-change-functions 'regex-tool-markup-text nil t))

(defface regex-tool-matched-face
  '((((background light)) (:foreground "Red" :bold t))
    (((background dark)) (:foreground "Orange" :bold t)))
  ""
  :group 'regex-tool)

(defcustom regex-tool-backend 'emacs
  "The backend used to process regular expressions.
The `emacs' backend handles regular expressions directly.
The `perl' backend talks to a perl subprocess to do the handling.\"
"
  :type '(choice
	  (const :tag "Emacs" emacs)
	  (const :tag "Perl" perl))
  :group 'regex-tool)

(defcustom regex-tool-new-frame t
  "*Non-nil means that regex-tool will be opened in a new frame. nil means that regex-tool will be opened in current frame."
  :type 'boolean
  :group 'regex-tool)

(defun regex-render-perl (regex sample)
  (with-temp-buffer
    (insert (format "@lines = <DATA>;
$line = join(\" \", @lines);
print \"(\";
while ($line =~ m/%s/mg) {
  print \"(\", length($`), \" \", length($&), \" \";
  for $i (1 .. 20) {
    if ($$i) {
      print \"(\", $i, \" . \\\"\", $$i, \"\\\") \";
    }
  }
  print \")\";
}
print \")\";
__DATA__
%s" regex sample))
   (call-process-region (point-min) (point-max) "perl" t t)
   (goto-char (point-min))
   (read (current-buffer))))

(defvar regex-expr-buffer nil)
(defvar regex-text-buffer nil)
(defvar regex-group-buffer nil)
(defvar regex-old-buffer nil)

(defun regex-tool-create-work-area ()
  "Create the regex-tool work area. If regex-tool-new-frame is nil, use current frame, otherwise create a new frame."
  (progn
    (setq regex-old-buffer (current-buffer))
    (if regex-tool-new-frame
	(select-frame (make-frame-command)) ;;Make a new frame
      (progn ;;Or, use current frame and build new window configuration
	(setq regex-tool-window-configuration-temp (list (current-window-configuration)
							 (point-marker))) ;; Remember current window configuration
	(delete-other-windows) ;;Now setup regex-tool window configuration
	)
      )
    (split-window-vertically)
    (split-window-vertically)
    (balance-windows)
    )
  )

(defun regex-tool-destroy-work-area ()
  "Destroys the regex-tool work area, since we are done with it now."
  (progn ;; First kill the regex-tool buffers.
    (if regex-tool-new-frame
	(progn  ;;If we created a new frame, destroy it.
	  (delete-frame)
	  )
      (progn ;; If we used the current frame, restore the old window configuration (if possible).
	(if regex-tool-window-configuration-temp
	    (progn
	      (set-window-configuration (car regex-tool-window-configuration-temp))
	      (goto-char (cadr regex-tool-window-configuration-temp)))
	  (error "No window configuration to restore."))
	)
      )
    (kill-buffer regex-expr-buffer)
    (kill-buffer regex-text-buffer)
    (kill-buffer regex-group-buffer)
    )
  )

(defun regex-tool ()
  (interactive)
  (regex-tool-create-work-area)
  (setq regex-expr-buffer (get-buffer-create "*Regex*"))
  (switch-to-buffer regex-expr-buffer)
  (regex-tool-mode)
  (other-window 1)
  (setq regex-text-buffer (get-buffer-create "*Text*"))
  (switch-to-buffer regex-text-buffer)
  (goto-char (point-min))
  (if (eolp)
      (insert "Hello, this is text your regular expression will match against."))
  (regex-tool-mode)
  (other-window 1)
  (setq regex-group-buffer (get-buffer-create "*Groups*"))
  (switch-to-buffer regex-group-buffer)
  (other-window 1))

(defun regex-tool-current-regex ()
  "A utility function which returns the current regex."
  (with-current-buffer regex-expr-buffer
    (buffer-string))
  )

(defun regex-tool-markup-text (&optional beg end len)
  (interactive)
  (let ((regex (with-current-buffer regex-expr-buffer
		 (buffer-string)))
	previous-point)
    (when (> (length regex) 0)
      (with-current-buffer regex-group-buffer
	(erase-buffer))
      (with-current-buffer regex-text-buffer
	(remove-overlays)
	(save-excursion
	  (ignore-errors
	    (goto-char (point-min))
	    (if (eq regex-tool-backend 'emacs)
		(while (and (setq previous-point (point))
			    (re-search-forward regex nil t))
		  (if (= (point) previous-point)
		      (forward-char 1)
		    (overlay-put (make-overlay (match-beginning 0)
					       (match-end 0))
				 'face 'regex-tool-matched-face)
		    (dotimes (i 10)
		      (let ((text (match-string i)))
			(if text
			    (save-match-data
			      (with-current-buffer regex-group-buffer
				(goto-char (point-max))
				(insert (format "Group %d: '%s'\n" i text)))))))
		    (with-current-buffer regex-group-buffer
		      (insert ?\n))))
	      (let ((results (regex-render-perl regex (buffer-string))))
		(dolist (result results)
		  (let ((offset (nth 0 result))
			(length (nth 1 result))
			(matches (nthcdr 2 result)))
		    (overlay-put (make-overlay (1+ offset) (+ offset length 1))
				 'face 'regex-tool-matched-face)
		    (let ((match-zero (buffer-substring (1+ offset)
							(+ offset length 1))))
		      (with-current-buffer regex-group-buffer
			(insert (format "Group 0: '%s'\n" match-zero))))
		    (dolist (match matches)
		      (with-current-buffer regex-group-buffer
			(goto-char (point-max))
			(insert (format "Group %d: '%s'\n" (car match)
					(cdr match)))))
		    (with-current-buffer regex-group-buffer
		      (insert ?\n)))))))))
      (with-current-buffer regex-group-buffer
	(goto-char (point-min))))))

(defun regex-tool-next-match ()
  "Sends the point in regex-text-buffer to the next match."
  (interactive)
  (with-selected-window (get-buffer-window regex-text-buffer)
    (progn
      (select-window (get-buffer-window regex-text-buffer))
      (re-search-forward (regex-tool-current-regex) nil t)
      )
    )
  )

(defun regex-tool-prev-match ()
  "Sends the point in regex-text-buffer to the next match."
  (interactive)
  (with-selected-window (get-buffer-window regex-text-buffer)
    (progn
      (select-window (get-buffer-window regex-text-buffer))
      (re-search-backward (regex-tool-current-regex) nil t)
      )
    )
  )

(defun regex-tool-insert-old-buffer-contents ()
  "Inserts the contents of whatever buffer we were in before calling regex tool into regex-text-buffer."
  (interactive)
  (with-current-buffer regex-text-buffer
    (progn
      (end-of-buffer)
      (newline)
      (insert-buffer regex-old-buffer)
      )
    )
  )

(defun regex-tool-quit ()
  (interactive)
  (regex-tool-destroy-work-area)
  )

(provide 'regex-tool)

;; regex-tool.el ends here
