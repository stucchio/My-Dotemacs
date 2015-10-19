(defvar stucchio-org-papers-directory "~/org/papers/")

(defun stucchio-org-humanize-filename (filename title)
  (let ((title-underscorified (replace-regexp-in-string "\\W+" "_" title))
        (fn (file-name-nondirectory filename))
        )
    (concat stucchio-org-papers-directory title-underscorified "__" fn)
    )
  )

(defun stucchio-org-move-to-papers (title)
  "Moves a file from anywhere into my org-mode papers directory. Specific location is determined by the stucchio-org-papers-directory variable."
  (interactive "sTitle of paper:")
  (let ((source-file (read-file-name nil))
        (cleaned-title (replace-regexp-in-string "\\W+" "_" title)))
    (let ((dest-file (stucchio-org-humanize-filename source-file title)))
      (rename-file source-file dest-file)
      (org-insert-link nil dest-file cleaned-title)
      (message (concat "Moved " source-file " to " dest-file))
      )
    )
  )

(defun stucchio-org-get-for-papers (title url)
  "Pulls a file from the web  into my org-mode papers directory. Specific location is determined by the stucchio-org-papers-directory variable."
  (interactive "sTitle of paper: \nsURL: ")

  (let ((cleaned-title (replace-regexp-in-string "\n+" " " title))
        (filename (file-name-nondirectory (url-filename (url-generic-parse-url url))))
        )
    (let ((dest-file (stucchio-org-humanize-filename filename title)))
      (url-copy-file url dest-file)
      (org-insert-link nil dest-file cleaned-title)
      (org-set-property "original-source" url)
      (message (concat "Retrieved document " cleaned-title " from " url ", stored at " dest-file))
      )
    )
  )