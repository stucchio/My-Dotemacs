;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is written by Chris Stucchio, and the latest version is available at
;;
;; http://cims.nyu.edu/~stucchio
;;
;; This file lets you create a ring of window configurations and cycle between them.

(defvar window-config-ring-ring (make-ring 100) "This ring stores a ring of window configurations." )
(defvar window-config-ring-index 0 )
(defvar last-window-configuration nil)

;;;###autoload
(defun window-config-ring-store (&optional ignored)
  "Save the current window configuration to the window config ring."
  (interactive "p")
  (progn
    (ring-insert window-config-ring-ring (current-window-configuration))
    (message (concat "Stored current window configuration at index " (number-to-string (ring-length window-config-ring-ring))))
    )
)

;;;###autoload
(defun window-config-ring-next (&optional ignored)
  "Jump to the next element in the window config ring. If that element is an invalid window configuration, delete it from the ring."
  (interactive "p")
  (if (not (ring-empty-p window-config-ring-ring))
      (progn
	(if (compare-window-configurations (current-window-configuration) (ring-ref window-config-ring-ring window-config-ring-index))
	    (setq window-config-ring-index (% (+ window-config-ring-index 1) (ring-length window-config-ring-ring)))
	  ) ; Increment the window config ring index, if possible
	(set-window-configuration (ring-ref window-config-ring-ring window-config-ring-index)) ; Set to that window configuration

	(if (not (compare-window-configurations (current-window-configuration) (ring-ref window-config-ring-ring window-config-ring-index)))
	    (progn
	      (ring-remove window-config-ring-ring window-config-ring-index)
	      (message "Invalid window configuration. Removing from window-config ring.")
	      )
	  (message (concat "Window configuration " (number-to-string window-config-ring-index) ))
	  )
	)
    (message "Empty window configuration ring."))
  )

;;;###autoload
(defun window-config-ring-remove (&optional ignored)
  "Remove the current window configuration from the config ring if current config ring position agrees with current window configuration."
  (interactive "p")
  (if (not (ring-empty-p window-config-ring-ring))
      (if (compare-window-configurations (current-window-configuration) (ring-ref window-config-ring-ring window-config-ring-index))
	  (progn
	    (ring-remove window-config-ring-ring window-config-ring-index)
	    (message (concat "Popped ring " (number-to-string window-config-ring-index) " from window config ring.")))
	(message "Cannot remove: window Configuration changed since last call to window-config-ring-next.")
	)
    (message "Empty window configuration ring.")
    )
  )

(provide 'window-config-ring)