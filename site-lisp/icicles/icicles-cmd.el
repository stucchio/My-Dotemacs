;;; icicles-cmd.el --- Top-level commands for Icicles
;;
;; Filename: icicles-cmd.el
;; Description: Top-level commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sun Apr 22 15:16:25 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 9927
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-cmd.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `color-theme',
;;   `cus-face', `custom', `dired', `dired+', `dired-aux', `dired-x',
;;   `easymenu', `ediff-diff', `ediff-help', `ediff-init',
;;   `ediff-merg', `ediff-mult', `ediff-util', `ediff-wind', `ffap',
;;   `ffap-', `fit-frame', `frame-cmds', `frame-fns', `hexrgb',
;;   `icicles-fn', `icicles-mcmd', `icicles-mode', `icicles-opt',
;;   `icicles-var', `info', `info+', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `pp', `pp+', `strings', `thingatpt',
;;   `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  top-level commands (and a few non-interactive functions used in
;;  those commands).  For commands to be used mainly in the minibuffer
;;  or buffer *Completions*, see `icicles-mcmd.el'.  For Icicles
;;  documentation, see `icicles.el' .
;;
;;  Commands defined here - (+) means a multi-command:
;;
;;    (+)`icicle-add-buffer-candidate', (+)`icicle-add-buffer-config',
;;    `icicle-add-candidate-to-saved-completion-set',
;;    `icicle-add-region', `icicle-apropos', `icicle-apropos-command',
;;    `icicle-apropos-function', `icicle-apropos-option',
;;    `icicle-apropos-variable', `icicle-apropos-zippy',
;;    (+)`icicle-bookmark', (+)`icicle-buffer',
;;    (+)`icicle-buffer-config', (+)`icicle-buffer-list',
;;    (+)`icicle-buffer-other-window', (+)`icicle-clear-option',
;;    (+)`icicle-color-theme', (+)`icicle-comint-command',
;;    (+)`icicle-comint-search', (+)`icicle-compilation-search',
;;    (+)`icicle-complete-keys', `icicle-complete-thesaurus-entry',
;;    `icicle-customize-apropos', `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    `icicle-customize-icicles-group', `icicle-dabbrev-completion',
;;    (+)`icicle-delete-file', (+)`icicle-delete-window',
;;    (+)`icicle-delete-windows',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window',
;;    (+)`icicle-doc', (+)`icicle-exchange-point-and-mark',
;;    (+)`icicle-execute-extended-command',
;;    (+)`icicle-execute-named-keyboard-macro', (+)`icicle-find-file',
;;    (+)`icicle-find-file-other-window', (+)`icicle-font',
;;    (+)`icicle-frame-bg', (+)`icicle-frame-fg', (+)`icicle-fundoc',
;;    (+)`icicle-generic-S-tab', (+)`icicle-goto-global-marker',
;;    (+)`icicle-goto-marker', (+)`icicle-imenu',
;;    (+)`icicle-Info-goto-node', (+)`icicle-Info-goto-node-cmd',
;;    (+)`icicle-Info-index', (+)`icicle-Info-index-20',
;;    (+)`icicle-Info-index-cmd', `icicle-insert-char',
;;    (+)`icicle-insert-kill', (+)`icicle-insert-thesaurus-entry',
;;    (+)`icicle-kill-buffer', (+)`icicle-kmacro',
;;    `icicle-lisp-complete-symbol', (+)`icicle-locate-file',
;;    (+)`icicle-locate-file-other-window', (+)`icicle-map',
;;    (+)`icicle-object-action', (+)`icicle-occur',
;;    (+)`icicle-other-window-or-frame', (+)`icicle-plist',
;;    `icicle-read-color', `icicle-read-kbd-macro',
;;    (+)`icicle-recent-file', (+)`icicle-recent-file-other-window',
;;    `icicle-region-open-all-files',
;;    (+)`icicle-remove-all-regions-in-buffer',
;;    (+)`icicle-remove-buffer-candidate',
;;    (+)`icicle-remove-buffer-config',
;;    `icicle-remove-candidate-from-saved-completion-set',
;;    (+)`icicle-remove-region',
;;    (+)`icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command',
;;    (+)`icicle-reset-option-to-nil',
;;    `icicle-save-string-to-variable', (+)`icicle-search',
;;    (+)`icicle-search-generic', `icicle-search-highlight-cleanup',
;;    (+)`icicle-search-region', (+)`icicle-search-text-property',
;;    (+)`icicle-select-frame', (+)`icicle-select-region',
;;    (+)`icicle-select-window', `icicle-send-bug-report',
;;    (+)`icicle-set-option-to-t', (+)`icicle-toggle-option',
;;    (+)`icicle-vardoc', (+)`icicle-yank-insert'.
;;
;;  Non-interactive functions defined here:
;;
;;    `custom-variable-p', `icicle-add-key+cmd',
;;    `icicle-binary-option-p', `icicle-choose-candidate-of-type',
;;    `icicle-comint-get-final-choice',
;;    `icicle-comint-get-minibuffer-input',
;;    `icicle-comint-send-input', `icicle-complete-keys-1',
;;    `icicle-complete-keys-action',
;;    `icicle-delete-file-or-directory',
;;    `icicle-delete-region-from-alist', `icicle-edmacro-parse-keys',
;;    `icicle-execute-extended-command-1',
;;    `icicle-find-file-other-window-w-wildcards',
;;    `icicle-find-file-w-wildcards', `icicle-imenu-in-buffer-p',
;;    `icicle-Info-goto-node-action', `icicle-Info-index-action',
;;    `icicle-insert-for-yank',
;;    `icicle-insert-thesaurus-entry-cand-fn',
;;    `icicle-keys+cmds-w-prefix', `icicle-kill-a-buffer',
;;    `icicle-kill-a-buffer-and-update-completions',
;;    `icicle-kmacro-action', `icicle-make-color-candidate',
;;    `icicle-map-action', `icicle-marker+text', `icicle-markers',
;;    `icicle-non-whitespace-string-p',
;;    `icicle-read-from-minibuf-nil-default',
;;    `icicle-read-single-key-description',
;;    `icicle-read-var-value-satisfying', `icicle-region-add-buffers',
;;    `icicle-region-help', `icicle-region-or-buffer-limits',
;;    `icicle-region-sorted', `icicle-remove-all-regions-action',
;;    `icicle-remove-buffer-candidate-action',
;;    `icicle-remove-buffer-config-action',
;;    `icicle-remove-color-duplicates', `icicle-search-action',
;;    `icicle-search-help',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-search-highlight-input-matches-here',
;;    `icicle-search-read-context-regexp',
;;    `icicle-search-regexp-scan', `icicle-search-region-action',
;;    `icicle-select-region-action',
;;    `icicle-search-replace-candidate',
;;    `icicle-search-replace-fixed-case-p',
;;    `icicle-search-replace-match',
;;    `icicle-search-replace-search-hit',
;;    `icicle-search-text-property-scan',
;;    `icicle-text-properties-in-buffer',
;;    `icicle-text-properties-in-buffers',
;;    `icicle-this-command-keys-prefix'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-complete-keys-alist'.
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED HERE:
;;
;;  `dabbrev-completion' - Use Icicles completion when you repeat
;;                         (`C-M-/').
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects *Completions* window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;
;;  ***** NOTE: The following functions defined in `cus-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `customize-apropos', `customize-apropos-faces',
;;  `customize-apropos-groups', `customize-apropos-options' -
;;     Use `completing-read' to read the regexp.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in `icicles.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Icicles multi-commands")
;;  (@> "Other top-level Icicles commands")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/04/22 dadams
;;     icicle-search: Allow for pre-bound icicle-candidate(-alternative)-action-fn.
;;     icicle-search-replace-search-hit:
;;       Call icicle-candidate-action-fn, not necessarily icicle-search-action.
;; 2007/04/20 dadams
;;     Added: icicle-search-highlight-context-levels.  Use in icicle-search-action.
;;     icicle-search-highlight-cleanup: Delete icicle-search-level-overlays.
;;     icicle-search: Set icicle-search-context-regexp to nil if scan-fn-or-regexp is not a string.
;;     icicle-search-replace-fixed-case-p: Return nil if arg is nil.
;;     icicle-search-read-context-regexp: Use default with read-number, and protect it with fboundp.
;;     Added soft require of strings.el.
;; 2007/04/18 dadams
;;     Added: icicle-search-replace-fixed-case-p.  Use in icicle-search-action.
;;     icicle-search-action:
;;       Set match data to region when icicle-search-replace-whole-candidate-flag is t.
;;       Don't search for icicle-search-context-regexp.
;;     icicle-search-replace-match: Added fixedcase arg.  Use icicle-search-replace-literally-flag.
;;     Use replace-match-maybe-edit (Emacs 22), not replace-match, and save and restore stuff.
;;     icicle-search-replace-search-hit:
;;       Use regexp-history in read-string.  Use icicle-search-define-replacement.
;; 2007/04/17 dadams
;;     Added: icicle-search-replace-match.  Treat Emacs < 22 also.  Use in icicle-search-action.
;;     icicle-search-replace-search-hit: Use regexp-history in read-string.
;; 2007/04/16 dadams
;;     icicle-search-action: Use replace-count.
;;     icicle-search: Initialize replace-count to 0.
;; 2007/04/15 dadams
;;     icicle-search-action: Allow \,... etc. replacement.
;;       Use query-replace-compile-replacement and replace-match, with error treatment.
;;       Removed unwind-protect.  Removed %s from error-msg return.
;;     icicle-search: Save search string as icicle-search-context-regexp.
;;                    Use "%s" for error message in failure error call.
;;                    Updated doc string for lisp-eval replacement etc.
;;     *(-all)-input-matches(-here): save-match-data around input matching.
;;     icicle-search-highlight-input-matches-here: Added not eobp to loop test.
;; 2007/04/13 dadams
;;     icicle-search-highlight-input-matches-here: Bound free var ov.
;; 2007/04/10 dadams
;;     icicle-search-regexp-scan: Use match indicated by icicle-search-context-level as context.
;;     Added: icicle-search-read-context-regexp.
;;     icicle-search(-region|-regexp-scan): Use icicle-search-read-context-regexp.
;; 2007/04/08 dadams
;;     icicle-map-action, icicle-search-action:
;;       Return nil for success, error msg for failure (it was reversed).
;;       Use icicle-highlight-candidate-in-Completions.
;;     icicle-map-action: Moved minibuffer frame selection to unwind-protect final clause.
;;     icicle-search: Give focus to original frame, in unwinding clause (C-g).
;;     Added: icicle-search-highlight-input-matches-here.
;;     icicle-search-highlight-all-input-matches: Highlight all input matches in candidate.
;;     icicle-search-replace-search-hit:
;;       Use 0 if icicle-candidate-nb is nil.  Display completions.
;;       Save and restore: icicle-last-completion-command, icicle-last-input.
;;       Prevent using same string as candidate for replacement.
;;     icicle-search-action: Rewrote.  Enable repeated replacment of input matches.
;;       Save and restore icicle-candidate-nb.  Wrap it around if at end.
;;       Warn about empty input for whole-candidate replacement.
;;       Update icicle-last-completion-candidate.  Display completions.
;; 2007/04/07 dadams
;;     icicle-search: Added query-replace functionality.
;;       Navigate first, then act (C-next).
;;       Give focus to last frame searched.
;;       Bind: icicle-candidate-alternative-action-fn, icicle-inhibit-sort-p, icicle-searching-p,
;;             icicle-expand-input-to-common-match-flag.
;;       Bind icicle-buffer-require-match-flag to partial-match-ok in interactive spec.
;;     icicle-search-action:
;;       Added optional replace arg: if non-nil, then fn becomes a replace action.
;;     Added: icicle-search-replace-search-hit, icicle-search-replace-candidate, 
;;     icicle-buffer-list: Bind all the stuff that is bound in icicle-buffer.
;; 2007/04/06 dadams
;;     icicle-execute-extended-command-1:
;;       Bind this-command, but only around call to cmd - don't set it.
;; 2007/04/02 dadams
;;     Added: icicle-search-text-property, icicle-search-text-property-scan,
;;            icicle-text-properties-in-buffer(s).
;;     icicle-search:
;;       Added &rest args.  Updated doc string.  Don't read regexp or reverse alist here.
;;       Use icicle-search-define-candidates, not icicle-search-regexp-scan.  
;;     icicle-search-regexp-scan: Read regexp here.  Reverse alist here.
;;     Moved to icicles-fn.el: icicle-filter-alist, icicle-first-matching-candidate.
;;     Renamed: icicle-search-region-beg-end to icicle-region-or-buffer-limits.
;; 2007/04/01 dadams
;;     icicle-repeat-complex-command:
;;       Remove duplicates and sort entries, but only for reading command to eval.
;; 2007/03/31 dadams
;;     icicle-lisp-complete-symbol: Bind icicle-top-level-when-sole-completion-flag to t.
;; 2007/03/27 dadams
;;     icicle-search: Hard-code C-next key in message.
;;     icicle-search-regexp-scan: Initialize last-beg for first time.
;; 2007/03/23 dadams
;;     icicle-execute-extended-command-1:
;;       Don't restore last-command.  Run (pre|post)-command-hook.  Set, don't bind, this-command.
;;       enable-recursive-minibuffers for interactive call of cmd.
;;     icicle-execute-extended-command, icicle-execute-named-keyboard-macro:
;;       Restore last-command at end.
;; 2007/03/20 dadams
;;     icicle-execute-extended-command-1: When wrong-number-of-arguments, icicle-help-on-candidate.
;; 2007/03/14 dadams
;;     icicle-dired-saved-file-candidates, icicle-buffer, icicle-(find|locate|recent)-file:
;;       Put 200 as value of property icicle-Completions-window-max-height.
;;     Added ;;;###autoload for icicle-define*.
;; 2007/03/09 dadams
;;     icicle-search, icicle-map, icicle-(remove|search|select)-region, icicle-imenu,
;;     icicle-occur, icicle-compilation-search:
;;       Bound icicle-transform-function to nil if interactive-p.
;;     icicle-comint-search: Updated doc string to mention false positives in command matching.
;;     Removed eval-when-compile from requires of Icicles libraries (except icicle-mac.el).
;; 2007/03/08 dadams
;;     icicle-insert-kill: Bound icicle-delete-candidate-object.
;; 2007/03/07 dadams
;;     icicle-delete-windows, icicle-map-action, icicle-search-action,
;;     icicle-choose-candidate-of-type, icicle-complete-keys-help:
;;       Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/06 dadams
;;     icicle-search, icicle-map, icicle-(remove|search|select)-region:
;;       Bind icicle-inhibit-sort-p to non-nil to prevent user sorting.
;;       Update doc string to mention that you cannot sort candidates.
;;     icicle-(remove|search|select)-region: Sort candidates by buffer and then by tag.
;;     icicle-search-region: Bound icicle-delete-candidate-object.
;;     icicle-search, icicle-map: Don't add or subtract one from candidate # if use action fn.
;;     icicle-search:
;;       If require-match, set icicle-completion-candidates and marker to reflect final choice.
;;     Renamed: icicle-get-current-candidate to icicle-get-alist-candidate.
;;     Added: icicle-region-sorted, icicle-region-add-buffers.
;; 2007/03/04 dadams
;;     icicle-get-current-candidate: Return nil if icicle-candidates-alist is nil.
;; 2007/03/02 dadams
;;     icicle-remove-buffer-config, icicle-remove-buffer-candidate: Removed extraneous quote.
;;     icicle-(find|recent)-file(-other-window): Bound icicle-delete-candidate-object.
;; 2007/02/28 dadams
;;     icicle-buffer-list, icicle-color-theme: Bound icicle-delete-candidate-object.
;; 2007/02/27 dadams
;;     icicle-recent-file(-other-window): Changed INITIAL-INPUT completing-read arg to nil.
;; 2007/02/25 dadams
;;     Added: icicle-remove-buffer-candidate-action, icicle-remove-buffer-config-action.
;;     icicle-add-buffer-candidate, icicle-bookmark, icicle-buffer-config:
;;       Bound icicle-delete-candidate-object.
;; 2007/02/24 dadams
;;     Added: icicle-kill-a-buffer-and-update-completions (was kill-a-buffer), 
;;            icicle-delete-region-from-alist.
;;     icicle-delete-window: Use icicle-kill-a-buffer...-completions, not icicle-kill-a-buffer.
;;     icicle-buffer(-other-window), icicle-choose-candidate-of-type:
;;       Bind icicle-delete-candidate-object to icicle-kill-a-buffer.
;;       Bind icicle-sort-function to icicle-sort-function if icicle-buffer-sort-function is nil.
;;     icicle-select-region:
;;       Bind icicle-delete-candidate-object to icicle-delete-region-from-alist.
;;     icicle-remove-region: Rewrote.
;;       Use icicle-delete-region-from-alist.
;;       Use icicle-remove-candidate-display-others, but don't redisplay unless completing.
;;       Respect icicle-add-buffer-name-flag (append buffer names).
;; 2007/02/20 dadams
;;     Added: icicle-search-region-action.  Open file associated with candidate.
;;     icicle-search-region:
;;       Use icicle-search-region-action.  Updated doc string.  Bind icicle-list-*.
;;       Fix default completing-read value.  Respect icicle-add-buffer-name-flag.
;;     icicle-select-region-action: Open file associated with candidate.
;;     icicle-region-open-all-files: Ignore files that are not readable.
;;     icicle-regions: Remove only non-existent buffers that are not associated with files.
;; 2007/02/19 dadams
;;     Added: icicle-region-open-all-files.
;;     icicle-(search|select)-region, icicle-search: Use icicle-region-open-all-files.
;;     icicle-add-region: Add file name to region entry in alist.
;;     icicle-select-region-action, icicle-region-help, icicle-search:
;;       Updated selectors for point and mark, because of addition of file name.
;;     icicle-region-help: Add file name to help message.
;; 2007/02/18 dadams
;;     Added: icicle-first-matching-candidate.
;;     icicle-get-current-candidate: Use icicle-first-matching-candidate, not assoc.
;; 2007/02/07 dadams
;;     icicle-search-action: Make *Completions* window dedicated.  Thx to Peter Povinec.
;; 2007/02/07 dadams
;;     icicle-search: pop-to-buffer and raise frame. Don't select orig-window.
;; 2007/02/06 dadams
;;     icicle-search, icicle-select-region, icicle-search-regexp-scan:
;;       Respect icicle-add-buffer-name-flag.
;;     icicle-search: Bound icicle-show-Completions-initially-flag to t.
;;                    Bound icicle-candidate-help-fn to icicle-search-help.
;;     icicle-search-regexp-scan: nil BUFFER arg means current buffer now.
;;     icicle-search-action, icicle-filter-alist: Treat multi-completion case also.
;;     Added: icicle-search-help.
;;     icicle-map-action, icicle-search-action: Removed unused local var, curr-cand-string.
;;     icicle-select-region, icicle-remove-region, icicle-search-region:
;;       Use icicle-candidate-alist, not regions-w-buffers (but bind it locally too).
;;     Renamed: icicle-get-current-region-candidate to icicle-get-current-candidate.
;;     icicle-region-help: Provide region limits also.
;;     Added note to some doc strings that the command is for Icicle mode only.
;; 2007/02/02 dadams
;;     icicle-search: Test for buffers, not buffer names, in WHERE.  Thx to Peter Povinec.
;;     icicle-buffer-list: Return the list of buffers.
;; 2007/01/28 dadams
;;     icicle-complete-keys:
;;       Bind icicle-sort-functions-alist, using icicle-command-names-alphabetic-p.
;; 2007/01/22 dadams
;;     icicle-read-color:
;;       Removed free var NTH-PARTS.
;;       Bound icicle-list-nth-parts-join-string the same as icicle-list-join-string.
;;       Remove duplicates: AliceBlue and alice blue etc.: downcase and remove whitespace.
;;     Added: icicle-remove-color-duplicates.
;;     Added soft require of hexrgb.el.
;;     icicle-*doc, icicle-plist: Bound icicle-candidate-properties-alist.
;; 2007/01/21 dadams
;;     Added: icicle-read-color, icicle-make-color-candidate.
;; 2007/01/18 dadams
;;     Added: icicle-get-current-region-candidate.
;;       Use current cand if cycled or `mouse-2'.  Else if single match, use that.  Else error.
;;     icicle-remove-region, icicle-search-region, icicle-region-help:
;;       Use icicle-get-current-region-candidate.
;;     icicle-add-region: Added optional TAG arg and prefix arg treatment.
;;     icicle-remove-region: Update completions list.  Bind regions-w-buffers.
;;     icicle-remove-all-regions-in-buffer: Use current buffer name as default.
;;     Added: icicle-select-region-action.
;;     icicle-select-region: Use icicle-select-region-action.
;;     Renamed: option icicle-regions to icicle-region-alist.
;;     icicle-regions: Sort entries.
;; 2007/01/17 dadams
;;    icicle-filter-alist: Reversed argument order.  Return alist arg if filter-keys arg is empty.
;; 2007/01/12 dadams
;;    icicle-delete-window: Do icicle-remove-Completions-window if in minibuffer.
;;    icicle-yank-insert: Do icicle-yank if in minibuffer.
;;    icicle-(fundoc|vardoc|doc|plist): Added condition-case to protect symbols that raise errors.
;; 2007/01/01 dadams
;;    Added: icicle-(add|remove)-candidate-(to|from)-saved-completion-set.
;;    icicle-add-buffer-config: Use nil, not "nil" as default, if icicle-buffer-sort is nil.
;;                              Use icicle-define-add-to-alist-command to define it.
;;    icicle-remove-buffer-config, icicle-remove-saved-completion-set:
;;      Use icicle-assoc-delete-all, not delete of assoc.
;;    icicle-remove-saved-completion-set: Update display after removal.
;;    Reformatted icicle-define(-file)-command, based on setup.el's lisp-indentation-hack.
;; 2006/12/25 dadams
;;    Bug fix: icicle-search-action: Use icicle-filter-alist on icicle-candidates-alist.
;;    icicle-(select|search)-region: Use pop-to-buffer and raise-frame, not set-buffer.
;;    icicle-select-region: Activate the region.
;; 2006/12/23 dadams
;;    Added: icicle-region-help.  Use in icicle-*-region.
;;    Added: icicle-remove-all-regions-in-buffer, icicle-remove-all-regions-action.
;;    icicle-(select|search)-region: Ignore regions in non-existent buffers.
;;    icicle-remove-region: Update the persistent value of icicle-regions.
;; 2006/12/22 dadams
;;    Added: icicle-exchange-point-and-mark.
;;    icicle-customize-icicles-group: icicles -> Icicles (group name).
;; 2006/12/18 dadams
;;    icicle-object-action: Remove print arg.  icicle-apply-to-* uses current-prefix-arg now.
;; 2006/12/17 dadams
;;    Added: icicle-object-action, icicle-choose-candidate-of-type,
;;           icicle-read-var-value-satisfying.
;; 2006/12/16 dadams
;;    icicle-map-action: Bug fix: Use icicle-candidate-nb, not assoc.
;;    Added: icicle-goto(-global)-marker, icicle-marker+text, icicle-markers.
;; 2006/12/10 dadams
;;    Moved minibuffer and *Completions* commands to new file, icicles-mcmd.el.
;;    Require icicles-opt.el.
;;    icicle-buffer-list: Added final message.
;; 2006/11/26 dadams
;;    icicle-search-action: Bug fix: Use icicle-candidate-nb, not assoc, to get cand+mrker.
;;    icicle-*-complete-1: Bug fix: Don't set icicle-current-input to icicle-last-input if nil.
;;    Renamed: icicle-search-region to icicle-search-region-beg-end.
;;    Added: icicle-(add|remove|select|search)-region.
;;    icicle-search: Use icicle-regions for numeric prefix arg.  Updated doc string.
;;    Added: icicle-Info-index-20 - thx to Tamas Patrovics.  Use it in icicle-Info-index.
;; 2006/11/25 dadams
;;    icicle-search: After final selection, select orig-window and give its frame input focus.
;; 2006/11/24 dadams
;;    Added: icicle-ensure-overriding-map-is-bound, icicle-universal-argument,
;;           icicle-universal-argument-more, icicle-negative-argument, icicle-digit-argument,
;;           icicle-universal-argument-other-key, icicle-universal-argument-minus,
;;           icicle-kmacro(-action).
;;    icicle-dabbrev-completion: Don't stop at common root, and use lax completion.
;;    Replaced icicle-select-window-or-frame by icicle-other-window-or-frame, which respects C-u 0.
;; 2006/11/23 dadams
;;    icicle-prefix-complete-1: Respect icicle-TAB-shows-candidates-flag.
;;    icicle-execute-extended-command-1: Treat named keyboard macros too.
;;    Added: icicle-execute-named-keyboard-macro.
;; 2006/11/18 dadams icicle-add/update-saved-completion-set, icicle-apropos*, icicle-bookmark,
;;    icicle-buffer-config, icicle-candidate-set-retrieve, icicle-candidate-set-save,
;;    icicle-color-theme, icicle-comint-command, icicle-complete-thesaurus-entry,
;;    icicle-customize-apropos*, icicle-delete-windows, icicle-font, icicle-frame-bg,
;;    icicle-frame-fg, icicle-insert-kill, icicle-insert-string-from-variable,
;;    icicle-insert-thesaurus-entry, icicle-locate-file*, icicle-map, icicle-narrow-candidates,
;;    icicle-remove-buffer-config, icicle-remove-saved-completion-set, icicle-reset-option-to-nil,
;;    icicle-save-string-to-variable, icicle-search, icicle-select-window, icicle-set-option-to-t,
;;    icicle-toggle-option:
;;      Use a specific history variable.
;; 2006/11/17 dadams
;;    Added: icicle-select-(frame|window), icicle-select-window-or-frame.
;; 2006/11/10 dadams
;;    icicle-mouse-candidate-action: read-event to swallow mouse up event.
;;    icicle-map-action: Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;                       Unwind-protect to reselect minibuffer frame.
;;                       Don't bind case-fold-search.
;;    icicle-map: enable-recursive-minibuffers.
;; 2006/11/09 dadams
;;    icicle-nb-of-candidate-in-Completions: Redefined using binary search, for better performance.
;;    icicle-toggle-ignored-space-prefix: Update doc string to use icicle-dispatch-C-^.
;;    icicle-search:
;;      Bind icicle-update-input-hook unconditionally, after icicle-search-regexp-scan.
;;    icicle-search-regexp-scan: Highlight up to icicle-search-highlight-threshold.
;;    icicle-search-highlight-all-input-matches:
;;      Only update input and highlight if icicle-search-highlight-all-current-flag.
;;    icicle-search-action: Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;    icicle-search-highlight-cleanup: Bind inhibit-quit to t.
;; 2006/11/07 dadams
;;    Added: icicle-toggle-highlight-all-current.
;; 2006/11/06 dadams
;;    icicle-search-action:
;;      Highlight icicle-current-input, not icicle-current-raw-input (not updated).
;;    Renamed icicle-search-highlight-all-flag to icicle-search-highlight-threshold.
;; 2006/11/05 dadams
;;    Added: icicle-search-regexp-scan.
;;    icicle-search:
;;      Added buffers arg.  Updated doc string.
;;      Use icicle-search-regexp-scan:  Scan each buffer in buffers.
;;                                      Add marker, not position, to icicle-candidates-alist.
;;      Go to candidate in its buffer.
;;      Added progress message.
;;    icicle-search-action: Pop to buffer of candidate (marker) and raise frame.
;;    icicle-occur: Added buffers arg.  Updated doc string.  Call icicle-search-highlight-cleanup.
;;    icicle-search-highlight-all-input-matches: set-buffer for each ov in dolist (minor opt.).
;;    icicle-search-highlight-cleanup: Added progress messages.  Minor optimization.
;; 2006/10/22 dadams
;;    icicle-complete-keys-action:
;;      Set last-nonmenu-event to non-mouse info, to ignore *Completions* click.
;;    icicle-complete-keys-1: Don't use a default value for completing-read.
;; 2006/10/21 dadams
;;    Added: icicle-insert-char.
;;    icicle-add-key+cmd: Respect icicle-complete-keys-self-insert-flag.
;; 2006/10/20 dadams
;;    icicle-map, icicle-delete-window: Corrected doc string.
;; 2006/10/16 dadams
;;    icicle-add-key+cmd: Protect :enable's eval with condition-case.
;;    icicle-complete-keys-1: No longer use icicle-extra-candidates.
;;                            Use default value of .. for completing-read (except at top level).
;;    icicle-complete-keys-action: Correct no-match case: must match whole and part.
;;    icicle-keys+cmds-w-prefix: Add .. to icicle-complete-keys-alist unless at top level.
;; 2006/10/15 dadams
;;    icicle-complete-keys:
;;      Bind icicle-complete-keys-action, not icicle-complete-keys-help, to icicle-*-action-fn.
;;      Bind orig-buff, orig-window, and icicle-completing-keys-p, for use elsewhere.
;;    Added: icicle-complete-keys-action.  Initial definition from code in icicle-complete-keys.
;;    icicle-complete-keys-action:
;;      Use orig-buff and orig-window; restore to originally selected window.
;;      Error if candidate doesn't match template xxx  =  yyy.
;;    icicle-complete-keys-1:
;;      Call icicle-complete-keys-action on chosen candidate.
;;    icicle-help-on-candidate: Treat key completion also.
;;    Added from cus-edit+.el: custom-variable-p.
;;    Moved to icicles-mode.el: icicle-select-minibuffer-contents, next-history-element.
;;    Moved here from icicles-mode.el: icicle-generic-S-tab.
;;    icicle-generic-S-tab (bug fix): Do not set last-command to icicle-apropos-complete.
;;    Added: eval-when-compile's.
;; 2006/10/13 dadams
;;    icicle-add-key+cmd:
;;      Add actual key to icicle-complete-keys-alist.  Thx to Stefan Monnier.
;;      Don't filter out index (Imenu) keymap.
;;      Treat :enable condition.
;;    icicle-complete-keys-1:
;;      Use actual key recorded in icicle-complete-keys-alist. Don't convert to key description.
;;      Treat digit-argument and negative-argument.
;;    icicle-complete-keys-alist: Updated doc string for new structure.
;; 2006/10/08 dadams
;;    Added: icicle-add-key+cmd, icicle-read-single-key-description.
;;    Added: icicle-complete-keys-alist.
;;           Use in icicle-complete-keys-1, icicle-keys+cmds-w-prefix, icicle-add-key+cmd.
;;    icicle-add-key+cmd: Update binding, depending on its type (menu item etc.).
;;      Push (cons candidate binding), not just candidate, onto icicle-complete-keys-alist.
;;    icicle-complete-keys-1:
;;      Use binding, not just command name.  Call it and put it in (this|last)-command.
;;      Flipped (corrected) use of icicle-key-descriptions-use-<>-flag.
;;      Use icicle-read-single-key-description.
;;    icicle-prefix-keys-first-p, icicle-complete-keys-1, icicle-complete-keys-help,
;;      icicle-keys+cmds-w-prefix: Don't use icicle-list-*-string.
;; 2006/10/05 dadams
;;    icicle-complete-keys-1: Remove icicle-special-candidate property from all candidates.
;;    icicle-keys+cmds-w-prefix:
;;      Intern candidate and, if local binding, put icicle-special-candidate property on it.
;;      Use single string for candidate (don't use multi-completion).
;; 2006/10/03 dadams
;;     icicle-complete-keys-1: Treat "..".
;;     icicle-complete-keys: Updated doc string accordingly.
;;     icicle-prefix-keys-first-p: ".." is less than all other strings.  Don't hard-code "= ".
;;     icicle-keys+cmds-w-prefix:
;;       Filtered out shadowed bindings, icicle-generic-S-tab, and icicle-complete-keys.
;;       Use only map-keymap & lookup-key, not accessible-keymaps, current-active-maps, map-keymap.
;; 2006/10/01 dadams
;;     icicle-complete-keys: Bind sort functions, to put prefix keys first, by default.
;;                           Set last-command, before recursing.
;;     Replaced icicle-alternative-sort with icicle-toggle-alternative-sorting (new).
;;     icicle-(apropos|prefix)-complete-1:
;;       Ensure icicle-(current|last)-input are strings, before compare.
;;     icicle-keys+cmds-w-prefix: Tolerate empty local and global maps.
;; 2006/09/30 dadams
;;     Added: icicle-read-kbd-macro, icicle-edmacro-parse-keys, icicle-toggle-angle-brackets.
;;     icicle-complete-keys-1, icicle-dabbrev-completion:
;;       key-description -> icicle-key-description, with icicle-key-descriptions-use-<>-flag.
;;     icicle-complete-keys-1:
;;       read-kbd-macro -> icicle-read-kbd-macro, with icicle-key-descriptions-use-<>-flag.
;;       Got rid of extra space in prompt before colon, when no prefix.
;;     icicle-keys+cmds-w-prefix: Use single-key-description with icicle-*-use-<>-flag.
;;     icicle-insert-key-description:
;;       Change arg to a toggle, and use icicle-key-descriptions-use-<>-flag.
;;     Bind icicle-candidate-set-retrieve, icicle-candidate-set-save to C-M-<, C-M->, not C-<, C->.
;;     icicle-dired-saved-file-candidates*:
;;       Changed doc strings and messages to use dynamic binding of icicle-candidate-set-save.
;; 2006/09/24 dadams
;;     Added: icicle-complete-keys-help.
;;     icicle-complete-keys:
;;       Bind icicle-*-action-fn to icicle-complete-keys-help.  Mention help keys in docstring.
;;     icicle-complete-keys-1:
;;       Set last-command to command, so completion doesn't think candidate was last-command.
;;     icicle-keys+cmds-w-prefix: Provide placeholder for future insertion of generic characters.
;; 2006/09/23 dadams
;;     icicle-complete-keys-1:
;;       Error if there are no keys for the prefix.
;;       Error, not self-insert-command, for key read-kbd-macro can't convert. Use condition-case.
;;       Report error if calling cmd fails.
;;       Use vconcat for recursive call.
;;       Read cmd, don't intern it - it might be a lambda or byte-compiled function.
;;       Remove duplicates.
;;       Provide KEYS arg to call-interactively, for error reporting. 
;;       No longer bind icicle-must-not-match-regexp to "^Character set .*=  self-insert-command"
;;     icicle-keys+cmds-w-prefix:
;;       Treat also local keymap and current minor maps.
;;       Do nothing if keys+maps is nil.
;;       Only map-keymap if the target is a keymap.
;;       Use keymapp, not functionp, as the binding test.
;;       Only add binding if it is a command or keymap.
;;       Only add self-insert-command binding if the key is char-valid-p.
;;       Use format %S, not %s for a command binding.
;;     icicle-insert-key-description: Added no-angle-brackets-p arg.
;; 2006/09/22 dadams
;;     icicle-complete-keys-1: Filter out keys described "Character set ...= self-insert-command".
;; 2006/09/20 dadams
;;     icicle-complete-keys-1: Treat self-insert-command specially.
;; 2006/09/17 dadams
;;     Added: icicle-complete-keys(-1), icicle-*-keys-prefix, icicle-keys+cmds-w-prefix, 
;;     icicle-doc: Removed one \n from each candidate.
;; 2006/09/12 dadams
;;     Renamed icicle-switch-to-minibuffer to icicle-insert-completion.
;;     Added: icicle-switch-to/from-minibuffer.
;;     icicle-completion-help: Keep focus in the minibuffer after displaying help.
;; 2006/09/02 dadams
;;     icicle-help-on-(next|previous)-(apropos|prefix)-candidate,
;;       icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Use save-selected-window, not save-window-excursion.
;;     icicle-find-file*: In Dired, ignore errors picking up current-line's file name.
;;     icicle-mouse-choose-completion: Error if minibuffer is not active.
;; 2006/08/27 dadams
;;     icicle-abort-minibuffer-input: If minibuffer is not active, just kill buffer *Completions*.
;;     icicle-execute-extended-command-1, icicle-insert-thesaurus-entry, icicle-search-action:
;;       Ensure orig-window is live before using it.
;; 2006/08/23 dadams
;;     Added: icicle-delete-window(s).
;;     Added soft require of frame-cmds.el.
;; 2006/08/22 dadams
;;     icicle-execute-extended-command-1: Bind this-command, don't assign it (fixes C-next).
;;     icicle-help-on-candidate: If no last candidate, then reset to first matching candidate.
;;     icicle-*-*-candidate-action, icicle-help-on-*-*-candidate: save-window-excursion. (good?)
;; 2006/08/20 dadams
;;     icicle-find-file*: Use diredp-find-a-file* in Dired mode (Emacs 22 or later).
;;     Bug fix: icicle-candidate-action: Use icicle-*-candidates, not icicle-next-*-candidate.
;;              icicle-next-*-candidate(-action): Set icicle-current-completion-mode.
;; 2006/08/18 dadams
;;     Added: icicle-Info-goto-node(-(action|cmd)).
;;     icicle-candidate-action: If no icicle-last-completion-candidate, use the first candidate.
;; 2006/08/15 dadams
;;     Added: icicle-help-on-*-*-candidate,icicle-mouse-help-on-candidate.
;;     No longer put icicle-candidate-action-command property on symbols (not used).
;;     Added: icicle-raise-Completions-frame.
;;     icicle*-candidate-action, icicle-help-on-candidate: Use icicle-raise-Completions-frame.
;;     icicle-help-on-candidate: Can use it from *Completions* too now.
;;                               Use icicle-barf-if-outside-Completions-and-minibuffer.
;; 2006/08/13 dadams
;;     Added: icicle-Info-index(-(action|cmd)).
;; 2006/08/04 dadams
;;     icicle-*-complete-1, icicle-prefix-word-complete, icicle-keep-only-past-inputs:
;;       Set icicle-last-completion-command to the explicit command, not this-command.
;;     icicle-history: Call icicle-last-completion-command, not icicle-apropos-complete.
;;     icicle-apropos-complete-1, icicle-narrow-candidates:
;;       Removed binding of icicle-apropos-completing-p (not used).
;;     Added: icicle-plist, icicle-remove-Completions-window, icicle-pp-eval-expression.
;;     Added soft require of pp+.el.
;;     icicle-exit-minibuffericicle-minibuffer-complete-and-exit, icicle-mouse-choose-completion,
;;     icicle-abort-minibuffer-input, icicle-(apropos|prefix)-complete-1,
;;     icicle-keep-only-past-inputs, icicle-insert-thesaurus-entry-cand-fn:
;;       Use icicle-remove-Completions-window.
;;     icicle-doc: Treat doc of faces also.
;;     icicle-non-whitespace-string-p: Added doc string.
;; 2006/08/03 dadams
;;     Added:
;;       icicle-comint-command, icicle-insert-kill, icicle-insert-for-yank,icicle-yank-insert.
;;     Bound icicle-comint-command to C-c TAB in comint-mode.
;;     icicle-search, icicle-comint-search: Cleaned up doc string.
;; 2006/08/02 dadams
;;     icicle-comint-search: Mention *-prompt-pattern.  Thx to Kevin Rodgers.
;;     icicle-insert-string-from-variable: Added some more variables to the completing-read alist.
;; 2006/07/29 dadams
;;     Added: icicle-dispatch-C-., toggle-icicle-search-cleanup, icicle-toggle-search-cleanup.
;; 2006/07/23 dadams
;;     Added: icicle-toggle-transforming.
;;     icicle-comint-search: Bind icicle-transform-function to icicle-remove-duplicates.
;; 2006/07/22 dadams
;;     Added: icicle-comint-search, icicle-comint-send-input, icicle-comint-get-minibuffer-input,
;;            icicle-comint-get-final-choice, icicle-search-generic.
;;     icicle-search: Added require-match arg for non-interactive calls.
;;                    Run the hooks if no match and no match required, and if we didn't cycle.
;;                    Return final choice as value (not used yet).
;;     icicle-insert-string-from-variable: Use buffer-local value of variable, if there is one.
;;     icicle-insert-string-from-variable:
;;       Make sure we use the buffer-local value of the variable, if there is one
;;       Added comint-prompt-regexp to regexp list.
;;     Added mode hooks for icicle-compilation-search and icicle-comint-send-input.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added: icicle-(next|previous)-context-candidate, icicle-scroll-completions.
;;       icicle-switch-to-completions, icicle-switch-to-Completions-buf,
;;         icicle-move-to-next-completion, icicle-map-action, icicle-search-action:
;;           Use icicle-start-of-completions.
;;       icicle-(apropos|prefix)-complete-1:
;;         Set icicle-current-completion-type vs use icicle-arrows-respect-completion-type-flag.
;;         Use icicle-scroll-completions.
;;       icicle-current-completion-in-Completions: Use point-min if no previous prop change.
;;       icicle-keep-only-past-inputs: Use icicle-scroll-completions.
;;     Renamed icicle-start-of-completions to icicle-start-of-candidates-in-Completions,
;;             icicle-current-completion-type to icicle-current-completion-mode,
;;             icicle-*-context-candidate to icicle-(next|previous)-candidate-per-mode,
;;             icicle-scroll-completions to icicle-scroll-Completions.
;;     icicle-(next|previous)-context-candidate: Use icicle-barf-if-outside-minibuffer.
;;     icicle-scroll-Completions: Changed with-selected-window to Emacs 20 equivalent.
;; 2006/07/18 dadams
;;     icicle-search: Bind completion-ignore-case to case-fold-search.
;;     icicle-search-highlight-all-input-matches, icicle-search-action:
;;       Put search inside condition-case, for bad regexp.
;;     Added: icicle-toggle-case-sensitivity, toggle-icicle-case-sensitivity.
;; 2006/07/10 dadams
;;     Added: icicle-search-region.  Use in search functions.  Thx to Le Wang.
;; 2006/07/08 dadams
;;     icicle-search-highlight-all-input-matches: Bug fix - Use *-current-*, not *-current-raw-*.
;;     icicle-execute-extended-command-1:
;;       First try a string candidate as arg, then read it to convert it to symbol or number.
;;       Reset focus back to the minibuffer, in action function.
;; 2006/07/07 dadams
;;     Added: icicle-alternative-sort.
;;     icicle-imenu: Show *Completions* initially for submenu choice (only).
;;     icicle-execute-extended-command: Echo prefix arg in prompt.  Thx: *.dhcp.mdsn.wi.charter.com
;; 2006/07/06 dadams
;;     Added (eval-when-compile (require 'icicles-mac)).
;; 2006/07/05 dadams
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;;     icicle-prefix-complete-1: Don't set icicle-current-raw-input.
;; 2006/07/04 dadams
;;     icicle-prefix-complete-1: No longer calculate common prefix and set current input to it.
;;     Added plist entries to categorize commands:
;;       icicle-(cycling|completing|candidate-action)-command.
;;     icicle-(apropos|prefix)-complete-1, icicle-prefix-word-complete,
;;     icicle-switch-to-Completions-buf, icicle-keep-only-past-inputs, icicle-history:
;;       Use icicle-cycling-command property.
;;     icicle-apropos-complete-1: Removed regexp-p arg in call to icicle-save-or-restore-input.
;; 2006/07/03 dadams
;;     icicle-(apropos|prefix)-complete-1: deactivate mark after inserting current input.
;; 2006/06/18 dadams
;;     icicle-apropos-complete-1, icicle-narrow-candidates: Bind icicle-apropos-completing-p.
;; 2006/06/09 dadams
;;     Bug fixes: Picked up matching subdir as default dir, even if there other files match. 
;;                  Thx to Andrey Zhdanov.
;;                Empty directory not treated as a match.
;;     icicle-(apropos|prefix)-complete-1:
;;       If input matches an empty directory, then use that directory as the sole completion.
;;       Do not expand file-name input before call icicle-file-name-(apropos|prefix)-candidates.
;;     icicle-retrieve-last-input: Use insert, not icicle-insert-input (no longer used).
;;                                 (Input backslashes reverted to slashes.)
;; 2006/06/08 dadams
;;     Bug fix: Could not complete after cycling file names.  Thx to Andrey Zhdanov.
;;     icicle-insert-input: Use icicle-expand-file-name.
;;     icicle-prefix-complete-1:
;;       Expand file-name input before call icicle-file-name-prefix-candidates.
;;       Expand icicle-last-completion-candidate if it is a directory name.
;; 2006/05/30 dadams
;;     icicle-erase-minibuffer-or-history-element: Fix for consecutive deletions.
;; 2006/05/26 dadams
;;     Added: icicle-erase-minibuffer-or-history-element.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;     icicle-narrow-candidates: Bind icicle-reminder-prompt-flag to nil, not t.
;; 2006/05/16 dadams
;;     Added: icicle-kill(-a)-buffer.
;; 2006/05/15 dadams
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     icicle-candidate-set-complement: Put back icicle-ignore-space-prefix-flag.
;;     icicle-buffer(-other-window): Bind icicle-buffer-ignore-space-prefix-flag.
;;     Added: icicle-toggle-ignored-space-prefix, toggle-icicle-ignored-space-prefix.
;; 2006/05/13 dadams
;;     icicle-occur: Make icicle-search-main-regexp-others unnoticeable instead of
;;                   setting icicle-search-highlight-all-flag to nil.
;;     icicle-candidate-set-complement: Use nil, not icicle-completion-nospace-flag.
;;     Renamed: icicle-search-imenu to icicle-imenu,
;;              icicle-search-imenu-in-buffer-p to icicle-imenu-in-buffer-p.
;; 2006/05/12 dadams
;;     icicle-search-imenu: Remove unmatched submenus.  Error if no imenu for the buffer.
;;     Added: icicle-search-imenu-in-buffer-p.
;;     icicle-insert-string-at-point: Use icicle-barf-if-outside-minibuffer.
;;     Moved to icicles-fn.el: icicle-barf-if-outside-*.
;;     Moved some commands to minibuffer-cmds section from top-level cmds section.
;; 2006/05/09 dadams
;;     Added: icicle-customize-icicles-group, icicle-send-bug-report, icicle-customize-button.
;; 2006/04/30 dadams
;;     Added: icicle-map, icicle-map-action.
;;     icicle-filter-alist: Corrected and simplified.
;;     icicle-search: Corrected cand-nb adjustment when cycle with action fns.
;;     Renamed: icicle-search-action-fn to icicle-search-action,
;;              icicle-search-candidates to icicle-candidates-alist.
;; 2006/04/28 dadams
;;     icicle-retrieve-last-input, icicle-(apropos|prefix)-complete-1:
;;       Use icicle-highlight-initial-whitespace.
;; 2006/04/25 dadams
;;     icicle-completion-help: Emacs 21.3's help-insert-xref-button signature is different.
;; 2006/04/16 dadams
;;     Added: icicle-search-imenu.
;;     icicle-search: Bug fixes:
;;       Treat completion without cycling: error or singleton go-to.
;;       Only subtract one from candidate number for C- cycling, not regular cycling.
;; 2006/04/14 dadams
;;     icicle-search:
;;       Bug fix: Position was off by one.
;;       Highlight input match inside each main regexp match (or not).
;;         Bind icicle-update-input-hook and icicle-incremental-completion-flag.
;;       Extract code to define icicle-search-action-fn.
;;       Use icicle-search-candidates instead of local variable search-candidates.
;;       Respect icicle-search-cleanup-flag.
;;     Added: icicle-search-highlight-*, icicle-search-action-fn,
;;            icicle-(insert|save)-text-(from|to)-variable.
;;     Renamed icicle-search-refined-regexp to icicle-search-current-input.
;; 2006/04/09 dadams
;;     icicle-(apropos|prefix)-complete-1: Deal with icicle-arrows-respect-completion-type-flag.
;;     Moved here from icicles-fn.el: icicle-customize-apropos*, icicle-repeat-complex-command.
;; 2006/04/07 dadams
;;     icicle-search: Highlight all occurrences at once (like isearch highlighting, but not lazy).
;;                    Error if no match for initial regexp.
;;     icicle-occur: Bind icicle-search-highlight-all-flag to nil, so don't highlight each line.
;; 2006/04/02 dadms
;;     Added: icicle-toggle-regexp-quote, icicle-find-file*-w-wildcards.
;;     icicle-find-file*: Use icicle-find-file*-w-wildcards.
;; 2006/03/31 dadams
;;     icicle-search: Wrap action function with unwind-protect to select minibuffer frame.
;;                    Use completion-ignore-case when highlighting search hits.
;;                    Protect delete-overlay with overlayp.
;;                    Turn off region highlighting (so can see highlighting done here).
;;                    Removed sit-for-period argument.
;;     icicle-candidate-set-save: Use prin1 instead of pp.
;; 2006/03/27 dadams
;;     Added: icicle-occur.
;;     icicle-search: Highlight also match of current regexp, inside that of regexp arg.
;;                    Use new faces icicle-search-*-regexp.
;;     icicle-search, icicle-switch-to-Completions-buf, icicle-move-to-next-completion:
;;       Use new, generic icicle-place-overlay.
;;     Removed icicle-place-search-overlay.
;; 2006/03/26 dadams
;;     icicle-search: Use icicle-search-overlay.  Ensure don't match twice at same position.
;;                    Added regexp arg.  Use 0 as sit-for default.
;;     Added: icicle-place-search-overlay.
;; 2006/03/25 dadams
;;     icicle-prefix-complete: Minor bug fix: Don't save try-completion if not a string.
;;     icicle-candidate-set-(save|retrieve): Allow use of a variable to save/retrieve.
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-variable, icicle-*-complete-no-display,
;;            icicle-prefix-complete-1.
;;     icicle-apropos-complete-1: Added no-display-p optional arg.
;;     Use no-display-p arg in calls to icicle-display-candidates-in-Completions.
;;     icicle-candidate-set-(retrieve-from|save-to)-cache-file: Pass a consp, not t.
;;     icicle-candidate-set-retrieve: Don't display *Completions*.
;; 2006/03/24 dadams
;;     Added icicle-delete-char.
;; 2006/03/23 dadams
;;     icicle-candidate-set-define: Rewrote.  Can also use at top level.
;;       Error if wrong result type.  Sort result.  Use display-completion-list and
;;       icicle-narrow-candidates (unless at top level).
;;     icicle-narrow-candidates: Can call from top-level (but not interactively).
;;     icicle-candidate-set-complement: Use icicle-maybe-sort-and-strip-candidates.
;;     Mention in doc strings of minibuffer and *Completions* functions: where, key.
;; 2006/03/22 dadams
;;     icicle-find-file*: Use default-directory as default, so opens directory on empty input.
;;     icicle-prefix-complete:
;;       Save icicle-current-regexp-input.
;;       Set icicle-current-input to common prefix.  Use it everywhere here.
;;     Calls to icicle-display-candidates-in-Completions: no root arg now.
;; 2006/03/21 dadams
;;     icicle-insert-input: Bug fix: Use directory of input, not default-directory.
;;                                   Append a slash if input itself is a directory.
;; 2006/03/20 dadams
;;     icicle-retrieve-last-input: Insert icicle-current-regexp-input if repeat C-l.
;;     Added: icicle-insert-input.
;; 2006/03/19 dadams
;;     icicle-apropos-complete-1: Call icicle-save-or-restore-input with non-nil regexp-p arg.
;; 2006/03/17 dadams
;;     Added: icicle-add/update-saved-completion-set, icicle-remove-saved-completion-set,
;;            icicle-retrieve-candidates-from-set.
;;     Removed: icicle-cache-file.
;;     icicle-candidate-set-retrieve: Read candidates set and use its cache file.
;;                                    Enable recursive minibuffers.
;;     icicle-candidate-set-save: Read candidates set and cache-file names.
;;                                Use icicle-add/update-saved-completion-set.
;;     icicle-barf-if-outside-minibuffer: Move interactive test to calling functions.
;;     icicle-files-within: Moved to icicle-fn.el.
;; 2006/03/16 dadams
;;     Added: icicle*-saved-completion-set.
;; 2006/03/14 dadams
;;     icicle-narrow-candidates: Handle no-catch error.  Don't use icicle-completing-p.
;;     icicle-candidate-set-complement:
;;       Do what we do in icicle-candidate-set-retrieve: call icicle-narrow-candidates.
;;     icicle-candidate-set-(retrieve|complement): Msg when display.
;;     icicle-(apropos|prefix)-complete-1:
;;       Removed test for last-command = icicle-candidate-set-complement.
;; 2006/03/13 dadams
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-cache-file.
;;     icicle-candidate-set-(retrieve|save): C-u uses cache file.
;; 2006/03/12 dadams
;;     Added: icicle-dired-saved-file-candidates(-other-window), icicle-locate-file*,
;;            icicle-files-within.
;; 2006/03/11 dadams
;;     icicle-find-file*, icicle-delete-file*:
;;       Reverted to simple form (moved directory control to icicles-mac.el).
;;     icicle-keep-only-past-inputs: Expand file name relative to directory of last input.
;; 2006/03/10 dadams
;;     icicle-find-file*, icicle-delete-file*: Expand file name relative to dir of last input.
;;     Renamed icicle-minibuffer-contents to icicle-minibuffer-contents-from-minibuffer.
;; 2006/03/09 dadams
;;     icicle-barf-if-outside-*: Removed argument - use this-command instead.
;; 2006/03/08 dadams
;;     icicle-bookmark: Use default value, not init value, for completing-read.
;; 2006/03/07 dadams
;;     icicle-doc: Save table in minibuffer-completion-table, so can access via C-RET too.
;;     icicle-insert-thesaurus-entry, icicle-*doc:
;;       Removed binding of icicle-incremental-completion-flag to nil.
;;     Added: icicle-barf-if-outside-(minibuffer|Completions).  Use in appropriate commands.
;;     Added: icicle-non-whitespace-string-p.  Use in icicle-*doc.
;; 2006/03/06 dadams
;;     Update doc strings of *-thesaurus*.
;; 2006/03/05 dadams
;;     Added: icicle-toggle-incremental-completion, toggle-icicle-incremental-completion.
;; 2006/03/03 dadams
;;     icicle-*doc: Clarified doc strings.  Updated prompts.
;;     Added: icicle-help-button.  Use in icicle-completion-help.
;; 2006/03/02 dadams
;;     icicle-insert-thesaurus-entry, icicle-complete-thesaurus-entry:
;;       Use synonyms-ensure-synonyms-read-from-cache.  Clarified doc strings.
;;     icicle-complete-thesaurus-entry: Error if no word at point.  Corrected looking-at regexp.
;; 2006/03/01 dadams
;;     Added: icicle-insert-thesaurus-entry, icicle-insert-thesaurus-entry-cand-fn,
;;            icicle-complete-thesaurus-entry.
;;     icicle-(previous|next)-(apropos|prefix)-candidate-action: Wrap in save-excursion.
;;     Use icicle-clear-minibuffer instead of icicle-erase-minibuffer non-interactively.
;;     icicle-erase-minibuffer: Use icicle-call-then-update-Completions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; loop
                                  ;; plus, for Emacs < 21: dolist, push
                                  ;; plus, for Emacs < 20: when, unless
(eval-when-compile (when (>= emacs-major-version 22) (require 'edmacro))) ;; edmacro-subseq
(eval-when-compile (when (>= emacs-major-version 21) (require 'recentf))) ;; recentf-mode

(eval-when-compile (require 'dabbrev))
  ;; dabbrev-case-fold-search, dabbrev-upcase-means-case-search, dabbrev--last-obarray,
  ;; dabbrev--last-completion-buffer, dabbrev--last-abbreviation, dabbrev--check-other-buffers,
  ;; dabbrev-case-replace, dabbrev--reset-global-variables, dabbrev--abbrev-at-point,
  ;; dabbrev--minibuffer-origin, dabbrev--find-all-expansions, dabbrev--substitute-expansion
(eval-when-compile (require 'cus-edit))
  ;; custom-buffer-create, custom-buffer-order-groups, custom-sort-items
(eval-when-compile (require 'bookmark))
  ;; bookmark-all-names, bookmark-buffer-name, bookmark-current-bookmark
(eval-when-compile (require 'comint))
  ;; comint-prompt-regexp, comint-input-ring, comint-mode-map, comint-check-proc,
  ;; comint-copy-old-input, comint-send-input
(eval-when-compile (require 'imenu)) ;; imenu-syntax-alist
(eval-when-compile (require 'compile)) ;; compilation-find-buffer
(eval-when-compile (require 'info)) ;; Info-goto-node

;; Commented out because `synonyms.el' soft-requires Icicles.
;; (eval-when-compile (require 'synonyms nil t)) ;; (no error if not found):
  ;; synonyms-ensure-synonyms-read-from-cache, synonyms-obarray
(eval-when-compile (require 'misc-cmds nil t)) ;; (no error if not found):
  ;; kill-buffer-and-its-windows
(require 'misc-fns nil t)   ;; (no error if not found): another-buffer
(require 'apropos-fn+var nil t) ;; (no error if not found):
  ;; apropos-command, apropos-function, apropos-option, apropos-variable
(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-find-a-file, diredp-find-a-file-other-window
(require 'frame-cmds nil t) ;; (no error if not found): delete-windows-on
(require 'hexrgb nil t) ;; hexrgb-read-color, hexrgb-color-name-to-hex
(require 'strings nil t) ;; (no error if not found): read-number

(eval-when-compile (require 'icicles-mac))
  ;; icicle-define-command, icicle-define-file-command, icicle-define-add-to-alist-command
(require 'icicles-mcmd) ;; icicle-remove-candidate-display-others,
                        ;; icicle-search-define-replacement, icicle-yank
(require 'icicles-mode) ;; icicle-completing-p
(require 'icicles-var)
  ;; frame-name-history, icicle-bookmark-history, icicle-bookmark-history,
  ;; icicle-buffer-config-history, icicle-candidate-action-fn, icicle-candidate-entry-fn,
  ;; icicle-candidate-nb, icicle-candidates-alist, icicle-color-history,
  ;; icicle-color-theme-history, icicle-completion-candidates, icicle-completion-set-history, 
  ;; icicle-current-input, icicle-current-raw-input, icicle-dictionary-history,
  ;; icicle-extra-candidates, icicle-font-history, icicle-function-history,
  ;; icicle-incremental-completion-p, icicle-kill-history, icicle-kmacro-alist,
  ;; icicle-kmacro-history, icicle-must-match-regexp, icicle-must-not-match-regexp,
  ;; icicle-must-pass-predicate, icicle-re-no-dot, icicle-saved-completion-candidates,
  ;; icicle-search-command, icicle-search-current-overlay, icicle-search-final-choice,
  ;; icicle-search-history, icicle-search-overlays, icicle-search-refined-overlays,
  ;; icicle-variable-history
(require 'icicles-opt)
  ;; icicle-alternative-sort-function, icicle-buffer-configs, icicle-buffer-extras,
  ;; icicle-buffer-ignore-space-prefix-flag, icicle-buffer-match-regexp,
  ;; icicle-buffer-no-match-regexp, icicle-buffer-predicate, icicle-buffer-require-match-flag,
  ;; icicle-buffer-sort, icicle-color-themes, icicle-complete-keys-self-insert-flag, 
  ;; icicle-ignore-space-prefix-flag, icicle-incremental-completion-flag, icicle-input-string,
  ;; icicle-key-descriptions-use-<>-flag, icicle-region-alist, icicle-regions-name-length-max,
  ;; icicle-require-match-flag, icicle-saved-completion-sets, icicle-search-cleanup-flag,
  ;; icicle-search-highlight-all-current-flag, icicle-search-highlight-threshold,
  ;; icicle-search-hook, icicle-show-Completions-initially-flag, icicle-sort-function, 
  ;; icicle-transform-function, icicle-update-input-hook
(require 'icicles-fn) ;; icicle-assoc-delete-all, icicle-get-alist-candidate



;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:
(defvar cookie-cache)
(defvar icicle-track-pt) ;; Defined in icicle-insert-thesaurus-entry
(defvar recentf-list)
(defvar yow-after-load-message)
(defvar yow-file)
(defvar yow-load-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Commands -----------------------------------------------


;;; Redefined standard commands.............................


;;; REPLACE ORIGINAL `dabbrev-completion' defined in `dabbrev.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-dabbrev-completion)
(fset 'old-dabbrev-completion (symbol-function 'dabbrev-completion)))

;;;###autoload
(defun icicle-dabbrev-completion (&optional arg) ; Bound to `C-M-/' globally.
  "Completion on current word.
Like \\[dabbrev-expand], but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by
`dabbrev-friend-buffer-function', to find the completions.

If the prefix argument is 16 (which comes from `C-u C-u'), then it
searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."
  (interactive "*P")
  (unless (featurep 'dabbrev)
    (unless (require 'dabbrev nil t) (error "Library `dabbrev' not found"))
    (icicle-mode 1))                    ; Redefine `dabbrev-completion' to Icicles version.
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers (and arg t))
         (dabbrev-check-all-buffers (and arg (= (prefix-numeric-value arg) 16)))
         (abbrev (dabbrev--abbrev-at-point))
         (ignore-case-p (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                 case-fold-search
                               dabbrev-case-fold-search)
                             (or (not dabbrev-upcase-means-case-search)
                                 (string= abbrev (downcase abbrev)))))
         (my-obarray dabbrev--last-obarray)
         init)
    ;; If new abbreviation to expand, then expand it.
    (save-excursion
      (unless (and (null arg)
                   my-obarray
                   (or (eq dabbrev--last-completion-buffer (current-buffer))
                       (and (window-minibuffer-p (selected-window))
                            (eq dabbrev--last-completion-buffer
                                (dabbrev--minibuffer-origin))))
                   dabbrev--last-abbreviation
                   (>= (length abbrev) (length dabbrev--last-abbreviation))
                   (string= dabbrev--last-abbreviation
                            (substring abbrev 0 (length dabbrev--last-abbreviation)))
                   (setq init (try-completion abbrev my-obarray)))
        (setq dabbrev--last-abbreviation abbrev)
        (let ((completion-list (dabbrev--find-all-expansions abbrev ignore-case-p))
              (completion-ignore-case ignore-case-p))
          ;; Make an obarray with all expansions
          (setq my-obarray (make-vector (length completion-list) 0))
          (unless (> (length my-obarray) 0)
            (error "No dynamic expansion for \"%s\" found%s" abbrev
                   (if dabbrev--check-other-buffers "" " in this-buffer")))
          (dolist (string completion-list)
            (cond ((or (not ignore-case-p) (not dabbrev-case-replace))
                   (intern string my-obarray))
                  ((string= abbrev (upcase abbrev))
                   (intern (upcase string) my-obarray))
                  ((string= (substring abbrev 0 1) (upcase (substring abbrev 0 1)))
                   (intern (capitalize string) my-obarray))
                  (t (intern (downcase string) my-obarray))))
          (setq dabbrev--last-obarray my-obarray)
          (setq dabbrev--last-completion-buffer (current-buffer))
          ;; Find the longest common string.
          (setq init (try-completion abbrev my-obarray)))))
    ;; Let the user choose between the expansions
    (unless (stringp init) (setq init abbrev))
    (cond
      ((and (not (string-equal init ""))
            (not (string-equal (downcase init) (downcase abbrev)))
            (<= (length (all-completions init my-obarray)) 1))
       (message "Completed (no other completions)")
       (if (< emacs-major-version 21)
           (dabbrev--substitute-expansion nil abbrev init)
         (dabbrev--substitute-expansion nil abbrev init nil))
       (when (window-minibuffer-p (selected-window)) (message nil)))
;;$$       ;; Complete text only up through the common root. NOT USED.
;;       ((and icicle-dabbrev-stop-at-common-root-p
;;             (not (string-equal init ""))
;;             (not (string-equal (downcase init) (downcase abbrev))))
;;        (message "Use `%s' again to complete further"
;;                 (icicle-key-description (this-command-keys)
;;                                         (not icicle-key-descriptions-use-<>-flag)))
;;        (if (< emacs-major-version 21)
;;            (dabbrev--substitute-expansion nil abbrev init)
;;          (dabbrev--substitute-expansion nil abbrev init nil))
;;        (when (window-minibuffer-p (selected-window)) (message nil))) ; $$ NEEDED?
      (t
       ;; String is a common root already.  Use Icicles completion.
       (message "Making completion list...")
       (search-backward abbrev)
       (replace-match "")
       (condition-case nil
           (let* ((icicle-show-Completions-initially-flag t)
                  (icicle-incremental-completion-p 'display)
                  (minibuffer-completion-table my-obarray)
                  (choice (completing-read "Complete: " my-obarray nil nil init nil init)))
             (when choice (insert choice)))
         (quit (insert abbrev)))))))


;;; REPLACE ORIGINAL `lisp-complete-symbol' defined in `lisp.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-lisp-complete-symbol)
(fset 'old-lisp-complete-symbol (symbol-function 'lisp-complete-symbol)))

;;;###autoload
(defun icicle-lisp-complete-symbol ()   ; Bound to `ESC TAB' globally.
  "Complete the Lisp symbol preceding point against known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

The context determines which symbols are considered.
If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (set-syntax-table emacs-lisp-mode-syntax-table)
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (buffer-substring beg end))
	 (predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    (function (lambda (sym)
			(or (boundp sym) (fboundp sym)
			    (symbol-plist sym))))))
         (enable-recursive-minibuffers (active-minibuffer-window))
         (icicle-top-level-when-sole-completion-flag t)
         (completion (completing-read "Complete Lisp symbol: " obarray predicate t pattern nil)))
    (delete-region beg end)
    (insert completion)))


;;; REPLACE ORIGINAL `customize-apropos' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos)
(fset 'old-customize-apropos (symbol-function 'customize-apropos)))

;;;###autoload
(defun icicle-customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which
  are not user-settable, as well as faces and groups.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg current-prefix-arg))
     (list (completing-read "Customize (regexp): " obarray
                            (lambda (symbol)
                              (or (get symbol 'custom-group)
                                  (custom-facep symbol)
                                  (and (boundp symbol)
                                       (or (get symbol 'saved-value)
                                           (custom-variable-p symbol)
                                           (if (null pref-arg)
                                               (user-variable-p symbol)
                                             (get symbol 'variable-documentation))))))
                            nil nil 'regexp-history)
           pref-arg)))
  (let ((found nil))
    (mapatoms (lambda (symbol)
                (when (string-match regexp (symbol-name symbol))
                  (when (and (not (memq all '(faces options))) ; groups or t
                             (get symbol 'custom-group))
                    (push (list symbol 'custom-group) found))
                  (when (and (not (memq all '(options groups))) ; faces or t
                             (custom-facep symbol))
                    (push (list symbol 'custom-face) found))
                  (when (and (not (memq all '(groups faces))) ; options or t
                             (boundp symbol)
                             (or (get symbol 'saved-value)
                                 (custom-variable-p symbol)
                                 (if (memq all '(nil options))
                                     (user-variable-p symbol)
                                   (get symbol 'variable-documentation))))
                    (push (list symbol 'custom-variable) found)))))
    (if (not found)
        (error "No matches")
      (custom-buffer-create (custom-sort-items found t custom-buffer-order-groups)
                            "*Customize Apropos*"))))

;; Define this for Emacs 20 and 21
(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Return non-nil if VARIABLE is a custom variable."
    (or (get variable 'standard-value) (get variable 'custom-autoload))))


;;; REPLACE ORIGINAL `customize-apropos-faces' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-faces)
(fset 'old-customize-apropos-faces (symbol-function 'customize-apropos-faces)))

;;;###autoload
(defun icicle-customize-apropos-faces (regexp)
  "Customize all user faces matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize faces (regexp): " obarray 'custom-facep nil nil
                          'regexp-history)))
  (customize-apropos regexp 'faces))


;;; REPLACE ORIGINAL `customize-apropos-groups' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-groups)
(fset 'old-customize-apropos-groups (symbol-function 'customize-apropos-groups)))

;;;###autoload
(defun icicle-customize-apropos-groups (regexp)
  "Customize all user groups matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize groups (regexp): " obarray
                          (lambda (symbol) (get symbol 'custom-group)) nil nil 'regexp-history)))
  (customize-apropos regexp 'groups))


;;; REPLACE ORIGINAL `customize-apropos-options' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-options)
(fset 'old-customize-apropos-options (symbol-function 'customize-apropos-options)))

;;;###autoload
(defun icicle-customize-apropos-options (regexp &optional arg)
  "Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg current-prefix-arg))
     (list (completing-read "Customize options (regexp): " obarray
                            (lambda (symbol)
                              (and (boundp symbol)
                                   (or (get symbol 'saved-value)
                                       (custom-variable-p symbol)
                                       (if (null pref-arg)
                                           (user-variable-p symbol)
                                         (get symbol 'variable-documentation)))))
                            nil nil 'regexp-history)
           pref-arg)))
  (customize-apropos regexp (or arg 'options)))


;;; REPLACE ORIGINAL `repeat-complex-command' defined in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the command to repeat, letting you use `S-TAB' and
;;; `TAB' to see the history list and `C-,' to toggle sorting that display.
;;;
(or (fboundp 'old-repeat-complex-command)
(fset 'old-repeat-complex-command (symbol-function 'repeat-complex-command)))

;;;###autoload
(defun icicle-repeat-complex-command (arg) ; Bound to `C-x ESC ESC', `C-x M-:' in Icicle mode.
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
You can use the minibuffer history commands \\<minibuffer-local-map>\\[next-history-element] and \
\\[previous-history-element]
to get different commands to edit and resubmit.

Use `S-TAB', [next], and [prior], to match regexp input - this gives
you the functionality of `repeat-matching-complex-command'."
  (interactive "p")
  (let ((elt (nth (1- arg) command-history))        
        newcmd)
    (if elt
        (progn
          (setq newcmd
                (let ((print-level nil)
                      (minibuffer-history-position arg)
                      (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
                  (unwind-protect
                       (let ((icicle-transform-function 'icicle-remove-duplicates))
                         (read (completing-read
                                "Redo: " (mapcar (lambda (entry) (list (prin1-to-string entry)))
                                                 command-history)
                                nil nil (prin1-to-string elt) (cons 'command-history arg)
                                (prin1-to-string elt))))
                    ;; If command was added to command-history as a string, get rid of that.
                    ;; We want only evaluable expressions there.
                    (if (stringp (car command-history))
                        (setq command-history (cdr command-history))))))
          ;; If command to be redone does not match front of history, add it to the history.
          (or (equal newcmd (car command-history))
              (setq command-history (cons newcmd command-history)))
          (eval newcmd))
      (if command-history
          (error "Argument %d is beyond length of command history" arg)
        (error "There are no previous complex commands to repeat")))))

;;;###autoload
(defun icicle-add-candidate-to-saved-completion-set (set string)
  "Add candidate STRING to saved completion-candidates set SET."
  (interactive
   (list (completing-read "Saved completion set: " icicle-saved-completion-sets
                          nil t nil 'icicle-completion-set-history
                          (caar icicle-saved-completion-sets))
         (completing-read "Candidate to add: "
                          (mapcar #'list icicle-saved-completion-candidates))))
  (let ((file-name (cdr (assoc set icicle-saved-completion-sets))))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf (find-file-noselect file-name 'nowarn 'raw))
          candidates)
      (unwind-protect (setq candidates (read list-buf)) (kill-buffer list-buf))
      (unless (consp candidates) (error "Bad data in cache file `%s'" file-name))
      (push string candidates)
      (with-temp-message (format "Writing candidate string to cache file `%s'..." file-name)
        (with-temp-file file-name (prin1 candidates (current-buffer))))
      (icicle-msg-maybe-in-minibuffer
       (format "`%s' added to saved set `%s', file `%s'" string set file-name)))))

;;;###autoload
(defun icicle-remove-candidate-from-saved-completion-set (set)
  "Remove a candidate from saved completion-candidates set SET."
  (interactive
   (list (completing-read "Saved completion set: " icicle-saved-completion-sets
                          nil t nil 'icicle-completion-set-history
                          (caar icicle-saved-completion-sets))))
  (let ((file-name (cdr (assoc set icicle-saved-completion-sets))))
    (unless (icicle-file-readable-p file-name)
      (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf (find-file-noselect file-name 'nowarn 'raw))
          string candidates)
      (unwind-protect (setq candidates (read list-buf)) (kill-buffer list-buf))
      (unless (consp candidates) (error "Bad data in cache file `%s'" file-name))
      (setq string (completing-read "Candidate to remove: " (mapcar #'list candidates)
                                    nil t nil nil (car candidates)))
      (setq candidates (delete string candidates))
      (with-temp-message (format "Writing remaining candidates to cache file `%s'..." file-name)
        (with-temp-file file-name (prin1 candidates (current-buffer))))
      (icicle-msg-maybe-in-minibuffer
       (format "`%s' removed from saved set `%s', file `%s'" string set file-name)))))

;;;###autoload
(icicle-define-command icicle-remove-saved-completion-set ; Command name
  "Remove an entry from `icicle-saved-completion-sets'.
This does not remove the associated cache file.
You can add entries to `icicle-saved-completion-sets' using command
`icicle-add/update-saved-completion-set'." ; Doc string
  (lambda (set-name)                    ; Action function
    (setq icicle-saved-completion-sets
          (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
    (customize-save-variable 'icicle-saved-completion-sets icicle-saved-completion-sets)
    (message "Candidate set `%s' removed" set-name)
    ;; Update the set of completions, then update *Completions*.
    (setq minibuffer-completion-table icicle-saved-completion-sets)
    (icicle-update-completions))
  "Remove set of completion candidates named: " ; `completing-read' args
  icicle-saved-completion-sets nil t nil 'icicle-completion-set-history
  (caar icicle-saved-completion-sets))

(put 'icicle-dired-saved-file-candidates 'icicle-Completions-window-max-height 200)
;;;###autoload
(defun icicle-dired-saved-file-candidates (prompt-for-dir-p)
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
With a prefix argument, you are prompted for the directory."
  (interactive "P")
  (unless icicle-saved-completion-candidates
    (error "No saved completion candidates.  Use \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' to save candidates"))
  (let* ((default-directory (if prompt-for-dir-p
                                (read-file-name "Directory: " nil default-directory nil)
                              default-directory))
         (existing-dired-buffer (get-buffer (file-name-nondirectory
                                             (directory-file-name default-directory)))))
    (unless (file-exists-p (car icicle-saved-completion-candidates))
      (error "Bad directory? No file `%s' in `%s'"
             (car icicle-saved-completion-candidates) default-directory))
    (when (and existing-dired-buffer
               (y-or-n-p (format "Replace existing Dired buffer `%s'? "
                                 (buffer-name existing-dired-buffer))))
      (kill-buffer existing-dired-buffer))
    (dired (cons default-directory icicle-saved-completion-candidates))))

;;;###autoload
(defun icicle-dired-saved-file-candidates-other-window (prompt-for-dir-p)
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
Open in another window.
With a prefix argument, you are prompted for the directory."
  (interactive "P")
  (unless icicle-saved-completion-candidates
    (error "No saved completion candidates.  Use \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' to save candidates"))
  (let* ((default-directory (if prompt-for-dir-p
                                (read-file-name "Directory: " nil default-directory nil)
                              default-directory))
         (existing-dired-buffer (get-buffer (file-name-nondirectory
                                             (directory-file-name default-directory)))))
    (unless (file-exists-p (car icicle-saved-completion-candidates))
      (error "Bad directory? No file `%s' in `%s'"
             (car icicle-saved-completion-candidates) default-directory))
    (when (and existing-dired-buffer
               (y-or-n-p (format "Replace existing Dired buffer `%s'? "
                                 (buffer-name existing-dired-buffer))))
      (if (fboundp 'kill-buffer-and-its-windows)
          (kill-buffer-and-its-windows existing-dired-buffer) ; Defined in `misc-cmds.el'
        (kill-buffer existing-dired-buffer)))
    (dired-other-window (cons default-directory icicle-saved-completion-candidates))))
 
;;(@* "Icicles multi-commands")
;;; Icicles multi-commands .   .   .   .   .   .   .   .   .

;;;###autoload
(icicle-define-command icicle-execute-extended-command ; Bound to `M-x' in Icicle mode.
  "Read command name, then read its arguments and call it.
This is `execute-extended-command', turned into a multi-command." ; Doc string
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute command%s: "         ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray 'commandp t nil 'extended-command-history nil nil
  ((last-cmd last-command))             ; Bindings: save the last command.
  nil (setq last-command last-cmd) (setq last-command last-cmd)) ; Undo, last code: restore last.

(defun icicle-execute-extended-command-1 (cmd-name)
  "Candidate action function for `icicle-execute-extended-command'."
  (when (get-buffer orig-buff) (set-buffer orig-buff)) ; `orig-buff', `orig-window' are free vars.
  (when (window-live-p orig-window) (select-window orig-window))

  ;; Rebind `icicle-candidate-action-fn' to a function that calls the
  ;; candidate CMD-NAME on a single argument that it reads.  This is
  ;; used only if CMD-NAME is a command that, itself, reads an input
  ;; argument with completion.  When that is the case, you can use
  ;; completion on that input, and if you do that, you can use `C-RET'
  ;; to use command CMD-NAME as a multi-command.  In other words, this
  ;; binding allows for two levels of multi-commands.
  (let* ((cmd (intern cmd-name))
         (icicle-candidate-action-fn
          (lambda (arg)
            (condition-case nil
                (funcall cmd arg)       ; Try to use string candidate `arg'.
              ;; If that didn't work, use a symbol or number candidate.
              (wrong-type-argument (funcall cmd (car (read-from-string arg))))
              (wrong-number-of-arguments (funcall #'icicle-help-on-candidate))) ; Punt - show help.
            (select-frame-set-input-focus (window-frame (minibuffer-window))))))
    (let ((fn (symbol-function cmd))
          (count (prefix-numeric-value current-prefix-arg)))
      (cond ((arrayp fn)
             (let ((this-command cmd)) (execute-kbd-macro fn count))
             (when (> count 1) (message "(%d times)" count)))
            (t
             (run-hooks 'post-command-hook)
             (run-hooks 'pre-command-hook)
             (let ((enable-recursive-minibuffers t)
                   (this-command cmd))
               (call-interactively cmd 'record-it)))))))

;;;###autoload
(icicle-define-command icicle-execute-named-keyboard-macro ; Bound to `C-x M-e' in Icicle mode.
  "Read the name of a keyboard macro, then execute it."
  icicle-execute-extended-command-1     ; Function to perform the action
  (format "Execute keyboard macro%s: "  ; `completing-read' args
          (if current-prefix-arg
              (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
            ""))
  obarray (lambda (fn) (and (commandp fn) (arrayp (symbol-function fn))))
  t nil 'icicle-kmacro-history nil nil
  ((last-cmd last-command))             ; Bindings: save the last command.
  nil (setq last-command last-cmd) (setq last-command last-cmd)) ; Undo code, last code: restore.

;;;###autoload
(when (boundp 'kmacro-ring)
  (icicle-define-command icicle-kmacro  ; Bound to `f5' in Icicle mode (Emacs 22).
    "Execute a keyboard macro according to its position in `kmacro-ring'.
Macros in the keyboard macro ring are given names \"macro #1\",
\"macro #2\", and so on, for use as completion candidates."
    icicle-kmacro-action                ; Function to perform the action
    (format "Execute keyboard macro%s: " ; `completing-read' args
            (if current-prefix-arg
                (format " (prefix %d)" (prefix-numeric-value current-prefix-arg))
              ""))
    (let ((count 0))
      (setq icicle-kmacro-alist
            (mapcar (lambda (x) (cons (format "macro #%d" (setq count (1+ count))) x))
                    (nreverse (cons (kmacro-ring-head) kmacro-ring)))))
    nil t nil 'icicle-kmacro-history nil nil
    nil                                 ; Additional bindings
    (unless (or (kmacro-ring-head) kmacro-ring) ; First code
      (error "No keyboard macro defined")))

  (defun icicle-kmacro-action (cand)
    "Action function for `icicle-kmacro'."
    (when (get-buffer orig-buff) (set-buffer orig-buff)) ; `orig-buff', `orig-window' are free vars
    (when (window-live-p orig-window) (select-window orig-window))
    (let ((count (prefix-numeric-value current-prefix-arg)))
      (execute-kbd-macro (cadr (assoc cand icicle-kmacro-alist)) count
                         #'kmacro-loop-setup-function)
      (when (> count 1) (message "(%d times)" count)))))

;;;###autoload
(icicle-define-command icicle-set-option-to-t ; Command name
  "Set option to t.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt)                         ; Function to perform the action
    (set (intern opt) t) (message "`%s' is now t" opt))
  "Set option to t: " obarray           ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         'user-variable-p)
        (current-prefix-arg 'boundp)
        (t 'icicle-binary-option-p))
  'must-confirm nil 'icicle-variable-history)

(defalias 'icicle-clear-option 'icicle-reset-option-to-nil)

;;;###autoload
(icicle-define-command icicle-reset-option-to-nil ; Command name
  "Set option to nil.  This makes sense for binary and list options.
By default, the set of completion candidates is limited to user
options.  Note: it is *not* limited to binary and list options.
With a prefix arg, all variables are candidates." ; Doc string
  (lambda (opt) (set (intern opt) nil) (message "`%s' is now nil" opt)) ; Action function
  "Clear option (set it to nil): " obarray ; `completing-read' args
  (if current-prefix-arg 'boundp 'user-variable-p) t nil 'icicle-variable-history)

;;;###autoload
(icicle-define-command icicle-toggle-option ; Command name
  "Toggle option's value.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
  (lambda (opt)                         ; Function to perform the action
    (let ((sym (intern opt)))
      (set sym (not (eval sym))) (message "`%s' is now %s" opt (eval sym))))
  "Toggle value of option: " obarray    ; `completing-read' args
  (cond ((and current-prefix-arg (wholenump (prefix-numeric-value current-prefix-arg)))
         'user-variable-p)
        (current-prefix-arg 'boundp)
        (t 'icicle-binary-option-p))
  'must-confirm nil 'icicle-variable-history)

(defun icicle-binary-option-p (symbol)
  "Non-nil if SYMBOl is a user option that has custom-type `boolean'."
  (eq (get symbol 'custom-type) 'boolean))

;;;###autoload
(icicle-define-command icicle-bookmark  ; Command name
  "Jump to a bookmark.
You can use `S-delete' on any candidate bookmark to delete it instead." ; Doc string
  bookmark-jump                         ; Function to perform the action
  "Bookmark: " (mapcar #'list (bookmark-all-names)) ; `completing-read' args
  nil t nil 'icicle-bookmark-history (or (and (boundp 'bookmark-current-bookmark)
                                              bookmark-current-bookmark)
                                         (bookmark-buffer-name))
  nil
  ((icicle-delete-candidate-object 'bookmark-delete)))

;;;###autoload
(defun icicle-other-window-or-frame (arg) ; Bound to `C-x o' in Icicle mode.
  "`other-window'/`other-frame', or, with `C-u 0', multi-command version.
With non-zero prefix argument, this is `other-window', or `other-frame'
if `one-window-p'.

With a zero prefix argument, this is `icicle-select-window', or
`icicle-select-frame' if `one-window-p'."
  (interactive "p")
  (if (zerop arg)
      (if (one-window-p) (icicle-select-frame) (icicle-select-window))
    (if (one-window-p) (other-frame arg) (other-window arg))))

;;;###autoload
(icicle-define-command icicle-select-frame ; Bound to `C-x 5 o' in Icicle mode.
  "Select frame by name and raise it."  ; Doc string
  select-frame-by-name                  ; Function to perform the action
  "Select frame: "                      ; `completing-read' args
  (make-frame-names-alist) nil t nil 'frame-name-history
  (cdr (assq 'name (frame-parameters (next-frame (selected-frame))))) t)

;;;###autoload
(icicle-define-command icicle-select-window ; Command name
  "Select window by its buffer name."   ; Doc string
  (lambda (buf) (select-window (get-buffer-window buf))) ; Function to perform the action
  "Select window: "                     ; `completing-read' args
  (let ((bufs (buffer-list (selected-frame)))
        (cand-bufs nil))
    (dolist (buf bufs) (when (get-buffer-window buf) (push (list (buffer-name buf)) cand-bufs)))
    cand-bufs)
  nil t nil 'buffer-name-history (buffer-name (window-buffer (other-window 1))) t)

;;;###autoload
(icicle-define-command icicle-delete-windows ; Command name
  "Delete windows showing a buffer, anywhere." ; Doc string
  delete-windows-on                     ; Function to perform the action
  "Delete windows on buffer: "          ; `completing-read' args
  (let ((all-bufs (buffer-list))
        (cand-bufs nil))
    (dolist (buf all-bufs)
      (when (get-buffer-window buf 0) (push (list (buffer-name buf)) cand-bufs)))
    cand-bufs)
  nil t nil 'buffer-name-history (buffer-name (current-buffer)) t)

;;;###autoload
(defun icicle-delete-window (bufferp)   ; Bound to `C-x 0' in Icicle mode.
  "`delete-window' or prompt for buffer and delete all its windows.
When called from the minibuffer, remove the *Completions* window.

Otherwise:
 With no prefix arg, delete the selected window.
 With a prefix arg, prompt for a buffer and delete all windows, on
   any frame, that show that buffer.

 With a prefix arg, this is an Icicles multi-command - see
 `icicle-mode'.  Input-candidate completion and cycling are
 available.  While cycling, these keys act on the current
 candidate:

 `C-RET'   - Act on current completion candidate only
 `C-down'  - Act, then move to next prefix-completion candidate
 `C-up'    - Act, then move to previous prefix-completion candidate
 `C-next'  - Act, then move to next apropos-completion candidate
 `C-prior' - Act, then move to previous apropos-completion candidate
 `up'      - Move to next prefix-completion candidate
 `down'    - Move to previous prefix-completion candidate
 `next'    - Move to next apropos-completion candidate
 `prior'   - Move to previous apropos-completion candidate
 `C-!'     - Act on *all* candidates, successively (careful!)

 Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit."
  (interactive "P")
  (if (window-minibuffer-p (selected-window))
      (icicle-remove-Completions-window)
    (if bufferp (icicle-delete-windows) (delete-window))))

;;;###autoload
(icicle-define-command icicle-kill-buffer ; Bound to `C-x k' in Icicle mode.
  "Kill a buffer."                      ; Doc string
  icicle-kill-a-buffer-and-update-completions ; Function to perform the action
  "Kill buffer: "                       ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list)) nil t nil 'buffer-name-history
  (buffer-name (current-buffer)) nil
  ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function icicle-buffer-sort)
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

(defun icicle-kill-a-buffer-and-update-completions (buf)
  "Kill buffer BUF and update the set of completions."
  (setq buf (get-buffer buf))
  (if buf
      (condition-case err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf))
            ;; Update the set of completions, then update *Completions*.
            (setq minibuffer-completion-table (mapcar (lambda (buf) (list (buffer-name buf)))
                                                      (buffer-list)))
            (icicle-update-completions))
        (error nil))
    (message "No such live buffer: `%s'" buf)))

(defun icicle-kill-a-buffer (buf)
  "Kill buffer BUF."
  (setq buf (get-buffer buf))
  (if buf
      (condition-case err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf)))
        (error nil))
    (message "No such live buffer: `%s'" buf)))

(put 'icicle-buffer 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-buffer    ; Bound to `C-x b' in Icicle mode.
  "Switch to a different buffer.
You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'." ; Doc string
  switch-to-buffer                      ; Function to perform the action
  "Switch to buffer: "                  ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

;;;###autoload
(icicle-define-command icicle-buffer-other-window ; Bound to `C-x 4 b' in Icicle mode.
  "Switch to a different buffer in another window.
You can use `S-delete' during completion to kill a candidate buffer.

These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'" ; Doc string
  switch-to-buffer-other-window         ; Function to perform the action
  "Switch to buffer: "                  ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

;;;###autoload
(icicle-define-command icicle-add-buffer-candidate ; Command name
  "Add buffer as an always-show completion candidate.
This just adds the buffer to `icicle-buffer-extras'.
You can use `S-delete' on any candidate to remove it from
`icicle-buffer-extras' instead." ; Doc string
  (lambda (buf)
    (add-to-list 'icicle-buffer-extras buf) ; Action function
    (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
    (message "Buffer `%s' added to always-show buffers" buf))
  "Buffer candidate to show always: "   ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history (buffer-name (if (fboundp 'another-buffer)
                                                    (another-buffer nil t)
                                                  (other-buffer (current-buffer))))
  nil
  ((icicle-delete-candidate-object 'icicle-remove-buffer-candidate-action))) ; Additional bindings

;;;###autoload
(icicle-define-command icicle-remove-buffer-candidate ; Command name
  "Remove buffer as an always-show completion candidate.
This just removes the buffer from `icicle-buffer-extras'." ; Doc string
  icicle-remove-buffer-candidate-action ; Action function
  "Remove buffer from always-show list: " ; `completing-read' args
  (mapcar #'list icicle-buffer-extras) nil t nil 'buffer-name-history (car icicle-buffer-extras))

(defun icicle-remove-buffer-candidate-action (buf)
  "Action function for command `icicle-remove-buffer-candidate'."
  (setq icicle-buffer-extras (delete buf icicle-buffer-extras))
  (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
  (message "Buffer `%s' removed from always-show buffers" buf))

;;;###autoload
(icicle-define-command icicle-buffer-config ; Command name
  "Choose a configuration of user options for `icicle-buffer'.
You can use `S-delete' on any candidate to remove it instead.
See user option `icicle-buffer-configs'.  See also commands
`icicle-add-buffer-config' and `icicle-remove-buffer-config'." ; Doc string
  (lambda (config-name)                 ; Function to perform the action
    (let ((config (assoc config-name icicle-buffer-configs)))
      (setq icicle-buffer-match-regexp (elt config 1))
      (setq icicle-buffer-no-match-regexp (elt config 2))
      (setq icicle-buffer-predicate (elt config 3))
      (setq icicle-buffer-extras (elt config 4))
      (setq icicle-buffer-sort (elt config 5))))
  "Configuration: " icicle-buffer-configs nil t nil ; `completing-read' args
  'icicle-buffer-config-history nil nil
  ((icicle-delete-candidate-object 'icicle-remove-buffer-config-action))) ; Additional bindings

;;;###autoload
(icicle-define-add-to-alist-command icicle-add-buffer-config
  "Add buffer configuration to `icicle-buffer-configs'.
You are prompted for the buffer configuration components.
For the list of extra buffers to always display, you can choose them
using `C-mouse-2', `C-RET', and so on, just as you would make any
Icicles multiple choice."
  (lambda ()
    (let ((name (read-from-minibuffer "Add buffer configuration.  Name: "))
          (match-regexp (icicle-read-from-minibuf-nil-default
                         "Regexp to match: " nil nil nil nil icicle-buffer-match-regexp))
          (nomatch-regexp (icicle-read-from-minibuf-nil-default
                           "Regexp not to match: " nil nil nil nil icicle-buffer-no-match-regexp))
          (pred (icicle-read-from-minibuf-nil-default "Predicate to satify: " nil nil nil nil
                                                      icicle-buffer-predicate))
          (sort-fn (icicle-read-from-minibuf-nil-default
                    "Sort function: " nil nil t nil
                    (and icicle-buffer-sort (symbol-name icicle-buffer-sort))))
          (extras (progn (message "Choose extra buffers to show...") (sit-for 1)
                         (icicle-buffer-list)))) ; Do last, for convenience.
      (list name match-regexp nomatch-regexp pred extras sort-fn)))
  icicle-buffer-configs)

(defun icicle-read-from-minibuf-nil-default (prompt &optional initial-contents keymap read hist
                                             default-value inherit-input-method)
  "Like `read-from-minibuffer', but return nil for empty input.
Args are as for `read-from-minibuffer'.
If nothing is input, then nil is returned."
  (let ((input (read-from-minibuffer prompt initial-contents keymap nil hist default-value
                                     inherit-input-method)))
    (if (string= "" input)
        nil
      (if read
          (car (read-from-string input))
        input))))

;;;###autoload
(icicle-define-command icicle-buffer-list ; Command name
  "Choose a list of buffer names.
You can use `S-delete' during completion to kill a candidate buffer.
The list of names (strings) is returned." ; Doc string
  (lambda (name) (push name buf-names)) ; Function to perform the action
  "Choose buffer (`RET' when done): "   ; `completing-read' args
  (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
  nil nil nil 'buffer-name-history nil nil
  ((buf-names nil)                     ; Additional bindings
   (icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
   (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
   (icicle-must-pass-predicate icicle-buffer-predicate)
   (icicle-extra-candidates icicle-buffer-extras)
   (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
   (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills current buffer.
   (icicle-require-match-flag icicle-buffer-require-match-flag)
   (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag))
  nil nil                               ; First code, undo code
  (prog1 (setq buf-names (delete "" buf-names)) ; Last code - return the list of buffers
    (when (interactive-p) (message "Buffers: %S" buf-names))))

;;;###autoload
(icicle-define-command icicle-remove-buffer-config ; Command name
  "Remove buffer configuration from `icicle-buffer-configs'." ; Doc string
  icicle-remove-buffer-config-action   ; Action function
  "Remove buffer configuration: "       ; `completing-read' args
  (mapcar (lambda (config) (list (car config))) icicle-buffer-configs)
  nil t nil 'icicle-buffer-config-history (caar icicle-buffer-configs))

(defun icicle-remove-buffer-config-action (config-name)
  "Action function for command `icicle-remove-buffer-config'."
  (setq icicle-buffer-configs (icicle-assoc-delete-all config-name icicle-buffer-configs))
  (customize-save-variable 'icicle-buffer-configs icicle-buffer-configs)
  (message "Buffer configuration `%s' removed" config-name))

;;;###autoload
(icicle-define-command icicle-color-theme ; Command name
  "Change color theme.
You can use `S-delete' during completion to remove the current
candidate from the list of color themes.

To use this command, you must have loaded library `color-theme.el',
available from http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme." ; Doc string
  (lambda (theme) (funcall (intern theme))) ; Action - just call the theme.
  "Theme: " icicle-color-themes nil t nil 'icicle-color-theme-history ; `completing-read' args
  nil nil
  ((icicle-delete-candidate-object 'icicle-color-themes))) ; Additional bindings  

;;;###autoload
(icicle-define-command icicle-insert-kill ; Command name
  "Insert previously killed text from the `kill-ring'.
This is like `yank', but it does not rotate the `kill-ring'.
You can use `S-delete' during completion to delete a candidate entry
from the `kill-ring'." ; Doc string
  icicle-insert-for-yank                ; Function to perform the action
  "Insert: " (mapcar #'list kill-ring) nil t nil 'icicle-kill-history ; `completing-read' args
  (car kill-ring) nil
  ((icicle-transform-function 'icicle-remove-duplicates)
   (icicle-delete-candidate-object 'kill-ring))) ; Additional bindings

(defun icicle-insert-for-yank (string)
  "`insert-for-yank', if defined; else, `insert' with `read-only' removed."
  (if (fboundp 'insert-for-yank)        ; Defined in `subr.el' (not required).
      (insert-for-yank string)
    (let ((opoint (point)))
      (insert string)
      (let ((inhibit-read-only t)) (remove-text-properties opoint (point) '(read-only nil))))))

;;;###autoload
(defun icicle-yank-insert (&optional arg) ;  Bound to `C-y' (or whatever `yank' was bound to).
  "`icicle-insert-kill', if prefix arg < 0; otherwise, `yank'.
When called from the minibuffer, however, this calls `icicle-yank':
The prefix arg is then ignored and *Completions* is updated with
regexp input matches."
  (interactive "*P")
  (if (window-minibuffer-p (selected-window))
      (icicle-yank arg)
    (if (wholenump (prefix-numeric-value arg)) (yank arg) (icicle-insert-kill))))

;;;###autoload
(icicle-define-file-command icicle-delete-file ; Command name
  "Delete a file or directory."         ; Doc string
  icicle-delete-file-or-directory       ; Function to perform the action
  "Delete file or directory: " default-directory nil t) ; `read-file-name' args

(defun icicle-delete-file-or-directory (file)
  "Delete file (or directory) FILE."
  (condition-case i-delete-file
      (if (eq t (car (file-attributes file)))
          (delete-directory file)
        (delete-file file))
    (error (message (error-message-string i-delete-file))
           (error (error-message-string i-delete-file)))))

(put 'icicle-find-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-file-command icicle-find-file ; Bound to `C-x C-f' in Icicle mode.
  "Visit a file or directory.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  icicle-find-file-w-wildcards          ; Function to perform the action
  "File or directory: " nil             ; `read-file-name' args
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file)) ; Defined in `dired+.el'.
      (condition-case nil               ; E.g. error because not on file line (ignore)
          (abbreviate-file-name (dired-get-file-for-visit))
        (error default-directory))
    default-directory)
  nil nil nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory))) ; Additional bindings

(defun icicle-find-file-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file))
      (diredp-find-a-file filename t)
    (find-file filename t)))

;;;###autoload
(icicle-define-file-command icicle-find-file-other-window ; Bound to `C-x 4 f' in Icicle mode.
  "Visit a file or directory in another window.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  icicle-find-file-other-window-w-wildcards ; Function to perform the action
  "File or directory: " nil             ; `read-file-name' args
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file-other-window)) ; In `dired+.el'
      (condition-case nil               ; E.g. error because not on file line (ignore)
          (abbreviate-file-name (dired-get-file-for-visit))
        (error default-directory))
    default-directory)
  nil nil nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory))) ; Additional bindings

(defun icicle-find-file-other-window-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file-other-window))
      (diredp-find-a-file-other-window filename t)
    (find-file-other-window filename t)))

(put 'icicle-recent-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-recent-file ; Command name
  "Open a recently used file.
You can use `S-delete' during completion to delete a candidate file."          ; Doc string
  find-file                             ; Function to perform the action
  "Recent file: " (mapcar #'list recentf-list) ; `completing-read' args
  nil t nil 'file-name-history (car recentf-list) nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory)) ; Additional bindings
  (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
         (when (fboundp 'recentf-mode) (recentf-mode 99))
         (unless (consp recentf-list) (error "No recently accessed files"))))

;;;###autoload
(icicle-define-command icicle-recent-file-other-window ; Command name
  "Open a recently used file in another window.
You can use `S-delete' during completion to delete a candidate file." ; Doc string
  find-file-other-window                ; Function to perform the action
  "Recent file: " (mapcar #'list recentf-list) ; `completing-read' args
  nil t nil 'file-name-history (car recentf-list) nil
  ((icicle-delete-candidate-object 'icicle-delete-file-or-directory)) ; Additional bindings
  (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
         (when (fboundp 'recentf-mode) (recentf-mode 99))
         (unless (consp recentf-list) (error "No recently accessed files"))))

(put 'icicle-locate-file 'icicle-Completions-window-max-height 200)
;;;###autoload
(icicle-define-command icicle-locate-file ; Command name
  "Visit a file within a directory or its subdirectories.
With a prefix argument, you are prompted for the directory.
Otherwise, the current directory is used.

The absolute names of all files within the directory and all of its
subdirectories are targets for completion.  Regexp input is matched
against all parts of the absolute name, not just the file-name part.

You can use this to find all files within your file system that match
a regexp, but be aware that gathering and matching the file names will
take some time.

Remember that you can save the set of files matching your input using \
`\\<minibuffer-local-completion-map>\\[icicle-candidate-set-save]' or \
`\\[icicle-candidate-set-save-to-cache-file]'.
You can then retrieve quickly them later using `\\[icicle-candidate-set-retrieve]' or \
`\\[icicle-candidate-set-retrieve-from-cache-file]'. " ; Doc string
  find-file                             ; Function to perform the action
  "File: " (mapcar #'list (icicle-files-within ; `completing-read' args
                           (directory-files dir 'full icicle-re-no-dot) nil))
  nil t nil 'file-name-history nil nil
  ((dir (if current-prefix-arg          ; Additional bindings
            (read-file-name "Locate under which directory: " nil default-directory nil)
          default-directory)))
  (message "Gathering files within `%s' (this could take a while)..." dir)) ;  First code

;;;###autoload
(icicle-define-command icicle-locate-file-other-window ; Command name
  "Like `icicle-locate-file' but the file is visited in another window." ; Doc string
  find-file-other-window                ; Function to perform the action
  "File: "                              ; `completing-read' args
  (mapcar #'list (icicle-files-within (directory-files dir 'full icicle-re-no-dot) nil))
  nil t nil 'file-name-history nil nil
  ((dir (if current-prefix-arg          ; Additional bindings
            (read-file-name "Locate under which directory: " nil default-directory nil)
          default-directory)))
  (message "Gathering files within `%s' (this could take a while)..." dir))

;;;###autoload
(icicle-define-command icicle-font      ; Command name
  "Change font of current frame."       ; Doc string
  (lambda (font) (modify-frame-parameters orig-frame (list (cons 'font font)))) ; Action function
  "Font: " (mapcar #'list (x-list-fonts "*")) ; `completing-read' args
  nil t nil 'icicle-font-history nil nil
  ((orig-frame (selected-frame))        ; Additional bindings
   (orig-font (frame-parameter nil 'font)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'font orig-font))) ; Undo code
  nil)                                  ; Last code

;;;###autoload
(icicle-define-command icicle-frame-bg  ; Command name
  "Change background of current frame." ; Doc string
  (lambda (color)                       ; Function to perform the action
    (modify-frame-parameters orig-frame (list (cons 'background-color color))))
  "Background color:: " (mapcar #'list (x-defined-colors)) ; `completing-read' args
  nil t nil 'icicle-color-history nil nil
  ((orig-frame (selected-frame))        ; Additional bindings
   (orig-bg (frame-parameter nil 'background-color)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'background-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;;;###autoload
(icicle-define-command icicle-frame-fg  ; Command name
  "Change foreground of current frame." ; Doc string
  (lambda (color)                       ; Function to perform the action
    (modify-frame-parameters orig-frame (list (cons 'foreground-color color))))
  "Foreground color:: " (mapcar #'list (x-defined-colors))
  nil t nil 'icicle-color-history nil nil ; `completing-read' args
  ((orig-frame (selected-frame))        ; Additional bindings
   (orig-bg (frame-parameter nil 'foreground-color)))
  nil                                   ; First code
  (modify-frame-parameters orig-frame (list (cons 'foreground-color orig-bg))) ; Undo code
  nil)                                  ; Last code

;; Bind this, not `icicle-Info-index', to `i' in Info mode,
;; so plain `Info-index' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-index-cmd ()         ; Bound to `i' in Info mode.
  "If in Icicle mode, run `icicle-Info-index'; else, run `Info-index'.
Note: In Emacs versions prior to version 22, this runs `Info-index'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-index 'Info-index)))

;;;###autoload
(defun icicle-Info-index ()
  "Like `Info-index', but you can use Icicles keys `C-RET', `C-up' etc."
  (interactive)
  (let ((info-buf (current-buffer))
        (info-window (selected-window))
        (icicle-candidate-action-fn 'icicle-Info-index-action))
    (call-interactively (if (> emacs-major-version 21) 'Info-index 'icicle-Info-index-20))))

;; Thx to Tamas Patrovics for this.
;;;###autoload
(defun icicle-Info-index-20 ()
  "Like `Info-index', but you can use completion for the index topic."
  (interactive)
  (Info-index "")
  (let ((pattern "\\* +\\([^:]*\\):.")
        (candidates nil))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t) (push (list (match-string 1)) candidates))
    (Info-index (completing-read "Index topic: " candidates nil t))))

(defun icicle-Info-index-action (topic)
  "Completion action function for `icicle-Info-index'."
  (let ((minibuf-win (selected-window)))
    (set-buffer info-buf)               ; `info-buf', `info-window' defined in `icicle-Info-index'.
    (select-window info-window)
    (Info-index topic)
    (select-window minibuf-win)))

;; Bind this, not `icicle-Info-goto-node', to `g' in Info mode,
;; so plain `Info-goto-node' will be used when not also in Icicle mode.
;;;###autoload
(defun icicle-Info-goto-node-cmd ()     ; Bound to `g' in Info mode.
  "In Icicle mode, run `icicle-Info-goto-node'; else, `Info-goto-node'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-goto-node 'Info-goto-node)))

;;;###autoload
(defun icicle-Info-goto-node ()
  "Like `Info-goto-node', but you can use keys `C-RET', `C-up' etc."
  (interactive)
  (let ((info-buf (current-buffer))
        (info-window (selected-window))
        (icicle-candidate-action-fn 'icicle-Info-goto-node-action))
    (call-interactively 'Info-goto-node)))

(defun icicle-Info-goto-node-action (node)
  "Completion action function for `icicle-Info-goto-node'."
  (let ((minibuf-win (selected-window)))
    (set-buffer info-buf) ; `info-buf', `info-window' defined in `icicle-Info-goto-node'.
    (select-window info-window)
    (Info-goto-node node)
    (select-window minibuf-win)))

;;;###autoload
(icicle-define-command icicle-insert-thesaurus-entry ; Command name
  "Insert an entry from a thesaurus.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  icicle-insert-thesaurus-entry-cand-fn ; Function to perform the action
  "Thesaurus entry to match: " synonyms-obarray ; `completing-read' args
  nil nil nil 'icicle-dictionary-history nil nil
  ((icicle-track-pt (point)))           ; Additional bindings
  (progn                                ; First code
    (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
      (error "You must first load library `synonyms.el'"))
    (synonyms-ensure-synonyms-read-from-cache))
  (when (window-live-p orig-window)     ; Undo code
    (select-frame-set-input-focus (window-frame orig-window))
    (goto-char icicle-track-pt))       
  (when (window-live-p orig-window)     ; Last code
    (select-frame-set-input-focus (window-frame orig-window))
    (goto-char icicle-track-pt)))      

(defun icicle-insert-thesaurus-entry-cand-fn (string)
  "Action function for `icicle-insert-thesaurus-entry'.
Insert STRING, followed by a space, at position TRACK-PT of buffer
ORIG-BUFF."
  (set-buffer orig-buff)                ; `orig-buff' is a free variable here.
  (goto-char icicle-track-pt)
  (insert string " ")
  (setq icicle-track-pt (point))
  (save-excursion (set-buffer (window-buffer (minibuffer-window))) (icicle-clear-minibuffer))
  (save-selected-window (icicle-remove-Completions-window)))

;;;###autoload
(defun icicle-complete-thesaurus-entry (word) ; Bound to `C-c /' in Icicle mode.
  "Complete WORD to an entry from a thesaurus.
The default value of WORD is the word at the cursor.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'."
  (interactive (list (word-at-point)))
  (unless word (error "No word at point to complete"))
  (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
    (error "You must first load library `synonyms.el'"))
  (synonyms-ensure-synonyms-read-from-cache)
  (when (and (looking-at "\\b") (not (looking-at "\\s-"))) (forward-word 1))
  (delete-region (progn (forward-word -1) (point)) (progn (forward-word 1) (point)))
  (insert (completing-read "Thesaurus entry to match: " synonyms-obarray
                           nil nil word 'icicle-dictionary-history word))
  (unless (looking-at "\\s-") (insert " ")))

;;;###autoload
(icicle-define-command icicle-plist     ; Command name
  "Choose description of a symbol and property list.
Each candidate for completion is a symbol name plus its property list
\(as a string).  They are separated by `icicle-list-join-string'
\(^G^J, by default).  You can match an input regexp against the symbol
name or the property list or both.  Use `C-q C-g C-q C-j' to input the
default separator.

Remember that you can use `[^^G]' to match any character except ^G,
which includes newline. Remember also that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion."
  (lambda (entry) (with-output-to-temp-buffer "*Help*" (princ entry))) ; Action function
  "SYMB `C-q C-g C-q C-j' PLIST (`RET' when done): " ; `completing-read' args
  (let ((result nil))                   ; TABLE arg is an alist with items ((symb plist-string))
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (condition-case nil
                    (let ((plist (symbol-plist symb)))
                      (when plist
                        (push (list (list (symbol-name symb) (format "%s" plist))) result)))
                  (error nil))))        ; Ignore symbols that produce errors.
    result)
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-special-candidate))))) ; Bindings
  (message "Gathering property lists...")) ; First code

;;;###autoload
(icicle-define-command icicle-vardoc    ; Command name
  "Choose a variable description.
Each candidate for completion is a variable name plus its
documentation.  They are separated by `icicle-list-join-string' (^G^J,
by default).  You can match an input regexp against the variable name
or the documentation or both.  Use `C-q C-g C-q C-j' to input the
default separator.

For example, use input

\"dired.*^G
\[^^G]*list\"

with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all variables whose
names contain \"dired\" and whose documentation contains \"list\".
Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.*' here, instead, then only the first lines of doc
strings are searched.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  (lambda (entry)                       ; Action function
    (with-output-to-temp-buffer "*Help*" (princ entry)))
  "VAR `C-q C-g C-q C-j' DOC (`RET' when done): " ; `completing-read' args
  (let ((result nil))                   ; TABLE arg is an alist whose items are ((symb doc)).
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (condition-case nil
                    (when (boundp symb)
                      (let ((doc (documentation-property symb 'variable-documentation)))
                        (when (icicle-non-whitespace-string-p doc)
                          (push (list (list (symbol-name symb) doc)) result))))
                  (error nil))))        ; Ignore symbols that produce errors.
    result)
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-special-candidate))))) ; Bindings
  (message "Gathering variable descriptions...")) ; First code

;;;###autoload
(icicle-define-command icicle-fundoc    ; Command name
  "Choose a function description.
Each candidate for completion is a function name plus its
documentation.  They are separated by `icicle-list-join-string' (^G^J,
by default).  You can match an input regexp against the function name
or the documentation or both.  Use `C-q C-g C-q C-j' to input the
default separator.

For example, use input

\"dired.*^G
\[^^G]*file\"

with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all functions whose
names contain \"dired\" and whose documentation contains \"file\".
Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.*' here, instead, then only the first lines of doc
strings are searched.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  (lambda (entry) (with-output-to-temp-buffer "*Help*" (princ entry))) ; Action function
  "FUNC `C-q C-g C-q C-j' DOC (`RET' when done): " ; `completing-read' args
  (let ((result nil))                   ; TABLE arg is an alist whose items are ((symb doc)).
    (mapatoms (lambda (symb)            ; Each completion candidate is a list of strings.
                (when (fboundp symb)
                  (condition-case nil
                      (let ((doc (documentation symb)))
                        (when (icicle-non-whitespace-string-p doc)
                          (push (list (list (symbol-name symb) doc)) result)))
                    (error nil)))))     ; Ignore symbols that produce errors.
    result)
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-special-candidate))))) ; Bindings
  (message "Gathering function descriptions...")) ; First code

;;;###autoload
(icicle-define-command icicle-doc       ; Command name
  "Choose documentation for a symbol.
Each candidate for completion is the description of a function,
variable, or face.  Displays the documentation and returns the
symbol.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
  (lambda (entry)                       ; Action function: display the doc.
    (let ((symb (cdr (assoc entry minibuffer-completion-table))))
      (when (and symb (boundp symb)) (describe-variable symb))
      (when (fboundp symb) (describe-function symb))
      (when (facep symb) (describe-face symb))
      symb))                            ; Return the symbol.
  "Find doc with regexp: "              ; `completing-read' args
  (let ((result nil)                    ; TABLE arg is an alist whose items are (doc . symb).
        doc)                            ; Each completion candidate is a list of strings.
    (mapatoms (lambda (symb)
                (condition-case nil
                    (progn
                      (when (fboundp symb)
                        (setq doc (concat (documentation symb) "\n\n"))
                        (when (icicle-non-whitespace-string-p doc) (push (cons doc symb) result)))
                      (when (boundp symb)
                        (setq doc (concat (documentation-property symb 'variable-documentation)
                                          "\n\n"))
                        (when (icicle-non-whitespace-string-p doc) (push (cons doc symb) result))))
                  (error nil))))        ; Ignore symbols that produce errors.
    ;; `icicle-candidate-action-fn' is used in the main body of command
    ;;`icicle-doc' and is also bound to `C-RET'.  We need to refer to the
    ;; TABLE arg to `completing-read' within the body of the function.
    ;; So, we cheat and pre-assign `minibuffer-completion-table' to it here.
    (setq minibuffer-completion-table result))
  nil nil nil nil nil nil
  ((icicle-candidate-properties-alist '((1 (face 'icicle-special-candidate))))) ; Bindings
  (message "Gathering documentation...")) ; First code

;;;###autoload
(defun icicle-non-whitespace-string-p (string)
  "Returns non-nil if STRING contains a non-whitespace character.
The `standard-syntax-table' definition of whitespace is used."
  (interactive "s")
  (let ((orig-syntable (syntax-table)))
    (unwind-protect
       (progn
         (set-syntax-table (standard-syntax-table))
         (and (stringp string) (> (length string) 0) (string-match "\\S-" string)))
      (set-syntax-table orig-syntable))))

;;;###autoload
(defun icicle-apropos (apropos-regexp &optional do-all)
  "Like `apropos', but lets you see the list of matches (with `S-TAB')."
  (interactive (list (completing-read "Apropos symbol (regexp or words): " obarray
                                      nil nil nil 'regexp-history)
                     current-prefix-arg))
  (apropos apropos-regexp do-all))

;;;###autoload
(cond
  ;; Use my versions of the `apropos*' commands, defined in `apropos-fn+var.el'.
  ;; Note that unlike my versions of `apropos-option' and `apropos-command', the `icicle-'
  ;; versions here do not respect `apropos-do-all': they always work with options and commands.
  ((fboundp 'apropos-option)
   (defun icicle-apropos-variable (pattern)
     "Show variables that match PATTERN.
This includes variables that are not user options.
You can see the list of matches with `S-TAB'.
See `apropos-variable' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos variable (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray
             #'(lambda (symbol) (and (boundp symbol) (get symbol 'variable-documentation)))
             nil nil 'regexp-history)))
     (apropos-variable pattern))

   (defun icicle-apropos-option (pattern)
     "Show user options (variables) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-option' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos user option (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'user-variable-p nil nil 'regexp-history)))
     (let ((apropos-do-all nil))
       (apropos-option pattern)))

   (defun icicle-apropos-function (pattern)
     "Show functions that match PATTERN.
This includes functions that are not commands.
You can see the list of matches with `S-TAB'.
See `apropos-function' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos function (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'functionp nil nil 'regexp-history)))
     (apropos-function pattern))

   (defun icicle-apropos-command (pattern)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-command' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos command (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'commandp nil nil 'regexp-history)))
     (let ((apropos-do-all nil))
       (apropos-command pattern))))

  ;; My versions are not available.  Use the vanilla Emacs versions of the `apropos...' commands.
  (t
   (defun icicle-apropos-variable (pattern &optional do-all)
     "Show variables that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-variable' for a description of PATTERN.

With optional prefix DO-ALL or if `apropos-do-all' is non-nil, also
show normal variables."
     (interactive
      (list (progn
              (unless (or (boundp 'apropos-do-all) (require 'apropos nil t))
                (error "Library `apropos' not found"))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "variable" "user option")
                       " (regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if (or current-prefix-arg apropos-do-all)
                           #'(lambda (symbol) (and (boundp symbol)
                                                   (get symbol 'variable-documentation)))
                         'user-variable-p)
               nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-variable pattern do-all))

   (defun icicle-apropos-command (pattern &optional do-all var-predicate)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-command' for a description of PATTERN.

With \\[universal-argument] prefix, or if `apropos-do-all' is non-nil,
also show noninteractive functions.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE."
     (interactive
      (list (progn
              (unless (boundp 'apropos-do-all)
                (unless (require 'apropos nil t) (error "Library `apropos' not found")))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "command or function" "command")
                       "(regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if current-prefix-arg 'functionp 'commandp) nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-command pattern do-all var-predicate))))

;;;###autoload
(defun icicle-apropos-zippy (regexp)
  "Show all Zippy quotes matching the regular-expression input.
Returns the list of matches."
  (interactive (progn (unless (boundp 'yow-file)
                        (unless (require 'yow nil t) (error "Library `yow' not found")))
                      (cookie yow-file yow-load-message yow-after-load-message)
                      (let* ((case-fold-search t)
                             (cookie-table-symbol (intern yow-file cookie-cache))
                             (string-table (symbol-value cookie-table-symbol))
                             (table (nreverse (mapcar #'list string-table))))
                        (list (completing-read "Apropos Zippy (regexp): " table
                                               nil nil nil 'regexp-history)))))
  (let ((matches (apropos-zippy icicle-current-input)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Zippy Apropos*"
        (while matches
          (princ (car matches))
          (setq matches (cdr matches))
          (and matches (princ "\n\n")))))
    matches))                           ; Return matching Zippyisms.

;;;###autoload
(defun icicle-map (alist fn)
  "Selectively apply a function to items in an alist.
FN is a function.  ALIST is an alist - interactively, it is a variable
whose value is an alist.

You are prompted for both arguments.  Completion is available, without
sorting of candidates.  The completion list for ALIST is a set of
variables whose value is a cons.  With no prefix arg, the names of
these variables must end with \"alist\".  With a prefix argument, the
first car of each variable value must itself be a cons.

Examples: If ALIST is `auto-mode-alist' and FN is `cdr', then the
completion candidates are the keys of the alist and the result of
applying FN to an alist entry is simply the value of that key.  If you
choose, for example, candidate \"\\.el\\'\", then the result is
`emacs-lisp-mode'.  In this case, the function performs simple lookup.

If, instead, FN were (lambda (x) (describe-function (cdr x))), then
the result of choosing \"\\.el\\'\" would be to display the help for
function `emacs-lisp-mode'.

During completion you can use these keys also.  Each displays the
value of applying FN to the current completion candidate.

`C-RET'   - Act on current completion candidate only
`C-down'  - Act, then move to next prefix-completion candidate
`C-up'    - Act, then move to previous prefix-completion candidate
`C-next'  - Act, then move to next apropos-completion candidate
`C-prior' - Act, then move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'.

`icicle-map' overrides `icicle-ignore-space-prefix-flag', binding it
to nil so that candidates with initial spaces can be matched."
  (interactive
   (list (symbol-value
          (intern (completing-read
                   "Alist (variable): " obarray
                   `(lambda (symb)
                     (and
                      (boundp symb) (consp (symbol-value symb))
                      ,(if current-prefix-arg
                           '(consp (car (symbol-value symb)))
                           '(string-match "alist$" (symbol-name symb)))))
                   t nil 'icicle-variable-history)))
         (read (completing-read "Function: " obarray 'functionp
                                nil nil 'icicle-function-history))))
  (setq icicle-candidates-alist         ; Make keys of ALIST be strings.  Save in global variable.
        (mapcar (lambda (key+val) (cons (format "%s" (car key+val)) (cdr key+val))) alist))
  (setq icicle-candidate-entry-fn fn)   ; Save in global variable.
  (let ((icicle-candidate-action-fn 'icicle-map-action)
        (icicle-incremental-completion-flag 'always)
        (icicle-sort-function nil)
        (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
        (icicle-inhibit-sort-p t)
        (icicle-ignore-space-prefix-flag nil)
        (enable-recursive-minibuffers t))
    (condition-case failure
        (let ((cand-nb 0)
              candidate-entries result)
          (completing-read "Choose an occurrence: " icicle-candidates-alist nil t)
          (setq candidate-entries (icicle-filter-alist icicle-candidates-alist
                                                       icicle-completion-candidates))
          (cond ((not (wholenump icicle-candidate-nb)) ; Didn't cycle to choose the candidate.
                 (when (cdr candidate-entries) ; Multiple entries with the same key
                   (error (substitute-command-keys "Ambiguous choice. Try again.")))
                 ;; Do nothing here if only one entry with the chosen key.
                 )
                (t
                 (setq cand-nb (mod icicle-candidate-nb (length icicle-candidates-alist)))
                 ;;$$$ If cycled with action functions, add or subtract one from candidate #.
                 ;; (when (memq last-command '(icicle-next-apropos-candidate-action
                 ;;                            icicle-next-prefix-candidate-action))
                 ;;   (setq cand-nb (1- cand-nb))
                 ;;   (when (< cand-nb 0) (setq cand-nb 0)))
                 ;; (when (memq last-command '(icicle-previous-apropos-candidate-action
                 ;;                            icicle-previous-prefix-candidate-action))
                 ;;   (setq cand-nb (1+ cand-nb))
                 ;;   (when (> cand-nb (length icicle-candidates-alist))
                 ;;     (setq cand-nb (length icicle-candidates-alist))))
                 ))
          ;; Note: If didn't cycle to choose candidate then `candidate-entries' is singleton.
          (setq result (funcall icicle-candidate-entry-fn (elt candidate-entries cand-nb)))
          (message "Key: %s,  Result: %s" (car (elt candidate-entries cand-nb)) result)
          result)
      (error (error (error-message-string failure))))))

(defun icicle-map-action (string)
  "Completion action function for `icicle-map'."
  (unwind-protect
       (condition-case icicle-map-action
           (progn
             (icicle-highlight-candidate-in-Completions)             
             ;; Apply function to candidate entry and display it.
             (if icicle-candidate-nb
                 (let ((key+result (elt icicle-candidates-alist icicle-candidate-nb)))
                   (message "Key: %s,  Result: %s" (car key+result)
                            (funcall icicle-candidate-entry-fn key+result)))
               (error "No such occurrence"))
             nil)                       ; Return nil for success.
         (error "%s" (error-message-string icicle-map-action))) ; Return error msg.
    (select-frame-set-input-focus (window-frame (minibuffer-window)))))

;;;###autoload
(defun icicle-goto-marker ()
  "Go to a marker in this buffer, choosing it by the line that includes it.
During completion you can use these keys to navigate among marks:
`C-RET'   - Act on current completion candidate only
`C-down'  - Act, then move to next prefix-completion candidate
`C-up'    - Act, then move to previous prefix-completion candidate
`C-next'  - Act, then move to next apropos-completion candidate
`C-prior' - Act, then move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to choose a candidate as the final destination,
or `C-g' to quit.  This is an Icicles command - see `icicle-mode'."
  (interactive)
  (let ((markers (icicle-markers mark-ring)))
    (unless (consp markers) (error "No markers in this buffer"))
    (icicle-map (mapcar #'icicle-marker+text markers)
                (lambda (cand)
                  (pop-to-buffer (marker-buffer (cdr cand)))
                  (goto-char (cdr cand))))))

;;;###autoload
(defun icicle-goto-global-marker ()
  "Go to a global marker, choosing it by the line that includes it.
During completion you can use these keys to navigate among marks:
`C-RET'   - Act on current completion candidate only
`C-down'  - Act, then move to next prefix-completion candidate
`C-up'    - Act, then move to previous prefix-completion candidate
`C-next'  - Act, then move to next apropos-completion candidate
`C-prior' - Act, then move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to choose a candidate as the final destination,
or `C-g' to quit.  This is an Icicles command - see `icicle-mode'."
  (interactive)
  (let ((markers (icicle-markers global-mark-ring)))
    (unless (consp markers) (error "No global markers"))
    (icicle-map (mapcar #'icicle-marker+text markers)
                (lambda (cand)
                  (pop-to-buffer (marker-buffer (cdr cand)))
                  (goto-char (cdr cand))))))

(defun icicle-marker+text (marker)
  "Cons of text line that includes MARKER with MARKER itself.
If the marker is on an empty line, then text \"<EMPTY LINE>\" is used."
  (save-excursion
    (set-buffer (marker-buffer marker))
    (goto-char marker)
    (let ((line (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))
                                                (save-excursion (end-of-line) (point)))))
      (when (string= "" line) (setq line "<EMPTY LINE>"))
      (cons line marker))))

(defun icicle-markers (ring)
  "Marks in mark RING that are in live buffers other than a minibuffer."
  (let ((markers nil))
    (dolist (mkr ring)
      (when (and (buffer-live-p (marker-buffer mkr))
                 (not (string-match "\\` \\*Minibuf-[0-9]+\\*\\'"
                                    (buffer-name (marker-buffer mkr)))))
        (push mkr markers)))
    markers))

;;;###autoload
(defun icicle-region-open-all-files ()
  "Visit all files that contain regions in `icicle-region-alist'.
The files are visited, but not displayed.
If a file listed in `icicle-region-alist' does not exist or is not
readable, then it is ignored."
  (interactive)
  (let ((alist (icicle-delete-if-not (lambda (entry) (car (cddr entry))) icicle-region-alist)))
    (unless alist (error "There are no file buffers in `icicle-region-alist'"))
    (dolist (entry alist)
      (let ((file (car (cddr entry))))
      (when (file-readable-p file) (find-file-noselect file))))))

;;;###autoload
(defun icicle-exchange-point-and-mark (&optional arg) ; Bound to `C-x C-x'.
  "`exchange-point-and-mark', `icicle-add-region', or `icicle-select-region'.
With no prefix arg: `exchange-point-and-mark'.
With a numeric prefix arg:`icicle-add-region'.
With a plain `C-u' prefix arg: `icicle-select-region'."
  (interactive "P")
  (if arg
      (if (atom current-prefix-arg)
          (call-interactively #'icicle-add-region)
        (unless (consp icicle-region-alist)
          (error "`icicle-region-alist' is empty; try again, with a numeric prefix arg"))
        (call-interactively #'icicle-select-region))
    (call-interactively #'exchange-point-and-mark)))

;;;###autoload
(defun icicle-add-region (start end &optional tag) ; Bound to `C-N C-x C-x', N = whole number.
  "Add current region to list of regions, `icicle-region-alist'.
Updates the persistent value of user option `icicle-region-alist'.

With a prefix argument, you are prompted for a tag to name the region.
Otherwise, the first `icicle-regions-name-length-max' characters of
the region itself serve as the name.

To remove a region from `icicle-region-alist', use command
`icicle-remove-region' or customize `icicle-region-alist'."
  (interactive "r\nP")
  (when (= start end) (error "Cannot add region; it is empty"))
  (when (> start end) (setq end (prog1 start (setq start end))))
  (let ((region-prefix
         (buffer-substring-no-properties start (+ start (min icicle-regions-name-length-max
                                                             (- end start))))))
    (add-to-list 'icicle-region-alist (list (setq tag (if tag
                                                          (read-string "Region name (tag): "
                                                                       nil nil region-prefix)
                                                        region-prefix))
                                            (buffer-name)
                                            (buffer-file-name)
                                            start
                                            end))
    (customize-save-variable 'icicle-region-alist icicle-region-alist)
    (message "Region added to `icicle-region-alist' with tag `%s'"
             (if (> (length tag) 20) (concat (substring tag 0 17) "...") tag))))

;;;###autoload
(icicle-define-command icicle-select-region ; Bound to `C-u C-x C-x'
  "Choose a region from the list of Icicles regions, and activate it.
Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

Regions in `icicle-region-alist' that are in buffers that do not
currently exist are ignored.

Note that each region is defined by its limits, so that if the
region's buffer has changed, then the text used to identify the region
might no longer correspond to the text at the beginning of the
region.

If user option `icicle-add-buffer-name-flag' is non-nil, then each
completion candidate is annotated with the name of the region's
buffer, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the start and end points of the region and its length.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict.

You can use `S-delete' during completion to delete a region from
`icicle-region-alist'.

You can use `icicle-add-region' to define the list of regions,
`icicle-region-alist'."                 ; Doc string
  icicle-select-region-action           ; Function to perform the action
  "Select region: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((ignored (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))) ; Bindings
   (icicle-candidate-help-fn 'icicle-region-help)
   (icicle-delete-candidate-object 'icicle-delete-region-from-alist) ; `S-delete' deletes region.
   (icicle-list-nth-parts-join-string "\t")
   (icicle-list-join-string "\t")
   (icicle-list-end-string "")
   (icicle-list-use-nth-parts '(1))
   (icicle-sort-function nil)
   (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p t)
   (sorted (icicle-region-sorted))
   (icicle-candidates-alist (setq icicle-candidates-alist (icicle-region-add-buffers sorted)))))

(defun icicle-select-region-action (reg-name)
  "Action function for `icicle-select-region'."
  (let* ((reg (icicle-get-alist-candidate reg-name))
         (buf (cadr reg))
         (file (car (cddr reg))))
    (when (and (not (get-buffer buf)) file (file-readable-p file)) (find-file-noselect file))
    (pop-to-buffer buf) (raise-frame)
    (goto-char (cadr (cddr reg))) (push-mark (car (cddr (cddr reg))) 'nomsg 'activate))
  (setq deactivate-mark nil))

(defun icicle-region-sorted ()
  "`icicle-region-alist', sorted first by buffer name and then by tag."
  (sort (icicle-regions) (lambda (x1 x2) (let ((buf1 (cadr x1))
                                               (buf2 (cadr x2)))
                                           (or (string-lessp buf1 buf2)
                                               (and (string= buf1 buf2)
                                                    (string-lessp (car x1) (car x2))))))))

(defun icicle-region-add-buffers (region-list)
  "Add buffer names to REGION-LIST, if `icicle-add-buffer-name-flag'."
  (if icicle-add-buffer-name-flag
      (mapcar (lambda (entry)
                (let ((buf (copy-sequence (cadr entry))))
                  (put-text-property 0 (length buf) 'face 'icicle-special-candidate buf)
                  (cons (list (car entry) buf) (cdr entry))))
              region-list)
    region-list))

;;;###autoload
(icicle-define-command icicle-remove-region ; Command name
  "Remove a region from the list of regions, `icicle-region-alist'.
Update the persistent value of user option `icicle-region-alist'.

Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

To add a region to `icicle-region-alist', use command
`icicle-add-region' or customize `icicle-region-alist'." ; Doc string
  (lambda (cand)                        ; Function to perform the action
    (icicle-delete-region-from-alist cand)
    (if (active-minibuffer-window)
        (icicle-remove-candidate-display-others)
      (setq icicle-completion-candidates ; Finished.  Remove it without redisplaying *Completions*.
            (delete (elt icicle-completion-candidates icicle-candidate-nb)
                    icicle-completion-candidates))))
  "Remove region from saved regions: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((icicle-candidate-help-fn 'icicle-region-help) ; Additional bindings
   (icicle-list-nth-parts-join-string "\t")
   (icicle-list-join-string "\t")
   (icicle-list-end-string "")
   (icicle-list-use-nth-parts '(1))
   (icicle-sort-function nil)
   (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p t)
   (sorted (icicle-region-sorted))
   (icicle-candidates-alist (setq icicle-candidates-alist (icicle-region-add-buffers sorted)))))

(defun icicle-delete-region-from-alist (reg-name)
  "Delete the region named REG-NAME from `icicle-region-alist'."
  (setq icicle-region-alist
        (delete (cons reg-name (cdr (icicle-get-alist-candidate reg-name))) icicle-region-alist))
  (customize-save-variable 'icicle-region-alist icicle-region-alist))

;;;###autoload
(icicle-define-command icicle-remove-all-regions-in-buffer ; Command name
  "Remove all regions in a buffer from `icicle-region-alist'.
To remove individual regions, use command `icicle-remove-region'.
To add a region to `icicle-region-alist', use `icicle-add-region'.
Alternatively, you can customize `icicle-region-alist'." ; Doc string
  icicle-remove-all-regions-action
                                        ; Function to perform the action
  "Buffer: "                            ; `completing-read' args
  (icicle-remove-duplicates (mapcar (lambda (reg) (list (cadr reg))) icicle-region-alist))
  nil t nil nil (buffer-name))

(defun icicle-remove-all-regions-action (buffer)
  "Action function for `icicle-remove-all-regions-in-buffer'.
Remove all regions in BUFFER from `icicle-region-alist'.
BUFFER is the name of a buffer."
  (dolist (reg icicle-region-alist)
    (when (string= buffer (cadr reg)) (setq icicle-region-alist (delete reg icicle-region-alist))))
  (customize-save-variable 'icicle-region-alist icicle-region-alist)
  (message "Removed all regions in buffer `%s'" buffer))

;;;###autoload
(icicle-define-command icicle-search-region ; Command name
  "Search a region from the list of regions, `icicle-region-alist'.
Completion is available.  The regions are sorted alphabetically by
buffer, and then by tag; you cannot change the sort order.

You can use `S-delete' during completion to delete a region from
`icicle-region-alist'.

You can use `icicle-add-region' to define the list of regions.
Regions in `icicles-regions' that are in buffers that do not currently
exist are ignored.

Note that each region is defined by its limits, so that if the
region's buffer has changed, then the text used to identify the region
might no longer correspond to the text at the beginning of the
region.

If user option `icicle-add-buffer-name-flag' is non-nil, then each
completion candidate is annotated with the name of the region's
buffer, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the start and end points of the region and its length.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict."               ; Doc string
  icicle-search-region-action           ; Function to perform the action
  "Search region: " icicle-candidates-alist ; `completing-read' args
  nil (not icicle-add-buffer-name-flag) nil nil (if icicle-add-buffer-name-flag
                                                    (car (caar icicle-candidates-alist))
                                                  (caar icicle-candidates-alist))
  nil
  ((ignored (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))) ; Bindings
   (icicle-candidate-help-fn 'icicle-region-help)
   (icicle-delete-candidate-object 'icicle-delete-region-from-alist) ; `S-delete' deletes region.
   (enable-recursive-minibuffers t)
   (regexp (icicle-search-read-context-regexp))
   (icicle-list-nth-parts-join-string "\t")
   (icicle-list-join-string "\t")
   (icicle-list-end-string "")
   (icicle-list-use-nth-parts '(1))
   (icicle-sort-function nil)
   (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
   (icicle-inhibit-sort-p t)
   (sorted (icicle-region-sorted))
   (icicle-candidates-alist (setq icicle-candidates-alist (icicle-region-add-buffers sorted)))))

(defun icicle-search-region-action (reg-name)
  "Action function for `icicle-search-region'."
  (let* ((reg (icicle-get-alist-candidate reg-name))
         (buf (cadr reg))
         (file (car (cddr reg))))
    (when (and (not (get-buffer buf)) file (file-readable-p file)) (find-file-noselect file))
    (pop-to-buffer buf) (raise-frame)
    (let ((icicle-show-Completions-initially-flag t)
          (icicle-candidates-alist icicle-candidates-alist))
      (icicle-search (cadr (cddr reg)) (car (cddr (cddr reg)))
                     regexp t)) ; `regexp' is free here.  It is bound in `icicle-search-region'.
    (save-excursion
      (set-buffer (window-buffer (minibuffer-window)))
      (icicle-erase-minibuffer))))

(defun icicle-region-help (reg-name)
  "Use as `icicle-candidate-help-fn' for `icicle-region-alist' commands."
  (icicle-msg-maybe-in-minibuffer
   (let* ((reg (icicle-get-alist-candidate reg-name))
          (file (car (cddr reg)))
          (start (cadr (cddr reg)))
          (end (car (cddr (cddr reg)))))
     (or (concat (format "%d to %d" end start) " in buffer `" (cadr reg)
                 (and file (format "', file `%s" file))
                 (format "', %d chars" (- end start)))
         "No help"))))

(defun icicle-regions ()
  "Variable `icicle-region-alist', but without non-existent non-file buffers."
  (let ((unsorted-regions (icicle-delete-if (lambda (reg) (and (not (car (cddr reg))) ; No file.
                                                               (not (get-buffer (cadr reg)))))
                                            icicle-region-alist)))
    (if icicle-sort-function
        (sort unsorted-regions (lambda (a b) (funcall icicle-sort-function (car a) (car b))))
      unsorted-regions)))

;;;###autoload
(defun icicle-search-generic ()         ; Bound to `C-x `'.
  "Run `icicle-search-command'.  By default, this is `icicle-search'.
In Compilation and Grep modes, this is `icicle-compilation-search'.
In Comint, Shell, GUD, and Inferior Lisp modes, this is
   `icicle-comint-search'."
  (interactive)
  (call-interactively icicle-search-command))

;;;###autoload
(defun icicle-search (beg end scan-fn-or-regexp require-match ; Bound to `C-c `'.
                      &optional where &rest args)
  "Search for matches, with completion, cycling, and hit replacement.
Interactively, search for regexp matches.  You are prompted for a
regexp, which you enter using `RET'.  The search hits (matches) are
available as completion candidates.  You can then use apropos
completion to filter the candidates using a different regexp, which
you can change dynamically (as always).  You can replace individual
matches with another string, as in `query-replace' or
`query-replace-regexp'.  Candidates appear in order of buffer
occurrence; you cannot sort them.

Non-interactively, search can be for regexp matches or any other kind
of matches.  Argument SCAN-FN-OR-REGEXP is the regexp to match, or it
is a function that defines an alist of buffer zones to search.  You
can navigate among the matching buffer zones (defined either as regexp
matches or via function), called search \"contexts\", and you can
match another regexp against the text in search context.  See the end
of this description for information about the other arguments.

If the search-context regexp contains regexp subgroups, that is,
subexpressions of the form `\(...\)', then you are prompted for the
subgroup to use to define the search contexts.  Subgroup 0 means the
context is whatever matches the whole regexp.  Subgroup 1 means the
context is whatever matches the first subgroup, and so on.  The
subgroup number is the number of occurrences of `\(', starting at the
beginning of the regexp.

Optional Behaviors: Prefix Argument
-----------------------------------

By default, search only the current buffer.  Search the active region,
or, if there is none, search the entire buffer.

With a prefix argument, you can search multiple buffers or regions, as
follows:

- With a simple prefix arg (`C-u'), search multiple buffers
completely.  You are prompted for the buffers to search - all of each
buffer is searched.

- With a numeric prefix arg, search all of the regions in
`icicle-region-alist'.  Those regions can be in any buffers.  If a
region is in a buffer that does not exist, it is skipped.  You can
always re-create the buffer (e.g. visit the file), and try again.

Note: To individually search selected regions in
`icicle-region-alist', use multi-command `icicle-search-region'.

Navigation and Help
-------------------

The use of completion for this command is special.  It is not unusual
in this context to have multiple completion candidates that are
identical - only the positions of their occurrences in the search
buffer(s) differ.  In that case, you cannot choose one simply by
completing it in the minibuffer, because the destination would be
ambiguous.  That is, simply completing your input and entering the
completion with `RET' will not take you to its occurrence in the
search buffer, unless it is unique.

Instead, choose search hits to visit using any of the candidate-action
keys: `C-RET', `C-mouse-2', `C-next', `C-prior', `C-down', and `C-up'.
The last four of these cycle among the search hits.  The current
candidate in *Completions* corresponds to the current location
visited (it is not off by one, as is usually the case in Icicles).

As always, the `C-M-' keys provide help on individual candidates:
`C-M-RET', `C-M-mouse-2', `C-M-next', `C-M-prior', `C-M-down', and
`C-M-up'.  For `icicle-search', they indicate the buffer and position
of the search hit.

You can cycle among candidates without moving to their occurrences in
the search buffer, using `next', `prior', `down', and `up' (no `C-').

Search and Replace
------------------

You can replace the current search match by using any of the
alternative action keys: `S-C-RET', `S-C-mouse-2' (in *Completions*),
`S-C-next', `S-C-prior', `S-C-down', and `S-C-up'.  You can use
`S-C-insert' to replace all matches at once.

At the first use of any of these, you are prompted for the replacement
string; it is used thereafter.  The replacement string can be anything
that is allowed as a replacement by `query-replace-regexp', including
Lisp-evaluation constructs: `\,...'.

Unlike `query-replace', you need not visit each search match - you can
visit and replace selected matches in any order.

What is meant here by a \"search match\"?  It can be either an entire
search context or whatever matches your current minibuffer input.

Key `C-,' toggles option `icicle-search-replace-whole-candidate-flag'.
By default, the entire current search context is replaced, that is,
whatever matches the context regexp that you entered initially, using
`RET'.  However, you can use `C-,' at any time during searching to
toggle between this default behavior and replacement of whatever your
current minibuffer input matches.

Remember this:

 - If `icicle-search-replace-whole-candidate-flag' is non-nil, then
   the granularity of replacement is a complete search context.  In
   this case, replacement behaves similarly to `query-replace-regexp'.
   You can still use minibuffer input to filter the set of search
   contexts, but replacement is on a whole-context basis.

 - If `icicle-search-replace-whole-candidate-flag' is nil, then you
   can replace multiple input matches separately within a search
   context.  This behavior is unique to Icicles.

If `icicle-search-replace-whole-candidate-flag' is non-nil, then you
can use the navigational alternative action keys, `S-C-next',
`S-C-prior', `S-C-down', and `S-C-up', to replace successive search
contexts.

Search traversal using these keys is always by search context, not by
input match.  This means that you cannot use these keys to replace
individual input matches within a search context, except for the first
such match.  That is, if `icicle-search-replace-whole-candidate-flag'
is nil and you use these keys, then only the first match of your input
in each search context is replaced.

If your input matches multiple parts of the search context, and you
want to replace them in order, then use `S-C-RET' repeatedly.  This
replaces successive input matches within a search context, then moves
on to the next context, and so on.  You can traverse all matches of
your input in the order they appear in the buffer by repeating
`S-C-RET'.

Repeated use of `S-C-RET' is generally for the case where you are
replacing input matches, not whole search contexts.  If you repeat
`S-C-RET' when `icicle-search-replace-whole-candidate-flag' is
non-nil, then you will, in effect, just replace the same context over
and over - unless, that is, your current input does not match the
replacement text.  In that case, the replacement is no longer a
matching search context (candidate), and `S-C-RET' moves on to the
next context.

Highlighting
------------

In the search buffer (that is, where the hits are), `icicle-search'
does the following:

- Highlights the current match (buffer zone) using face
  `icicle-search-main-regexp-current'.

- Highlights the match for your current regexp input using face
  `icicle-search-current-input'.  Interactively, this is a different
  regexp from the initial one that defines the match zones.

- Highlights the first `icicle-search-highlight-threshold' matches
  (buffer zones), using face `icicle-search-main-regexp-others'.

If user option `icicle-search-cleanup-flag' is non-nil (the default),
then all search highlighting is removed from the search buffer when
you are finished searching.  If it is nil, then you can remove this
highlighting later using command `icicle-search-highlight-cleanup'.
You can toggle `icicle-search-cleanup-flag' at any time using `C-.'
in the minibuffer.

Using Regexps
-------------

At any time, you can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' (command
`icicle-insert-string-from-variable') to insert text (e.g. a regexp)
from a variable into the minibuffer.  For example, you can search for
ends of sentences by using `C-u \\[icicle-insert-string-from-variable]' and choosing variable
`sentence-end' as the variable.  And you can use
`\\[icicle-save-string-to-variable]' to save a string to a variable
for later use by `\\[icicle-insert-string-from-variable]'.

When employed with useful regexps, `C-=' can turn `icicle-search' into
a general navigator or browser of code, mail messages, and many other
types of buffer.  Imenu regexps work fine, for example - command
`icicle-imenu' simply uses `icicle-search' this way.  See
`icicle-insert-string-from-variable' for more tips on inserting
regexps from variables.

Additional Information
----------------------

If user option `icicle-add-buffer-name-flag' is non-nil, then each
candidate is annotated with the name of the buffer where the hit
occurs, to facilitate orientation.  Note that even when the value is
nil, you can use `C-M-mouse-2' and so on to see the buffer name, as
well as the position of the hit in the buffer.

Completion is lax if `icicle-add-buffer-name-flag' is non-nil;
otherwise, it is strict.

After you visit a completion candidate, the hooks in variable
`icicle-search-hook' are run.

`icicle-search' overrides `icicle-ignore-space-prefix-flag', binding
it to nil, so that candidates with initial spaces can be matched.

`icicle-search' sets `icicle-search-final-choice' to the final user
choice, which might not be one of the search candidates if
REQUIRE-MATCH is nil.

Non-Interactive Use
-------------------

When called non-interactively, these are the `icicle-search'
arguments:

BEG is the beginning of the region to search; END is the end.
SCAN-FN-OR-REGEXP: Regexp or function that determines the set of
  initial candidates (match zones).
REQUIRE-MATCH is passed to `completing-read'.
Optional arg WHERE is either a list of buffers or a list of region
  entries that have the same form as `icicle-region-alist'.  If nil,
  then only the current buffer is used.
ARGS are arguments that are passed to function SCAN-FN-OR-REGEXP.

This command is intended for use only in Icicle mode."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(icicle-search-read-context-regexp)
                 ,(not icicle-add-buffer-name-flag)
                 ,(cond ((consp current-prefix-arg)
                         (let ((icicle-show-Completions-initially-flag t))
                           (mapcar #'get-buffer
                                   (let ((icicle-buffer-require-match-flag 'partial-match-ok))
                                     (icicle-buffer-list)))))
                        (current-prefix-arg icicle-region-alist)
                        (t nil))))
  (let ((icicle-incremental-completion-flag 'always)
        (icicle-sort-function nil)
        (icicle-transform-function (if (interactive-p) nil icicle-transform-function))
        (icicle-candidate-alternative-action-fn (or icicle-candidate-alternative-action-fn
                                                    'icicle-search-replace-search-hit))
        (save-action-first-p icicle-act-first-then-navigate-p)
        (replace-count 0)
        (icicle-inhibit-sort-p t)
        (icicle-searching-p t)
        (icicle-ignore-space-prefix-flag nil)
        (icicle-list-nth-parts-join-string "\t")
        (icicle-list-join-string "\t")
        (icicle-list-end-string "")
        (icicle-list-use-nth-parts '(1))
        (icicle-expand-input-to-common-match-flag nil)
        (completion-ignore-case case-fold-search)
        (orig-point (point))
        (orig-window (selected-window))
        (mark-active nil))              ; So region highlighting doesn't hide highlighting here.
    (setq icicle-candidates-alist nil    icicle-search-replacement nil)
    (setq icicle-search-context-regexp (and (stringp scan-fn-or-regexp) scan-fn-or-regexp))
    ;; Build `icicle-candidates-alist'. Highlight up to `icicle-search-highlight-threshold' matches
    (message "Finding matches...")
    (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
           (dolist (buf where)
             (icicle-search-define-candidates buf scan-fn-or-regexp nil nil args)))
          ((consp where)                ; Search all regions in `icicle-region-alist'.
           (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))
           (dolist (reg where)
             (if (bufferp (get-buffer (cadr reg)))
                 (icicle-search-define-candidates (get-buffer (cadr reg)) scan-fn-or-regexp
                                                  (cadr (cddr reg)) (car (cddr (cddr reg))) args)
               (message "Skipping regions in `%s' - no such buffer" (cadr reg)) (sit-for 2))))
          (t                            ; Search this buffer only.
           (icicle-search-define-candidates nil scan-fn-or-regexp beg end args)))
    (unless icicle-candidates-alist
      (error (concat "No match" (and (stringp scan-fn-or-regexp)
                                     (format " for regexp `%s'" scan-fn-or-regexp)))))
    (let ((icicle-candidate-action-fn (or icicle-candidate-action-fn 'icicle-search-action))
          (icicle-candidate-help-fn 'icicle-search-help)
          (icicle-update-input-hook (list 'icicle-search-highlight-all-input-matches)))
      (setq icicle-search-final-choice nil)
      (unwind-protect
           (condition-case failure
               (let (candidate-entries)
                 (setq icicle-act-first-then-navigate-p nil
                       icicle-search-final-choice (completing-read
                                                   "Choose an occurrence: "
                                                   icicle-candidates-alist nil require-match
                                                   nil 'icicle-search-history)
                       icicle-completion-candidates (if require-match
                                                        (list icicle-search-final-choice)
                                                      icicle-completion-candidates)
                       candidate-entries (icicle-filter-alist icicle-candidates-alist
                                                              icicle-completion-candidates))
                 (cond
                   ;; No match required, and not a match - just run the hook.
                   ((and (not require-match) (null icicle-completion-candidates))
                    (run-hooks 'icicle-search-hook))
                   ;; Didn't cycle - completed.
                   ((not (wholenump icicle-candidate-nb))
                    (if (cdr candidate-entries)
                        (error "Ambiguous choice. Try again, using `C-next' to browse.")
                      (goto-char (cdr (car candidate-entries))) ; Go to sole completion.
                      (run-hooks 'icicle-search-hook)))
                   ;; Cycled.
                   (t
                    (let (cand-nb marker buf)
                      (if require-match
                          (setq marker (cdar candidate-entries))
                        (setq cand-nb (mod icicle-candidate-nb (length icicle-candidates-alist)))
                        ;;$$$ If cycled with action function, add or subtract 1 from candidate #.
                        ;; (when (memq last-command '(icicle-next-apropos-candidate-action
                        ;;                            icicle-next-prefix-candidate-action))
                        ;;   (setq cand-nb (1- cand-nb))
                        ;;   (when (< cand-nb 0) (setq cand-nb 0)))
                        ;; (when (memq last-command '(icicle-previous-apropos-candidate-action
                        ;;                            icicle-previous-prefix-candidate-action))
                        ;;   (setq cand-nb (1+ cand-nb))
                        ;;   (when (> cand-nb (length icicle-candidates-alist))
                        ;;     (setq cand-nb (length icicle-candidates-alist))))
                        (setq marker (cdr (elt candidate-entries cand-nb))))
                      (unless marker (error "No such occurrence"))
                      (setq buf (marker-buffer marker))
                      (unless (bufferp buf) (error "No such buffer: %s" buf))
                      (pop-to-buffer buf)
                      (raise-frame)
                      (goto-char (marker-position marker))
                      (select-frame-set-input-focus (selected-frame))
                      (run-hooks 'icicle-search-hook)))))
             (quit (goto-char orig-point))
             (error (goto-char orig-point)
                    (error "%s" (error-message-string failure))))
        (setq icicle-act-first-then-navigate-p save-action-first-p)
        (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
        (when (window-live-p orig-window)
        (select-window orig-window) (select-frame-set-input-focus (selected-frame))))
      icicle-search-final-choice)))

;; This is the same as `region-or-buffer-limits' in `misc-fns.el'.
(defun icicle-region-or-buffer-limits ()
    "Return the start and end of the region as a list, smallest first.
If the region is not active or empty, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun icicle-search-read-context-regexp (&optional prompt initial-contents keymap read hist
                                          default-value inherit-input-method keep-all)
  "Read context regexp and determine `icicle-search-context-level'.
Arguments are as for `read-from-minibuffer'.
A default prompt is provided.
By default, `regexp-history' is used as the history list."
  (setq prompt   (or prompt "Search for (regexp): ")
        hist     (or hist 'regexp-history))
  (let ((regexp (read-from-minibuffer prompt initial-contents keymap read hist
                                      default-value inherit-input-method keep-all)))
        
    (setq prompt                      "Subgroup to use as search context [0, 1, 2,...]: "
          icicle-search-context-level (if (string-match "\\\\(" regexp)
                                          (truncate (if (fboundp 'read-number)
                                                        (read-number prompt 0)
                                                      (read-from-minibuffer ; Hope for a number.
                                                       prompt nil nil nil nil 0)))
                                        0))
    regexp))

(defun icicle-search-define-candidates (buffer scan-fn-or-regexp &optional beg end args)
  "Define completion candidates for `icicle-search'.
BUFFER is a buffer to scan for candidates.
SCAN-FN-OR-REGEXP, BEG, and END are the same as for `icicle-search'.
ARGS are other arguments that are passed to SCAN-FN-OR-REGEXP."
  (if (functionp scan-fn-or-regexp)
      (apply scan-fn-or-regexp buffer beg end args)
    (icicle-search-regexp-scan buffer scan-fn-or-regexp beg end)))

(defun icicle-search-regexp-scan (buffer regexp &optional beg end)
  "Scan BUFFER for REGEXP, pushing hits onto `icicle-candidates-alist'.
If REGEXP has subgroups, then what the Nth subgroup matches is used as
the search context (hit), where N = `icicle-search-context-level'.  If
N=0, then the overall match of REGEXP is used as the search context.

If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are non-nil, scan only between positions BEG and END."
  (setq regexp (or regexp (icicle-search-read-context-regexp)))
  (let ((add-bufname-p (and buffer icicle-add-buffer-name-flag)))
    (unless buffer (setq buffer (current-buffer)))
    (when (bufferp buffer)
      (let ((last-beg nil))
        (set-buffer buffer)
        (unless (and beg end) (setq beg (point-min) end (point-max)))
        (condition-case icicle-search-regexp-scan
            (save-excursion
              (goto-char (setq last-beg beg))
              (while (and beg (< beg end))
                (while (and (setq beg (re-search-forward regexp end t)) (eq last-beg beg))
                  (forward-char) (setq beg (1+ beg))) ; Matched again, same place.  Advance 1 char.
                (when beg               ; Add (strg . pos) to candidates.
                  (unless (match-beginning icicle-search-context-level)
                    (error "Search context has no subgroup of level %d - try a lower number"
                           icicle-search-context-level))
                  (push (cons (if add-bufname-p
                                  (list (buffer-substring-no-properties
                                         (match-beginning icicle-search-context-level)
                                         (match-end icicle-search-context-level))
                                        (let ((string (copy-sequence (buffer-name))))
                                          (put-text-property 0 (length string)
                                                             'face 'icicle-special-candidate string)
                                          string))
                                (buffer-substring-no-properties
                                 (match-beginning icicle-search-context-level)
                                 (match-end icicle-search-context-level)))
                              (copy-marker (match-end icicle-search-context-level)))
                        icicle-candidates-alist)
                  ;; Highlight search context in buffer.
                  (when (<= (length icicle-candidates-alist) icicle-search-highlight-threshold)
                    (let ((ov (make-overlay (match-beginning icicle-search-context-level)
                                            (match-end icicle-search-context-level))))
                      (push ov icicle-search-overlays)
                      (overlay-put ov 'priority 200) ; > ediff's 100+, < isearch-overlay's 1001.
                      (overlay-put ov 'face 'icicle-search-main-regexp-others))))
                (setq last-beg beg))
              (setq icicle-candidates-alist (nreverse icicle-candidates-alist)))
          (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
          (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
                 (error (error-message-string icicle-search-regexp-scan))))))))

(defun icicle-search-highlight-all-input-matches (&optional input)
  "Highlight, inside each search context, what the current input matches."
  (save-excursion
    ;; Update by deleting (if it exists) and then creating.
    ;; If a single overlay exists, it means that the user just changed
    ;; `icicle-search-highlight-threshold' to non-zero.
    ;; Otherwise, it's nil or a list of overlays.
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays))))
  (when icicle-search-highlight-all-current-flag
    (setq input (or input icicle-current-input))
    (unless (or (string= "" input) (null icicle-search-overlays))
      (save-excursion
        (dolist (ov icicle-search-overlays)
          (set-buffer (overlay-buffer ov))
          (save-restriction             ; Search within the current search context.
            (narrow-to-region (overlay-start ov) (overlay-end ov))
            (goto-char (point-min))
            (save-match-data
              (while (condition-case nil (re-search-forward input nil 'move-to-end) (error nil))
                (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                (push ov icicle-search-refined-overlays)
                (overlay-put ov 'priority 204)
                (overlay-put ov 'face 'icicle-search-current-input)))))))))

(defun icicle-search-replace-search-hit (candidate)
  "Replace search hit CANDIDATE with `icicle-search-replacement'."
  (let ((cand-nb (or icicle-candidate-nb 0)) ; Replace-all has nil, so use 0.
        (cands icicle-completion-candidates)
        (completion-cmd icicle-last-completion-command)
        (last-input icicle-last-input)
        (compl-win (get-buffer-window "*Completions*" 0)))
    (unless icicle-search-replacement
      (icicle-search-define-replacement)
      (when (and compl-win cands)
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list icicle-completion-candidates))))
    (setq icicle-completion-candidates   cands
          icicle-candidate-nb            cand-nb
          icicle-last-completion-command completion-cmd
          icicle-last-input              last-input))
  (funcall icicle-candidate-action-fn candidate icicle-search-replacement))

(defun icicle-search-action (string &optional replace-string)
  "Completion action function for command `icicle-search'.
1. Highlight the current candidate in *Completions*.
2. Move to the regexp match in the original buffer and highlight it.
3. If `icicle-search-highlight-threshold' is zero, highlight what the
   current input matches, inside the match of the initial regexp.
4. If REPLACE-STRING is non-nil, replace current candidate with it.
   If `icicle-search-replace-whole-candidate-flag' is nil, replace
   only the candidate part that matches the current input.

   Note: The replacement can be anything allowed as a replacement by
   `query-replace-regexp', including Lisp-evaluation
   constructs (`\,...')."
  ;; Argument STRING is ignored.
  (condition-case icicle-search-action
      (progn
        (icicle-highlight-candidate-in-Completions)
        ;; Move cursor to the match in the original buffer and highlight it.
        (let* ((cand+mrker (elt (icicle-filter-alist icicle-candidates-alist
                                                     icicle-completion-candidates)
                                icicle-candidate-nb))
               (candidate (if (consp (car-safe cand+mrker))
                              (car-safe (car-safe cand+mrker))
                            (car-safe cand+mrker)))
               (marker (cdr-safe cand+mrker)))
          (unless marker (error "No such occurrence"))
          (save-selected-window
            (when (window-live-p orig-window) (select-window orig-window))
            (let ((completion-ignore-case case-fold-search)
                  (buf (marker-buffer marker))
                  (first-p t)
                  (icicle-candidate-nb icicle-candidate-nb)) ; Save and restore this.
              (unless (bufferp buf) (error "No such buffer: %s" buf))
              (pop-to-buffer buf)
              (raise-frame)
              (goto-char marker)
              ;; Highlight current search context using `icicle-search-main-regexp-current'.
              (icicle-place-overlay (- marker (length candidate)) marker
                                    'icicle-search-current-overlay
                                    'icicle-search-main-regexp-current
                                    buf)
              (overlay-put icicle-search-current-overlay 'priority 202)
              (save-excursion
                (save-restriction       ; Search within the current search context.
                  (narrow-to-region (- marker (length candidate)) marker)
                  (icicle-search-highlight-context-levels)
                  (icicle-search-highlight-input-matches-here)
                  (when replace-string
                    (goto-char (point-min))
                    (cond (icicle-search-replace-whole-candidate-flag
                           (cond ((string= candidate replace-string) ; Sanity check only.
                                  (save-restriction (widen) (message
                                                             "Replacement = candidate, and \
current input matches candidate") (sit-for 2)))
                                 (t
                                  (set-match-data (list (point-min) (point-max)))
                                  (icicle-search-replace-match replace-string
                                                               (icicle-search-replace-fixed-case-p
                                                                icicle-search-context-regexp)))))
                          (t
                           (save-match-data
                             (while (and (re-search-forward icicle-current-input nil 'move-to-end)
                                         (or first-p icicle-all-candidates-action-p))
                               (setq first-p nil)
                               (icicle-search-replace-match replace-string
                                                            (icicle-search-replace-fixed-case-p
                                                             icicle-current-input))))))
                    (icicle-search-replace-candidate cand+mrker
                                                     (buffer-substring (point-min) (point-max)))
                    (save-selected-window (select-window (minibuffer-window))
                                          (icicle-retrieve-last-input))
                    (icicle-update-completions)
                    (save-window-excursion (icicle-display-candidates-in-Completions))
                    (when (>= icicle-candidate-nb (length icicle-completion-candidates))
                      (setq icicle-candidate-nb 0))
                    (setq icicle-last-completion-candidate (buffer-substring (point-min)
                                                                             (point-max)))
                    (icicle-highlight-candidate-in-Completions)
                    (icicle-search-highlight-context-levels)
                    (icicle-search-highlight-input-matches-here))))
              (run-hooks 'icicle-search-hook)))
          nil))                         ; Return nil for success.
    (error (error-message-string icicle-search-action))) ; Return error message.
  (select-frame-set-input-focus (window-frame (minibuffer-window))))

(defun icicle-search-replace-match (replace-string fixedcase)
  "Replace current match with REPLACE-STRING, interpreting escapes.
Treat REPLACE-STRING as it would be by `query-replace-regexp'.
FIXEDCASE is as for `replace-match'.  Non-nil means do not alter case."
  (if (fboundp 'query-replace-compile-replacement) ; Emacs 22.
      (let ((compiled
             (save-match-data
               (query-replace-compile-replacement replace-string
                                                  (not icicle-search-replace-literally-flag)))))
        (condition-case icicle-search-replace-match1
            (let ((enable-recursive-minibuffers t) ; So we can read input from \?.
                  ;; Save and restore these, because we might read input from \?.
                  (icicle-last-completion-command icicle-last-completion-command)
                  (icicle-last-input icicle-last-input))
              (replace-match-maybe-edit
               (if (consp compiled)
                   (funcall (car compiled) (cdr compiled) (setq replace-count (1+ replace-count)))
                 compiled)
               fixedcase icicle-search-replace-literally-flag nil (match-data)))
          (error (icicle-remove-Completions-window) (error "No match for %s" replace-string))))
    (condition-case icicle-search-replace-match2 ; Emacs < 22.  Try to interpret `\'.
        (replace-match replace-string fixedcase icicle-search-replace-literally-flag)
      (error (replace-match replace-string fixedcase t))))) ;   If error, replace literally.

(defun icicle-search-highlight-context-levels ()
  "Highlight context levels differently (up to 8 levels).
No such highlighting is done if any of these conditions holds:
 * `icicle-search-context-level' is not 0 (search context < regexp).
 * `icicle-search-highlight-context-levels-flag' is nil.
 * `icicle-search-context-regexp' is nil (non-regexp searching)."
  (unless (or (/= icicle-search-context-level 0)
              (not icicle-search-highlight-context-levels-flag)
              (not icicle-search-context-regexp)) ; E.g. text-property searching
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays (cdr icicle-search-level-overlays)))
    (save-match-data
      (let ((level 1)
            (max-levels (regexp-opt-depth icicle-search-context-regexp)))
        (goto-char (point-min))
        (re-search-forward icicle-search-context-regexp nil t)
        (while (<= level (min max-levels 8))
          (let ((ov (make-overlay (match-beginning level) (match-end level))))
            (push ov icicle-search-level-overlays)
            (overlay-put ov 'priority 205) ; > ediff's 100+, < isearch-overlay's 1001.
            (overlay-put ov 'face (intern (concat "icicle-search-context-level-"
                                                  (number-to-string level)))))
          (setq level (1+ level)))))))

(defun icicle-search-highlight-input-matches-here ()
  "Highlight all input matches in the current search context."
  (unless (or (> 0 icicle-search-highlight-threshold) (string= "" icicle-current-input))
    (goto-char (point-min))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays)))
    (let ((ov nil))
      (save-match-data
        (while (and (not (eobp)) (re-search-forward icicle-current-input nil 'move-to-end))
          (setq ov (make-overlay (match-beginning 0) (match-end 0)))
          (push ov icicle-search-refined-overlays)
          (overlay-put ov 'priority 204)
          (overlay-put ov 'face 'icicle-search-current-input))))))

(defun icicle-search-replace-fixed-case-p (from)
  "Return non-nil if FROM should be replaced without transferring case.
FROM is a string or nil.  If FROM is nil, then return nil."
  (and from (not (and case-fold-search case-replace (string= from (downcase from))))))

(defun icicle-search-replace-candidate (cand+mrker new-cand)
  "In `icicle-candidates-alist', replace car of CAND+MRKER with NEW-CAND."
  (let ((newlist icicle-candidates-alist))
    (while newlist
      (when (equal (car newlist) cand+mrker) (setcar newlist (cons new-cand (cdr-safe cand+mrker))))
      (setq newlist (cdr newlist)))
  icicle-candidates-alist))

(defun icicle-search-help (cand)
  "Use as `icicle-candidate-help-fn' for `icicle-search' commands."
  (icicle-msg-maybe-in-minibuffer
   (let ((marker (cdr (icicle-get-alist-candidate cand))))
     (or (concat "Buffer: `" (buffer-name (marker-buffer marker))
                 (format "', Position: %d" (marker-position marker)))
         "No help"))))

;;;###autoload
(defun icicle-search-text-property (beg end require-match ; Bound to `C-c "'.
                                    &optional where prop value)
  "Search for text that has a text property with a certain value.
If the property is `face' or `font-lock-face', then you can use
completion to pick the face.

This command is defined using `icicle-search'; see the `icicle-search'
documentation for more information."
  (interactive
   (let* ((where (cond ((consp current-prefix-arg)
                        (let ((icicle-show-Completions-initially-flag t))
                          (mapcar #'get-buffer (icicle-buffer-list))))
                       (current-prefix-arg icicle-region-alist)
                       (t nil)))
          (beg+end (icicle-region-or-buffer-limits))
          (beg1 (car beg+end))
          (end1 (cadr beg+end))
          (props (mapcar (lambda (prop) (list (symbol-name prop)))
                         (icicle-text-properties-in-buffers where beg1 end1)))
          (prop (intern (completing-read "Text property to search: " props nil nil nil nil "face")))
          (value (if (memq prop '(face font-lock-face))
                     (read-face-name "Face: ")
                   (intern (read-from-minibuffer "Property value: " nil nil nil
                                                 'icicle-text-property-value-history)))))
     `(,beg1 ,end1 ,(not icicle-add-buffer-name-flag) ,where ,prop ,value)))
  (icicle-search beg end 'icicle-search-text-property-scan require-match where prop value))

(defun icicle-text-properties-in-buffers (where beg end)
  "List of all text properties in WHERE.
Only the text properties are included, not their values.
WHERE is either a list of buffers or a list of region
  entries that have the same form as `icicle-region-alist'.  If nil,
  then only the current buffer is used."
  (cond ((and (consp where) (bufferp (car where))) ; List of buffers - search buffers.
         (dolist (buf where) (icicle-text-properties-in-buffer buf nil nil)))
        ((consp where)                  ; Search all regions in `icicle-region-alist'.
         (when icicle-region-auto-open-files-flag (icicle-region-open-all-files))
         (dolist (reg where)
           (when (bufferp (get-buffer (cadr reg)))
             (icicle-text-properties-in-buffer (get-buffer (cadr reg)) 
                                               (cadr (cddr reg)) (car (cddr (cddr reg)))))))
        (t                              ; Search this buffer only.
         (icicle-text-properties-in-buffer (current-buffer) beg end))))

(defun icicle-text-properties-in-buffer (&optional buffer beg end)
  "List of all text properties in BUFFER between BEG and END.
Only the text properties are included, not their values."
  (let ((props nil)
        curr-props)
    (save-excursion
      (unless buffer (setq buffer (current-buffer)))
      (set-buffer buffer)
      (unless (and beg end) (setq beg (point-min) end (point-max)))
      (while (< beg end)
        (setq beg        (or (next-property-change beg nil end) end)
              curr-props (text-properties-at beg))
        (while curr-props
          (unless (memq (car curr-props) props) (push (car curr-props) props))
          (setq curr-props (cddr curr-props)))))
    props))

(defun icicle-search-text-property-scan (buffer beg end prop value)
  "Scan BUFFER for text property PROP with value VALUE.
Push hits onto `icicle-candidates-alist'.
If BUFFER is nil, scan the current buffer.
Highlight the matches in face `icicle-search-main-regexp-others'.
If BEG and END are non-nil, scan only between positions BEG and END."
  (let ((add-bufname-p (and buffer icicle-add-buffer-name-flag))
        (zone-end nil))
    (unless buffer (setq buffer (current-buffer)))
    (set-buffer buffer)
    (unless (and beg end) (setq beg (point-min) end (point-max)))
    (condition-case icicle-search-text-property-scan
        (save-excursion
          (while (and (< beg end)
                      (let ((currval (get-text-property beg prop)))
                        (if (consp currval) (not (memq value currval)) (not (eq value currval)))))
            (setq beg (next-single-property-change beg prop nil end)))
          (while (and beg (< beg end))
            (setq zone-end (or (next-single-property-change beg prop nil end) end))
            (push (cons (if add-bufname-p
                            (list (buffer-substring-no-properties beg zone-end)
                                  (let ((string (copy-sequence (buffer-name))))
                                    (put-text-property 0 (length string)
                                                       'face 'icicle-special-candidate string)
                                    string))
                          (buffer-substring-no-properties beg zone-end))
                        (copy-marker zone-end))
                  icicle-candidates-alist)
            ;; Highlight candidate in buffer.
            (when (<= (length icicle-candidates-alist) icicle-search-highlight-threshold)
              (let ((ov (make-overlay beg zone-end)))
                (push ov icicle-search-overlays)
                (overlay-put ov 'priority 200) ; > ediff's 100+, but < isearch overlays
                (overlay-put ov 'face 'icicle-search-main-regexp-others)))
            (setq beg zone-end)
            (while (and (< beg end)
                        (let ((currval (get-text-property beg prop)))
                          (if (consp currval) (not (memq value currval)) (not (eq value currval)))))
              (setq beg (next-single-property-change beg prop nil end))))
          (setq icicle-candidates-alist (nreverse icicle-candidates-alist)))
      (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
      (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
             (error (error-message-string icicle-search-text-property-scan))))))

;;;###autoload
(defun icicle-search-highlight-cleanup ()
  "Remove all highlighting from the last use of `icicle-search'."
  (interactive)
  (let ((inhibit-quit t))
    (message "Removing search highlighting...")
    (while icicle-search-overlays
      (delete-overlay (car icicle-search-overlays))
      (setq icicle-search-overlays (cdr icicle-search-overlays)))
    (while icicle-search-level-overlays
      (delete-overlay (car icicle-search-level-overlays))
      (setq icicle-search-level-overlays (cdr icicle-search-level-overlays)))
    (when (overlayp icicle-search-current-overlay)
      (delete-overlay icicle-search-current-overlay))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays)))
    (message "Removing search highlighting...done")))

;;;###autoload
(defun icicle-occur (beg end &optional buffers) ; Bound to `C-c ''.
  "`icicle-search' with a regexp of \".*\".  An `occur' with icompletion.
Type a regexp to match within each line of the buffer.  Use `S-TAB' to
show matching lines.  Use `C-RET' or `C-mouse-2' to go to the line of
the current candidate.  Use `C-next', `C-prior', `C-down', or`C-up' to
cycle among the matching lines.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match."
  (interactive `(,@(icicle-region-or-buffer-limits)
                 ,(and current-prefix-arg
                       (let ((icicle-show-Completions-initially-flag t))
                         (mapcar #'get-buffer (icicle-buffer-list))))))
  (let ((fg (face-foreground 'icicle-search-main-regexp-others))
        (bg (face-background 'icicle-search-main-regexp-others))
        (icicle-transform-function (if (interactive-p) nil icicle-transform-function)))
    (unwind-protect
         (progn
           (set-face-foreground 'icicle-search-main-regexp-others nil)
           (set-face-background 'icicle-search-main-regexp-others nil)
           (icicle-search beg end ".*" (not icicle-add-buffer-name-flag) buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-comint-search (beg end)   ; Bound to `C-x `' in `comint-mode'.
  "Use `icicle-search' to pick up a previous input for reuse.
Use this in a `comint-mode' buffer, such as *shell* or
*inferior-lisp*.  This searches your interactive history in the buffer
for a match to your current input, which you can change dynamically.
When you choose a previous input, it is copied to the current prompt,
for reuse.  If the region is active, then only it is searched;
otherwise, the entire buffer is searched.

Use `C-RET' or `C-mouse-2' to choose a previous input for reuse.  Use
`C-next', `C-prior', `C-down', or `C-up' to cycle among your previous
inputs.

As for other Icicles search commands, your current input narrows the
set of possible candidates.  See `icicle-search' for more
information.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

Note that previous commands are identified by looking through the
shell buffer for a shell prompt.  This is not foolproof.  If, for
instance you use command `ls', the output includes an auto-save file
such as #foo.el#, and `#' in the first column represents a shell
prompt, then #foo.el# will be misinterpreted as a previous command.

Also, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

Being a search command, `icicle-comint-search' cannot give you access
to previous commands that are not visible in the current buffer.  See
also \\<comint-mode-map>\\[icicle-comint-command] for another way to
reuse commands, including those from previous sessions."
  (interactive (icicle-region-or-buffer-limits))
  ;; Is there a better test we can use, to make sure the current mode inherits from `comint-mode'?
  (unless (where-is-internal 'comint-send-input (keymap-parent (current-local-map)))
    (error "Current mode must be derived from comint mode"))
  (let ((orig-search-hook icicle-search-hook)
        (icicle-transform-function 'icicle-remove-duplicates))
    (add-hook 'icicle-search-hook 'icicle-comint-send-input)
    (icicle-search beg end (concat comint-prompt-regexp "\\S-.*") nil) ; Match not required (edit).
    (remove-hook 'icicle-search-hook 'icicle-comint-send-input))
  (goto-char (point-max)))

(defun icicle-comint-send-input ()
  "Grab current completion input and use that for comint input."
  (unless (comint-check-proc (current-buffer))
    (error "No live process associated with this buffer"))
  (let ((comint-get-old-input
         (if (minibuffer-window-active-p (minibuffer-window))
             'icicle-comint-get-minibuffer-input ; Use minibuffer input (e.g. for action fn).
           'icicle-comint-get-final-choice))) ; Use final choice.
    (comint-copy-old-input))
  (comint-send-input))

(defun icicle-comint-get-minibuffer-input ()
  "Return the minibuffer input, beyond the prompt."
  (let* ((cand (icicle-minibuffer-contents))
         (input-start (and (string-match comint-prompt-regexp cand) (match-end 0))))
    (if input-start (substring cand input-start) cand)))

(defun icicle-comint-get-final-choice ()
  "Return the final choice, beyond the prompt."
  (let ((input-start (and (string-match comint-prompt-regexp icicle-search-final-choice)
                          (match-end 0))))
    (if input-start
        (substring icicle-search-final-choice input-start)
      icicle-search-final-choice)))

;;;###autoload
(icicle-define-command icicle-comint-command ; Bound to `C-c TAB' in `comint-mode'.
  "Retrieve a previously used command.
Use this in a `comint-mode' buffer such as *shell* or *inferior-lisp*.

Note, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

See also \\<comint-mode-map>\\[icicle-comint-search] for another way to reuse commands."
  insert
  "Choose a previous command: "         ; `completing-read' args
  (mapcar #'list (cddr comint-input-ring)) nil nil nil 'shell-command-history
  (aref (cddr comint-input-ring) 0) nil
  ((icicle-transform-function 'icicle-remove-duplicates))) ; Additional bindings

(add-hook 'comint-mode-hook
          (lambda ()
            (set (make-local-variable 'icicle-search-command) 'icicle-comint-search)
            (define-key comint-mode-map "\C-c\C-i" 'icicle-comint-command)
            (define-key comint-mode-map [(control ?c) tab] 'icicle-comint-command)))

;;;###autoload
(defun icicle-compilation-search (beg end) ; Bound to `C-c `' in `compilation(-minor)-mode'.
  "Like `icicle-search', but shows the matching compilation-buffer
hit.  Use this in a compilation buffer, such as `*grep*', searching
for a regexp as with `icicle-search'.  Use `C-RET' or `C-mouse-2' to
show the target-buffer hit corresponding to the current completion
candidate.  Use `C-next', `C-prior', `C-down', or `C-up' to cycle
among the target-buffer hits.

As for `icicle-search', you can further narrow the match candidates by
typing a second regexp to search for among the first matches.  See
`icicle-search' for more information.

Altogether, using this with `grep' gives you two or three levels of
regexp searching: 1) the `grep' regexp, 2) the major `icicle-search'
regexp, and optionally 3) the refining `icicle-search' regexp."
  (interactive (icicle-region-or-buffer-limits))
  (unless (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))
    (error "Current buffer must be a compilation buffer"))
  (let ((orig-search-hook icicle-search-hook)
        (icicle-transform-function (if (interactive-p) nil icicle-transform-function)))
    (add-hook 'icicle-search-hook 'compile-goto-error)
    (icicle-search beg end nil t)
    (remove-hook 'icicle-search-hook 'compile-goto-error)))

(add-hook 'compilation-minor-mode-hook
          (lambda () (set (make-local-variable 'icicle-search-command) 'icicle-compilation-search)))
(add-hook 'compilation-mode-hook
          (lambda () (set (make-local-variable 'icicle-search-command) 'icicle-compilation-search)))

;;;###autoload
(defun icicle-imenu (beg end)
  "Go to an Imenu entry using `icicle-search'.
See `icicle-search' for usage notes.
This command is intended only for use in Icicle mode."
  (interactive (icicle-region-or-buffer-limits))
  (unless imenu-generic-expression (error "No Imenu for this buffer"))
  (let ((case-fold-search (if (or (local-variable-p 'imenu-case-fold-search)
				  (not (local-variable-p 'font-lock-defaults)))
			      imenu-case-fold-search
			    (nth 2 font-lock-defaults)))
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus (icicle-delete-if-not
                          #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                          (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                    (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                  imenu-generic-expression)))
                  (submenu (let ((icicle-show-Completions-initially-flag t))
                             (completing-read "Choose: " menus nil t)))
                  (regexp (cadr (assoc submenu menus)))
                  (icicle-transform-function (if (interactive-p) nil icicle-transform-function)))
             (unless (stringp regexp) (error "No match"))
             (icicle-search beg end regexp t)))
      (set-syntax-table old-table))))

(defun icicle-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

;;;###autoload
(defun icicle-save-string-to-variable (askp)
  "Save a string (text) to a variable.
By default, the variable is user option `icicle-input-string'.
To save to a different variable, use a prefix argument; you are then
prompted for the variable to use.  You can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' to insert a string from a
variable.  Typically, you store a regexp or part of a regexp in the
variable."
  (interactive "P")
  (let* ((enable-recursive-minibuffers t)
         (var (if askp
                  (intern (completing-read
                           "Variable: " obarray 'boundp nil nil
                           'icicle-variable-history (symbol-name 'icicle-input-string)))
                'icicle-input-string))
         (text (read-string (format "Text to save in `%s': " var))))
    (set var text)))
  
;;;###autoload
(defun icicle-object-action ()
  "Apply a function to an object of a given type.
You are prompted for the object type, then the object of that type,
then the function to apply.

There are two kinds of objects that can be chosen:

1. Objects that are easily associated with names.
2. Objects that are not easily named.

Completion candidates for objects that are not easily named are
symbols whose values are the objects of the appropriate type.  The
object types used here for these candidates are really type predicate
names, which all end in `p', except for `atom'.

Be aware that the function you choose to act on the object must
accomodate that object type as its only an argument.  Also, completion
of the function itself is not strict, so you can enter a lambda form.

With a prefix argument, the result of applying the function to the
object is pretty-printed using `pp-eval-expression'.  Otherwise, the
function is called for its effect only, and its value is not
displayed.

This command is intended only for use in Icicle mode."
  (interactive)
  (let* ((type (intern
                (completing-read
                 "Object type: "
                 (mapcar #'list
                         (append
                          '(;; Types whose objects can easily be associated with names.
                            "buffer" "command" "face" "frame" "function" "option" "process"
                            "symbol" "variable" "window" 
                            ;; Type predicate names:
                            "atom" "arrayp" "bool-vector-p" "bufferp" "byte-code-function-p"
                            "case-table-p" "char-or-string-p" "char-table-p" "commandp" "consp"
                            "facep" "floatp" "frame-configuration-p" "frame-live-p"
                            "framep" "functionp" "hash-table-p" "integer-or-marker-p" "integerp"
                            "keymapp" "keywordp" "listp" "markerp" "wholenump" "nlistp" "numberp"
                            "number-or-marker-p" "overlayp" "processp" "sequencep" "stringp"
                            "subrp" "symbolp" "syntax-table-p" "user-variable-p" "vectorp"
                            "window-configuration-p" "window-live-p" "windowp")
                          ;; These are conditional because not defined for some Emacs versions.
                          (and (fboundp 'display-table-p) '("display-table-p"))
                          (and (fboundp 'string-or-null-p) '("string-or-null-p"))
                          (and (fboundp 'booleanp) '("booleanp"))))
                 nil t)))
         (icicle-saved-completion-candidate (icicle-choose-candidate-of-type type))
         (icicle-candidate-action-fn 'icicle-apply-to-saved-candidate))
    (icicle-apply-to-saved-candidate
     (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                      obarray 'functionp))))
    
(defun icicle-choose-candidate-of-type (type)
  "Read an object of type TYPE with completion, and return it."
  (case type
    ;; Types with named objects.
    (buffer
     (let ((icicle-must-match-regexp icicle-buffer-match-regexp)
           (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
           (icicle-must-pass-predicate icicle-buffer-predicate)
           (icicle-extra-candidates icicle-buffer-extras)
           (icicle-sort-function (or icicle-buffer-sort icicle-sort-function))
           (icicle-delete-candidate-object 'icicle-kill-a-buffer) ; `S-delete' kills current buffer
           (icicle-require-match-flag icicle-buffer-require-match-flag)
           (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag))
       (get-buffer (completing-read
                    "Buffer: " (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                    nil nil nil 'buffer-name-history nil nil))))
    (command (intern (completing-read "Command: " obarray 'commandp)))
    (face (intern (completing-read "Face: " (mapcar (lambda (x) (list (format "%s" x)))
                                                    (face-list)))))
    (frame (let ((frame-names-alist (make-frame-names-alist)))
             (cdr (assoc (completing-read "Frame: " frame-names-alist) frame-names-alist))))
    (function (intern (completing-read "Function: " obarray 'fboundp)))
    (option (intern (completing-read "User option: " obarray 'user-variable-p)))
    (process
     (get-process (completing-read "Process: " (mapcar (lambda (proc) (list (process-name proc)))
                                                       (process-list)))))
    (symbol (intern (completing-read "Symbol: " obarray)))
    (variable (intern (completing-read "Variable: " obarray 'boundp)))
    (window
     (let ((buffers nil))
       (walk-windows (lambda (win) (push (list (format "%s" (window-buffer win))) buffers)) nil t)
       (get-buffer-window (completing-read "Window showing buffer: " buffers) 0)))

    ;; Types chosen by predicate name.
    (otherwise (icicle-read-var-value-satisfying type))))

(defun icicle-read-var-value-satisfying (pred)
  "Reads a variable that satisfies PRED and returns its value."
  (symbol-value (intern (completing-read (format "Symbol with %s value: " pred) obarray
                                         `(lambda (symb)
                                           (and (boundp symb)
                                            (funcall #',pred (symbol-value symb))))))))

;;;###autoload
(defun icicle-customize-icicles-group ()
  "Customize Icicles options and faces.  View their documentation."
  (interactive)
  (customize-group-other-window 'Icicles))

;;;###autoload
(defun icicle-send-bug-report ()
  "Send a bug report about an Icicles problem."
  (interactive)
  (browse-url (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions.")))
 
;;(@* "Other top-level Icicles commands")
;;; Other top-level Icicles commands   .   .   .   .   .   .

;;;###autoload
(when (fboundp 'map-keymap)             ; Emacs 22.

  (defvar icicle-complete-keys-alist nil "Alist of keys and their bindings.
Each alist element is of the form (NAME KEY . BINDING), where:
 NAME is a symbol naming the key and its binding, whose name has form:
   KEYNAME  =  BINDING-NAME
 KEY is the actual key sequence
 BINDING is the actual binding of KEY.")

  (defun icicle-generic-S-tab ()
    "Treat `S-TAB' in minibuffer, *Completions*, and elsewhere.
Bound to `S-TAB' by default, but whatever it is bound to, does this:
 - Call `icicle-apropos-complete' if in minibuffer during completion.
 - Call `icicle-move-to-previous-completion' if in *Completions*.
 - Call `icicle-complete-keys', otherwise.

If you do not use the default minibuffer completion binding of
`S-TAB' for `icicle-apropos-complete', then you can simply bind
\[S-tab] to nil in `icicle-mode-map' in `icicle-mode-hook'."
    (interactive)
    (cond ((icicle-completing-p)        ; Cannot use the var here, since not sure to be in minibuf.
           (setq this-command 'icicle-apropos-complete)
           (icicle-apropos-complete))   ; Defined in `icicles-cmd.el'
          ((eq (get-buffer "*Completions*") (current-buffer))
           (setq this-command 'icicle-move-to-previous-completion)
           (icicle-move-to-previous-completion (prefix-numeric-value current-prefix-arg)))
          (t
           (setq this-command 'icicle-complete-keys)
           (icicle-complete-keys))))

  ;; This is a quick-and-dirty definition, not an efficient one.
  ;; It gathers all key bindings and then throws most of them away!  Good enough.
  (defun icicle-insert-char ()
    "Insert a character, using key completion.
Keys bound to `self-insert-command' are completion candidates."
    (interactive)
    (barf-if-buffer-read-only)
    (let ((icicle-complete-keys-self-insert-flag t)
          (icicle-must-match-regexp "^.+  =  self-insert-command"))
      (icicle-complete-keys)))

  (defun icicle-complete-keys () ; Bound to prefix keys followed by `S-TAB' (unless defined).
    "Complete a key sequence for the currently invoked prefix key.
Input-candidate completion and cycling are available.

You can navigate the key-binding hierarchy (prefix-key hierarchy),
just as would navigate a file-system hierarchy (to complete directory
and file names) or a menu hierarchy (to complete submenu and menu-item
names).

Completion candidates generally have the form `KEY  =  COMMAND'.

If COMMAND is `...', then KEY is a prefix key; choosing it updates the
completion candidates list to the keys under that prefix.  For
example, choosing `C-x = ...' changes the candidates to those with
prefix `C-x'.

The special candidate `..' means to go up one level of the key-binding
hierarchy and complete candidates there.  For example, if you are
currently completing prefix key `C-x 5', and you choose candidate
`..', then you will be completing prefix `C-x', the parent of `C-x 5'.

Except at the top level, the default value for completion is `..'.

If option `icicle-complete-keys-self-insert-flag' is non-nil, then
keys bound to `self-insert-command' are included as possible
completion candidates; otherwise (the default), they are not.  Command
`icicle-insert-char' works like `icicle-complete-keys', but in
includes only keys bound to `self-insert-command' - use it to insert a
character that is difficult or impossible to type with your keyboard.

You can use `M-C-,' at any time to switch between sorting with local
bindings first and sorting with prefix keys first.  You can use `C-,'
at any time to change the sort order among these two and sorting by
command name.

While cycling, these keys describe the command of the current
candidate:

`C-RET'   - Describe command of current completion candidate only
`C-down'  - Describe; move to next prefix-completion candidate
`C-up'    - Describe; move to previous prefix-completion candidate
`C-next'  - Describe; move to next apropos-completion candidate
`C-prior' - Describe; move to previous apropos-completion candidate
`up'      - Move to next prefix-completion candidate
`down'    - Move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Describe *all* candidates, successively - use the [back]
            button in buffer *Help* to visit the descriptions

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'."
    (interactive)
    (let* ((icicle-transform-function 'icicle-remove-duplicates)
           (icicle-show-Completions-initially-flag t)
           (icicle-candidate-action-fn 'icicle-complete-keys-action)
           (enable-recursive-minibuffers t)
           (orig-buff (current-buffer)) ; Used as free var in `icicle-complete-keys-action'.
           (orig-window (selected-window)) ; Used as free var in `icicle-complete-keys-action'.
           (icicle-completing-keys-p t) ; Provide a condition to test key completion.
           (icicle-sort-function 'icicle-special-candidates-first-p)
           (icicle-alternative-sort-function 'icicle-prefix-keys-first-p)
           (icicle-sort-functions-alist
            '(("by key name, local bindings first" . icicle-special-candidates-first-p)
              ("by key name, prefix keys first" . icicle-prefix-keys-first-p)
              ("by command name" . icicle-command-names-alphabetic-p)
              ("turned OFF"))))
      (icicle-complete-keys-1 (icicle-this-command-keys-prefix))))

  (defun icicle-this-command-keys-prefix ()
    "Return the prefix of the currently invoked key sequence."
    (let ((this-key (this-command-keys))) (substring this-key 0 (1- (length this-key)))))

  (defun icicle-complete-keys-1 (prefix) ; PREFIX is a free var in `icicle-complete-keys-action'.
    "Complete a key sequence for prefix key PREFIX (a vector)."
    (let ((orig-extra-candidates icicle-extra-candidates)) ; Free in `icicle-complete-keys-action'.
      (unwind-protect
           (progn
             (icicle-keys+cmds-w-prefix prefix)
             (unless icicle-complete-keys-alist (error "No keys for prefix `%s'" prefix))
             (let* ((this-cmd-keys ; For error report - e.g. mouse cmd.
                     (this-command-keys-vector)) ; Free var in `icicle-complete-keys-action'.
                    (prefix-description
                     (icicle-key-description prefix (not icicle-key-descriptions-use-<>-flag)))
                    (candidate (completing-read
                                (concat "Complete keys" (and (not (string= "" prefix-description))
                                                             (concat " " prefix-description))
                                        ": ")
                                icicle-complete-keys-alist nil t nil nil
                                ;;$$ (if (equal [] prefix) nil "\\.\\.")
                                )))
               (icicle-complete-keys-action candidate)))
        (mapc (lambda (cand) (put (car cand) 'icicle-special-candidate nil)) ; Reset the property.
              icicle-complete-keys-alist))))

  (defun icicle-complete-keys-action (candidate)
    "Completion action function for `icicle-complete-keys'."
    (let* ((key+binding (cdr-safe (assq (intern candidate) icicle-complete-keys-alist)))
           (key (car-safe key+binding))
           (binding (cdr-safe key+binding))
           (cmd-name nil)
           (action-window (selected-window)))
      (unwind-protect
           (progn
             ;; `orig-buff' and `orig-window' are free here.  Defined in `icicle-complete-keys'.
             (set-buffer orig-buff)
             (select-window orig-window)
             (if (string= ".." candidate)
                 (setq cmd-name "..")
               (unless (and (string-match "\\(.+\\)  =  \\(.+\\)" candidate) (match-beginning 2))
                 (error "No match"))
               (setq cmd-name (substring candidate (match-beginning 2) (match-end 2))))      
             ;; `prefix', `orig-extra-candidates', and `this-cmd-keys' are free vars here.
             ;; They are defined in `icicle-complete-keys-1'.
             (cond ((string= ".." cmd-name) ; Go back up to parent prefix.
                    (setq last-command 'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat (nbutlast (append prefix nil)))))
                   ((and key (string= "..." cmd-name)) ; Go down to prefix.
                    (setq last-command 'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat prefix key)))
                   (t
                    (setq this-command binding last-command binding)
                    (setq icicle-extra-candidates orig-extra-candidates) ; Restore it.
                    (when (eq 'self-insert-command binding)
                      (unless key (error "Cannot insert `%s'" key))
                      (setq last-command-char (aref key 0)))
                    (when (eq 'digit-argument binding)
                      (setq last-command-char (aref key 0))
                      (icicle-msg-maybe-in-minibuffer "Numeric argument"))
                    (when (eq 'negative-argument binding)
                      (icicle-msg-maybe-in-minibuffer "Negative argument"))
                    (setq last-nonmenu-event 1) ; So *Completions* mouse-click info is ignored.
                    (condition-case try-command
                        (call-interactively binding nil this-cmd-keys)
                      (error (error (error-message-string try-command)))))))
        (select-window action-window))))

  (defun icicle-keys+cmds-w-prefix (prefix)
    "Fill `icicle-complete-keys-alist' for prefix key PREFIX (a vector)."
    (let ((prefix-map nil))
      (setq icicle-complete-keys-alist nil)
      (dolist (map (current-active-maps t))
        (setq prefix-map (lookup-key map prefix))
        ;; NOTE: `icicle-add-key+cmd' Uses `prefix' and `map' as free vars.
        (when (keymapp prefix-map) (map-keymap #'icicle-add-key+cmd prefix-map)))
      (unless (equal [] prefix) (push (list '\.\.) icicle-complete-keys-alist))
      icicle-complete-keys-alist))

  (defun icicle-add-key+cmd (event binding)
    "Add completion for EVENT and BINDING to `icicle-complete-keys-alist'."
    (cond
      ;; (menu-item ITEM-STRING): non-selectable item - skip it.
      ((and (eq 'menu-item (car-safe binding))
            (null (cdr-safe (cdr-safe binding))))
       (setq binding nil))          ; So `keymapp' test, below, fails.
      
      ;; (ITEM-STRING): non-selectable item - skip it.
      ((and (stringp (car-safe binding)) (null (cdr-safe binding)))
       (setq binding nil))          ; So `keymapp' test, below, fails.
      
      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
      ((eq 'menu-item (car-safe binding))
       (let ((enable-condition (memq ':enable (cdr-safe (cdr-safe (cdr-safe binding))))))
         (if (or (not enable-condition)
                 (condition-case nil ; Don't enable if we can't check the condition.
                     (eval (cadr enable-condition))
                   (error nil)))
             (setq binding (car-safe (cdr-safe (cdr-safe binding))))
           (setq binding nil))))
      
      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe binding))
       (setq binding (cdr binding))
       ;; Skip HELP-STRING
       (when (stringp (car-safe binding)) (setq binding (cdr binding)))
       ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
       (when (and (consp binding) (consp (car binding))) (setq binding (cdr binding)))))
    
    ;; Follow indirections to ultimate symbol naming a command.
    (while (and (symbolp binding) (fboundp binding) (keymapp (symbol-function binding)))
      (setq binding (symbol-function binding)))

    ;; NOTE: `prefix' and `map' are free here.  They are bound in `icicle-keys+cmds-w-prefix'.
    (cond ((and (or (keymapp binding)
                    (and (commandp binding)
                         (equal binding (key-binding (vconcat prefix (vector event))))
                         (not (eq binding 'icicle-generic-S-tab))
                         (not (eq binding 'icicle-complete-keys))))
                (or (not (eq binding 'self-insert-command)) ; Command, keymap.
                    (and icicle-complete-keys-self-insert-flag ; Insert normal char.
                         (char-valid-p event))))
           (let ((candidate
                  (intern
                   (concat
                    (single-key-description event (not icicle-key-descriptions-use-<>-flag))
                    "  =  "
                    (if (keymapp binding) "..." (prin1-to-string binding))))))
             (push (cons candidate (cons (vector event) binding)) icicle-complete-keys-alist)
             (when (eq map (current-local-map)) (put candidate 'icicle-special-candidate t))))
          ((and (integerp event) (generic-char-p event) ; Insert generic char.
                (eq 'self-insert-command  binding))
           (ignore))))                  ; Placeholder for future use.

  ;; $$ No longer used.  Was used in `icicle-complete-keys-1'.
  (defun icicle-read-single-key-description (string need-vector &optional no-angles)
    "If STRING contains a space, then the vector containing the symbol named STRING.
Otherwise, call `icicle-read-kbd-macro'.
Other args are as for `icicle-read-kbd-macro'."
    (cond ((and no-angles (string-match " " string)) (vector (intern string)))
          ((string-match "^<\\([^>]* [^>]*\\)>" string)
           (vector (intern (substring string (match-beginning 1) (match-end 1)))))
          (t (icicle-read-kbd-macro string need-vector no-angles))))

  ;; $$ No longer used.  Was used as `icicle-candidate-action-fn' in `icicle-complete-keys'.
  (defun icicle-complete-keys-help (candidate)
    "Describe the command associated with the current completion candidate."
    (interactive)              ; Interactively, just describes itself.
    (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
    (string-match "\\(.+\\)  =  \\(.+\\)" candidate)
    (let ((frame-with-focus (selected-frame))
          (cmd (intern-soft (substring candidate (match-beginning 2) (match-end 2)))))
      (if (not (functionp cmd))
          (icicle-msg-maybe-in-minibuffer "No help")
        (describe-function cmd))
      (icicle-raise-Completions-frame)
      ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
      ;; because the *Help* frame takes the focus away from the minibuffer frame.
      ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
      (let* ((help-window (get-buffer-window "*Help*" 0))
             (help-frame (and help-window (window-frame help-window))))
        (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
    (message nil))        ; Let minibuffer contents show immmediately.

  (defun icicle-read-kbd-macro (start &optional end no-angles)
    "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always.

Optional argument NO-ANGLES non-nil means to expect key
descriptions not to use angle brackets (<...>).  For example:
 
 (icicle-read-kbd-macro \"<mode-line>\" t)   returns [mode-line]
 (icicle-read-kbd-macro  \"mode-line\"  t t) returns [mode-line]"
    (interactive "r")
    (if (stringp start)
        (icicle-edmacro-parse-keys start end no-angles)
      (setq last-kbd-macro
            (icicle-edmacro-parse-keys (buffer-substring start end) nil no-angles))))

  (defun icicle-edmacro-parse-keys (string &optional need-vector no-angles)
    "Same as `edmacro-parse-keys', but with added NO-ANGLES argument.
NO-ANGLES is the same as for `icicle-read-kbd-macro'."
    (let ((case-fold-search nil)
          (pos 0)
          (res []))
      (while (and (< pos (length string))
                  (string-match "[^ \t\n\f]+" string pos))
        (let ((word (substring string (match-beginning 0) (match-end 0)))
              (key nil)
              (times 1))
          (setq pos (match-end 0))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" string pos)))
                ((and (string-match (if no-angles
                                        "^\\(\\([ACHMsS]-\\)*\\)\\(..+\\)$"
                                      "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$")
                                    word)
                      (or (not no-angles)
                          (save-match-data (not (string-match "^\\([ACHMsS]-.\\)+$" word))))
                      (progn
                        (setq word (concat (substring word (match-beginning 1)
                                                      (match-end 1))
                                           (substring word (match-beginning 3)
                                                      (match-end 3))))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key (list (intern word))))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (incf bits (cdr (assq (aref word 0)
                                           '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                             (?H . ?\H-\^@) (?M . ?\M-\^@)
                                             (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                     (incf prefix 2)
                     (callf substring word 2))
                   (when (string-match "^\\^.$" word)
                     (incf bits ?\C-\^@)
                     (incf prefix)
                     (callf substring word 1))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (loop for ch across word
                        for n = 0 then (+ (* n 8) ch -48)
                        finally do (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\^@) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (loop for x across word collect (+ x bits))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\^@)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (loop repeat times do (callf vconcat res key)))))
      (when (and (>= (length res) 4)
                 (eq (aref res 0) ?\C-x)
                 (eq (aref res 1) ?\()
                 (eq (aref res (- (length res) 2)) ?\C-x)
                 (eq (aref res (- (length res) 1)) ?\)))
        (setq res (edmacro-subseq res 2 -2)))
      (if (and (not need-vector)
               (loop for ch across res
                  always (and (char-valid-p ch)
                              (let ((ch2 (logand ch (lognot ?\M-\^@))))
                                (and (>= ch2 0) (<= ch2 127))))))
          (concat (loop for ch across res
                     collect (if (= (logand ch ?\M-\^@) 0)
                                 ch (+ ch 128))))
        res))))

;;;###autoload
;; See also `hexrgb-read-color' in `hexrgb.el'.
(defun icicle-read-color (&optional arg)
  "Read a color name or hex RGB color value #RRRRGGGGBBBB.
A string value is returned.
Interactively, the optional ARG is the prefix arg.

With plain `C-u', use `hexrgb-read-color', which lets the user
complete a color name or input any valid RGB hex value (without
completion).  

With no prefix arg, return a string with both the color name and the
RGB value, separated by `icicle-list-nth-parts-join-string'.

With a numeric prefix arg of 0 or 1, return the color name.  With any
other numeric prefix arg, return the RGB value.

In the plain `C-u' case, user input is checked to ensure that it
represents a valid color.

In all other cases:

- A user can complete input against the color name or the RGB value,
  or both.

- If a user enters input without completing or cycling, the input is
  not checked: whatever is entered is returned as the string value.

From Emacs Lisp, you can use ARG to control the behavior, or you can
use `icicle-list-use-nth-parts' if ARG is nil.

Note: Duplicate color names are removed by downcasing and removing
whitespace.  For example, \"AliceBlue\" and \"alice blue\" are both
treated as \"aliceblue\".  Otherwise, candidates with different names
but the same RGB values are not considered duplicates, so, for
example, users can match either \"darkred\" or \"red4\", which both
have RGB #8b8b00000000.  Users can toggle duplicate removal at any
time using `C-$'.

This command is intended only for use in Icicle mode (but it can be
used with `C-u', with Icicle mode turned off)."
  (interactive "P")
  (unless (require 'hexrgb) (error "`icicle-read-color' requires library `hexrgb.el'"))
  (let (color)
    (if (consp arg)                     ; Plain `C-u': complete against color name only, 
        (let ((icicle-transform-function 'icicle-remove-color-duplicates))
          (setq color (hexrgb-read-color t))) ; and be able to input any valid RGB string.

      ;; Complete against name+RGB pairs, but user can enter invalid value without completing.
      (let ((completion-ignore-case t)
            (icicle-transform-function 'icicle-remove-color-duplicates)
            (icicle-sort-functions-alist
             '(("by color name" . icicle-part-1-lessp)
               ("by color hue" . (lambda (s1 s2) (not (icicle-color-hue-lessp s1 s2))))
               ("by color purity (saturation)" .
                (lambda (s1 s2) (not (icicle-color-saturation-lessp s1 s2))))
               ("by color brightness (value)" .
                (lambda (s1 s2) (not (icicle-color-value-lessp s1 s2))))
               ("by amount of red" . (lambda (s1 s2) (not (icicle-color-red-lessp s1 s2))))
               ("by amount of green" . (lambda (s1 s2) (not (icicle-color-green-lessp s1 s2))))
               ("by amount of blue" . (lambda (s1 s2) (not (icicle-color-blue-lessp s1 s2))))
               ("by color rgb" . (lambda (s1 s2) (not (icicle-part-2-lessp s1 s2))))
               ("turned OFF")))
            ;; Make the two `*-join-string' variables the same, so past inputs are recognized.
            ;; Do not use " " as the value, because color names such as "white smoke" would be
            ;; split, and "smoke" would not be recognized as a color name when trying to list
            ;; candidates in *Completions*.
            (icicle-list-nth-parts-join-string ": ")
            (icicle-list-join-string ": ")
            (icicle-list-end-string ""))
        (let ((icicle-list-use-nth-parts
               (or (and arg (list arg)) ; 1 or 2, by program or via `C-1' or `C-2'.
                   icicle-list-use-nth-parts ; Bound externally by program.
                   '(1 2))))            ; Both parts, by default.
          (setq color (completing-read
                       "Color: " (mapcar #'icicle-make-color-candidate (x-defined-colors)))))))
    (when (interactive-p) (message "Color: `%s'" color))
    color))

(defun icicle-make-color-candidate (color-name)
  "Return multi-completion candidate of COLOR-NAME and its hex RGB string.
The hex RGB string has the color as its background text property."
  (let ((rgb-string (hexrgb-color-name-to-hex color-name)))
    (put-text-property 0 (length rgb-string) 'face (cons 'background-color rgb-string)
                       rgb-string)
    (list (list color-name rgb-string))))

(defun icicle-remove-color-duplicates (list)
  "Copy of LIST with duplicate color candidates removed.
Candidates are considered duplicates if they have the same color name,
abstracting from whitespace and letter case."
  (let ((tail list)
        new)
    (save-match-data
      (while tail
        (let ((this (car tail)))
          (string-match ": " this)
          (setq this (icicle-delete-whitespace-from-string (downcase this) 0 (match-beginning 0)))
          (unless (member this new) (push this new)))
        (pop tail)))
    (nreverse new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd.el ends here
