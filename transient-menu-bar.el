;;; transient-menu-bar.el --- Transient menu for menu bar -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>
;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/transient-menu-bar
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient menu for menu bar

;;; Code:

(eval-when-compile (require 'subr-x))

(defvar transient-menu-bar-default-transient-commands
  '(flymake-goto-prev-error
    flymake-goto-next-error
    next-buffer
    undo-redo
    undo-tree-redo
    undo-tree-undo
    winner-undo
    winner-redo
    undo-tree-visualizer-select-next
    previous-buffer
    org-toggle-link-display
    org-previous-link
    org-next-link
    outline-backward-same-level
    outline-forward-same-level
    outline-previous-visible-heading
    outline-up-heading
    outline-next-visible-heading
    hs-hide-all hs-show-all
    hs-hide-block
    hs-hide-level
    hs-show-block
    hs-toggle-hiding
    hs-hide-initial-comment-block
    flycheck-next-error
    flycheck-previous-error
    next-file
    next-line
    next-error
    next-match
    next-buffer
    next-completion
    next-logical-line
    next-error-no-select
    next-history-element
    next-window-any-frame
    next-multiframe-window
    next-error-select-buffer
    next-error-follow-minor-mode
    next-line-or-history-element
    next-complete-history-element
    next-matching-history-element
    comint-next-input
    avy-next
    Info-next
    ffap-next
    hl-todo-next
    ffap-next-url
    org-next-item
    org-next-link
    shortdoc-next
    shr-next-link
    tar-next-line
    array-next-row
    diff-file-next
    diff-hunk-next
    ffap-gnus-next
    ido-next-match
    tags-next-file
    xref-next-line
    yas-next-field
    goto-next-locus
    image-next-file
    image-next-line
    ivy-next-action
    magit-blob-next
    magit-next-line
    popup-page-next
    xref-next-group
    Info-search-next
    append-next-kill
    edebug-next-mode
    idate-next-field
    image-next-frame
    occur-next-error
    senator-next-tag
    archive-next-line
    bibtex-next-entry
    bibtex-next-field
    company-next-page
    dired-next-subdir
    eww-next-bookmark
    comint-next-prompt
    dired-next-dirline
    doc-view-next-page
    mc/mark-next-lines
    org-table-next-row
    rmail-next-message
    Info-next-menu-item
    Info-next-reference
    company-select-next
    fido-fd-next-switch
    flycheck-next-error
    helm-goto-next-file
    help-goto-next-page
    rectangle-next-line
    dired-vc-next-action
    eww-buffer-show-next
    ivy-occur-next-error
    org-agenda-next-item
    org-agenda-next-line
    org-table-next-field
    outline-next-heading
    tide-find-next-error
    vc-edit-next-command
    change-log-next-error
    compilation-next-file
    elisp-refs-next-match
    ert-results-next-test
    flyspell-correct-next
    gnus-button-next-page
    gnus-group-next-group
    iedit-next-occurrence
    shortdoc-next-section
    switch-to-next-buffer
    compilation-next-error
    diff-next-complex-hunk
    dired-next-marked-file
    edebug-next-breakpoint
    gnus-article-next-page
    gnus-summary-next-page
    kmacro-cycle-ring-next
    magit-blame-next-chunk
    mc/mark-next-like-this
    tab-switcher-next-line
    transient-history-next
    xref-next-line-no-show
    next-buffer
    flymake-goto-next-error
    git-commit-next-message
    org-agenda-todo-nextset
    rmail-next-same-subject
    smerge-vc-next-conflict
    expand-jump-to-next-slot
    flyspell-goto-next-error
    gnus-summary-next-thread
    ivy-next-line-or-history
    long-line-next-long-line
    mc/unmark-next-like-this
    multi-source-select-next
    org-babel-next-src-block
    org-next-visible-heading
    smerge-combine-with-next
    smie--next-indent-change
    speedbar-restricted-next
    tide-find-next-reference
    tide-next-error-function
    add-log-edit-next-comment
    calculator-displayer-next
    gdb-memory-show-next-page
    gnus-summary-next-article
    gnus-summary-next-subject
    mc/skip-to-next-like-this
    org-agenda-next-date-line
    tide-cycle-next-reference
    vterm-next-error-function
    comint-next-matching-input
    dired-subtree-next-sibling
    doc-view-search-next-match
    minibuffer-next-completion
    restclient-test-next-error
    rmail-next-labeled-message
    tab-bar-switch-to-next-tab
    tabulated-list-next-column
    gnus-article-goto-next-page
    mc/mark-next-like-this-word
    mc/mark-next-word-like-this
    comint-get-next-from-history
    company-select-next-or-abort
    js-imports-select-next-alias
    org-jira-progress-issue-next
    outline-next-visible-heading
    rmail-next-undeleted-message
    tide-next-reference-function
    webkit-eval-show-next-result
    calendar-mayan-next-haab-date
    code-review-comment-jump-next
    company--select-next-and-warn
    gnus-article-button-next-page
    highlight-changes-next-change
    mc/mark-next-like-this-symbol
    mc/mark-next-symbol-like-this
    xmltok-next-prolog-token-test
    calendar-mayan-next-round-date
    flycheck-error-list-next-error
    gnus-summary-next-same-subject
    which-key-show-next-page-cycle
    compilation-next-error-function
    doc-view-next-line-or-next-page
    doc-view-scroll-up-or-next-page
    org-property-next-allowed-value
    previous-line
    previous-error
    previous-buffer
    previous-completion
    tab-previous
    comint-previous-matching-input-from-input
    org-previous-item
    org-previous-link
    dired-previous-line
    rectangle-previous-line
    magit-blame-next-chunk-same-commit
    comint-next-matching-input-from-input))

(defcustom transient-menu-bar-symbol-suffixes-props (mapcar
                                                     (lambda (it)
                                                       `(,it :transient t))
                                                     transient-menu-bar-default-transient-commands)
  "Alist of functions and extra props for transient."
  :group 'transient-menu-bar
  :type '(alist
          :key-type (function :tag "Symbol")
          :value-type
          (plist :options ((:transient boolean)))))

(defcustom transient-menu-bar-generated-prefix-name-suffix
  "-dispatch-menu-transient"
  "Suffix to append to generated transient prefixes."
  :type 'string
  :group 'transient-menu-bar)

(defun transient-menu-bar--get-alphabete (&optional start-char n)
  "Return N letters from alphabete starting at START-CHAR.
Default value for START-CHAR is \"a\" and for N - 26."
  (unless n (setq n 26))
  (let ((start-char (if (stringp start-char)
                        (string-to-char start-char)
                      (or start-char
                          (string-to-char "a"))))
        (letters))
    (dotimes (i n)
      (let ((str (char-to-string (+ i start-char))))
        (push str letters)))
    (reverse letters)))

(defun transient-menu-bar-plist-pick (keywords pl)
  "Return non-nullable plist from PL with KEYWORDS."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when-let ((value (plist-get pl keyword)))
        (unless (null value)
          (setq result (append result (list keyword value))))))
    result))

(defun transient-menu-bar--map-keywords (item &optional description)
  "Map props of menu ITEM to transient slots.
DESCRIPTION should be a string."
  (when-let* ((keys (if (proper-list-p item)
                        (seq-filter 'keywordp item)
                      nil))
              (pl (transient-menu-bar-plist-pick  keys item)))
    (let ((new-pl))
      (dolist (plkey keys)
        (let ((res (pcase plkey
                     ((or :active :enable)
                      `(:inapt-if-not
                        (lambda ()
                          ,(plist-get pl plkey))))
                     ((or :visible :included)
                      (setq new-pl
                            `(:if
                              (lambda ()
                                ,(plist-get pl plkey)))))
                     (:label `(:description
                               (lambda ()
                                 ,(plist-get pl plkey))))
                     (:suffix
                      `(:description
                        (lambda ()
                          (concat ,description
                                  ,(plist-get pl plkey)))))
                     (:selected `(:description
                                  (lambda ()
                                    (if ,(plist-get pl plkey)
                                        (concat ,description
                                                " (Selected)")
                                      ,description)))))))
          (setq new-pl (append new-pl res))))
      new-pl)))

(defun transient-menu-bar-shortcut-pred (used-keys key)
  "Return non nil if KEY is a valid and not present in USED-KEYS."
  (and
   (key-valid-p key)
   (not (member key used-keys))))

(defun transient-menu-bar--generate-key (flag &optional used-keys)
  "Generate key for option FLAG that not present in USED-KEYS."
  (let ((pred (apply-partially #'transient-menu-bar-shortcut-pred used-keys))
        (parts (split-string flag "" t)))
    (or (seq-find
         pred
         (remove parts "-"))
        (seq-find pred parts)
        (seq-find pred
                  (seq-difference
                   (nconc (transient-menu-bar--get-alphabete "a")
                          (transient-menu-bar--get-alphabete "A")
                          (delete "\""
                                  (transient-menu-bar--get-alphabete "!"
                                                                     25)))
                   used-keys))
        "")))

(defun transient-menu-bar-compose-while-not-nil (&rest fns)
  "Return right-to-left function composition from FNS until first nil result."
  (setq fns (reverse fns))
  (lambda (&rest args)
    (when-let ((init-value (apply (pop fns) args)))
      (if (not fns)
          init-value
        (seq-reduce (lambda (acc curr)
                      (when-let ((result (funcall curr acc)))
                        result))
                    fns init-value)))))

(defun transient-menu-bar--when (pred fn &rest args)
  "Apply FN with ARGS if result of applying PRED to args is non-nil."
  (when (apply pred args)
    (apply fn args)))

(defun transient-menu-bar-find-keymap (binding)
  "Find keymap in menu item BINDING."
  (when (and (listp binding)
             (listp (cdr binding)))
    (or (cdr-safe (memq 'keymap binding))
        (cdr-safe (seq-find #'listp binding))
        (when (or (keymapp (cdr-safe (cdr-safe (cdr-safe binding)))))
          binding)
        (when (symbolp (cdr-safe (cdr-safe (cdr-safe binding))))
          (when
              (keymapp (symbol-value (cdr-safe (cdr-safe (cdr-safe binding)))))
            (symbol-value (cdr-safe (cdr-safe (cdr-safe binding))))))
        (funcall
         (transient-menu-bar-compose-while-not-nil
          (apply-partially
           #'transient-menu-bar--when
           (apply-partially
            #'transient-menu-bar-compose-while-not-nil
            (apply-partially
             #'eq 'keymap)
            'car-safe)
           #'cdr-safe)
          (apply-partially
           #'transient-menu-bar--when
           #'symbolp
           #'symbol-value)
          'car-safe
          (apply-partially
           #'transient-menu-bar--when
           #'listp
           #'last))
         binding))))




(defun transient-menu-bar--menu-bar-keymap-bindings (binding &optional
                                                             used-keys)
  "Return list of transient prefixes for menu bar BINDING.
USED-KEYS is a list of not allowed characters formatted to strings."
  (when (and (listp binding))
    (let ((transients))
      (when-let* ((title
                   (when (proper-list-p binding)
                     (or (seq-find 'stringp binding)
                         (seq-find 'symbolp binding))))
                  (vect
                   (when-let ((sublist
                               (transient-menu-bar-find-keymap
                                binding)))
                     (let ((result))
                       (dolist (item sublist)
                         (if (proper-list-p item)
                             (let ((subgenerated
                                    (ignore-errors
                                      (transient-menu-bar--menu-bar-keymap-bindings
                                       item))))
                               (when subgenerated
                                 (setq transients (append subgenerated
                                                          transients))
                                 (dolist (subresult subgenerated)
                                   (when subresult
                                     (let ((key (transient-menu-bar--generate-key
                                                 (when (proper-list-p
                                                        subresult)
                                                   (seq-find
                                                    'stringp
                                                    subresult))
                                                 used-keys)))
                                       (when key (push key used-keys))
                                       (setq result
                                             (append result
                                                     (list (list
                                                            key
                                                            (seq-find
                                                             'stringp
                                                             subresult)
                                                            (nth 1 subresult)))))))))
                               (when-let ((description
                                           (if
                                               (and
                                                (functionp
                                                 (cddr
                                                  item))
                                                (stringp
                                                 (caar item)))
                                               (caar item)
                                             (when (and (proper-list-p
                                                         item)
                                                        (stringp
                                                         (nth
                                                          2
                                                          item)))
                                               (nth 2 item))))
                                          (cmd (if
                                                   (functionp
                                                    (cddr
                                                     item))
                                                   (cddr item)
                                                 (when (proper-list-p item)
                                                   (seq-find
                                                    'functionp
                                                    item)))))
                                 (cond ((stringp cmd)
                                        (push cmd result))
                                       ((and cmd
                                             (not (byte-code-function-p cmd))
                                             (not
                                              (string-match-p
                                               "^menu-function\\([-]\\|$\\)"
                                               (format
                                                "%s"
                                                cmd)))
                                             (commandp cmd))
                                        (let ((pl (transient-menu-bar--map-keywords
                                                   item
                                                   description))
                                              (key
                                               (when description
                                                 (transient-menu-bar--generate-key
                                                  (downcase description)
                                                  used-keys))))
                                          (push key used-keys)
                                          (push (append
                                                 (list
                                                  key
                                                  (or
                                                   (transient-menu-bar-plist-pick
                                                    '(:description)
                                                    pl)
                                                   description)
                                                  cmd)
                                                 (append
                                                  (alist-get
                                                   cmd
                                                   transient-menu-bar-symbol-suffixes-props)
                                                  pl))
                                                result))))))
                           (when (consp item)
                             (when-let ((description
                                         (seq-find 'stringp
                                                   (list (car-safe item)
                                                         (car-safe (car-safe
                                                                    item)))))
                                        (cmd (if
                                                 (functionp
                                                  (cddr
                                                   item))
                                                 (cddr item)
                                               (when (proper-list-p item)
                                                 (seq-find
                                                  'functionp
                                                  item)))))
                               (cond ((stringp cmd)
                                      (push cmd result))
                                     ((and cmd
                                           (not (byte-code-function-p cmd))
                                           (not
                                            (string-match-p
                                             "^menu-function\\([-]\\|$\\)"
                                             (format
                                              "%s"
                                              cmd)))
                                           (commandp cmd))
                                      (let ((pl (transient-menu-bar--map-keywords
                                                 item
                                                 description))
                                            (key
                                             (when description
                                               (transient-menu-bar--generate-key
                                                (downcase description)
                                                used-keys))))
                                        (push key used-keys)
                                        (push (append
                                               (list
                                                key
                                                (or
                                                 (transient-menu-bar-plist-pick
                                                  '(:description)
                                                  pl)
                                                 description)
                                                cmd)
                                               (append
                                                (alist-get
                                                 cmd
                                                 transient-menu-bar-symbol-suffixes-props)
                                                pl))
                                              result))))))))
                       (when result
                         (apply 'vector (reverse result)))))))
        (append (list `(transient-define-prefix
                         ,(intern
                           (concat (string-join
                                    (split-string
                                     (downcase
                                      (if
                                          (symbolp
                                           title)
                                          (symbol-name
                                           title)
                                        title))
                                     nil
                                     t)
                                    "-")
                                   (or
                                    transient-menu-bar-generated-prefix-name-suffix
                                    "-dispatch-menu-transient")))
                         () ,title ,(or vect)))
                transients)))))

(defun transient-menu-bar-print-generated-transients (items &optional
                                                            should-eval)
  "Prettify transient ITEMS.
If SHOULD-EVAL is non nil, also evaluate them."
  (let* ((print-length nil)
         (result (pp-to-string items)))
    (setq result (replace-regexp-in-string "(lambda[\s\t\n]+nil[\s\t\n]"
                                           "(lambda () "
                                           result))
    (setq result (replace-regexp-in-string
                  "(transient-define-prefix[\s\t\r][a-zz-a-/]+[\s\t]+\\(nil\\)"
                  "()\n\s\s"
                  result
                  nil
                  nil
                  1))
    (with-current-buffer
        (get-buffer-create "*transient-menu-bar*")
      (erase-buffer)
      (insert
       ";;; transient-menu-bar.el --- Generated transients -*- lexical-binding: t -*-\n\n;;;Code:\n\n"
       result)
      (delay-mode-hooks
        (emacs-lisp-mode)
        (when should-eval
          (eval-buffer)))
      (pop-to-buffer (current-buffer)))))


(defun transient-menu-bar--map-menu-map (map)
  "Generate transients from menu bar MAP."
  (let ((result))
    (map-keymap (lambda (_it binding)
                  (unless (symbolp binding)
                    (setq
                     result
                     (append (transient-menu-bar--menu-bar-keymap-bindings
                              binding)
                             result))))
                map)
    (reverse result)))


(defun transient-menu-bar-generate (menu-bar-map)
  "Generate transient commands from MENU-BAR-MAP."
  (let ((transients (seq-filter
                     (lambda (it)
                       (eq (car it) 'transient-define-prefix))
                     (transient-menu-bar--map-menu-map menu-bar-map)))
        (used-keys)
        (final-transient)
        (filtered (seq-uniq
                   (delq nil (mapcar (lambda (it)
                                       (ignore-errors (seq-find #'stringp it)))
                                     (cdr menu-bar-map))))))
    (dolist (it transients)
      (when-let* ((description (replace-regexp-in-string
                                " transient menu[.]$"
                                ""
                                (seq-find #'stringp it)))
                  (key (when (member description filtered)
                         (transient-menu-bar--generate-key
                          (downcase description)
                          used-keys))))
        (push key used-keys)
        (push (list key
                    description
                    (nth 1 it))
              final-transient)))
    (setq final-transient `(transient-define-prefix menu-bar-transient ()
                             "Command dispatcher for menu bar."
                             ,(apply #'vector (reverse final-transient))))
    (append
     (list 'eval-and-compile)
     (append transients (list
                         final-transient)
             (list '(menu-bar-transient))))))

;;;###autoload
(defun transient-menu-bar-pp-menu ()
  "Generate and print transient prefixes from `menu-bar-keymap'."
  (interactive)
  (transient-menu-bar-print-generated-transients
   (transient-menu-bar-generate
    (menu-bar-keymap))))

;;;###autoload
(defun transient-menu-bar ()
  "Generate, evaluate and call transient prefixes from `menu-bar-keymap'."
  (interactive)
  (eval (transient-menu-bar-generate (menu-bar-keymap))
        t))

(provide 'transient-menu-bar)
;;; transient-menu-bar.el ends here