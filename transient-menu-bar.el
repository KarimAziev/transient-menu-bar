;;; transient-menu-bar.el --- Transient menu for menu bar -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>
;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/transient-menu-bar
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1") (transient "0.4.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;  This package provides transient menu access to the menu bar.

;;; Code:


(require 'transient)

(defvar transient-menu-bar-default-suffixes-props
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
    hs-hide-initial-comment-block))

(defcustom transient-menu-bar-symbol-suffixes-props (append
                                                     (mapcar
                                                      (lambda (it)
                                                        `(,it :transient t))
                                                      transient-menu-bar-default-suffixes-props))
  "Alist of functions and extra props for transient."
  :group 'transient-menu-bar
  :type '(alist
          :key-type (function :tag "Symbol")
          :value-type
          (plist :options ((:transient boolean)))))

(defvar transient-menu-bar-suffix-indicators-regex nil)
(defcustom transient-menu-bar-suffix-indicators '("forward" "backward"
                                                  "backwards"
                                                  "up" "down" "undo"
                                                  "paste"
                                                  "redo"
                                                  "toggle"
                                                  "mode"
                                                  "cycle""next" "prev"
                                                  "back"
                                                  "previous" "more"
                                                  "less")
  "List of words in easy menu descriptions to stay transient after calling.
Case is ignored.
If the description of easy menu item contains such word,
call the command and don't exit transient."
  :group 'transient-menu-bar
  :type '(repeat string))

(defcustom transient-menu-bar-use-long-descriptions t
  "Whether to use long help descriptions if available."
  :group 'transient-menu-bar
  :type 'boolean)


(defcustom transient-menu-bar-top-menu-name 'transient-menu-bar-main-menu
  "The symbol to eval as a name of top level transient command."
  :group 'transient-menu-bar
  :type 'symbol)

(defcustom transient-menu-bar-make-align-to 50
  "Column to display radio and button indicators."
  :group 'transient-menu-bar
  :type 'integer)


(defun transient-menu-bar-shortcut-pred (used-keys key)
  "Return non nil if KEY is a valid and not present in USED-KEYS."
  (and
   (key-valid-p key)
   (not
    (member key used-keys))))

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


(defun transient-menu-bar--generate-key (flag &optional used-keys)
  "Generate a unique key for a menu item based on its FLAG and used keys.

Argument FLAG is a string that is used to generate a key.
It is modified by replacing all `non-alphabetical' characters with spaces.

Optional argument USED-KEYS is a list of keys that have already been used.
It is used to ensure that the generated key is unique."
  (setq flag (replace-regexp-in-string "[^a-z]" " " flag))
  (cond ((and (string-match-p (regexp-opt (list "next")) flag)
              (not (member "n" used-keys)))
         "n")
        ((and (string-match-p (regexp-opt (list "prev" "previous")) flag)
              (not (member "p" used-keys)))
         "p")
        (t
         (let ((pred (apply-partially #'transient-menu-bar-shortcut-pred
                                      used-keys))
               (parts (nconc (split-string flag nil t)
                             (split-string flag "" t)))
               (found))
           (while (and parts (not found))
             (let ((char (downcase (substring-no-properties (pop parts) 0 1))))
               (setq found
                     (if (funcall pred char)
                         char
                       (when (funcall pred (setq char (upcase char)))
                         char)))))
           (or
            found
            (seq-find pred
                      (seq-difference
                       (nconc (transient-menu-bar--get-alphabete "a")
                              (transient-menu-bar--get-alphabete "A")
                              (delete "\""
                                      (transient-menu-bar--get-alphabete "!"
                                                                         25)))
                       used-keys))
            "")))))

(defun transient-menu-bar-make-toggle-description (description value &optional
                                                               on-label
                                                               off-label
                                                               left-separator
                                                               right-separator
                                                               divider)
  "Generate a formatted string for menu bar toggle descriptions.

Argument DESCRIPTION is a string that represents the description of the toggle.

Argument VALUE is a boolean value that determines the state of the toggle.

Optional argument ON-LABEL is a string that represents the label when the toggle
is on.
If not provided, it defaults to +.

Optional argument OFF-LABEL is a string that represents the label when the
toggle is off.
If not provided, it defaults to -.

Optional argument LEFT-SEPARATOR is a string that represents the separator on
the left side of the toggle.

Optional argument RIGHT-SEPARATOR is a string that represents the separator on
the right side of the toggle.

Optional argument DIVIDER is a string or a character that represents the divider
between the DESCRIPTION and the toggle.
If not provided, it defaults to the character \".\"."
  (let* ((description (or description ""))
         (align (apply #'max (list (+ 5 (length description))
                                   transient-menu-bar-make-align-to)))
         (face (if value 'success 'transient-inactive-value)))
    (concat
     (substring (concat
                 (or description "")
                 " "
                 (make-string (1- (1- align))
                              (if (stringp divider)
                                  (string-to-char divider)
                                (or divider ?\.)))
                 " ")
                0
                align)
     (or left-separator "")
     (if value
         (propertize
          (or on-label "+")
          'face
          face)
       (propertize
        (or off-label "-")
        'face
        face))
     (or right-separator "")
     " ")))




(defun transient-menu-bar-get-button-props (plist description &optional
                                                  enabled-keyword)
  "Generate button properties for a transient menu bar in a formatted string.

Argument PLIST is a property list from which the function extracts the button
properties.

Argument DESCRIPTION is a string that represents the description of the button.

Optional argument ENABLED-KEYWORD is a keyword that, if present, is used to
determine whether the button is enabled or not."
  (let* ((btn (plist-get plist :button))
         (badges
          (pcase (car-safe btn)
            (:toggle '("+" . " "))
            (:radio '("*" . " "))))
         (separators
          (pcase (car-safe btn)
            (:toggle '("[" . "]"))
            (:radio '("(" . ")")))))
    (if enabled-keyword
        `(:description
          (lambda ()
            (let ((descr (transient-menu-bar-make-toggle-description
                          ,(or
                            description
                            (plist-get
                             plist
                             :help)
                            "")
                          ,(cdr btn)
                          ,(car badges)
                          ,(cdr badges)
                          ,(car separators)
                          ,(cdr separators))))
             (if (ignore-errors ,(plist-get plist enabled-keyword))
                 descr
               (propertize descr 'face 'transient-inapt-suffix))))
          :transient t)
      `(:description
        (lambda ()
          (transient-menu-bar-make-toggle-description
           ,(or
             description
             (plist-get
              plist
              :help)
             "")
           ,(cdr btn)
           ,(car badges)
           ,(cdr badges)
           ,(car separators)
           ,(cdr separators)))
        :transient t))))

(defun transient-menu-bar--map-plist (pl &optional description)
  "Map properties list to a new list with additional button properties.

Argument PL is a property list from which the function extracts the button
properties.

Optional argument DESCRIPTION is a string that represents the description of the
button."
  (let ((new-pl)
        (button (plist-member pl :button))
        (visible (or (plist-get pl :visible)
                     (plist-get pl :included)))
        (enabled-keyword (seq-find (apply-partially #'plist-member pl)
                                   '(:active :enable))))
    (when visible
      (setq new-pl (plist-put new-pl :if
                              `(lambda ()
                                 (ignore-errors ,visible)))))
    (if-let ((button-props
              (when button (transient-menu-bar-get-button-props
                            pl
                            description
                            enabled-keyword))))
        (append new-pl button-props)
      (or
       (when enabled-keyword
         (append new-pl
                 `(:description
                   (lambda ()
                     (if (ignore-errors ,(plist-get pl
                                                    enabled-keyword))
                         ,description
                       (propertize ,description
                                   'face
                                   'transient-inapt-suffix))))))
       new-pl))))

(defun transient-menu-bar-print-generated-transients (&rest items)
  "Prettify transient ITEMS.
If SHOULD-EVAL is non nil, also evaluate them."
  (let* ((print-length nil)
         (result (mapconcat #'pp-to-string items "\n\n")))
    (setq result (replace-regexp-in-string "(lambda[\s\t\n]+nil[\s\t\n]"
                                           "(lambda () "
                                           result))
    (setq result (replace-regexp-in-string
                  "(transient-define-prefix[\s\t\r][a-z0-9/-]+[\s\t]+\\(nil\\)"
                  "()\n\s\s"
                  result
                  nil
                  nil
                  1))
    (with-current-buffer
        (get-buffer-create "*transient-menu-bar*")
      (erase-buffer)
      (delay-mode-hooks
        (emacs-lisp-mode)
        (insert result))
      (pop-to-buffer (current-buffer)))))

(defvar transient-menu-bars-all-prefixes)
(defvar transient-menu-bar-counter 0)


(defun transient-menu-bar-get-defaults ()
  "Construct a keymap from menu-bar-keymaps that not present in temp buffers."
  (let* ((menu-main (menu-bar-keymap))
         (menu-temp (with-temp-buffer (menu-bar-keymap)))
         (filtered
          (let ((result))
            (map-keymap (lambda (key v)
                          (when (and (symbolp key)
                                     (not (assq key menu-temp)))
                            (push (cons 'keymap v) result)))
                        menu-main)
            result)))
    (make-composed-keymap filtered)))

(defun transient-menu-bar-inc-when-integer (sym)
  "Increment symbol value SYM if it is an integer."
  (when-let ((value (symbol-value sym)))
    (when (integerp value)
      (set
       sym
       (1+
        value)))))

(defun transient-menu-bar-generate-name (&rest args)
  "Generate menu bar name from ARGS.
ARGS is a list of strings, symbols or integers."
  (mapconcat (lambda (it)
               (downcase
                (replace-regexp-in-string
                 "[^a-z-]" ""
                 (if (symbolp it)
                     (symbol-name it)
                   (format "%s" it)))))
             (flatten-list (remove nil args))
             ""))


(defun transient-menu-bar-get-prefix-name (form)
  "Return name from `transient-define-prefix' FORM."
  (pcase form
    (`(transient-define-prefix ,(and sym (guard (symbolp sym))) ,(pred listp)
        .
        ,_rest)
     sym)))

(defun transient-menu-bar-map (map prefix-name counter top-menu-name)
  "Map menu bar keymap MAP to a transient menu TOP-MENU-NAME.
PREFIX-NAME, SUFFIX-NAME and COUNTER are used for generated forms."
  (let (transient-menu-bars-all-prefixes)
    (setq transient-menu-bar-counter
          counter)
    (let* ((res (transient-menu-bar-recoursive map
                                               prefix-name)))
      (append
       transient-menu-bars-all-prefixes
       (list
        `(transient-define-prefix
           ,top-menu-name
           nil
           "Command dispatcher for menu bar."
           :refresh-suffixes t
           ,(apply
             #'vector
             res)))))))

;;;###autoload
(defun transient-menu-bar-eval (menu-bar)
  "Execute a transient menu bar command sequence.

Argument MENU-BAR is a keymap representing the menu bar to be evaluated."
  (setq transient-menu-bar-counter 0)
  (let* ((eval-expression-debug-on-error nil)
         (forms (transient-menu-bar-map
                 menu-bar "trm-"
                 transient-menu-bar-counter
                 transient-menu-bar-top-menu-name)))
    (eval
     (cons 'progn
           forms)
     nil)
    forms))

;;;###autoload
(defun transient-menu-bar-eval-minors ()
  "Evaluate minor mode menu bar commands."
  (let* ((eval-expression-debug-on-error nil)
         (forms (transient-menu-bar-map-minors)))
    (eval
     (cons 'progn
           forms)
     nil)
    forms))

(defun transient-menu-bar-map-minors ()
  "Generate and eval `transient-define-prefix' forms from MENU-BAR keymap."
  (setq transient-menu-bar-counter
        0)
  (let* (transient-menu-bars-all-prefixes
         (forms (transient-menu-bar-map
                 (transient-menu-bar-filter-map (menu-bar-keymap)
                                                (seq-intersection
                                                 (transient-menu-bar-keymap-keys
                                                  (menu-bar-keymap))
                                                 (transient-menu-bar-keymap-keys
                                                  (with-temp-buffer
                                                    (menu-bar-keymap)))))
                 "trm-"
                 transient-menu-bar-counter
                 (make-symbol "trm-other"))))
    (eval
     (cons 'progn
           forms)
     nil)
    (let* ((menu-main (menu-bar-keymap))
           (menu-temp (with-temp-buffer (menu-bar-keymap)))
           (keys '("o"))
           (max-height (/ (window-height) 2))
           (should-split)
           (filtered
            (let ((result
                   (list
                    (vector
                     (list "o" "others"
                           (nth 1
                                (car
                                 (last
                                  forms)))))))
                  (idx 0))
              (map-keymap (lambda (key v)
                            (when (and (symbolp key)
                                       (not (assq key menu-temp))
                                       (listp v))
                              (when (and (> (length keys) max-height)
                                         (not should-split))
                                (setq should-split idx))
                              (when-let ((res (transient-menu-bar-recoursive
                                               (cons
                                                'keymap
                                                v)
                                               (concat "transient-menu-bar-"
                                                       (symbol-name
                                                        key))
                                               keys)))
                                (setq keys (append keys (mapcar #'car-safe res)))
                                (setq idx (1+ 0))
                                (setq result (push (apply #'vector (push
                                                                    (capitalize
                                                                     (symbol-name
                                                                      key))
                                                                    res))
                                                   result)))))
                          menu-main)
              result)))
      (setq filtered (if should-split
                         (mapcar (lambda (it)
                                   (apply #'vector it))
                                 (seq-split (seq-sort-by #'length '> filtered)
                                            (/ (length filtered) 2)))
                       filtered))
      (append
       transient-menu-bars-all-prefixes
       (list
        `(transient-define-prefix
           ,transient-menu-bar-top-menu-name
           nil
           "Command dispatcher for menu bar."
           ,@filtered))))))



(defun transient-menu-bar-get-plist-props (str)
  "Return extra props for STR."
  (when (string-match-p
         (regexp-opt transient-menu-bar-suffix-indicators 'words)
         (downcase (format
                    "%s"
                    str)))
    (list :transient t)))


(defun transient-menu-bar-shorten-key-description (binding)
  "Return short version of BINDING."
  (when-let* ((key-descr (key-description binding))
              (parts
               (unless (string-match-p "mouse" key-descr)
                 (split-string key-descr nil t))))
    (let ((str (car (reverse parts))))
      (car (reverse (split-string str "-" t))))))

(defun transient-menu-bar-merge-plist (&rest plists)
  "Merge all given property lists into a single one, overwriting duplicate keys.

Argument PLISTS is a list of property lists."
  (let ((result-plist '()))
    (dolist (plist plists)
      (while plist
        (setq result-plist (plist-put result-plist (car plist) (cadr plist)))
        (setq plist (cddr plist))))
    result-plist))

(defun transient-menu-bar-plist-omit (keys plist)
  "Remove KEYS and values from PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and (not (memq key keys))
                       (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun transient-menu-bar-plist-pick (keywords pl)
  "Pick values from a plist based on given KEYWORDS.

Argument PL is a property list that contains key-value pairs.
Argument KEYWORDS is a list of KEYWORDS used to pick specific values from the
property list."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when-let ((value (plist-get pl keyword)))
        (unless (null value)
          (setq result (append result (list keyword value))))))
    result))

(defun transient-menu-bar-make-mouse-event (&optional pos)
  "Return syntetic mouse event at POS."
  (unless pos (setq pos (window-point)))
  `(mouse-1             ;; button
    (,(selected-window) ;; window
     ,(1+
       pos)
       ;; position
     (0 . 0) ;; window-relative pixel
     0       ;; timestamp
     nil     ;; object
     ,pos
     ;; text position
     (,(current-column)
      . ;; column
      ,(line-number-at-pos
        pos)) ;; line
     nil      ;; image
     (0 . 0)
     ;; object-relative pixel
     (1 . 1))))

(defun transient-menu-bar-make-event-command (km)
  "Return lambda that call KM with fake mouse event."
  (if (symbolp km)
      `(lambda ()
         (interactive)
         (let ((inhibit-mouse-event-check t))
           (call-interactively #',km
                               (transient-menu-bar-make-mouse-event))))
    `(lambda ()
       (interactive)
       (let ((inhibit-mouse-event-check t))
         (call-interactively ,km
                             (transient-menu-bar-make-mouse-event))))))

(defun transient-menu-bar-make-binary-event-command (km)
  "Return lambda that call KM with fake mouse event and `current-prefix-arg'."
  (if (symbolp km)
      `(lambda ()
         (interactive)
         (let ((inhibit-mouse-event-check t))
           (call-interactively #',km
                               (transient-menu-bar-make-mouse-event)
                               current-prefix-arg)))
    `(lambda ()
       (interactive)
       (let ((inhibit-mouse-event-check t))
         (call-interactively ,km
                             (transient-menu-bar-make-mouse-event)
                             current-prefix-arg)))))

(defun transient-menu-bar-parse-menu-item (elt)
  "Parse a menu item for the transient menu bar.

Argument ELT is a menu item to be parsed.
It can be a function, a keymap, or a list representing a menu item."
  (let ((km)
        (str)
        (plist)
        (visible)
        (filter))
    (cond ((or (functionp elt)
               (keymapp elt))
           (setq km elt))
          ((or (keymapp (cdr-safe elt))
               (functionp (cdr-safe elt)))
           (setq km (cdr elt))
           (and (stringp (car elt))
                (setq str (car elt))))
          ((or (keymapp (cdr-safe (cdr-safe elt)))
               (functionp (cdr-safe (cdr-safe elt))))
           (setq km (cddr elt))
           (and (stringp (car elt))
                (setq str (car elt))))
          ((eq (car-safe elt) 'menu-item)
           (setq plist (cdr-safe (cdr-safe (cdr-safe elt))))
           (when (consp (car-safe plist))
             (setq plist (cdr-safe plist)))
           (setq km (nth 2 elt))
           (setq str (eval (nth 1 elt)))
           (setq filter (plist-get plist :filter))
           (if filter
               (setq km (funcall filter km)))
           (setq visible (plist-member plist :visible))
           (when visible
             (unless (eval (plist-get plist :visible) t)
               (setq km nil)))
           (setq plist (transient-menu-bar--map-plist plist
                                                      str)))
          ((or (keymapp (cdr-safe (cdr-safe (cdr-safe
                                             elt))))
               (functionp (cdr-safe (cdr-safe (cdr-safe
                                               elt)))))
           (setq km (cdr (cddr elt)))
           (and (stringp (car elt))
                (setq str (car elt)))))
    (when (and km str)
      (list km str plist))))

(defvar-local transient-menu-bar-last-prefix nil)

(defun transient-menu-bar-get-extra-props (sym description)
  "Retrieve and merge extra properties for a given symbol and DESCRIPTION.

Argument SYM is a symbol that represents the menu item.
Argument DESCRIPTION is a string that provides a description for the menu item."
  (transient-menu-bar-merge-plist
   (or (transient-menu-bar-get-plist-props description)
       (ignore-errors
         (transient-menu-bar-get-plist-props
          (symbol-name sym))))
   (alist-get sym transient-menu-bar-symbol-suffixes-props)))

(defvar-local transient-menu-bar-commands-history nil)
(defvar-local transient-menu-bar-curr-commands nil)

(defun transient-menu-bar-cleanup-command-hook ()
  "Remove specific hooks from `transient-exit-hook' and `post-command-hook'."
  (remove-hook 'transient-exit-hook 'transient-menu-bar-cleanup-command-hook)
  (remove-hook 'post-command-hook 'transient-menu-bar-record-command-stack))

(defun transient-menu-bar-record-command-stack ()
  "Record the command stack for the transient menu bar."
  (let ((cmd (and transient--prefix (oref transient--prefix command))))
    (cond ((and transient--stack cmd
                (>= (length transient--stack)
                    (length transient-menu-bar-curr-commands)))
           (setq transient-menu-bar-curr-commands (cons cmd transient--stack))))
    (unless transient--window
      (transient-menu-bar-cleanup-command-hook))))

(defun transient-menu-bar-resume ()
  "Resume the last command from the transient menu bar stack."
  (interactive)
  (message "resuming")
  (let* ((cell transient-menu-bar-curr-commands)
         (cmd (car cell))
         (stack (cdr cell)))
    (when stack
      (call-interactively cmd)
      (setq transient--stack stack)
      (setq transient-menu-bar-curr-commands nil))))

(defun transient-menu-bar-mapper (key elt &optional keys prefix-name path)
  "Generate transient prefixes for Emacs menu bar items.

Argument KEY is a symbol representing the key for the menu item.

Argument ELT is the menu item to be mapped.

Optional argument KEYS is a list of keys that have already been used.

Optional argument PREFIX-NAME is a string to prepend to the name of the
corresponding menu item.

Optional argument PATH is used for recursive purposes."
  (if (and key (symbolp key)
           (string-match-p "^sep" (symbol-name key)))
      ""
    (let ((res (transient-menu-bar-parse-menu-item elt)))
      (let ((km (nth 0 res))
            (str (nth 1 res))
            (plist (nth 2 res))
            (binding)
            (genkey)
            (key-descr)
            (sep "")
            (is-command))
        (if (not (and km str))
            sep
          (setq binding (where-is-internal km nil t))
          (setq key-descr
                (transient-menu-bar-shorten-key-description binding))
          (setq genkey (if (and key-descr
                                (<= (length key-descr) 3)
                                (not (member key-descr keys)))
                           key-descr
                         (transient-menu-bar--generate-key
                          str
                          keys)))
          (push genkey keys)
          (setq is-command (commandp km))
          (cond ((keymapp km)
                 (when (symbolp km)
                   (setq km (indirect-function km)))
                 (let* ((next-path (if path
                                       (append
                                        path
                                        (list
                                         (symbol-name
                                          key)))
                                     (list (symbol-name
                                            key))))
                        (rec (transient-menu-bar-recoursive
                              km
                              prefix-name
                              nil
                              next-path))
                        (name (make-symbol
                               (transient-menu-bar-generate-name
                                prefix-name
                                (or (and key
                                         (symbolp key) key)
                                    str)
                                (transient-menu-bar-inc-when-integer
                                 'transient-menu-bar-counter))))
                        (tran
                         (when (and (listp rec)
                                    (stringp str))
                           `(transient-define-prefix ,name
                              nil
                              ,(format
                                "Transient menu for %s commands."
                                (substring-no-properties (replace-regexp-in-string
                                                          "[\\.]+$"
                                                          ""
                                                          str)))
                              :refresh-suffixes t
                              [,(string-join next-path
                                 " -> ")
                               ,(apply
                                 #'vector rec)]))))
                   (push tran transient-menu-bars-all-prefixes)
                   (append (list
                            genkey
                            str
                            name)
                           plist)))
                ((not km)
                 sep)
                ((member str menu-bar-separator)
                 str)
                ((and
                  (setq is-command (functionp km))
                  (plist-get plist :description))
                 (let ((merged
                        (append
                         (list genkey
                               km)
                         (transient-menu-bar-plist-pick
                          '(:description) plist)
                         (let ((pl (transient-menu-bar-plist-omit
                                    '(:description)
                                    plist)))
                           (transient-menu-bar-merge-plist
                            pl
                            (transient-menu-bar-get-extra-props
                             km str))))))
                   merged))
                ((ignore-errors
                   (and is-command
                        (eq 2 (car-safe (func-arity km)))
                        (when-let ((intype (car-safe
                                            (cdr-safe (interactive-form
                                                       km)))))
                          (and (stringp intype)
                               (string-match-p "^e" intype)))))
                 (append
                  (list genkey
                        (format "%s" str)
                        (transient-menu-bar-make-binary-event-command km))
                  (transient-menu-bar-merge-plist
                   (list :transient nil) plist
                   (transient-menu-bar-get-extra-props km str))))
                ((ignore-errors
                   (and is-command
                        (when-let ((intype (car-safe
                                            (cdr-safe (interactive-form
                                                       km)))))
                          (or (equal intype "e")
                              (and (stringp intype)
                                   (string-match-p "^e"
                                                   intype))))))
                 (append
                  (list genkey
                        (format "%s" str)
                        (transient-menu-bar-make-event-command km))
                  (transient-menu-bar-merge-plist
                   plist
                   (transient-menu-bar-get-extra-props
                    km str)
                   (list :transient nil))))
                (is-command
                 (append
                  (list genkey (format "%s" str) km)
                  (transient-menu-bar-merge-plist
                   plist
                   (transient-menu-bar-get-extra-props km str))))))))))




(defun transient-menu-bar-recoursive (map &optional prefix-name used-keys path)
  "Map menu bar MAP to transient prefixes.

Prepend PREFIX-NAME to the name of corresponding menu item.


<transient-menu-bar-prefix>-<menu-item-name>-<transient-menu-bar-suffix>.

Bindings are uniq to USED-KEYS.

Stores a list of all the generated commands in the free variable:
`transient-menu-bars-all-prefixes'.

PATH is used for recoursive purposes."
  (let ((result)
        (keys used-keys))
    (cond ((keymapp map)
           (map-keymap
            (lambda (item-key elt)
              (let ((item (transient-menu-bar-mapper item-key elt keys
                                                     prefix-name
                                                     path)))
                (cond ((and item (listp item))
                       (push (car item) keys)
                       (push item result))
                      ((and path
                            (or (not item)
                                (stringp item))
                            (not (stringp (car result))))
                       (push item result)))))
            map)))
    (reverse result)))


(defun transient-menu-bar-filter-map (map &rest syms)
  "Filter keymap entries by symbols.

Argument MAP is a keymap to filter.

Remaining arguments SYMS are symbols representing keys to include in the
filtered keymap."
  (let ((result))
    (setq syms (flatten-list syms))
    (map-keymap (lambda (key v)
                  (when (memq key syms)
                    (push (cons key v) result)))
                map)
    (cons 'keymap result)))

(defun transient-menu-bar-keymap-keys (map)
  "List non-separator keys from a keymap.

Argument MAP is a keymap from which to extract menu bar keys."
  (let ((result))
    (map-keymap (lambda (key _v)
                  (when (and (symbolp key)
                             (not (string-match-p "^separator-[0-9]+$"
                                                  (symbol-name key))))
                    (push key result)))
                map)
    (reverse result)))


;;;###autoload
(defun transient-menu-bar-show-one ()
  "Generate and print one transient from `menu-bar-keymap'."
  (interactive)
  (let (transient-menu-bars-all-prefixes)
    (setq transient-menu-bar-counter
          (when (yes-or-no-p
                 "Add counter to names?")
            (read-number "Start from: "
                         transient-menu-bar-counter)))
    (run-hooks 'menu-bar-update-hook)
    (let* ((keymap-fn (intern (completing-read "Menu: "
                                               '(menu-bar-keymap
                                                 context-menu-map)
                                               nil t)))
           (keymap
            (pcase keymap-fn
              ('context-menu-map
               (context-menu-map
                (transient-menu-bar-make-mouse-event)))
              (_
               (funcall keymap-fn))))
           (name
            (completing-read "Symbol: "
                             (transient-menu-bar-keymap-keys keymap)))
           (sym (intern name))
           (map
            (transient-menu-bar-filter-map keymap
                                           sym)))
      (transient-menu-bar-recoursive map
                                     (read-string "transient-define-prefix "
                                                  (concat name "-")))
      (let ((eval-expression-debug-on-error nil))
        (apply #'transient-menu-bar-print-generated-transients
               transient-menu-bars-all-prefixes)))))

(defun transient-menu-bar-call-one (sym)
  "Generate and call transient menu form menu-bar item with name SYM."
  (setq transient-menu-bar-counter 0)
  (let (transient-menu-bars-all-prefixes)
    (let ((map (transient-menu-bar-filter-map (menu-bar-keymap)
                                              (list sym))))
      (transient-menu-bar-recoursive map (gensym "transient-menu-bar-"))
      (let* ((eval-expression-debug-on-error nil)
             (prefix (nth 1 (car transient-menu-bars-all-prefixes))))
        (list 'quote (eval (cons 'progn
                                 (append
                                  transient-menu-bars-all-prefixes
                                  `((,prefix))))
                           nil))))))


;;;###autoload
(defun transient-menu-bar-show-all ()
  "Generate and print transients from `menu-bar-keymap'."
  (interactive)
  (setq transient-menu-bar-counter
        (when (yes-or-no-p
               "Add counter to names?")
          (read-number "Start from: "
                       transient-menu-bar-counter)))
  (let* ((name (read-string "transient-define-prefix "))
         (eval-expression-debug-on-error nil)
         (forms (transient-menu-bar-map
                 (menu-bar-keymap)
                 name
                 transient-menu-bar-counter
                 (intern name))))
    (apply #'transient-menu-bar-print-generated-transients
           forms)))


;;;###autoload
(defun transient-menu-bar-context-menu-dispatch ()
  "Create a transient menu from `context-menu-map'."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (when (and (boundp 'yank-menu)
             (listp yank-menu))
    (let* ((forms (transient-menu-bar-eval
                   (context-menu-map (transient-menu-bar-make-mouse-event))))
           (sym (nth 1 (car (last forms)))))
      (funcall-interactively sym))))

;;;###autoload
(defun transient-menu-bar-dwim-minor ()
  "Create a menu with `menu-bar-keymap' commands that not present in temp buffer."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (when (and (boundp 'yank-menu)
             (listp yank-menu))
    (let* ((forms (transient-menu-bar-eval-minors))
           (sym (nth 1 (car (last forms)))))
      (if (minibufferp)
          (with-minibuffer-selected-window (funcall-interactively sym))
        (funcall-interactively sym)))))

;;;###autoload
(defun transient-menu-bar-dispatch ()
  "Execute or resume last commands from menu-bar for current buffer."
  (interactive)
  (cond (transient-menu-bar-curr-commands
         (call-interactively 'transient-menu-bar-resume))
        (t (run-hooks 'menu-bar-update-hook)
           (run-hooks 'activate-menubar-hook)
           (when (and (boundp 'yank-menu)
                      (listp yank-menu))
             (let* ((forms (transient-menu-bar-eval (menu-bar-keymap)))
                    (sym (nth 1 (car (last forms)))))
               (add-hook 'post-command-hook
                         'transient-menu-bar-record-command-stack)
               (if (minibufferp)
                   (with-minibuffer-selected-window (funcall-interactively sym))
                 (funcall-interactively sym)))))))


(provide 'transient-menu-bar)
;;; transient-menu-bar.el ends here