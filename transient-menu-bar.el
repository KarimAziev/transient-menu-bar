;;; transient-menu-bar.el --- Transient menu for menu bar -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>
;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/transient-menu-bar
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1") (transient "0.5.3"))
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

;; transient-menu-bar.el provides a handy way to integrate Emacs menu bar
;; features into the Transient commands.

;; The main features include:
;; - A transient interface for the Emacs menu bar, preserving familiar menu
;;   item names and hierarchies.
;; - Support for generating transient commands dynamically based on the
;;   current state of the menu bar, ensuring context-awareness for available
;;   commands.
;; - Customizable indicator list and indicators that allow users to define
;;   which actions should be transient, promoting efficiency in command
;;   execution.
;; - Smooth transitions between transient menus corresponding to submenus in
;;   the menu bar, maintaining the nested structure of menus.

;; As an Emacs Lisp package, transient-menu-bar.el requires at least Emacs
;; 29.1 and the Transient 0.5.3 library to work correctly. It is designed to be
;; free software under the GPL-3.0-or-later license and respects the free
;; software philosophy.

;;; Code:

(require 'transient)

(defcustom transient-menu-bar-suffix-props (append
                                            (mapcar
                                             (lambda (it)
                                               `(,it :transient t))
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
                                               hs-hide-initial-comment-block)))
  "Properties for transient menu bar items.

A list of additional properties to apply to specific menu bar items when using
the transient menu bar feature. Each element in the list is a cons cell where
the car is a symbol representing a menu bar item, and the cdr is a property list
of additional properties to apply to that item.

The properties in the property list can be used to modify the behavior of the
menu bar items when they are displayed in a transient menu bar. For example,
setting the `:transient' property to t for a menu bar item will make that item
transient, meaning it will disappear after being used once.

To add or modify the properties for a menu bar item, use `add-to-list' or `setf'
with the appropriate symbol and property list. The property list should contain
keyword-value pairs, where the keywords correspond to the properties you want to
set, and the values are the desired settings for those properties."
  :group 'transient-menu-bar
  :type '(alist
          :key-type (function :tag "Symbol")
          :value-type
          (plist :options ((:transient boolean)))))

(defvar transient-menu-bar-indicator-regex nil)

(defcustom transient-menu-bar-indicators '("forward" "backward"
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
  "List of strings for transient menu bar indicators.

A list of strings that represent indicators for transient menu bar items. These
indicators are used to identify menu items commands that shouldn't exit
transient after calling.

Each element in the list is a string that matches a part of the menu item's
name, indicating that the item is transient. The default list includes common
navigation and editing actions such as \"forward\", \"backward\", and \"undo\".

When the value is updated, a regular expression is generated to match these
indicators within menu item names. This regular expression is used to determine
which menu items should have transient behavior.

To modify this list, add or remove strings as needed, ensuring that each string
accurately represents an action or state that should trigger transient display
in the menu bar."
  :group 'transient-menu-bar
  :set (lambda (sym value)
         (set-default sym value)
         (setq transient-menu-bar-indicator-regex
               (regexp-opt value 'words)))
  :type '(repeat string))

(defcustom transient-menu-bar-top-name 'transient-menu-bar-main-menu
  "The symbol to eval as a name of top level transient command."
  :group 'transient-menu-bar
  :type 'symbol)

(defcustom transient-menu-bar--align-column 50
  "Column to display radio and button indicators."
  :group 'transient-menu-bar
  :type 'integer)

(defun transient-menu-bar--valid-shortcut-p (used-keys key)
  "Return non nil if KEY is a valid and not present in USED-KEYS."
  (and
   (key-valid-p key)
   (not
    (member key used-keys))))

(defun transient-menu-bar--get-alphabet (&optional start-char n)
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
         (let ((pred (apply-partially #'transient-menu-bar--valid-shortcut-p
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
                       (nconc (transient-menu-bar--get-alphabet "a")
                              (transient-menu-bar--get-alphabet "A")
                              (delete "\""
                                      (transient-menu-bar--get-alphabet "!"
                                                                        25)))
                       used-keys))
            "")))))

(defun transient-menu-bar--format-toggle (description value &optional on-label
                                                      off-label left-separator
                                                      right-separator divider)
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
                                   transient-menu-bar--align-column)))
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

(defun transient-menu-bar--button-props (plist description &optional
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
            (let ((descr (transient-menu-bar--format-toggle
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
          (transient-menu-bar--format-toggle
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
              (when button (transient-menu-bar--button-props
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

(defvar transient-menu-bar--name-counter 0)

(defun transient-menu-bar--increment-counter (sym)
  "Increment symbol value SYM if it is an integer."
  (when-let ((value (symbol-value sym)))
    (when (integerp value)
      (set
       sym
       (1+
        value)))))

(defun transient-menu-bar--make-name (&rest args)
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

(defun transient-menu-bar--plist-flags (str)
  "Extract properties from STR if it matches indicators.

Argument STR is a string to be checked against transient menu bar suffix
indicators."
  (unless transient-menu-bar-indicator-regex
    (when transient-menu-bar-indicators
      (setq transient-menu-bar-indicator-regex
            (regexp-opt transient-menu-bar-indicators 'words))))
  (when transient-menu-bar-indicator-regex
    (when (string-match-p
           (regexp-opt transient-menu-bar-indicators 'words)
           (downcase (format
                      "%s"
                      str)))
      (list :transient t))))

(defun transient-menu-bar--simplify-binding (binding)
  "Return short version of BINDING."
  (when-let* ((key-descr (key-description binding))
              (parts
               (unless (string-match-p "mouse" key-descr)
                 (split-string key-descr nil t))))
    (let ((str (car (reverse parts))))
      (car (reverse (split-string str "-" t))))))

(defun transient-menu-bar--combine-plists (&rest plists)
  "Merge all given property lists into a single one, overwriting duplicate keys.

Argument PLISTS is a list of property lists."
  (let ((result-plist '()))
    (dolist (plist plists)
      (while plist
        (setq result-plist (plist-put result-plist (car plist) (cadr plist)))
        (setq plist (cddr plist))))
    result-plist))

(defun transient-menu-bar--plist-omit (keys plist)
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

(defun transient-menu-bar--plist-pick (keywords pl)
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

(defun transient-menu-bar--create-mouse-event (&optional pos)
  "Return syntetic mouse event at POS."
  (unless pos (setq pos (window-point)))
  `(mouse-1
    ,(posn-at-point pos)))

(defun transient-menu-bar--simulate-event (km)
  "Return lambda that call KM with fake mouse event."
  (if (symbolp km)
      `(lambda ()
         (interactive)
         (let ((inhibit-mouse-event-check t))
          (call-interactively #',km
           (transient-menu-bar--create-mouse-event))))
    `(lambda ()
       (interactive)
       (let ((inhibit-mouse-event-check t))
        (call-interactively ,km
         (transient-menu-bar--create-mouse-event))))))

(defun transient-menu-bar--simulate-event-with-prefix (km)
  "Create a command that simulates a mouse event for a keymap entry.

Argument KM is a symbol representing a command or a lambda expression to be
called interactively."
  (if (symbolp km)
      `(lambda ()
         (interactive)
         (let ((inhibit-mouse-event-check t))
          (call-interactively #',km
           (transient-menu-bar--create-mouse-event)
           current-prefix-arg)))
    `(lambda ()
       (interactive)
       (let ((inhibit-mouse-event-check t))
        (call-interactively ,km
         (transient-menu-bar--create-mouse-event)
         current-prefix-arg)))))

(defun transient-menu-bar--analyze-menu-item (elt)
  "Parse a menu item for the transient menu bar.

Argument ELT is a menu item to be parsed. It can be a function, a keymap, or a
list representing a menu item.

Return a list containing the keymap, string, and property list if both keymap
and string are present."
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

(defun transient-menu-bar-extra-properties (sym description)
  "Merge extra properties for a given symbol SYM and DESCRIPTION.

Argument SYM is a symbol that represents the menu item.

Argument DESCRIPTION is a string that provides a description for the menu item."
  (transient-menu-bar--combine-plists
   (or (and (stringp description)
            (transient-menu-bar--plist-flags description))
       (when (symbolp sym)
         (transient-menu-bar--plist-flags
          (symbol-name sym))))
   (alist-get sym transient-menu-bar-suffix-props)))

(defun transient-menu-bar-tidy-exit-hooks ()
  "Remove specific hooks from `transient-exit-hook' and `pre-command-hook'."
  (remove-hook 'transient-exit-hook #'transient-menu-bar-tidy-exit-hooks)
  (remove-hook 'pre-command-hook #'transient-menu-bar-log-command-stack))

(defvar transient-menu-bar--prefix-stack nil)

(defun transient-menu-bar-log-command-stack ()
  "Record the command stack for the transient menu bar."
  (when (or (eq this-command 'transient-quit-all)
            (and (not (eq this-command 'transient-quit-one))
                 (not transient--window)))
    (setq transient-menu-bar--prefix-stack nil))
  (when (and (eq this-command 'transient-quit-one)
             (car transient-menu-bar--prefix-stack))
    (call-interactively (pop transient-menu-bar--prefix-stack))))

(defun transient-menu-bar-map-item-to-transient (key elt &optional keys
                                                     prefix-name path)
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
    (let ((res (transient-menu-bar--analyze-menu-item elt)))
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
                (transient-menu-bar--simplify-binding binding))
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
                        (name (make-symbol
                               (transient-menu-bar--make-name
                                prefix-name
                                (or (and key
                                         (symbolp key) key)
                                    str)
                                (transient-menu-bar--increment-counter
                                 'transient-menu-bar--name-counter)))))
                   (append (list
                            genkey
                            str
                            (lambda ()
                              (interactive)
                              (when (and transient-current-command)
                                (push transient-current-command
                                      transient-menu-bar--prefix-stack))
                              (eval
                               `(progn
                                  (transient-define-prefix
                                    ,name
                                    ()
                                    ,(format
                                      "Transient menu for %s commands."
                                      (substring-no-properties
                                       (replace-regexp-in-string
                                        "[\\.]+$" ""
                                        str)))
                                    ,(vector
                                      :description (string-join
                                                    next-path
                                                    " -> ")
                                      :setup-children
                                      (lambda (&rest _args)
                                        (mapcar
                                         (apply-partially
                                          #'transient-parse-suffix
                                          transient--prefix)
                                         (remove nil
                                          (transient-menu-bar-generate-transients
                                           km
                                           prefix-name
                                           nil
                                           next-path)))))
                                    (interactive)
                                    (transient-setup ',name))
                                  ',name)
                               t)
                              (let ((inhibit-mouse-event-check t))
                                (ignore-errors
                                  (call-interactively name
                                                      (transient-menu-bar--create-mouse-event)))))
                            :transient t)
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
                         (transient-menu-bar--plist-pick
                          '(:description) plist)
                         (let ((pl (transient-menu-bar--plist-omit
                                    '(:description)
                                    plist)))
                           (transient-menu-bar--combine-plists
                            pl
                            (transient-menu-bar-extra-properties
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
                        (transient-menu-bar--simulate-event-with-prefix km))
                  (transient-menu-bar--combine-plists
                   (list :transient nil) plist
                   (transient-menu-bar-extra-properties km str))))
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
                        (transient-menu-bar--simulate-event km))
                  (transient-menu-bar--combine-plists
                   plist
                   (transient-menu-bar-extra-properties
                    km str)
                   (list :transient nil))))
                (is-command
                 (append
                  (list genkey (format "%s" str)
                        (lambda ()
                          (interactive)
                          (let ((chosen-string (car-safe elt)))
                            (if (stringp chosen-string)
                                (let
                                    ((last-command-event (intern chosen-string)))
                                  (call-interactively km))
                              (call-interactively km)))))
                  (transient-menu-bar--combine-plists
                   plist
                   (transient-menu-bar-extra-properties km str))))))))))

(defun transient-menu-bar-generate-transients (map &optional prefix-name
                                                   used-keys path)
  "Map menu bar MAP to transient prefixes.

Prepend PREFIX-NAME to the name of corresponding menu item.


<transient-menu-bar-prefix>-<menu-item-name>-<transient-menu-bar-suffix>.

Bindings are uniq to USED-KEYS.

Stores a list of all the generated commands in the free variable:
`transient-menu--bar-all-prefixes'.

PATH is used for recoursive purposes."
  (let ((result)
        (keys used-keys))
    (cond ((keymapp map)
           (map-keymap
            (lambda (item-key elt)
              (let ((item (transient-menu-bar-map-item-to-transient item-key elt
                                                                    keys
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

;;;###autoload (autoload 'transient-menu-bar-dwim-menu "transient-menu-bar" nil t)
(transient-define-prefix transient-menu-bar-dwim-menu ()
  "Generate dwim from context menu menu bar."
  [["" ""]
   ["" ""]
   ["Misc"
    ("M-`" "Menu bar" transient-menu-bar)
    ("M-c" "Context menu" transient-menu-bar-context-menu)]]
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (run-hooks 'activate-menubar-hook)
  (add-hook 'pre-command-hook
            #'transient-menu-bar-log-command-stack)
  (add-hook 'transient-exit-hook
            #'transient-menu-bar-tidy-exit-hooks)
  (transient-replace-suffix 'transient-menu-bar-dwim-menu
    `(0 0)
    [""
     ""])
  (transient-replace-suffix 'transient-menu-bar-dwim-menu
    `(0 1)
    [""
     ""])
  (let* ((menu-main (menu-bar-keymap))
         (menu-temp
          (with-temp-buffer (menu-bar-keymap)))
         (keys)
         (max-height (/ (window-height) 2))
         (should-split)
         (sub-group-idx 0)
         (group-idx 0)
         (idx 0))
    (map-keymap (lambda (key v)
                  (when (and (symbolp key)
                             (not (assq key menu-temp))
                             (listp v))
                    (setq idx 0)
                    (when-let ((label (capitalize (symbol-name key)))
                               (res (transient-menu-bar-generate-transients
                                     (cons
                                      'keymap
                                      v)
                                     (concat "transient-menu-bar-"
                                             (symbol-name
                                              key))
                                     keys)))
                      (setq keys (append keys (mapcar #'car-safe res)))
                      (transient-insert-suffix
                        'transient-menu-bar-dwim-menu
                        `(0 ,group-idx -1)
                        label
                        nil)
                      (dolist (item res)
                        (transient-insert-suffix
                          'transient-menu-bar-dwim-menu
                          `(0 ,group-idx -1)
                          item))
                      (transient-insert-suffix
                        'transient-menu-bar-dwim-menu
                        `(0 ,group-idx -1)
                        "")
                      (if (not (and (> (length keys) max-height)
                                    (not should-split)))
                          (setq sub-group-idx (1+ sub-group-idx))
                        (setq should-split idx)
                        (setq group-idx (1+ group-idx))
                        (setq sub-group-idx 0)
                        (setq idx 0)))))
                menu-main)
    (transient-setup #'transient-menu-bar-dwim-menu)))

(defalias 'transient-menu-bar-dwim-minor #'transient-menu-bar-dwim-menu)

;;;###autoload (autoload 'transient-menu-bar-context-menu "transient-menu-bar" nil t)
(transient-define-prefix transient-menu-bar-context-menu ()
  "Generate transient from context menu menu bar."
  [:setup-children
   (lambda (&rest _args)
     (mapcar
      (apply-partially
       #'transient-parse-suffix
       transient--prefix)
      (remove nil
              (transient-menu-bar-generate-transients
               (context-menu-map (transient-menu-bar--create-mouse-event))))))]
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (run-hooks 'activate-menubar-hook)
  (add-hook 'pre-command-hook
            #'transient-menu-bar-log-command-stack)
  (add-hook 'transient-exit-hook
            #'transient-menu-bar-tidy-exit-hooks)
  (transient-setup #'transient-menu-bar-context-menu))

(defalias 'transient-menu-bar-context-menu-dispatch
  #'transient-menu-bar-context-menu)

;;;###autoload (autoload 'transient-menu-bar "transient-menu-bar" nil t)
(transient-define-prefix transient-menu-bar ()
  "Generate transient prefixes from the menu bar."
  [:setup-children
   (lambda (&rest _args)
     (mapcar
      (apply-partially
       #'transient-parse-suffix
       transient--prefix)
      (remove nil
              (transient-menu-bar-generate-transients
               (menu-bar-keymap)
               ""))))]
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (run-hooks 'activate-menubar-hook)
  (add-hook 'pre-command-hook
            #'transient-menu-bar-log-command-stack)
  (add-hook 'transient-exit-hook
            #'transient-menu-bar-tidy-exit-hooks)
  (transient-setup #'transient-menu-bar))

(defalias 'transient-menu-bar-dispatch #'transient-menu-bar)

(provide 'transient-menu-bar)
;;; transient-menu-bar.el ends here
