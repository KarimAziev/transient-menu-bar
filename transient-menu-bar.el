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

(eval-when-compile (require 'subr-x))

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
  "Generate key for option FLAG that not present in USED-KEYS."
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
  "Enhance DESCRIPTION for VALUE with ON-LABEL or OFF-LABEL.
Wraps result in LEFT-SEPARATOR and RIGHT-SEPARATOR.
Add DIVIDER before result."
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
  "Return new props for menu-bar's PLIST with DESCRIPTION.
ENABLED-KEYWORD is a either :active or :enabled."
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
            descr))
        :transient t))))

(defun transient-menu-bar--map-plist (pl &optional description)
  "Map props of menu PL to transient slots.
DESCRIPTION should be a string."
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
                  "(transient-define-prefix[\s\t\r][a-zz-a-0-9/]+[\s\t]+\\(nil\\)"
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
             (remove nil args)
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
           ,(apply
             #'vector
             res)))))))

;;;###autoload
(defun transient-menu-bar-eval (menu-bar)
  "Generate and eval `transient-define-prefix' forms from MENU-BAR keymap."
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
  "Generate and eval `transient-define-prefix' forms from MENU-BAR keymap."
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
  (let (transient-menu-bars-all-prefixes)
    (let* ((menu-main (menu-bar-keymap))
           (menu-temp (with-temp-buffer (menu-bar-keymap)))
           (keys)
           (max-height (/ (window-height) 2))
           (should-split)
           (filtered
            (let ((result)
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

(defun transient-menu-bar-merge-plist (plist-a plist-b)
  "Add props from PLIST-B to PLIST-A."
  (dotimes (idx (length plist-b))
    (when (eq (logand idx 1) 0)
      (let ((prop-name (nth idx plist-b)))
        (let ((val (plist-get plist-b prop-name)))
          (plist-put plist-a prop-name val)))))
  plist-a)

(defun transient-menu-bar-make-mouse-event ()
  "Return syntetic mouse event."
  `(mouse-1             ;; button
    (,(selected-window) ;; window
     ,(1+
       (point))
     ;; position
     (0 . 0) ;; window-relative pixel
     0       ;; timestamp
     nil     ;; object
     ,(point)
     ;; text position
     (,(current-column)
      . ;; column
      ,(line-number-at-pos
        (point))) ;; line
     nil          ;; image
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
  "Parse ELT."
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

(defun transient-menu-bar-mapper (key elt &optional keys prefix-name path)
  "Map KEY and ELT to transient item.

Bindings are uniq to KEYS.

Prepend PREFIX-NAME to the name of corresponding menu item.

Stores a list of all the generated commands in the free variable:
`transient-menu-bars-all-prefixes'.

PATH is used for recoursive purposes."
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
                              nil
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
                              [,(string-join next-path
                                             " -> ")
                               ,(apply
                                 #'vector rec)]))))
                   (push tran transient-menu-bars-all-prefixes)
                   (append (list
                            genkey
                            name
                            :description (lambda ()
                                           (transient-menu-bar-make-toggle-description
                                            str
                                            (> (length rec) 0)
                                            ""
                                            ""
                                            " "
                                            (format "%s" (length rec))
                                            ".")))
                           plist)))
                ((not km)
                 sep)
                ((member str menu-bar-separator)
                 str)
                ((and
                  (setq is-command (functionp km))
                  (plist-get plist :description))
                 (let ((merged (append
                                (list genkey
                                      km
                                      :description
                                      (lambda ()
                                        (plist-get
                                         plist
                                         :description)))
                                (transient-menu-bar-merge-plist
                                 (or (transient-menu-bar-get-plist-props
                                      str)
                                     (ignore-errors (transient-menu-bar-get-plist-props
                                                     (symbol-name km))))
                                 plist)
                                plist
                                (alist-get
                                 km
                                 transient-menu-bar-symbol-suffixes-props))))
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
                  (or (transient-menu-bar-get-plist-props
                       str)
                      (ignore-errors (transient-menu-bar-get-plist-props
                                      (symbol-name km))))
                  plist
                  (alist-get
                   km
                   transient-menu-bar-symbol-suffixes-props)
                  (list :transient nil)))
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
                  (or (transient-menu-bar-get-plist-props
                       str)
                      (ignore-errors (transient-menu-bar-get-plist-props
                                      (symbol-name km))))
                  plist
                  (alist-get
                   km
                   transient-menu-bar-symbol-suffixes-props)
                  (list :transient nil)))
                (is-command
                 (append
                  (list genkey (format "%s" str) km)
                  (or (transient-menu-bar-get-plist-props
                       str)
                      (ignore-errors (transient-menu-bar-get-plist-props
                                      (symbol-name km))))
                  plist
                  (alist-get
                   km
                   transient-menu-bar-symbol-suffixes-props)))))))))




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
  "Filter MAP by SYMS."
  (let ((result))
    (setq syms (flatten-list syms))
    (map-keymap (lambda (key v)
                  (when (memq key syms)
                    (push (cons key v) result)))
                map)
    (cons 'keymap result)))


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
    (let* ((name
            (completing-read "Symbol: "
                             (remove 'mouse-1
                                     (mapcar #'car
                                             (cdr
                                              (menu-bar-keymap))))))
           (sym (intern name))
           (map
            (transient-menu-bar-filter-map (menu-bar-keymap)
                                           sym)))
      (transient-menu-bar-recoursive map
                                     (read-string "transient-define-prefix "
                                                  name))
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

(defun transient-menu-bar-resume-last ()
  "Resume last local prefix command."
  (interactive)
  (if (commandp transient-menu-bar-last-prefix)
      (call-interactively transient-menu-bar-last-prefix)
    (transient-menu-bar-dispatch)))

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
  "Create a transient menu of possible choices from `menu-bar-keymap'."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (run-hooks 'activate-menubar-hook)
  (when (and (boundp 'yank-menu)
             (listp yank-menu))
    (let* ((forms (transient-menu-bar-eval (menu-bar-keymap)))
           (sym (nth 1 (car (last forms)))))
      (if (minibufferp)
          (with-minibuffer-selected-window (funcall-interactively sym))
        (funcall-interactively sym)))))


(provide 'transient-menu-bar)
;;; transient-menu-bar.el ends here