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



(defcustom transient-menu-bar-top-menu-name 'transient-menu-bar-main-menu
  "The symbol to eval as a name of top level transient command."
  :group 'transient-menu-bar
  :type 'symbol)

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
(defvar transient-menu-bar-make-align-to 50)
(defun transient-menu-bar-make-toggle-description (description value &optional
                                                               on-label
                                                               off-label
                                                               left-separator
                                                               right-separator)
  "Enhance DESCRIPTION for VALUE with ON-LABEL or OFF-LABEL.
Wraps result in LEFT-SEPARATOR and RIGHT-SEPARATOR."
  (let* ((description (or description ""))
         (align (apply #'max (list (+ 5 (length description))
                                   transient-menu-bar-make-align-to))))
    (concat
     (substring (concat
                 (or description "")
                 " "
                 (make-string (1- (1- align)) ?\.)
                 " ")
                0
                align)
     (or left-separator "")
     (if value
         (propertize
          (or on-label "+")
          'face
          'success)
       (propertize
        (or off-label "-")
        'face
        'transient-inactive-value))
     (or right-separator "")
     " ")))

(defun transient-menu-bar-get-button-props (plist description)
  "Return new props for menu-bar's PLIST with DESCRIPTION."
  (let* ((btn (plist-get plist :button))
         (badges
          (pcase (car-safe btn)
            (:toggle '("+" . " "))
            (:radio '("*" . " "))))
         (separators
          (pcase (car-safe btn)
            (:toggle '("[" . "]"))
            (:radio '("(" . ")")))))
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
      :transient t)))

(defun transient-menu-bar--map-plist (pl &optional description)
  "Map props of menu PL to transient slots.
DESCRIPTION should be a string."
  (let ((new-pl))
    (dotimes (idx (length pl))
      (when (eq (logand idx 1) 0)
        (let ((plkey (nth idx pl)))
          (let ((res
                 (pcase plkey
                   ((or :active :enable)
                    `(:inapt-if-not
                      (lambda ()
                        (ignore-errors ,(plist-get pl plkey)))))
                   ((or :visible :included)
                    (setq new-pl
                          `(:if
                            (lambda ()
                              (ignore-errors ,(plist-get pl plkey))))))
                   (:label
                    `(:description
                      (lambda ()
                        ,(plist-get pl plkey))))
                   (:suffix
                    `(:description
                      (lambda ()
                        (concat ,description
                                ,(plist-get pl plkey)))))
                   (:button
                    (transient-menu-bar-get-button-props pl description))
                   (:selected `(:description
                                (lambda ()
                                  (if ,(plist-get pl plkey)
                                      (concat ,description
                                              " (Selected)")
                                    ,description)))))))
            (setq new-pl (append new-pl res))))))
    new-pl))

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
      (insert
       ";;; generated.el --- Generated transients -*- lexical-binding: t -*-\n\n;;; Code:\n\n"
       result)
      (delay-mode-hooks
        (emacs-lisp-mode))
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
                 "[^a-z]" ""
                 (if (symbolp it)
                     (symbol-name it)
                   (format "%s" it)))))
             (remove nil args)
             ""))

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
     lexical-binding)
    forms))

;;;###autoload
(defun transient-menu-bar-eval-minors ()
  "Generate and eval `transient-define-prefix' forms from MENU-BAR keymap."
  (let* ((eval-expression-debug-on-error nil)
         (forms (transient-menu-bar-map-minors)))
    (eval
     (cons 'progn
           forms)
     lexical-binding)
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


(defun transient-menu-bar-mapper (key elt &optional keys prefix-name)
  "Map KEY and ELT to transient item.

Bindings are uniq to KEYS.

Prepend PREFIX-NAME to the name of corresponding menu item.

Stores a list of all the generated commands in the free variable:
`transient-menu-bars-all-prefixes'."
  (if (and key (symbolp key)
           (string-match-p "^sep" (symbol-name key)))
      ""
    (let ((km)
          (str)
          (plist)
          (binding)
          (visible)
          (filter)
          (genkey)
          (key-descr)
          (sep ""))
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
               (unless (eval (plist-get plist :visible))
                 (setq km nil)
                 (setq sep nil)))
             (setq plist (transient-menu-bar--map-plist plist
                                                        str)))
            ((or (keymapp (cdr-safe (cdr-safe (cdr-safe
                                               elt))))
                 (functionp (cdr-safe (cdr-safe (cdr-safe
                                                 elt)))))
             (setq km (cdr (cddr elt)))
             (and (stringp (car elt))
                  (setq str (car elt)))))
      (when (and km str
                 (not (member str menu-bar-separator)))
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
        (push genkey keys))
      (cond ((not km)
             sep)
            ((member str menu-bar-separator)
             str)
            ((keymapp km)
             (let* ((rec (transient-menu-bar-recoursive km))
                    (name (make-symbol
                           (transient-menu-bar-generate-name
                            prefix-name
                            (or (and key (symbolp key) key)
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
                          ,(apply #'vector rec)))))
               (when (ignore-errors (eval tran))
                 (push tran transient-menu-bars-all-prefixes)
                 (list
                  genkey
                  (format "%s" str)
                  name))))
            ((and plist
                  (plist-get plist :description)
                  (commandp km))
             (append
              (list genkey
                    km
                    :description
                    (plist-get
                     plist
                     :description))
              (transient-menu-bar-merge-plist
               (transient-menu-bar-get-plist-props
                str)
               plist)
              plist
              (alist-get
               km
               transient-menu-bar-symbol-suffixes-props)))
            ((functionp km)
             (append
              (list genkey (format "%s" str) km)
              (transient-menu-bar-get-plist-props
               str)
              plist
              (alist-get
               km
               transient-menu-bar-symbol-suffixes-props)))))))

(defun transient-menu-bar-recoursive (map &optional prefix-name used-keys)
  "Map menu bar MAP to transient prefixes.

Prepend PREFIX-NAME to the name of corresponding menu item.


<transient-menu-bar-prefix>-<menu-item-name>-<transient-menu-bar-suffix>.

Bindings are uniq to USED-KEYS.

Stores a list of all the generated commands in the free variable:
`transient-menu-bars-all-prefixes'."
  (let ((result)
        (keys used-keys))
    (cond ((keymapp map)
           (map-keymap
            (lambda (it elt)
              (when (and it (symbolp it)
                         (not (eq 'mouse-1 it)))
                (let ((item (transient-menu-bar-mapper it elt keys
                                                       prefix-name)))
                  (cond ((stringp (car-safe item))
                         (push (car item) keys)
                         (push item result))
                        ((and
                          (not (stringp (car result)))
                          (or (not item)
                              (stringp item)))
                         (push item result))))))
            map))
          ((listp map)
           (dolist (elt map)
             (cond ((stringp elt)
                    (push elt result))
                   ((listp elt)
                    (push (transient-menu-bar-recoursive elt)
                          result))
                   ((vectorp elt)
                    (dotimes (i (length elt))
                      (push (transient-menu-bar-recoursive
                             (cons i (aref elt i)))
                            result)))))))
    (or (seq-filter
         (lambda (it)
           (if (listp it)
               (car it)
             (when (stringp it)
               (member it
                       (append menu-bar-separator (list ""))))))
         (reverse result))
        map)))


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
    (let* ((name (completing-read "Symbol: "
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
                           lexical-binding))))))


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
                   (context-menu-map)))
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
  (when (and (boundp 'yank-menu)
             (listp yank-menu))
    (let* ((forms (transient-menu-bar-eval (menu-bar-keymap)))
           (sym (nth 1 (car (last forms)))))
      (if (minibufferp)
        (with-minibuffer-selected-window (funcall-interactively sym))
        (funcall-interactively sym)))))


(provide 'transient-menu-bar)
;;; transient-menu-bar.el ends here