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

(defcustom transient-menu-bar-symbol-suffixes-props (mapcar
                                                     (lambda (it)
                                                       `(,it :transient t))
                                                     transient-menu-bar-default-suffixes-props)
  "Alist of functions and extra props for transient."
  :group 'transient-menu-bar
  :type '(alist
          :key-type (function :tag "Symbol")
          :value-type
          (plist :options ((:transient boolean)))))

(defun transient-menu-bar-shortcut-pred (used-keys key)
  "Return non nil if KEY is a valid and not present in USED-KEYS."
  (and
   (key-valid-p key)
   (not (member key used-keys))))

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


(defun transient-menu-bar--map-plist (pl &optional description)
  "Map props of menu PL to transient slots.
DESCRIPTION should be a string."
  (let ((new-pl))
    (dotimes (idx (length pl))
      (when (eq (logand idx 1) 0)
        (let ((plkey (nth idx pl)))
          (when (readablep pl)
            (let ((res (pcase plkey
                         ((or :active :enable)
                          `(:inapt-if-not
                            (lambda ()
                              (ignore-errors ,(plist-get pl plkey)))))
                         ((or :visible :included)
                          (setq new-pl
                                `(:if
                                  (lambda ()
                                    (ignore-errors ,(plist-get pl plkey))))))
                         (:label `(:description
                                   (lambda ()
                                     ,(plist-get pl plkey))))
                         (:suffix
                          `(:description
                            (lambda ()
                              (concat ,description
                                      ,(plist-get pl plkey)))))
                         (:button
                          (let* ((btn (plist-get pl :button))
                                 (val
                                  (pcase (car-safe btn)
                                    ((or :toggle :radio)
                                     `(:description
                                       (lambda ()
                                         (concat ,(or
                                                   description
                                                   (plist-get
                                                    pl
                                                    :help)
                                                   "")
                                                 (if ,(cdr
                                                       btn)
                                                     (propertize " (on)"
                                                                 'face
                                                                 'success)
                                                   (propertize " (off)"
                                                               'face 'error))))
                                       :transient t)))))
                            val))
                         (:selected `(:description
                                      (lambda ()
                                        (if ,(plist-get pl plkey)
                                            (concat ,description
                                                    " (Selected)")
                                          ,description)))))))
              (setq new-pl (append new-pl res)))))))
    new-pl))

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
       ";;; transient-menu-bar.el --- Generated transients -*- lexical-binding: t -*-\n\n;;;Code:\n\n"
       result)
      (delay-mode-hooks
        (emacs-lisp-mode)
        (when should-eval
          (eval-buffer)))
      (pop-to-buffer (current-buffer)))))

(defvar transients-menu-bars-all-prefixes)
(defvar transient-menu-bar-counter 0)
(defvar transient-menu-bar-prefix)
(defvar transient-menu-bar-suffix)

;;;###autoload
(defun transient-menu-bar-generate (menu-bar)
  "Generate `transient-define-prefix' forms from MENU-BAR keymap."
  (setq transient-menu-bar-counter 0)
  (let (transients-menu-bars-all-prefixes
        (transient-menu-bar-prefix "trm-")
        transient-menu-bar-suffix)
    (let ((res
           (seq-filter (lambda (it)
                         (if (listp it)
                             (car it)
                           (when (stringp it)
                             (member it
                                     (append menu-bar-separator (list ""))))))
                       (transient-menu-bar-recoursive menu-bar))))
      (let ((eval-expression-debug-on-error nil))
        (list 'quote (eval (cons 'progn
                                 (append
                                  transients-menu-bars-all-prefixes
                                  (list `(transient-define-prefix
                                           km-menu-bar-transient-main
                                           nil
                                           "Command dispatcher for menu bar."
                                           ,(apply #'vector res)))))
                           lexical-binding))))))

(defun transient-menu-bar-get-plist-props (str)
  "Return extra props for STR."
  (when (string-match-p
         "\\(^\\|[-\t\s]\\)\\(forward\\|backward\\|up\\|down\\|undo\\|redo\\|next\\|prev\\|previous\\|more\\|less\\|\\)\\([-\s\t]\\|$\\)"
         (format
          "%s"
          str))
    (list :transient t)))

(defun transient-menu-bar-recoursive (map)
  "Map menu bar keymap MAP to a transient menu.
Put generated `transient-define-prefix' forms in
the free variable `transients-menu-bars-all-prefixes'."
  (let ((result)
        (keys))
    (cond ((keymapp map)
           (map-keymap
            (lambda (it elt)
              (if (and it (symbolp it)
                       (string-match-p "^separator-" (symbol-name it)))
                  (setq result (push "" result))
                (let ((km)
                      (str)
                      (plist)
                      (binding)
                      (visible)
                      (filter)
                      (key))
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
                         (setq visible (plist-get plist :visible))
                         (if visible
                             (setq km (and (eval visible) km)))
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
                    (setq key (transient-menu-bar--generate-key
                               (downcase str)
                               keys))
                    (setq binding (where-is-internal km nil t))
                    (when binding
                      (setq binding (key-description binding))))
                  (when key
                    (push key keys))
                  (cond ((member str menu-bar-separator)
                         (setq result (push
                                       str
                                       result)))
                        ((keymapp km)
                         (let* ((rec (transient-menu-bar-recoursive km))
                                (name (intern
                                       (transient-menu-bar-generate-name
                                        str)))
                                (tran (when (and (listp rec)
                                                 (stringp str))
                                        `(transient-define-prefix ,name
                                           nil
                                           ,(format
                                             "Transient menu for %s commands."
                                             (substring-no-properties (replace-regexp-in-string
                                                                       "[\\.]+$"
                                                                       ""
                                                                       str)))
                                           ,(apply 'vector rec)))))
                           (when (ignore-errors (eval tran))
                             (push tran transients-menu-bars-all-prefixes)
                             (setq result (push
                                           (list
                                            key
                                            (format "%s" str)
                                            name)
                                           result)))))
                        ((and plist
                              (plist-get plist :description)
                              (commandp km))
                         (setq result
                               (push
                                (append
                                 (list key
                                       km
                                       :description
                                       (plist-get
                                        plist
                                        :description))
                                 (transient-menu-bar-get-plist-props
                                  str)
                                 plist
                                 (alist-get
                                  km
                                  transient-menu-bar-symbol-suffixes-props))
                                result)))
                        ((functionp km)
                         (setq result
                               (push
                                (append
                                 (list key
                                       (if  binding
                                           (format "%s (%s)" str binding)
                                         (format "%s" str))
                                       km)
                                 (transient-menu-bar-get-plist-props
                                  str)
                                 plist
                                 (alist-get
                                  km
                                  transient-menu-bar-symbol-suffixes-props))
                                result)))))))
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
    (or (reverse result) map)))


;;;###autoload
(defun transient-menu-bar-show-all ()
  "Generate and print transients from `menu-bar-keymap'."
  (interactive)
  (let (transients-menu-bars-all-prefixes
        (transient-menu-bar-prefix (read-string "Prefix name: "))
        (transient-menu-bar-suffix (read-string "Suffix name: ")))
    (setq transient-menu-bar-counter
          (when (yes-or-no-p
                 "Add counter to names?")
            (read-number "Start from: "
                         transient-menu-bar-counter)))
    (run-hooks 'menu-bar-update-hook)
    (let ((res (seq-filter
                (lambda (it)
                  (if (listp it)
                      (car it)
                    (when (stringp it)
                      (member it
                              (append menu-bar-separator (list ""))))))
                (transient-menu-bar-recoursive (menu-bar-keymap)))))
      (let ((eval-expression-debug-on-error nil))
        (transient-menu-bar-print-generated-transients
         (cons 'progn
               (append
                transients-menu-bars-all-prefixes
                (list
                 `(transient-define-prefix
                    km-menu-bar-transient-main
                    nil
                    "Command dispatcher for menu bar."
                    ,(apply
                      #'vector
                      res))))))))))

(defun transient-menu-bar-generate-name (label)
  "Generate name menu bar LABEL."
  (concat (mapconcat
           (lambda (it)
             (replace-regexp-in-string "[\s\t\r\f\t]" "-"
                                       (downcase it)))
           (remove nil
                   (list transient-menu-bar-prefix
                         label
                         transient-menu-bar-suffix))
           "-")
          (if (numberp transient-menu-bar-counter)
              (format "-%d"
                      (setq
                       transient-menu-bar-counter
                       (1+
                        transient-menu-bar-counter)))
            "")))


;;;###autoload
(defun transient-menu-bar-context-menu-dispatch ()
  "Create a transient menu of possible choices from `menu-bar-keymap'."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (when (and (boundp 'yank-menu)
             (listp yank-menu))
    (eval (transient-menu-bar-generate (context-menu-map)))
    (when (fboundp 'km-menu-bar-transient-main)
      (funcall-interactively 'km-menu-bar-transient-main))))


;;;###autoload
(defun transient-menu-bar-dispatch ()
  "Create a transient menu of possible choices from `menu-bar-keymap'."
  (interactive)
  (run-hooks 'menu-bar-update-hook)
  (when (and (boundp 'yank-menu)
             (listp yank-menu))
    (eval (transient-menu-bar-generate (menu-bar-keymap)))
    (when (fboundp 'km-menu-bar-transient-main)
      (funcall-interactively 'km-menu-bar-transient-main))))

(provide 'transient-menu-bar)
;;; transient-menu-bar.el ends here