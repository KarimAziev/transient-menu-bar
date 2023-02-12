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
  (let* ((keys (seq-filter 'keywordp item))
         (pl (transient-menu-bar-plist-pick  keys item))
         (new-pl))
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
    new-pl))

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

(defun transient-menu-bar--menu-bar-keymap-bindings (binding &optional
                                                             used-keys)
  "Return list of transient prefixes for menu bar BINDING.
USED-KEYS is a list of not allowed characters formatted to strings."
  (let ((transients))
    (when-let* ((title
                 (or (seq-find 'stringp binding)
                     (seq-find 'symbolp binding)))
                (vect
                 (when-let ((sublist (when (listp binding)
                                       (or (cdr (memq 'keymap
                                                      binding))
                                           (cdr (seq-find 'listp
                                                          binding))))))
                   (let ((result))
                     (dolist (item sublist)
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
                                           (seq-find
                                            'stringp
                                            subresult)
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
                         (when-let ((description (seq-find 'stringp item))
                                    (cmd (seq-find 'functionp item)))
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
                                          result)))))))
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
              transients))))

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