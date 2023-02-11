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
  "Transform menu bar ITEM props to transient.
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

(defun transient-menu-bar--generate-key (flag &optional used-keys)
  "Generate hydra key for option FLAG that not present in USED-KEYS."
  (when (> (length flag) 2)
    (setq flag (replace-regexp-in-string "--" "" flag)))
  (if (and flag (member flag '("--" "-"))
           (not (member "-" used-keys)))
      "-"
    (or (seq-find
         #'(lambda
             (it)
             (and
              (key-valid-p it)
              (let ((args
                     (list it)))
                (not
                 (apply
                  #'(lambda
                      (&rest pre-args)
                      (apply #'member
                             (append pre-args
                                     (list used-keys))))
                  args)))))
         (mapcar #'(lambda
                     (&rest pre-args)
                     (apply #'substring
                            (append pre-args
                                    (list 0 1))))
                 (split-string
                  flag "-" t)))
        (seq-find #'(lambda
                      (it)
                      (and
                       (key-valid-p it)
                       (let ((args
                              (list it)))
                         (not
                          (apply
                           #'(lambda
                               (&rest pre-args)
                               (apply #'member
                                      (append pre-args
                                              (list used-keys))))
                           args)))))
                  (split-string flag "" t))
        (seq-find #'(lambda
                      (it)
                      (and
                       (key-valid-p it)
                       (let ((args
                              (list it)))
                         (not
                          (apply
                           #'(lambda
                               (&rest pre-args)
                               (apply #'member
                                      (append pre-args
                                              (list used-keys))))
                           args)))))
                  (seq-difference
                   (nconc (transient-menu-bar--get-alphabete "a")
                          (transient-menu-bar--get-alphabete "A")
                          (delete "\""
                                  (transient-menu-bar--get-alphabete "!"
                                                             25)))
                   used-keys)))))

(defun transient-menu-bar--menu-bar-keymap-bindings (binding)
  "Return list of transient prefixes for menu bar BINDING."
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
                   (let ((result)
                         (used-keys))
                     (dolist (item sublist)
                       (let* ((subresult
                               (car
                                (ignore-errors
                                  (transient-menu-bar--menu-bar-keymap-bindings
                                   item))))
                              (subtransient
                               (ignore-errors
                                 (when subresult
                                   (list
                                    (transient-menu-bar--generate-key
                                     (seq-find
                                      'stringp
                                      subresult))
                                    (seq-find
                                     'stringp
                                     subresult)
                                    (nth 1 subresult))))))
                         (when subtransient
                           (setq transients (append (list subresult)
                                                    transients))
                           (push subtransient result))
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
                                        (key (when description
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
                                           pl)
                                          result)))))))
                     (when result
                       (apply 'vector (reverse result)))))))
      (append (list `(transient-define-prefix
                       ,(intern
                         (concat (string-join (split-string
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
                                 "-dispatch-menu-transient"))
                       () ,title ,(or vect)))
              transients))))

(defun transient-menu-bar-format-transient (items &optional should-eval)
  "Prettify transient ITEMS.
If SHOULD-EVAL is non nil, also evaluate them."
  (let* ((print-length nil)
         (result (pp-to-string (append
                                (list 'progn)
                                items))))
    (setq result (replace-regexp-in-string "(lambda[\s\t\n]+nil[\s\t\n]"
                                           "(lambda () "
                                           result))
    (setq result (replace-regexp-in-string
                  "(transient-define-prefix[\s\t\r][a-zz-a-]+[\s\t]+\\(nil\\)"
                  "()\n\s\s"
                  result
                  nil
                  nil
                  1))
    (with-current-buffer (get-buffer-create "*km-transient-generated*")
      (erase-buffer)
      (insert
       ";;; km-transient-generated.el --- Generated transients -*- lexical-binding: t -*-\n\n;;;Code:\n\n"
       result)
      (emacs-lisp-mode)
      (when should-eval
        (eval-buffer)))))

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

;;;###autoload
(defun transient-menu-bar ()
  "Evaluate and call `menu-bar-transient'."
  (interactive)
  (let ((transients (seq-filter
                     (lambda (it)
                       (eq (car it) 'transient-define-prefix))
                     (transient-menu-bar--map-menu-map (menu-bar-keymap))))
        (used-keys)
        (final-transient)
        (filtered (seq-uniq
                   (delq nil (mapcar (lambda (it)
                                       (ignore-errors (seq-find #'stringp it)))
                                     (cdr (menu-bar-keymap)))))))
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
    (transient-menu-bar-format-transient (append transients (list
                                                             final-transient)
                                                 (list '(menu-bar-transient)))
                                         t)))

(provide 'transient-menu-bar)
;;; transient-menu-bar.el ends here