;;; quicklisp-apropos.el --- Commands for quicklisp-apropos -*- lexical-binding: t -*-

;; Copyright (C) 2023 Mariano Montone

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Install:
;; Emacs side: just put this file in your load-path and load it on init.
;; Lisp side: (load "quicklisp-apropos.lisp") in your init file (i.e. .sbclrc).

;; Use:
;; M-x quicklisp-apropos
;; Customize max results with: M-x customize-variable RET quicklisp-apropos-max-results RET

;;; Code:

(require 'slime)

(defgroup quicklisp-apropos nil
  "Quicklisp-apropos settings."
  :group 'slime)

(defcustom quicklisp-apropos-max-results 50
  "Maximum number of results to be returned by quicklisp-apropos."
  :type 'integer
  :group 'quicklisp-apropos)

(defcustom quicklisp-apropos-query-results-function
  'quicklisp-apropos--query-results
  "Internal function to use for fetching and showing quicklisp-apropos results."
  :type 'symbol
  :group 'quicklisp-apropos)

(defun quicklisp-apropos-update-index ()
  "Download and update quicklisp-apropos index."
  (interactive)
  (message "Downloding quicklisp-apropos index ...")
  (slime-eval '(quicklisp-apropos:download-index))
  (message "quicklisp-apropos index updated."))

;; Taken from elisp-mode, after elisp-mode--docstring-first-line.
;; Note that any leading `*' in the docstring (which indicates the variable
;; is a user option) is removed.
(defun quicklisp-apropos--docstring-first-line (doc)
  "Return first line of DOC."
  (and (stringp doc)
       (substitute-command-keys
        (save-match-data
          ;; Don't use "^" in the regexp below since it may match
          ;; anywhere in the doc-string.
          (let ((start (if (string-match "\\`\\*" doc) (match-end 0) 0)))
            (cond ((string-match "\n" doc)
                   (substring doc start (match-beginning 0)))
                  ((zerop start) doc)
                  (t (substring doc start))))))))

(defun quicklisp-apropos--open-buffer-with-results (buffer-name results)
  "Open a buffer named with BUFFER-NAME and show the list of apropos RESULTS."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (dolist (result results)
        (let ((name (cdr (assoc-string "name" result)))
              (type (cdr (assoc-string "type" result)))
              (doc (cdr (assoc-string "doc" result)))
              (system (cdr (assoc-string "system" result))))
          (if (string= type "system")
              (insert-button (upcase name)
                             'follow-link t
                             'help-echo "Load system."
                             'face 'slime-apropos-symbol
                             'action (lambda (_)
                                       (when (yes-or-no-p (format "Load %s system?" name))
                                         (slime-eval `(ql:quickload ,name)))))
            ;; else
            (insert-button name
                           'follow-link t
                           'help-echo "Load system and edit definition."
                           'face 'slime-apropos-symbol
                           'action (lambda (_)
                                     (when (yes-or-no-p (format "Load %s system?" system))
                                       (slime-eval `(ql:quickload ,system))
                                       (slime-edit-definition name)))))
          (when system
            (insert " in system ")
            (insert-button system
                           'follow-link t
                           'help-echo "Load system"
                           'action (lambda (_)
                                     (when (yes-or-no-p (format "Load %s system?" system))
                                       (slime-eval `(ql:quickload ,system))))))
          (newline)
          (insert "  " (propertize (capitalize type) 'face 'underline) ": ")
          (if doc
              (insert (quicklisp-apropos--docstring-first-line doc))
            (insert "Not documented"))
          (newline)))
      (local-set-key "q" 'kill-buffer)
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (goto-char 0)
      (pop-to-buffer buffer))))

(defun quicklisp-apropos--open-buffer-with-printed-results (buffer-name results)
  "Open a buffer named with BUFFER-NAME and show the printed apropos RESULTS."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (insert results)
      (local-set-key "q" 'kill-buffer)
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (goto-char 0)
      (pop-to-buffer buffer))))

(defun quicklisp-apropos--query-printed-results (apropos-function query)
  "Call APROPOS-FUNCTION with QUERY.
The printed results are show in an Emacs buffer."
  (let* ((results
          (slime-eval `(cl:with-output-to-string
                        (cl:*standard-output*)
                        (,apropos-function ,query :count ,quicklisp-apropos-max-results))))
         (buffer-name (format "*quicklisp-apropos: %s*" query)))
    (quicklisp-apropos--open-buffer-with-printed-results buffer-name results)))

(defun quicklisp-apropos--query-results (apropos-function query)
  "Call APROPOS-FUNCTION with QUERY.  Show result in an Emacs buffer."
  (let* ((results
          (slime-eval `(,apropos-function ,query :count ,quicklisp-apropos-max-results :print-results nil)))
         (buffer-name (format "*quicklisp-apropos: %s*" query)))
    (quicklisp-apropos--open-buffer-with-results buffer-name
                                                 (mapcar #'car results))))

(defun quicklisp-apropos (query)
  "Apropos quicklisp using a generic QUERY.
If QUERY contains a ?: color character, then interpret the query
as a Montezuma query string.
Otherwise, build a proper Montezuma query with the term,
one that looks into 'name' and 'doc' fields."

  (interactive "sQuicklisp apropos: ")

  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos query))

(defun quicklisp-apropos-system (query)
  "Search across ASDF systems in Quicklisp libraries that match the QUERY."
  (interactive "sQuicklisp apropos system: ")
  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos-system query))

(defun quicklisp-apropos-package (query)
  "Search across Lisp packages in Quicklisp libraries that match the QUERY."
  (interactive "sQuicklisp apropos package: ")
  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos-package query))

(defun quicklisp-apropos-variable (query)
  "Search across Lisp variables exported in Quicklisp libraries that match the QUERY."
  (interactive "sQuicklisp apropos variable: ")
  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos-variable query))

(defun quicklisp-apropos-class (query)
  "Search across CLOS classes exported in Quicklisp libraries that match the QUERY."
  (interactive "sQuicklisp apropos class: ")
  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos-class query))

(defun quicklisp-apropos-function (query)
  "Search across Lisp functions exported in Quicklisp libraries that match the QUERY."
  (interactive "sQuicklisp apropos function: ")

  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos-function query))

(defun quicklisp-apropos-macro (query)
  "Search across Lisp macros exported in Quicklisp libraries that match the QUERY."
  (interactive "sQuicklisp apropos macro: ")

  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos-macro query))

(defun quicklisp-apropos-generic-function (query)
  "Search across CLOS generic functions exported in Quicklisp libraries that match the QUERY."
  (interactive "sQuicklisp apropos generic function: ")

  (funcall quicklisp-apropos-query-results-function
           'quicklisp-apropos:apropos-generic-function query))

;;---- SLIME integration ------------------------------------------------------

(defun quicklisp-apropos--add-to-slime-menu ()
  "Add quicklisp-apropos menu to SLIME menu."
  (easy-menu-add-item 'menubar-slime nil '("---"))
  (easy-menu-add-item 'menubar-slime nil
                      '("Quicklisp apropos"
                        ["Apropos" quicklisp-apropos
                         :help "Apropos across Quicklisp libraries."]
                        ["Apropos function" quicklisp-apropos-function
                         :help "Apropos functions exported across Quicklisp libraries."]
                        ["Apropos variable" quicklisp-apropos-variable
                         :help "Apropos variables exported across Quicklisp libraries."]
                        ["Apropos class" quicklisp-apropos-class
                         :help "Apropos classes exported across Quicklisp libraries."]
                        ["Apropos system" quicklisp-apropos-system
                         :help "Apropos ASDF systems across Quicklisp libraries."]
                        ["Apropos package" quicklisp-apropos-package
                         :help "Apropos packages across Quicklisp libraries."]
                        ["Update index" quicklisp-apropos-update-index
                         :help "Download and update quicklisp-apropos index."]
                        )))

(define-slime-contrib quicklisp-apropos
  "Apropos across Quicklisp libraries."
  (:authors "Mariano Montone")
  (:license "GPL")
  (:swank-dependencies quicklisp-apropos)
  (:on-load
   (quicklisp-apropos--add-to-slime-menu)))

(provide 'quicklisp-apropos)

;;; quicklisp-apropos.el ends here
