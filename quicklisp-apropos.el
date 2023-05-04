;;; quicklisp-apropos.el --- Commands for quicklisp-apropos      -*- lexical-binding: t -*-

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

;; Install:
;; Emacs side: just put this file in your load-path and load it on init.
;; Lisp side: add (require :quicksearch) to your compiler init file (i.e. .sbclrc).
;; NOTE:
;; Current quicksearch is buggy for some results.
;; I recommend you apply this patch: https://github.com/tkych/quicksearch/issues/10

;; Use:
;; M-x quicksearch
;; Customize max results with: M-x customize-variable RET quicksearch-max-results RET

(require 'slime)

(defcustom quicklisp-apropos-max-results 50
  "Maximum number of results to be returned by quicklisp-apropos.")

(defcustom quicklisp-apropos-query-results-function
  'quicklisp-apropos--query-results
  "Internal function to use for fetching and showing quicklisp-apropos results.")



;; (define-button-type 'quicksearch-link-button
;;   'action #'quicksearch--follow-link
;;   'follow-link t
;;   'help-echo "Follow this link")

;; (defun quicksearch--propertize-links (string)
;;   "Convert URL links in strings to buttons."
;;   (replace-regexp-in-string
;;    (rx (group (or string-start space "<"))
;;        (group "http" (? "s") "://" (+? (not (any space))))
;;        (group (? (any "." ">" ")"))
;;               (or space string-end ">")))
;;    (lambda (match)
;;      (let ((space-before (match-string 1 match))
;;            (url (match-string 2 match))
;;            (after (match-string 3 match)))
;;        (concat
;;         space-before
;;         (quicksearch--button
;;          url
;;          'quicksearch-link-button
;;          'url url)
;;         after)))
;;   string))

(defun quicklisp-apropos--open-buffer-with-results (buffer-name results)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (dolist (result results)
        (let ((name (cdr (assoc-string "name" result)))
              (type (cdr (assoc-string "type" result)))
              (doc (cdr (assoc-string "doc" result)))
              (system (cdr (assoc-string "system" result))))
          (insert type)
          (insert " ")
          (insert-button name
                         'follow-link t
                         'help-echo "Load system and edit definition."
                         'action (lambda (_)
                                   (when (yes-or-no-p (format "Load %s system?" system))
                                     (slime-eval `(ql:quickload ,system))
                                     (slime-edit-definition name))))
          (insert " in system ")
          (insert-button system
                         'follow-link t
                         'help-echo "Load system"
                         'action (lambda (_)
                                   (when (yes-or-no-p (format "Load %s system?" system))
                                     (slime-eval `(ql:quickload ,system)))))
          (newline 2)
          (insert doc)
          (newline)
          (insert "--------------------------------------------------------------------------------")
          (newline)))
      (local-set-key "q" 'kill-buffer)
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (goto-char 0)
      (pop-to-buffer buffer))))

(defun quicklisp-apropos--open-buffer-with-printed-results (buffer-name results)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (insert results)
      (local-set-key "q" 'kill-buffer)
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (goto-char 0)
      (pop-to-buffer buffer))))

(defun quicklisp-apropos--query-printed-results (query)
  (let* ((results
          (slime-eval `(cl:with-output-to-string
                        (cl:*standard-output*)
                        (quicklisp-apropos:quicklisp-apropos ,query :count ,quicklisp-apropos-max-results))))
         (buffer-name (format "*quicksearch: %s*" query)))
    (quicklisp-apropos--open-buffer-with-printed-results buffer-name results)))

(defun quicklisp-apropos--query-results (query)
  (let* ((results
          (slime-eval `(quicklisp-apropos:quicklisp-apropos ,query :count ,quicklisp-apropos-max-results :print-results nil)))
         (buffer-name (format "*quicksearch: %s*" query)))
    (quicklisp-apropos--open-buffer-with-results buffer-name
                                                 (mapcar #'car results))))

(defun quicklisp-apropos (query)
  (interactive "sQuicklisp apropos: ")

  (funcall quicklisp-apropos-query-results-function
           query))

(defun quicklisp-apropos-system (query)
  (interactive "s"))

(defun quicklisp-apropos-package (query)
  (interactive "s"))

(defun quicklisp-apropos-variable (query)
  (interactive "s"))

(defun quicklisp-apropos-class (query)
  (interactive "s"))

(defun quicklisp-apropos-function (query)
  (interactive "s"))

(defun quicklisp-apropos-macro (query)
  (interactive "s"))

(defun quicklisp-apropos-generic-function (query)
  (interactive "s"))

(define-slime-contrib quicklisp-apropos
  "SLIME extension for running apropos queries across Quicklisp libraries."
  (:swank-dependencies quicklisp-apropos))

(provide 'quicklisp-apropos)
