;; Copyright (C) 2021 Mariano Montone

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

(require :dexador)
(require :quicklisp)
(require :chipz)
(require :archive)

(defpackage :quicklisp-apropos
  (:use :cl)
  (:shadow #:apropos)
  (:export
   #:*index-path*
   #:apropos
   #:apropos-system
   #:apropos-package
   #:apropos-class
   #:apropos-variable
   #:apropos-function
   #:apropos-generic-function
   #:apropos-macro
   #:apropos-name
   #:apropos-doc
   #:download-index))

(in-package :quicklisp-apropos)

(defparameter *quicklisp-apropos-directory* (uiop/pathname:pathname-directory-pathname *load-pathname*))
(defparameter *index-path* (merge-pathnames "quicklisp-apropos-index/" *quicklisp-apropos-directory*))
(defparameter *index* nil)
(defvar *results-count* 50)
(defvar *index-file-url* "https://github.com/mmontone/quicklisp-apropos/releases/latest/download/quicklisp-apropos-index.tar.gz")
(defvar *index-file-name* "quicklisp-apropos-index.tar.gz")

(defun ensure-index ()
  (when (null *index*)
    (when (not (probe-file *index-path*))
      (download-index))
    (setf *index* (make-instance 'montezuma:index :path *index-path*
                                                  :create-if-missing-p nil))))

(defun format-query (query)
  (when (stringp query)
    (return-from format-query query))
  (when (listp query)
    ()))

(defun parse-document (doc)
  (flet ((docvalue (field)
           (let ((val (montezuma:document-value doc field)))
             ;; NILs in Montezuma are stored as a string "NIL"
             (when (not (string= val "NIL"))
               val))))
    (string-case:string-case ((docvalue "type"))
      ("system"
       (list (cons "type" "system")
             (cons "name" (docvalue "name"))
             (cons "doc" (docvalue "doc"))))
      ("package"
       (list (cons "type" "package")
             (cons "name" (docvalue "name"))
             (cons "doc" (docvalue "doc"))
             (cons "system" (docvalue "system"))))
      (t
       (list (cons "type" (docvalue "type"))
             (cons "name" (docvalue "name"))
             (cons "doc" (docvalue "doc"))
             (cons "package" (docvalue "package"))
             (cons "system" (docvalue "system")))))))

(defun query-index (query &key (count *results-count*))
  (let (found)
    (montezuma:search-each *index* query
                           #'(lambda (doc score)
                               (push (cons (parse-document (montezuma:get-document *index* doc)) score) found))
                           `(:num-docs ,count))
    (nreverse found)))

(defun print-result (result)
  (format t "~a ~a in system ~a~%"
          (alexandria:assoc-value result "type" :test #'string=)
          (alexandria:assoc-value result "name" :test #'string=)
          (alexandria:assoc-value result "system" :test #'string=))
  (when (alexandria:assoc-value result "doc" :test #'string=)
    (format t "~%~a~%" (alexandria:assoc-value result "doc" :test #'string=))))

(defun print-results (results)
  (format t "~a results:~%~%" (length results))
  (format t "--------------------------------------------------------------------------------~%")
  (dolist (result results)
    (print-result (car result))
    (format t "--------------------------------------------------------------------------------~%")))

(defun maybe-print-results (results print-p)
  (if print-p
      (print-results results)
      results))

;;------ Apropos api ------------------------------------------------------------

(defun apropos (query &key (count *results-count*)
                        (print-results t))
  (ensure-index)
  (when (not (find #\: query))
    (setq query (format nil "name:'~a', doc:'~a'" query query)))
  (maybe-print-results (query-index query :count count) print-results))

(defun apropos-system (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:system, name: '~a', doc:'~a'"
                                            query query)
                                    :count count)
                       print-results))

(defun apropos-package (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:package, name:'~a', doc:'~a'" query query) :count count)
                       print-results))

(defun apropos-name (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+name:'~a'" query) :count count) print-results))

(defun apropos-doc (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+doc:'~a'" query) :count count) print-results))

(defun apropos-variable (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:variable, name:'~a', doc:'~a'" query query) :count count) print-results))

(defun apropos-class (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:class, name:'~a',doc:'~a'" query query) :count count) print-results))

(defun apropos-function (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:function, name:'~a', doc:'~a'" query query) :count count) print-results))

(defun apropos-macro (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:'macro', name:'~a', doc:'~a'" query query) :count count) print-results))

(defun apropos-generic-function (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:'generic-function', name:'~a', doc:'~a'" query query) :count count) print-results))

;;-------- Index update -------------------------------------------------------

(defun extract-tarball (pathname)
  "Extract a tarball (.tar.gz) file to a directory (*default-pathname-defaults*)."
  (with-open-file (tarball-stream pathname
                                  :direction :input
                                  :element-type '(unsigned-byte 8))
    (archive::extract-files-from-archive
     (archive:open-archive 'archive:tar-archive
                           (chipz:make-decompressing-stream 'chipz:gzip tarball-stream)
                           :direction :input))))

(defun download-index (&optional (index-file-url *index-file-url*))
  (format t "Downloading quicklisp-apropos index from ~a ... ~%" index-file-url)
  (ql-util:with-temporary-file (quicklisp-apropos-index.tar.gz (pathname-name *index-file-name*))
    (dex:fetch index-file-url quicklisp-apropos-index.tar.gz)
    (format t "Extracting index ...~%")
    (let ((*default-pathname-defaults* *quicklisp-apropos-directory*))
      (extract-tarball quicklisp-apropos-index.tar.gz))
    (format t "Index created in ~a~%" *quicklisp-apropos-directory*)))

(provide :quicklisp-apropos)
