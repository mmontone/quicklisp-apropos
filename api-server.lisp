(require :hunchentoot)
(require :montezuma)
(require :string-case)
(require :cl-json)

(defpackage :quicklisp-docs-index-api-server
  (:use :cl))

(in-package :quicklisp-docs-index-api-server)

(defparameter *index* (make-instance 'montezuma:index :path "/home/marian/src/quicklisp-docs-index"))

(defun parse-document (doc)
  (flet ((docvalue (field)
	   (montezuma:document-value doc field)))
    (string-case:string-case ((docvalue "type"))
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

(defun query-index (query)
  (let (found)
    (montezuma:search-each *index* query
			   #'(lambda (doc score)
                               (push (cons (parse-document (montezuma:get-document *index* doc)) score) found)))
    (nreverse found)))

(hunchentoot:define-easy-handler (query-handler :uri "/")
    (q)
  (setf (hunchentoot:header-out "content-type") "application/json")
  (json:encode-json-to-string
   (mapcar #'car (query-index q))))

(defvar *acceptor*)

(defun start (&rest args)
  (setf *acceptor*
	(hunchentoot:start (apply #'make-instance 'hunchentoot:easy-acceptor args))))
