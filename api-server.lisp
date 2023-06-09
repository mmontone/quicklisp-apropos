(require :hunchentoot)
(require :montezuma)
(require :string-case)
(require :cl-json)

(defpackage :quicklisp-apropos-server
  (:use :cl))

(in-package :quicklisp-apropos-server)

(defparameter *index* (make-instance 'montezuma:index
				     :path "/home/marian/src/quicklisp-docs-index"
				     :create-if-missing-p nil))

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

(defun query-index (query)
  (let (found)
    (montezuma:search-each *index* query
			   #'(lambda (doc score)
                               (push (cons (parse-document (montezuma:get-document *index* doc)) score) found))
			   '(:num-docs 50))
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
