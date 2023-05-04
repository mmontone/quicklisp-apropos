(require :drakma)
(require :cl-json)

(defpackage :quicklisp-apropos-client
  (:use :cl))

(in-package :quicklisp-apropos-client)

(defvar *index-url*)

(defun format-query (query)
  (when (stringp query)
    (return-from format-query query))
  (when (listp query)
    ()))

(defun query-api (query)
  (json:decode-json-from-source
   (drakma:http-request *index-url*
			:parameters (list (cons "q" query))
			:want-stream t)))

(defun print-result (result)
  (format t "~a ~a in system ~a~%"
	  (alexandria:assoc-value result :type)
	  (alexandria:assoc-value result :name)
	  (alexandria:assoc-value result :system))
  (when (alexandria:assoc-value result :doc)
    (format t "~%~a~%" (alexandria:assoc-value result :doc))))

(defun print-results (results)
  (format t "~a results:~%~%" (length results))
  (format t "--------------------------------------------------------------------------------~%")
  (dolist (result results)
    (print-result result)
    (format t "--------------------------------------------------------------------------------~%")))
