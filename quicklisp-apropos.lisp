(defpackage :quicklisp-apropos
  (:use :cl)
  (:export
   :*index-path*
   :quicklisp-apropos
   :quicklisp-apropos-system
   :quicklisp-apropos-package
   :quicklisp-apropos-class
   :quicklisp-apropos-variable
   :quicklisp-apropos-function
   :quicklisp-apropos-generic-function
   :quicklisp-apropos-macro))

(in-package :quicklisp-apropos)

(defvar *index-path* #p"~/src/quicklisp-docs-index")
(defvar *index* nil)

(defun ensure-index ()
  (when (null *index*)
    (setf *index* (make-instance 'montezuma:index :path *index-path*))))

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

(defun quicklisp-apropos (query)
  (ensure-index)
  (print-results (query-index query)))

(defun quicklisp-apropos-system (query)
  (ensure-index)
  (print-results (query-index (format nil "system:'~a'" query))))

(defun quicklisp-apropos-package (query)
  (ensure-index)
  (print-results (query-index (format nil "package:'~a'" query))))

(defun quicklisp-apropos-variable (query)
  (ensure-index)
  (print-results (query-index (format nil "type:'variable' AND (name:'~a' OR doc:'~a')" query query))))

(defun quicklisp-apropos-class (query)
  )

(defun quicklisp-apropos-function (query)
  )

(defun quicklisp-apropos-macro (query)
  )

(defun quicklisp-apropos-generic-function (query)
  )

(in-package :quicklisp-apropos)
