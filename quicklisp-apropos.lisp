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
   :quicklisp-apropos-macro
   :quicklisp-apropos-name
   :quicklisp-apropos-doc))

(in-package :quicklisp-apropos)

(defvar *index-path* #p"~/src/quicklisp-docs-index")
(defvar *index* nil)
(defvar *results-count* 50)

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

(defun quicklisp-apropos (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index query :count count)))

(defun quicklisp-apropos-system (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "system:'~a'" query) :count count)))

(defun quicklisp-apropos-package (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "package:'~a'" query) :count count)))

(defun quicklisp-apropos-name (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "name:'~a'" query) :count count)))

(defun quicklisp-apropos-doc (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "doc:'~a'" query) :count count)))

(defun quicklisp-apropos-variable (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "type:'variable' AND (name:'~a' OR doc:'~a')" query query) :count count)))

(defun quicklisp-apropos-class (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "type:'class' AND (name:'~a' OR doc:'~a')" query query) :count count)))

(defun quicklisp-apropos-function (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "type:'function' AND (name:'~a' OR doc:'~a')" query query) :count count)))

(defun quicklisp-apropos-macro (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "type:'macro' AND (name:'~a' OR doc:'~a')" query query) :count count)))

(defun quicklisp-apropos-generic-function (query &key (count *results-count*))
  (ensure-index)
  (print-results (query-index (format nil "type:'generic-function' AND (name:'~a' OR doc:'~a')" query query) :count count)))

(in-package :quicklisp-apropos)
