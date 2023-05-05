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

(defun quicklisp-apropos (query &key (count *results-count*)
				  (print-results t))
  (ensure-index)
  (when (not (find #\: query))
    (setq query (format nil "name:'~a', doc:'~a'" query query)))
  (maybe-print-results (query-index query :count count) print-results))

(defun quicklisp-apropos-system (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:system, name: '~a', doc:'~a'"
				      query query)
				    :count count)
		       print-results))

(defun quicklisp-apropos-package (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:package, name:'~a', doc:'~a'" query query) :count count)
		       print-results))

(defun quicklisp-apropos-name (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+name:'~a'" query) :count count) print-results))

(defun quicklisp-apropos-doc (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+doc:'~a'" query) :count count) print-results))

(defun quicklisp-apropos-variable (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:variable, name:'~a', doc:'~a'" query query) :count count) print-results))

(defun quicklisp-apropos-class (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:class, name:'~a',doc:'~a'" query query) :count count) print-results))

(defun quicklisp-apropos-function (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:function, name:'~a', doc:'~a'" query query) :count count) print-results))

(defun quicklisp-apropos-macro (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:'macro', name:'~a', doc:'~a'" query query) :count count) print-results))

(defun quicklisp-apropos-generic-function (query &key (count *results-count*) (print-results t))
  (ensure-index)
  (maybe-print-results (query-index (format nil "+type:'generic-function', name:'~a', doc:'~a'" query query) :count count) print-results))

(defvar *index-file-url* "https://github.com/mmontone/quicklisp-apropos/releases/latest/download/quicklisp-apropos-index.tar.gz")

(defun download-index (&optional (index-file-url *index-file-url*))
   (trivial-download:download index-file-url))

(defun extract-tarball (pathname)
  "Extract a tarball (.tar.gz) file to a directory (*default-pathname-defaults*)."
  (with-open-file (tarball-stream pathname
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (archive::extract-files-from-archive
     (archive:open-archive 'archive:tar-archive
      (chipz:make-decompressing-stream 'chipz:gzip tarball-stream)
      :direction :input))))

(in-package :quicklisp-apropos)
