(require :docparser)
(require :montezuma)

(defpackage :system-parser
  (:use :cl)
  (:export :index-system))

(in-package :system-parser)

(defparameter *index* (make-instance 'montezuma:index :path "quicklisp-docs-index"))

(defun make-document (node system)
  (typecase node
    (docparser:package-index
     (list (cons "type" "package")
	   (cons "name" (docparser:package-index-name node))
	   (cons "doc" (docparser:package-index-docstring node))
	   (cons "system" (princ-to-string system))))
    (docparser:class-node
     (list (cons "type" "class")
	   (cons "name" (prin1-to-string (docparser:node-name node)))
	   (cons "doc" (docparser:node-docstring node))
	   (cons "package" (package-name (symbol-package (docparser:node-name node))))
	   (cons "system" (princ-to-string system))))
    (docparser:generic-function-node
     (list (cons "type" "generic-function")
	   (cons "name" (prin1-to-string (docparser:node-name node)))
	   (cons "doc" (docparser:node-docstring node))
	   (cons "package" (package-name (symbol-package (docparser:node-name node))))
	   (cons "system" (princ-to-string system))))
    (docparser:function-node
     (list (cons "type" "function")
	   (cons "name" (prin1-to-string (docparser:node-name node)))
	   (cons "doc" (docparser:node-docstring node))
	   (cons "package" (package-name (symbol-package (docparser:node-name node))))
	   (cons "system" (princ-to-string system))))
    (docparser:macro-node
     (list (cons "type" "macro")
	   (cons "name" (prin1-to-string (docparser:node-name node)))
	   (cons "doc" (docparser:node-docstring node))
	   (cons "package" (package-name (symbol-package (docparser:node-name node))))
	   (cons "system" (princ-to-string system))))
    (docparser:variable-node
     (list (cons "type" "variable")
	   (cons "name" (prin1-to-string (docparser:node-name node)))
	   (cons "doc" (docparser:node-docstring node))
	   (cons "package" (package-name (symbol-package (docparser:node-name node))))
	   (cons "system" (princ-to-string system))))))

(defun read-system-doc (system-designator)
  (let ((readme-files (concatenate 'list
				   (uiop/filesystem:directory-files 
				    (asdf:system-source-directory system-designator)
				    "README*")
				   (uiop/filesystem:directory-files 
				    (asdf:system-source-directory system-designator)
				    "README.*")))
	(system (asdf:find-system system-designator)))
    ;; Concatenate all documentation we found
    (with-output-to-string (s)
      (when (asdf:system-description system)
	(write-string (asdf:system-description system) s)
	(terpri s))
      (when (asdf:system-long-description system)
	(write-string (asdf:system-long-description system) s)
	(terpri s))
      (dolist (readme-file readme-files)
	(write-string (alexandria:read-file-into-string readme-file) s)
	(terpri s)))))

(defun make-system-document (system)
  (list (cons "type" "system")
	(cons "name" (princ-to-string system))
	(cons "doc" (read-system-doc system))))

(defun index-system (system)
  (let ((index (docparser:parse system)))
    (montezuma:add-document-to-index *index* (make-system-document system))
    (docparser:do-packages (package index)
      (let ((doc (make-document package system)))
	(montezuma:add-document-to-index *index* doc))
      (docparser:do-nodes (node package)
	(when (docparser:symbol-external-p (docparser:node-name node))
	  (let ((doc (make-document node system)))
	    (when doc
	      (format t ".")
	      (montezuma:add-document-to-index *index* doc))))))))
