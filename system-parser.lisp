(require :docparser)
(require :montezuma)

(defpackage :system-parser
  (:use :cl)
  (:export :index-system))

(in-package :system-parser)

(defparameter *index* (make-instance 'montezuma:index :path "/home/marian/src/quicklisp-docs-index"))

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

(defun index-system (system)
  (let ((index (docparser:parse system)))
    (docparser:do-packages (package index)
      (let ((doc (make-document package system)))
	(montezuma:add-document-to-index *index* doc))
      (docparser:do-nodes (node package)
	(when (docparser:symbol-external-p (docparser:node-name node))
	  (let ((doc (make-document node system)))
	    (when doc
	      (format t ".")
	      (montezuma:add-document-to-index *index* doc))))))))
