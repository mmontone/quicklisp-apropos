;; - Clone quicklisp-projects and quicklisp-controller repositories.
;; - Setup quicklisp-controller: (quicklisp-controller:setup-directories "~/src/lisp/quicklisp-projects/")
;; - Update the list of Quicklisp systems using QUICKLISP-CONTROLLER::UPDATE-WHAT-YOU-CAN.

(defpackage #:quicklisp-docs-index
  (:use #:cl))

(in-package #:quicklisp-docs-index)

(defparameter *quicklisp-controller-directory* #p"~/quicklisp-controller/")

(defun find-files-do (path pattern function &optional (include-subdirectories t))
  "Find files in PATH using PATTERN. Invokes FUNCTION on found files.
If INCLUDE-SUBDIRECTORIES is T, then work recursively."
  (dolist (file (uiop/filesystem:directory-files path pattern))
    (funcall function file))
  (when include-subdirectories
    (dolist (subdir (uiop/filesystem:subdirectories path))
      (find-files-do subdir pattern function include-subdirectories))))

(defun index-quicklisp-systems (&key start-after-system)
  (let ((start (not start-after-system)))
    (find-files-do
     (merge-pathnames #p"upstream-cache/" *quicklisp-controller-directory*)
     "*.asd"
     (lambda (file)
       (let ((system-name (pathname-name file)))
	 (print system-name)
	 (when (and (not start) start-after-system)
	   (when (string= start-after-system system-name)
	     (setq start t)))
	 (when start
	   (with-simple-restart (skip "Skip to next system")
	     (uiop:run-program
	      (format nil "sbcl --load 'system-parser.lisp' --eval '(system-parser:index-system \"~a\")' --quit" system-name)
	      :output t :error-output t))))))))
