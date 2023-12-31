(in-package #:cl-yassg)

(defvar *excluded-dirs* '("assets" "templates"))

(defclass page-node ()
  ((path
   :initarg :path
   :accessor node-path)
  (is-file-p
   :initarg :is-file
   :accessor node-is-file-p)
  (children
   :initform '()
   :accessor node-children)))

(defmethod node-to-list ((node page-node))
  (let ((lst '()))
    (dolist (child (node-children node))
      (push (node-path child) lst)
      (if (node-children child)
	  (setf lst (append lst (node-to-list child)))))
    lst))


(defun make-site-tree (path-name &key (include-drafts nil))
  "Find all the markdown files in a given directory, recursively"
  (let ((node (make-instance 'page-node :path path-name :is-file nil))
	(files (uiop:directory-files path-name))
	(dirs (uiop:subdirectories path-name)))
    (dolist (dir dirs)
      (let ((dirname (first (last (pathname-directory dir)))))
	(unless (or (member dirname *excluded-dirs* :test #'equal)
		    (and (string= "drafts" dirname) (not include-drafts)))
	  (setf (node-children node) (append (node-children node) (list (find-markdown-files dir)))))))
    (dolist (file files)
      (if (string= (pathname-type file) "md")
	  (setf (node-children node)
		(append (node-children node) (list (make-instance 'page-node :path file :is-file t))))))
    node))
