(in-package #:cl-yassg)

(defvar *excluded-dirs* '("assets" "templates"))

(defvar *templates* '())

(defun register-template (key func)
  (setf *templates* (acons key func *templates*)))

(defclass page-node ()
  ((path
    :initarg :path
    :accessor node-path)
   (is-file-p
    :initarg :is-file
    :accessor node-is-file-p)
   (children
    :initform '()
    :accessor node-children)
   (variables
    :initform '()
    :accessor node-variables)))

(defmethod node-path-name ((node page-node))
  (first (last (pathname-directory (node-path node)))))

(defmethod parse-node-variables ((node page-node))
  (unless (node-is-file-p node)
    (return-from parse-node-variables '()))

  (let ((parsing-metadata nil)
	(parsed-metadata nil)
	(body "")
	(body-html "")
	(vars '()))
    (with-open-file (stream (node-path node))
      (do ((line (read-line stream nil) (read-line stream nil)))
    	((null line))
        (cond ((and (not parsing-metadata) (not parsed-metadata) (string= line "---"))
	       (setf parsing-metadata t))
	      ((and parsing-metadata (string= line "---"))
	       (setf parsing-metadata nil)
	       (setf parsed-metadata t))
	      (parsing-metadata
	       (multiple-value-bind (key value) (parse-metadata-line line)
		 (setf vars (acons key value vars))))
	      (t (setf body (concatenate 'string body line))))))
    (setf vars (acons "body" body vars))
    (setf body-html (with-output-to-string (stream)
		      (cl-markdown:markdown body :stream stream)))
    (setf vars (acons "body-html" body-html vars))
    vars))


(defmethod parse-tree-variables ((node page-node))
  (let ((vars (parse-node-variables node))
	(leaf-vars '()))
    (dolist (child (node-children node))
      (if (node-is-file-p child)
	  (setf leaf-vars (append leaf-vars (parse-tree-variables child)))
	  (let ((key ""))
	    (setf key (first (last (pathname-directory (node-path child)))))
	    (setf vars (acons key (parse-tree-variables child) vars)))))
    (unless (null leaf-vars)
      (let ((key ""))
	(if (node-is-file-p node)
	    (setf key (file-namestring (node-path node)))
	    (setf key (first (last (pathname-directory (node-path node))))))
	(setf vars (acons key leaf-vars vars))))
    (setf (node-variables node) (remove-if #'null vars))))

(defmethod node-to-files ((node page-node) dst-dir)
  (let ((curr-dir (if (node-is-file-p node)
		      (ensure-trailing-slash dst-dir)
		      (ensure-trailing-slash (merge-pathnames (node-path-name node) dst-dir)))))

    (ensure-directories-exist curr-dir)

    (dolist (child (node-children node))
      (node-to-files child curr-dir))

    (if (node-is-file-p node)
	(let* ((filename (file-namestring (node-path node)))
	       (new-filename (concatenate 'string (subseq filename 0 (- (length filename) 3)) ".html"))
	       (template-name (cdr (assoc "template" (node-variables node) :test #'equal))))
	  (with-open-file (stream (merge-pathnames new-filename curr-dir)
	  		 :direction :output
	  		 :if-exists :supersede
	  		 :if-does-not-exist :create)
	    (format stream "~A" (apply-template template-name (node-variables node))))))))

(defun parse-metadata-line (line)
  (let ((data (split ":" line)))
    (values (car data) (string-trim '(#\Space #\Tab #\Newline) (cadr data)))))

(defun ensure-trailing-slash (dir)
  (let ((str (format nil "~A" dir)))
    (if (not (eql (char str (- (length str) 1)) #\/))
	(concatenate 'string str "/")
	dir)))

(defun make-site-tree (path-name &key (include-drafts nil))
  "Find all the markdown files in a given directory, recursively"
  (let ((node (make-instance 'page-node :path path-name :is-file nil))
	(files (uiop:directory-files path-name))
	(dirs (uiop:subdirectories path-name)))
    (dolist (dir dirs)
      (let ((dirname (first (last (pathname-directory dir)))))
	(unless (or (member dirname *excluded-dirs* :test #'equal)
		    (and (string= "drafts" dirname) (not include-drafts)))
	  (setf (node-children node) (append (node-children node) (list (make-site-tree dir)))))))
    (dolist (file files)
      (if (string= (pathname-type file) "md")
	  (setf (node-children node)
		(append (node-children node) (list (make-instance 'page-node :path file :is-file t))))))
    node))

(defun make-site (input-dir output-dir)
  (let ((root-node (make-site-tree (ensure-trailing-slash input-dir))))
    (parse-tree-variables root-node)
    (node-to-files root-node output-dir)))

(defun apply-key-args (function assoc-list)
  (let ((apply-list (loop for (key . value) in assoc-list
			  collect (read-from-string (concatenate 'string ":" key))
			  collect value)))
    (apply function apply-list)))

(defun apply-template (template vars)
  (apply-key-args (cdr (assoc template *templates* :test #'equal)) vars))
