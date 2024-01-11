(in-package #:cl-yassg)

;; globals
(defvar *excluded-dirs* '("templates" ".git"))
(defvar *excluded-files* '())
(defvar *templates* '())

(defun register-template (key func)
  "Register a template function to a given key"
  (setf *templates* (acons key func *templates*)))

(defun exclude-file (filename)
  "Exclude files of a given name"
  (push filename *excluded-files*))

(defclass page-node ()
  ((path
    :initarg :path
    :initform nil
    :accessor node-path)
   (children
    :initform nil
    :accessor node-children)
   (variables
    :initform nil
    :accessor node-variables)))

(defclass dir-node (page-node) ())

(defclass file-node (page-node) ())

(defclass static-file-node (page-node) ())

(defmethod node-path-name ((node dir-node))
  "give the directory name of the current node"
  (first (last (pathname-directory (node-path node)))))

(defmethod parse-node-variables ((node page-node) curr-path)
  "simple nodes don't have variables, return empty list"
  '())

(defmethod parse-node-variables ((node file-node) curr-path)
  "parse a source file into variables, parse markdown into html and store as variable"
  (let ((parsing-metadata nil)
	(parsed-metadata nil)
	(body "")
	(body-html "")
	(vars '()))
    (with-open-file (stream (node-path node))
      (do ((line (read-line stream nil) (read-line stream nil)))
    	((null line))
        (cond ((and (not parsing-metadata) (not parsed-metadata) (string= line "---")) ;; first line
	       (setf parsing-metadata t))
	      ((and parsing-metadata (string= line "---"))                             ;; second '---'
	       (setf parsing-metadata nil)
	       (setf parsed-metadata t))
	      (parsing-metadata                                                        ;; read metadata
	       (multiple-value-bind (key value) (parse-metadata-line line)
		 (setf vars (acons key value vars))))
	      (t (setf body (concatenate 'string body (format nil "~A~%" line)))))))   ;; read markdown
    (setf vars (acons "body" body vars))

    (setf body-html (with-output-to-string (stream)                                    ;; convert to html
		      (let ((3bmd-code-blocks:*code-blocks* t)
			    (3bmd-code-blocks:*renderer* :pygments))
			(3bmd:parse-string-and-print-to-stream body stream))))
    (setf vars (acons "body-html" body-html vars))

    (let* ((filename (file-namestring (node-path node)))
	   (new-filename (concatenate 'string (subseq filename 0 (- (length filename) 3)) ".html"))
	   (link (merge-pathnames new-filename curr-path)))
      (setf vars (acons "link" link vars)))
    vars))


(defmethod parse-tree-variables ((node page-node) curr-path)
  "parse all the variables for a node and its children"
  (let ((node-vars (parse-node-variables node curr-path)) ;; current node variables
	(file-child-vars '())                            ;; file children variables
	(dir-child-vars '()))                            ;; dir children variables

    ;; iterate through children, appending variables to the approproate let form
    (dolist (child (node-children node))
      (cond ((typep child 'file-node)
	     (let ((key (file-namestring (node-path child))))
	       (setf file-child-vars (acons key (parse-tree-variables child curr-path) file-child-vars))))
	    ((typep child 'dir-node)
	     (let* ((key (node-path-name child))
		    (new-path (ensure-trailing-slash (merge-pathnames key curr-path)))
		    (child-vars (parse-tree-variables child new-path)))
	       (setf node-vars (acons key child-vars node-vars)
		     dir-child-vars (acons key child-vars dir-child-vars))))
	    (t nil)))  ;; ignore static-files

    ;; if there are file children variables, attach them to current node
    (when file-child-vars
      (setf node-vars (append file-child-vars node-vars)))

    ;; if there are dir children variables, attach them to file siblings (allows for aggregate pages)
    (when dir-child-vars
      (dolist (child (node-children node))
	(when (typep child 'file-node)
	  (setf (node-variables child) (append (node-variables child) dir-child-vars)))))

    ;; set this nodes variables
    (setf (node-variables node) (remove nil node-vars))))


(defmethod tree-to-files ((node page-node) dst-dir)
  "convert node and its children into files using the registered templates"
  (let ((curr-dir (if (or (typep node 'file-node) (typep node 'static-file-node))
		      (ensure-trailing-slash dst-dir)
		      (ensure-trailing-slash (merge-pathnames (node-path-name node) dst-dir)))))

    ;; ensure current directory exists
    (ensure-directories-exist curr-dir)

    ;; convert all children
    (dolist (child (node-children node))
      (tree-to-files child curr-dir))

    ;; if file-node, pass its variables into the appropriate template and write the result to a file
    (when (typep node 'file-node)
      (let* ((filename (file-namestring (node-path node)))
	       (new-filename (concatenate 'string (subseq filename 0 (- (length filename) 3)) ".html"))
	       (template-name (cdr (assoc "template" (node-variables node) :test #'equal))))
	  (when (null template-name)
	    (error "Unkown template. Dumping vars ~A" (node-variables node)))
	  (with-open-file (stream (merge-pathnames new-filename curr-dir)
	  		 :direction :output
	  		 :if-exists :supersede
	  		 :if-does-not-exist :create)
	    (format stream "~A" (apply-template template-name (node-variables node))))))

    ;; if static-file, copy it to the new location
    (when (typep node 'static-file-node)
      (let ((filename (file-namestring (node-path node))))
	  (uiop:copy-file (node-path node) (merge-pathnames filename curr-dir))))))

(defun parse-metadata-line (line)
  "Turn a colon seperated string into a key value pair"
  (let ((data (str:split ":" line)))
    (values (car data) (string-trim '(#\Space #\Tab #\Newline) (cadr data)))))

(defun ensure-trailing-slash (dir)
  "Ensure directory has trailing slash, required for most directory operations"
  (let ((str (format nil "~A" dir)))
    (if (not (eql (char str (- (length str) 1)) #\/))
	(concatenate 'string str "/")
	dir)))

(defun make-site-tree (path-name &key (include-drafts nil))
  "Find all the markdown files in a given directory, recursively"
  (let ((node (make-instance 'dir-node :path path-name))
	(files (uiop:directory-files path-name))
	(dirs (uiop:subdirectories path-name)))
    (dolist (dir dirs)
      (let ((dirname (first (last (pathname-directory dir)))))
	(unless (or (member dirname *excluded-dirs* :test #'equal)
		    (and (string= "drafts" dirname) (not include-drafts)))
	  (setf (node-children node) (append (node-children node) (list (make-site-tree dir)))))))
    (dolist (file files)
      (when (not (member (file-namestring file) *excluded-files* :test #'equal))
	(if (string= (pathname-type file) "md")
	    (setf (node-children node)
		  (append (node-children node) (list (make-instance 'file-node :path file))))
	    (setf (node-children node)
		  (append (node-children node) (list (make-instance 'static-file-node :path file)))))))
    node))

(defun make-site (input-dir output-dir)
  "Read and parse all the files in input directory, apply appropriate templates, output to output-dir"
  (let ((root-node (make-site-tree (ensure-trailing-slash input-dir))))
    (parse-tree-variables root-node "/")
    (tree-to-files root-node output-dir)))

(defun apply-key-args (function assoc-list)
  "Apply an assoc list as key arguments to a given function"
  (let ((apply-list (loop for (key . value) in assoc-list
			  collect (read-from-string (concatenate 'string ":" key))
			  collect value)))
    (apply function apply-list)))

(defun apply-template (template vars)
  "Apply variables to the templates with a given key"
  (apply-key-args (cdr (assoc template *templates* :test #'equal)) vars))
