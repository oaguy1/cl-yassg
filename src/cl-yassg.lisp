(in-package #:cl-yassg)

(defun find-markdown-files (pathname)
  "Find all the markdown files in a given directory, recursively"
  (let ((filelist '())
	(files (uiop:directory-files pathname))
	(dirs (uiop:subdirectories pathname)))
    (dolist (dir dirs)
      (setf filelist (append filelist (find-markdown-files dir))))
    (dolist (file files)
      (if (string= (pathname-type file) "md")
	  (push file filelist)))
    filelist))


