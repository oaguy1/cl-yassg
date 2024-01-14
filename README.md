# cl-yassg

Yet Another Static Site Generator, this time in Common Lisp

[![Build Status](https://app.travis-ci.com/oaguy1/cl-yassg.svg?branch=main)](https://app.travis-ci.com/oaguy1/cl-yassg)

This is less of an actual static site generator as it is a tool for parsing markdown files with simple front matter into a tree of variables that can then send off into the templating engine of your choice. While this approach is slightly less "batteries included" it is intended to give the user ultimate freedom to use whatever templates/outputs they like. To see a "canonical" implementation of what using this engine looks like, checkout [this repo](https://github.com/oaguy1/blog).

## Usage

This generator takes arbitrarily nested folders and turns them into variables for the Common Lisp templating software of your choice. Templates are represented are functions that get passed into the engine using the `register-template` function. All top level variables are passed in as string values in named key arguments. Template functions may receive more key arguments than are expected, therefore it is required to include the `&allow-other-keys` flag set in the key arguments to your function. Usage demonstrated below:

```Lisp
;;; using Spinneret HTML DSL

(defpackage #:templates
  (:use #:cl #:spinneret #:cl-yassg))

(in-package #:templates)

(defmacro with-page-string ((&key title type twitter description) &body body)
   `(with-html-string
      (:doctype)
      (:html
        (:head
         (:title (format nil "Lily Hughes-Robinson - ~A" ,title)))
        (:body ,@body))))

(defun post (&key title description type twitter body-html date &allow-other-keys)
  (with-page-string (:title title :type type :twitter twitter :description description)
    (:header
     (:hgroup
      (:h2 title)
      (:h4 description)
      (:h5 date))
     (:section
      (:raw body-html)))))

(register-template "post" #'post)
```

Variables for nested pages (e.g. accessing  variables for all blog posts from the home page) are also sent in as keys names after the directory containing the nested pages. Unlike normal variables, nested variables are passed in as assoc lists. Here is an example template handling assoc lists:

```Lisp
(defun home-page (&key title description type twitter posts body-html &allow-other-keys)
  (let ((sorted-posts (sort posts #'local-time:timestamp> :key #'(lambda (x) (local-time:parse-timestring (cdr (assoc "date" (cdr x) :test #'equal)))))))
    (with-page-string (:title title :type type :twitter twitter :description description)
      (:h1 title)
      (:raw body-html)
      (:section
       (:h2 "Posts")
       (dolist (post sorted-posts)
	 (let ((post-vars (cdr post)))
	   (:article
	    (:a
	     :href (cdr (assoc "link" post-vars :test #'equal))
	     (:strong (cdr (assoc "title" post-vars :test #'equal))))
	    " • "
	    (cdr (assoc "date" post-vars :test #'equal))
	    (:br)
	    (:em (cdr (assoc "description" post-vars :test #'equal))))))))))
```

Next, one wants to consider files to be ignored. By default, no files are excluded and the `templates` and `.git` directories are excluded. One can exclude more files by invoking `exclude-file` or `exclude-dir` in their build scripts.

Finally, one can invoke the build pipeline using the `make-site` function. `make-site` takes in input directory and output directory. The input directory needs to contain assets and markdown files. The output directory doesn't need to exist, but **will be overwritten if it does exist**. Currently, files you remove from the input directory are not automatically removed from the output directory, they must be manually removed.

Bringing this all together, here is a sample build script that builds a basic blog on macOS using QuickLisp with both input and target directories in source control and ignoring build scripts.

```lisp
;;; build.lisp

;; Load Dependencies
(ql:quickload "spinneret")
(ql:quickload "local-time")
(ql:quickload "cl-yassg")

;; Load Templates
(load "templates/templates.lisp")

(defpackage #:blog
  (:use #:cl #:cl-yassg #:templates))

(in-package #:blog)

(exclude-file "README.md")
(exclude-file ".gitignore")
(exclude-file ".DS_Store")
(exclude-file "build.lisp")
(exclude-file "build.sh")

(register-template "home-page" #'templates::home-page)
(register-template "page" #'templates::page)
(register-template "post" #'templates::post)

(make-site "." "../blog-output/")
```

This can be invoked in SBCL using the following command, which could be make into a script for convenience.

```sh
sbcl --load build.lisp --quit
```

## Sample site tree

Below is a sample site tree, minus scripts for generating the site

```
blog
├── assets
│   └── default.css
├── index.md
├── posts
│   ├── 2024-01-07-starting-fresh.md
│   ├── 2024-01-11-learning-common-lisp-in-2024.md
│   └── ...
└── templates
    └── templates.lisp
```

In this site, there is an `assets` directory whose content gets copied to the new site. There is a `posts` directory containing markdown that holds the blog posts. There is a `template` directory which contains all the Lisp code for generating templates specific to your site. Finally, all markdown files in the root directory also make into pages on the site.

## Roadmap

This project is in a state where I am able to generate my own personal blog, which is roughly what I wanted to achieve. That said, the source code is small and there is room for expansion. Here are some of the things I plan on adding.

* Handling drafts
* Proper testing
* Adding a RSS feed generator

## Contributing

The list above is not exhaustive and there is much more that can be done with this project. If you want to add or improve upon what I have built, please do so! Feel free to file an issue on GitHub or, better yet, fork the code and make a pull request. I will do my best to review pull requests in good time, but please understand this is a side project and not my full time employment.
