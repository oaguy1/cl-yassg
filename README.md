cl-yassg
========
Yet Another Static Site Generator, this time in Common Lisp

[![Build Status]
    (https://travis-ci.org/oaguy1/cl-yassg.svg)]
    (https://travis-ci.org/oaguy1/cl-yassg)]

This is less of an actual static site generator as it is a tool for parsing markdown files with YAML metadata into a tree of variables that can then send off into the templating engine of your choice. While this approach is slightly less "batteries included" it is intended to give the user ultimate freedom to use whatever templates/outputs they like. To see a "canonical" implementation of what using this engine looks like, checkout [this repo](https://github.com/oaguy1/blog).
