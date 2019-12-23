(defpackage   :lisp8-asd
  (:use       :cl
              :asdf))
(in-package #:lisp8-asd)

(asdf:defsystem :lisp8
  :version      "0.1.0"
  :description  "A CHIP-8 emulator written in Common Lisp"
  :author       "Alex Cameron <ascottcameron@gmail.com>"
  :serial       t
  :license      "GNU GPL, version 3"
  :components   ((:file "lisp8")))
