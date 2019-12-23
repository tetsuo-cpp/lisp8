'(in-package #:cl-user)
(defpackage   :lisp8
  (:use       :cl
              :sdl2)
  (:export    :run-rom))
(in-package #:lisp8)

;; Rom data is loaded 512 bytes into memory.
(defconstant +rom-addr+ 512)
;; Max bytes of memory allowed.
(defconstant +max-memory-addr+ 4096)
;; Font data.
(defconstant +font-set+
  #(
    #xF0 #x90 #x90 #x90 #xF0 ;; 0
    #x20 #x60 #x20 #x20 #x70 ;; 1
    #xF0 #x10 #xF0 #x80 #xF0 ;; 2
    #xF0 #x10 #xF0 #x10 #xF0 ;; 3
    #x90 #x90 #xF0 #x10 #x10 ;; 4
    #xF0 #x80 #xF0 #x10 #xF0 ;; 5
    #xF0 #x80 #xF0 #x90 #xF0 ;; 6
    #xF0 #x10 #x20 #x40 #x40 ;; 7
    #xF0 #x90 #xF0 #x90 #xF0 ;; 8
    #xF0 #x90 #xF0 #x10 #xF0 ;; 9
    #xF0 #x90 #xF0 #x90 #x90 ;; A
    #xE0 #x90 #xE0 #x90 #xE0 ;; B
    #xF0 #x80 #x80 #x80 #xF0 ;; C
    #xE0 #x90 #x90 #x90 #xE0 ;; D
    #xF0 #x80 #xF0 #x80 #xF0 ;; E
    #xF0 #x80 #xF0 #x80 #x80 ;; F
    ))

(defclass rom ()
  ((memory
    :initform
    (make-array +max-memory-addr+
                :element-type 'unsigned-byte
                :initial-element #x0))
   (addr
    :initform +rom-addr+)
   (callstack
    :initform
    (make-array 0 :fill-pointer 0 :adjustable t))))

(defun load-fonts (rom)
  (dotimes (i (length +font-set+))
    (setf (aref (slot-value rom 'memory) i) (aref +font-set+ i))))

(defun load-rom (filename rom)
  (with-open-file (file filename :direction :input)
    (read-sequence (slot-value rom 'memory) file :start (slot-value rom 'addr))))

(defun run-rom (filename)
  (let ((rom (make-instance 'rom)))
    (load-fonts rom)
    (load-rom filename rom)
    (format t "text was: ~a~%" (slot-value rom 'memory))))
