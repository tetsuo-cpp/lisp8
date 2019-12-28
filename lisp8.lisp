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
;; There are 16 registers on a CHIP-8 machine.
(defconstant +register-num+ 16)
;; Ops are two bytes in length.
(defconstant +op-size+ 2)

(defclass rom ()
  ((memory
    :initform
    (make-array +max-memory-addr+
                :element-type '(unsigned-byte 8)
                :initial-element #x0)
    :accessor memory)
   (addr
    :initform +rom-addr+
    :accessor addr)
   (callstack
    :initform
    (make-array 0 :fill-pointer 0 :adjustable t)
    :accessor callstack)))

(defun load-fonts (rom)
  (dotimes (i (length +font-set+))
    (setf (aref (memory rom) i) (aref +font-set+ i))))

(defun load-rom (filename rom)
  (with-open-file (file filename
                        :direction :input
                        :element-type '(unsigned-byte 8))
    (read-sequence (memory rom) file :start (addr rom))))

(defun read-op (rom)
  (when (< (addr rom) +max-memory-addr+)
    (let* ((first-byte (aref (memory rom) (addr rom)))
           (second-byte (aref (memory rom) (1+ (addr rom))))
           (op (logior (ash first-byte 8) second-byte)))
      (incf (addr rom) +op-size+)
      op)))

(defclass cpu ()
  ((ins
    :initform #x0
    :accessor ins)
   (registers
    :initform
    (make-array +register-num+
                :element-type 'unsigned-byte
                :initial-element #x0)
    :accessor registers)))

(defun run-rom (filename)
  (let ((rom (make-instance 'rom))
        (cpu (make-instance 'cpu)))
    (load-fonts rom)
    (load-rom filename rom)
    (let ((op))
      (loop while (setf op (read-op rom)) do
           (format t "op is: ~a~%" op)))))
