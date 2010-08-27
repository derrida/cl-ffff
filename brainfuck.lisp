;;;; brainfuck.lisp

(defpackage #:brainfuck
  (:use #:cl))

(in-package #:brainfuck)

(defparameter index 0)
(defparameter value 0)
(defparameter ptr '(0))
(defparameter *code* '())


(defmacro times (index &rest body)
  `(loop for i from 0 to ,index do (,@body)))

(defun bf-read (stream char arg)
  (declare (ignore char arg))
  (format t "~A" (sec (read-delimited-list #\] stream))))

(set-dispatch-macro-character #\# #\[ #'bf-read)
(set-macro-character #\] (get-macro-character #\) ))
(set-macro-character #\. (format t "\."))
(set-macro-character #\, (format t "\,"))

(let ((reader)) 
  (defun main ()
    (format t "~A " "/>>")
    (setq reader (read))
    (brainfuck-to-lisp reader)
    (main)))

(defun brainfuck-to-lisp (c)
  (case c
    (+ (bf+))
    (- (bf-))
    (> (bf>))
    (< (bf<))
    (\. (bf\.))
    (\, (progn
          ;; 2nd ptr as continuation?
          (format t "~A => ~A" ptr ptr)
          ;; <begin/cc>
          (bf\,)
          ;; <end/cc> this changes the value of ptr
          ))
    (ptr (format t "=> ~A~%" ptr))
    (otherwise (error "Not a valid BrainFuck command."))))

(defun bf> ()
  (setf *code* (reverse *code*))
  (push #\> *code*)
  (setf *code* (reverse *code*))
  (setf ptr (reverse ptr))
  (push value ptr)
  (setf ptr (reverse ptr))
  (incf index)
  (nth index ptr))

(defun bf< ()
  (setf *code* (reverse *code*))
  (push #\< *code*)
  (setf *code* (reverse *code*))
  (setf ptr (reverse ptr))
  (pop ptr)
  (setf ptr (reverse ptr))
  (decf index)
  (nth index ptr))

(defun bf+ ()
  (setf *code* (reverse *code*))
  (push #\+ *code*)
  (setf *code* (reverse *code*))
  (incf (nth index ptr)))

(defun bf- ()
  (setf *code* (reverse *code*))
  (push #\- *code*)
  (setf *code* (reverse *code*))
  (decf (nth index ptr)))

(defun bf\. ()
  (setf *code* (reverse *code*))
  (push #\. *code*)
  (setf *code* (reverse *code*))
  (format t "~A~%"
          (mapcar #'code-char (loop for i in ptr collect i))))

(defun bf\, ()
  (format t "~A~%"
          (mapcar #'code-char (loop for i in ptr collect i))))

(defun bf-zero ()
  (setf ptr '(0)))
