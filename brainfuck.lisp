;;;; brainfuck.lisp
;;;; .polyvalent.org
;;;; michael simpson
;;;;
;;;; bsd-license. have fun!

(defpackage #:brainfuck
  (:use #:cl))

(in-package #:brainfuck)

(defparameter index 0)
(defparameter ptr '(0))
(defparameter *code* "")

(defun set-reader-macro ()
    (set-macro-character #\] (get-macro-character #\) ))
  (defun bf-read (stream char arg)
    (declare (ignore char arg))
    (read-delimited-list #\] stream t))
  (set-dispatch-macro-character #\# #\[ #'bf-read))

(let ((reader)) 
  (set-reader-macro)
  (defun main ()
    (format t "~A " "/>>")
    (setq reader (read))
    (brainfuck-reader reader)
    (main)))

(defun brainfuck-reader (input)
  (cond
    ((stringp input) (bf-read-string input))
    ((typep input 'symbol) (bf-read-char input)))
  (setf index 0))

;(let ((string (loop for char across string collect char)))
(defun bf-read-string (string)
  (loop for c on `,(coerce string 'list) do (bf-read-char (car c))))

(defun bf-read-char (char)
  (case char
    (#\+ (funcall #'bf+))
    (#\- (funcall #'bf-))
    (#\> (bf>))
    (#\< (bf<))
    (#\. (funcall #'bf\.))
    (#\, (funcall #'bf\,))
    (ptr (format t "=> ~A~%" ptr))
    (code (format t "=> ~A~%" *code*))
    (index (format t "=> ~A~%" index))
    (zero (bf-zero))
    (clear (bf-zero))
    (quit (error "You quit deliberately."))
    (otherwise (error "Not a valid BrainFuck command."))))

(defun record-to-*code* (c)
  (setf *code* (concatenate 'string *code* (string c))))

(defun add-cell ()
  (setf ptr (reverse ptr))
  (push 0 ptr)
  (incf index)
  (setf ptr (reverse ptr)))

(defun pop-cell ()
  (setf ptr (reverse ptr))
  (pop ptr)
  (setf ptr (reverse ptr)))

(defun bf> ()
  (add-cell)
  (record-to-*code* #\>)
  (nth index ptr))

(defun bf< ()
  (record-to-*code* #\<)
  (pop-cell)
  (decf index)
  (nth index ptr))

(defun bf+ ()
  (record-to-*code* #\+)
  (setf (elt ptr index) (incf (elt ptr index))))

(defun bf- ()
  (record-to-*code* #\-)
  (decf (nth index ptr)))

(defun bf\. ()
  (record-to-*code* #\.)
  (format t "=> ~A~%"
          (mapcar #'code-char (loop for i in ptr collect i))))

(defun bf\, ()
  (record-to-*code* #\,)
  (format t "~A =>" ptr)
  (format t "~A~%"
          (mapcar #'code-char (loop for i in ptr collect i))))

(defun bf-zero ()
  (setf ptr '(0))
  (setf index 0)
  (setf *code* "")
  (format t "=> Fresh symbols have now been initialized. Have some brainfuck.~%"))