;;;; brainfuck.asd

(asdf:defsystem #:brainfuck
  :depends-on (#:alexandria #:cl-ppcre)
  :components ((:file "brainfuck")))