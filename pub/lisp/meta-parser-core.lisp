;;;; parser-core.lisp

(in-package #:parser)

(defstruct (meta
             (:print-function
               (lambda (m s d &aux (char (meta-char m)) (form (meta-form m)))
                 (ecase char
                   ((#\@ #\! #\$) (format s "~A~A" char form))
                   (#\[ (format s "[~{~A~^ ~}]" form))
                   (#\{ (format s "{~{~A~^ ~}}" form))))))
  char
  form)

(defun meta-reader (s c) (make-meta :char c :form (read s)))

(mapc #'(lambda (c) (set-macro-character c #'meta-reader)) '(#\@ #\$ #\!))

(set-macro-character #\[
  #'(lambda (s c) (make-meta :char c :form (read-delimited-list #\] s t))))

(set-macro-character #\{
  #'(lambda (s c) (make-meta :char c :form (read-delimited-list #\} s t))))

(mapc #'(lambda (c) (set-macro-character c (get-macro-character #\) nil)))
      '(#\] #\}))

(defun compileit (x)
  (typecase x
    (meta
      (ecase (meta-char x)
        (#\! (meta-form x))
        (#\[ `(and ,@(mapcar #'compileit (meta-form x))))
        (#\{ `(or ,@(mapcar #'compileit (meta-form x))))
        (#\$ `(not (do () ((not ,(compileit (meta-form x)))))))
        (#\@ (let ((f (meta-form x))) `(match-type ,(car f) ,(cadr f))))))
    (t `(match ,x))))

(defmacro matchit (x) (compileit x))

;;; Stream
(defmacro match-stream (x) `(when (eql (peek-char) ',x)
                       (read-char)))

(defmacro match-type-stream (x v) `(when (typep (peek-char) ',x)
                              (setq ,v (read-char))))

;;; String
(defmacro match (x)
  (etypecase x
    (character
      `(when (and (< index end) (eql (char str index) ',x))
         (incf index)))
    (string
      `(let ((old-index index)) ; 'old-index is a lexical variable.
         (or (and ,@(map 'list #'(lambda (c) `(match ,c)) x))
             (progn (setq index old-index) nil))))))

(defmacro match-type (x v)
  `(when (and (< index end) (typep (char str index) ',x))
     (setq ,v (char str index)) (incf index)))

