;; define one local variable
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; split list into head and tail (and define head and tail variables)
(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                 (let ((head (car ,g))
                       (tail (cdr ,g)))
                   ,yes)
                 ,no))))

;; (a b c d) --> ((a . b) (c . d))
(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                     (if tail
                       (f (cdr tail) (cons (cons head (car tail)) acc))
                       (reverse acc))
                     (reverse acc))))
           (f lst nil)))

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
          alst)
        (princ #\>))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg" 
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
        ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 9 (+ x amt))))
          col))

(defun svg-transform-rotate (transform)
  (format nil
	  "~{rotate(~a,~a,~a)~}"
	  transform))

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

(defun circle (center radius color transform)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color)
		  transform (svg-transform-rotate transform))))

(defun rect (left-top width height color transform)
  (tag rect (x (car left-top)
	       y (cdr left-top)
	       width width
	       height height
	       style (svg-style color)
	       transform (svg-transform-rotate transform))))

(defun polygon (points color transform)
  (tag polygon (points (format nil "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color)
		       transform (svg-transform-rotate transform))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                         (1- value)
                         (1+ value))
                       (1- length)))))
; Example:
;(with-open-file (*standard-output* "random_walk.svg"
;                                   :direction :output
;                                   :if-exists :supersede)
;  (svg (loop repeat 10
;             do (polygon (append '((0 . 200))
;                                 (loop for x from 0
;                                       for y in (random-walk 100 400)
;                                       collect (cons x y))
;                                 '((400 . 200)))
;                         (loop repeat 3
;                               collect (random 256))))))

