(load "svg")

;;***
;; A picture of the win patterns for the blog.
(defparameter *unit* 20)
(defparameter *width* 7)

;; draw board at desired place
(defun board (pos)
  (let ((color '(127 193 127))
	 (center-color '(10 10 10))
	 (cell (+ *unit* (* pos *unit* (1+ *width*)))))
    (loop
      for i below *width*
      for x = (+ cell (* i *unit*))
      do (loop 
	   for j below *width*
	   for y = (* j *unit*)
	   do (rect (cons x y) *unit* *unit* color '())))
    (circle (cons 
	      (+ cell (ash (* *width* *unit*) -1))
	      (ash (* *width* *unit*) -1)) 
	    (- (ash *unit* -1) 3)
	    center-color '())))

;; arrow
(defun arrow (pos color transform)
  (let* 
    ((arrow-width (- *unit* 14))
     (cell (+ *unit* (* pos *unit* (1+ *width*))))
     (x (+ cell (ash (* *width* *unit*) -1)))
     (y (ash (* *unit* *width*) -1)))
    (polygon
      (list
	(cons (- x (- *unit* arrow-width)) (- y (ash arrow-width -1)))
	(cons (- x *unit*) (+ y (ash arrow-width -1)))
	(cons (- x *unit* *unit*) (+ y (ash arrow-width -1)))
	(cons (- x *unit* *unit*) (+ y arrow-width))
	(cons (- x *unit* *unit* (truncate *unit* 2)) y)
	(cons (- x *unit* *unit*) (- y arrow-width))
	(cons (- x *unit* *unit*) (- y (ash arrow-width -1))))
      color
      (if (eq 0 transform)
	'()
	(list transform x y)))))

;; left arrow
(defun left-arrow (pos color)
  (arrow pos color 0))

;; left arrow
(defun right-arrow (pos color)
  (arrow pos color 180))

;; up arrow
(defun up-arrow (pos color)
  (arrow pos color 90))

;; up arrow
(defun down-arrow (pos color)
  (arrow pos color -90))

;; left-up arrow
(defun left-up-arrow (pos color)
  (arrow pos color 45))

;; right-down arrow
(defun right-down-arrow (pos color)
  (arrow pos color -135))

;; right-up arrow
(defun right-up-arrow (pos color)
  (arrow pos color 135))

;; left-down arrow
(defun left-down-arrow (pos color)
  (arrow pos color -45))

;; go
(defparameter *arrow-colors*
  (list '(40 55 167) '(40 167 55) '(167 40 55) '(100 80 20)))
(defun get-color (pos)
  (nth pos *arrow-colors*))

(with-open-file (*standard-output* "../../imgs/lisp/win-patterns.svg"
				   :direction :output
				   :if-exists :supersede)
  (svg
    (progn
      (board 0)
      (left-arrow 0 (get-color 0))
      (right-arrow 0 (get-color 0))
      (board 1)
      (up-arrow 1 (get-color 1))
      (down-arrow 1 (get-color 1))
      (board 2)
      (left-up-arrow 2 (get-color 2))
      (right-down-arrow 2 (get-color 2))
      (board 3)
      (right-up-arrow 3 (get-color 3))
      (left-down-arrow 3 (get-color 3))
      )))

