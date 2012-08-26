;;;; pegs.lisp: Yields solutions to the infamous Cracker Barrel peg game.
;;;; Copyright (c) 2008 Corey Abshire

(defpackage :pegs (:use :common-lisp))

(in-package :pegs)

(defconstant +height+ 5)

(defconstant +size+
  (loop for i from 1 upto +height+
	sum i))

(defconstant +indices+
  (loop for i from 0 below +size+
	collect i))

(defconstant +all-holes+
  (mapcan (lambda (r) (loop for c upto r
			    collect (format nil "~a:~a" r c)))
	  (loop for r below +height+
		collect r)))

(defun index (r c)
  (+ c (loop for i from 0 upto r
	   sum i)))

(defconstant +all-moves+
  (mapcan (lambda (a)
	    (mapcar (lambda (b) (list (index (car a) (cdr a))
				      (index (car b) (cdr b))
				      (index (/ (+ (car a) (car b)) 2)
					     (/ (+ (cdr a) (cdr b)) 2))))
		    (remove-if-not (lambda (b) (<= 0 (cdr b) (car b) (- +height+ 1)))
				   (mapcar (lambda (o) (cons (+ (car a) (car o))
							     (+ (cdr a) (cdr o))))
					   '((2 . 0) (0 . 2) (-2 . 0) (0 . -2) (2 . 2) (-2 . -2))))))
	  (mapcan (lambda (r) (loop for c upto r
				    collect (cons r c)))
		  (loop for r below +height+
			collect r))))

(defun print-solutions (board &optional path)
  (cond ((solved-p board) (print-solution (reverse path)))
	(t (loop for move in (possible-moves board)
		 do (print-solutions (apply-move move board)
				     (cons move path))))))

(defun print-solution (solution)
  (format t "~{~a~^, ~}~%"
	  (mapcar (lambda (move)
		    (format nil "~a->~a"
			    (nth (nth 0 move) +all-holes+)
			    (nth (nth 1 move) +all-holes+)))
		  solution)))

(defun initial (&optional (empty 0))
  (loop for i upto (- +size+ 1)
	collect (not (= empty i))))

(defun solved-p (board)
  (= (count t board) 1))

(defun possible-moves (board)
  (remove-if-not (lambda (move) (valid-move-p move board))
		 +all-moves+))

(defun valid-move-p (move board)
  (and (nth (nth 0 move) board)
       (not (nth (nth 1 move) board))
       (nth (nth 2 move) board)))

(defun apply-move (move board)
  (mapcar (lambda (i p) (if (member i move) (not p) p))
	  +indices+ board))

