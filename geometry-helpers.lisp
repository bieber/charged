;    Copyright 2011, Robert Bieber   
; 
;    This file is part of Charged.
;
;    Charged is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Charged is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with Charged.  If not, see <http://www.gnu.org/licenses/>.
;

(in-package :charged)

;; Helper functions for geometry

; Check for points in shapes
(defun point-in-rectangle (v left right top bottom)
  (and (>= (x v) left)
       (<= (x v) right)
       (>= (y v) bottom)
       (<= (y v) top)))

(defun point-in-circle (circle-x circle-y radius point-x point-y)
  (>= radius (sqrt (+ (expt (- circle-x point-x) 2)
                      (expt (- circle-y point-y) 2)))))

; Vector rotation
(defun rotate-vector-cw (v theta)
  (vector (+ (* (x v)
                (cos theta))
             (* (y v)
                (sin theta)))
          (- (* (y v)
                (cos theta))
             (* (x v)
                (sin theta)))))

(defun rotate-vector-ccw (v theta)
  (vector (- (* (x v)
                (cos theta))
             (* (y v)
                (sin theta)))
          (+ (* (x v)
                (sin theta))
             (* (y v)
                (cos theta)))))

; Some vector arithmetic
(defun add-vectors (v1 v2)
  (vector (+ (x v1) (x v2))
          (+ (y v1) (y v2))))

(defun sub-vectors (v1 v2)
  (vector (- (x v1) (x v2))
          (- (y v1) (y v2))))

(defun dot-product (v1 v2)
  (+ (* (x v1) (x v2))
     (* (y v1) (y v2))))

(defun vector-multiply (v n)
  (vector (* n (x v)) (* n (y v))))

(defun magnitude (v)
  (sqrt (+ (expt (x v) 2) (expt (y v) 2))))

; Projects v onto the axis in the direction of l
(defun vector-projection (v l)
  (let ((u (vector-multiply l (/ (magnitude l)))))
    (vector-multiply u (dot-product v u))))

; Reflects the vector v about the given angle
(defun vector-reflection (v angle)
  (let ((n (vector (cos angle) (sin angle)))) ; N is our unit vector
    (sub-vectors (vector-multiply (vector-projection v n) 2) v)))

; Makes a list of +/- x
(defun plus-minus (x)
  (list x (- x)))

; Takes two lists, returns vectors with 1st element from l1, 2nd from l2
(defun combinations (l1 l2)
  (mapcan #'(lambda (e)
              (mapcar #'(lambda (e2)
                          (vector e e2))
                      l2))
          l1))

(defun point-from-vector (v)
  (point :x (x v) :y (y v)))

; Translates a possibly negative angle into the [0..2pi] range
(defun normal-angle (theta)
  (cond ((> theta (* 2 pi)) (normal-angle (- theta (* 2 pi))))
        ((< theta 0) (normal-angle (+ theta (* 2 pi))))
        (t theta)))
