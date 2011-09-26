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

; Vector addition and subtraction
(defun add-vectors (v1 v2)
  (vector (+ (x v1) (x v2))
          (+ (y v1) (y v2))))

(defun sub-vectors (v1 v2)
  (vector (- (x v1) (x v2))
          (- (y v1) (y v2))))

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

; Finds the radius of a rectangle at a given angle
(defun rectangle-radius (size theta)
  (let* (; Finding angle to top-right corner
         (alpha (atan (y size) (x size)))
         ; Getting effective theta in convenient comparison range
         (e-theta (cond ((>= theta (- (* 2 pi) alpha)) (- theta (* 2 pi)))
                        ((>= theta (- pi alpha)) (- theta pi))
                        ((<= theta (- (+ pi alpha))) (+ theta (* 2 pi)))
                        ((<= theta (- alpha)) (+ theta pi))
                        (t theta)))
         (half-width (/ (x size) 2))
         (half-height (/ (y size) 2)))
    (if (<= e-theta alpha)
        (sqrt (+ (expt half-width 2)
                 (expt (* half-width (tan e-theta)) 2)))
        (sqrt (+ (expt half-height 2)
                 (expt (* half-height
                          (tan (- e-theta (/ pi 2))))
                       2))))))

                         