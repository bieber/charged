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

(defun point-in-rectangle (v left right top bottom)
  (and (>= (x v) left)
       (<= (x v) right)
       (>= (y v) bottom)
       (<= (y v) top)))

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

(defun add-vectors (v1 v2)
  (vector (+ (x v1) (x v2))
          (+ (y v1) (y v2))))

(defun plus-minus (x)
  (list x (- x)))

(defun combinations (l1 l2)
  (mapcan #'(lambda (e)
              (mapcar #'(lambda (e2)
                          (vector e e2))
                      l2))
          l1))

(defun point-from-vector (v)
  (point :x (x v) :y (y v)))