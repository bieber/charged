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

;; This file houses generic functions specialized on instances of more than
;; one class.

(defun circle-box-collision (circle box)
  (let* (; Translating so box is centered at origin
         (circle-center-pre (sub-vectors (entity-position circle)
                                         (entity-position box)))
         ; Rotating so the box is normal to the axes
         (circle-center (rotate-vector-cw circle-center-pre (box-angle box)))
         ; Finding distance and angle between centers
         (dx (- (x circle-center)))
         (dy (- (y circle-center)))
         (theta (atan dy dx))
         (distance (sqrt (+ (* dx dx) (* dy dy))))
         ; Finding the corners of the box
         (corners (combinations (plus-minus (/ (x (box-size box)) 2))
                                (plus-minus (/ (y (box-size box)) 2)))))
    (or (< distance 
           (+ (circle-radius circle)
              (rectangle-radius (box-size box) theta)))
        (reduce #'(lambda (x y) (or x y))
                (mapcar #'(lambda (v)
                            (point-in-circle (x circle-center)
                                             (y circle-center)
                                             (circle-radius circle)
                                             (x v)
                                             (y v)))
                        corners)))))
        
        

(defmethod collisionp ((box box) (circle circle))
  (circle-box-collision circle box))

(defmethod collisionp ((circle circle) (box box))
  (circle-box-collision circle box))
