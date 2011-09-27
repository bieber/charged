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
         ; Finding distance and angle between centers and corner
         (dx (abs (x circle-center)))
         (dy (abs (y circle-center)))
         (distance (sqrt (+ (* dx dx) (* dy dy))))
         (corner-distance-sqr (+ (expt (- dx (/ (x (box-size box)) 2)) 2)
                                 (expt (- dy (/ (y (box-size box)) 2)) 2))))

    ; Disqualifying if circle is >r away from an edge
    (cond ((> dx (+ (/ (x (box-size box)) 2)
                   (circle-radius circle)))
           nil)
          ((> dy (+ (/ (y (box-size box)) 2)
                   (circle-radius circle)))
           nil)
          ; Returning true if circle center is within r of an edge
          ((<= dx (/ (x (box-size box)) 2))
           t)
          ((<= dy (/ (y (box-size box)) 2))
           t)
          ; All that's left is to check the corners
          (t (<= corner-distance-sqr (expt (circle-radius circle) 2))))))
                   


(defmethod collisionp ((box box) (circle circle))
  (circle-box-collision circle box))

(defmethod collisionp ((circle circle) (box box))
  (circle-box-collision circle box))
