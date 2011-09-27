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

;; This is the boundary class.  It represents a rectangular boundary in the 
;; game environment which never moves.  Objects colliding with it will simply 
;; reflect off one of its surfaces

(defclass boundary (box)
  () ; No new slots, just different behavior
  (:documentation "A static, rectangular boundary"))

; Method definitions for the boundary

(defmethod move ((obj boundary) time)
  ; Disable movement for boundaries
  nil)

; Entities that collide with a boundary will reflect off of it
(defun boundary-collision (boundary entity)
  (let* (; Determining the angle between the entity's center and the boundary
         ; This will only work flawlessly with circles
         (dx (- (x (entity-position boundary)) (x (entity-position entity))))
         (dy (- (y (entity-position boundary)) (y (entity-position entity))))
         (d (sqrt (+ (* dx dx) (* dy dy))))
         ; Get theta relative to box angle for convenience
         (theta (- (normal-angle (atan dy dx)) (box-angle boundary)))
         ; Determining the diagonal angle of the box
         (alpha (normal-angle (atan (y (box-size boundary))
                                    (x (box-size boundary)))))
         (original-velocity (entity-velocity entity)))
    ; Check to see if collision occured with top/bottom or left/right
    (if (or (and (>= theta alpha)
                 (<= theta (- pi alpha)))
            (and (>= theta (+ pi alpha))
                 (<= theta (- (* 2 pi) alpha))))
        ;For top/bottom, reflect about angle of box
        (setf (entity-velocity entity) 
              (vector-reflection original-velocity (box-angle boundary)))
        ;For left/right, reflect about angle + pi/2
        (setf (entity-velocity entity)
              (vector-reflection
               original-velocity (normal-angle (+ (/ pi 2) 
                                                  (box-angle boundary))))))))

(defmethod collide ((entity entity) (boundary boundary))
  (boundary-collision boundary entity))

(defmethod collide ((boundary boundary) (entity entity))
  (boundary-collision boundary entity))

; This allows boundaries to overlap without entering an infinite loop
(defmethod collisionp ((b1 boundary) (b2 boundary))
  nil)