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

;; This is the base entity class, from which all game entities will derive.
;; It allows for the expected basic functionality: movement, drawing, collision
;; detection, and etc.

(defclass entity ()
  ((position
    :documentation "Location on the canvas"
    :initarg :position
    :initform #(0 0)
    :accessor entity-position)
   (velocity
     :documentation "Velocity in #(x y) format, pixels/second."
     :initarg :velocity
     :initform #(0 0)
     :accessor entity-velocity)
   (mass
    :documentation "Mass in arbitrary units."
    :initarg :mass
    :initform 1
    :accessor entity-mass))
  (:documentation "The base entity class for Charged."))

; Generic functions

(defgeneric int-position (entity)
  (:documentation "Returns integer coordinates for an entity."))

(defgeneric draw (entity &optional surface)
  (:documentation "Draws an entity at its position."))

(defgeneric move (entity time)
  (:documentation "Moves an entity according to its velocity."))

(defgeneric collisionp (entity-1 entity-2)
  (:documentation "Checks for a collision between two entities."))

(defgeneric collide (entity-1 entity-2)
  (:documentation "Carries out a collision between two entities."))

; Method implementations for base class

(defmethod int-position ((entity entity))
  (point :x (x (entity-position entity)) :y (y (entity-position entity))))
          
(defmethod move ((entity entity) time)
  (let* ((x-i (x (entity-position entity)))
         (y-i (y (entity-position entity)))
         (v-x (x (entity-velocity entity)))
         (v-y (y (entity-velocity entity)))
         (d-x (* v-x time))
         (d-y (* v-y time)))
    
    ; Return movement vector, in case it's ever needed
    (setf (entity-position entity) (vector (+ x-i d-x) (+ y-i d-y)))
    (vector d-x d-y)))

; Basic perfectly elastic collision between two entities
(defmethod collide ((entity-1 entity) (entity-2 entity))
  (let* (; Distance between center points
         (dx (- (x (entity-position entity-2))
                (x (entity-position entity-1))))
         (dy (- (y (entity-position entity-2))
                (y (entity-position entity-1))))
         ; Direction and magnitude of entity-1's velocity
         (theta-1 (atan (y (entity-velocity entity-1))
                        (x (entity-velocity entity-1))))
         (v-1 (sqrt (+ (expt (x (entity-velocity entity-1)) 2)
                       (expt (y (entity-velocity entity-1)) 2))))
         ; Direction and magnitude of entity-2's velocity
         (theta-2 (atan (y (entity-velocity entity-2))
                        (x (entity-velocity entity-2))))
         (v-2 (sqrt (+ (expt (x (entity-velocity entity-2)) 2)
                       (expt (y (entity-velocity entity-2)) 2))))
         ; Angle of collision (AOC)
         (angle (atan dy dx))
         (perp (+ angle (/ pi 2)))
         ; Velocity components parellel (c) and perpendicular to (n) AOC
         (vc-1 (* v-1
                  (cos (- theta-1 angle))))
         (vn-1 (* v-1
                  (sin (- theta-1 angle))))
         (vc-2 (* v-2
                  (cos (- theta-2 angle))))
         (vn-2 (* v-2
                  (sin (- theta-2 angle))))
         ; New velocities parallel to AOC
         (vc-1-n (/ (+ (* vc-1
                          (- (entity-mass entity-1)
                             (entity-mass entity-2)))
                       (* 2
                          (entity-mass entity-2)
                          vc-2))
                    (+ (entity-mass entity-1)
                       (entity-mass entity-2))))
         (vc-2-n (/ (+ (* vc-2
                          (- (entity-mass entity-2)
                             (entity-mass entity-1)))
                       (* 2
                          (entity-mass entity-1)
                          vc-1))
                    (+ (entity-mass entity-1)
                       (entity-mass entity-2))))
         ; New magnitudes and directions of velocity
         (theta-1-n (+ angle 
                       (atan vn-1 vc-1-n)))
         (v-1-n (sqrt (+ (expt vc-1-n 2)
                         (expt vn-1 2))))
         (theta-2-n (+ angle
                       (atan vn-2 vc-2-n)))
         (v-2-n (sqrt (+ (expt vc-2-n 2)
                         (expt vn-2 2)))))

    ; Setting the new velocities
    (setf (entity-velocity entity-1)
          (vector (* v-1-n (cos theta-1-n))
                  (* v-1-n (sin theta-1-n))))
    (setf (entity-velocity entity-2)
          (vector (* v-2-n (cos theta-2-n))
                  (* v-2-n (sin theta-2-n))))))

; Makes sure two entities are clear of each other after a collision
(defun space-out-entities (entity-1 entity-2)
  (loop while (collisionp entity-1 entity-2) do
       (move entity-1 0.01)
       (move entity-2 0.01)))


(defmethod collide :after ((entity-1 entity) (entity-2 entity))
  (space-out-entities entity-1 entity-2))


