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
     :documentation "Velocity in #(x y) format, pixels/second"
     :initarg :velocity
     :initform #(0 0)
     :accessor entity-velocity)
   (radius
    :documentation "Radius in pixels.  Entities are circular by default"
    :initarg :radius
    :initform 10
    :accessor entity-radius))
  (:documentation "The base entity class for Charged"))

; Generic functions

(defgeneric int-position (entity)
  (:documentation "Returns integer coordinates for an entity"))

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
          

(defmethod draw ((entity entity) &optional (surface *default-surface*))
  (draw-filled-circle (int-position entity) (entity-radius entity)
                      :color *green* :surface surface)
  (let ((x (x (int-position entity)))
        (y (y (int-position entity)))
        (vx (x (entity-velocity entity)))
        (vy (y (entity-velocity entity))))
        
    (draw-line (int-position entity) (point :x (+ x vx) :y (+ y vy))
               :color *red* :surface surface)))

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

(defmethod collisionp ((entity-1 entity) (entity-2 entity))
  (if (eq entity-1 entity-2)
      nil
      (within-range (entity-position entity-1) (entity-position entity-2)
                    (+ (entity-radius entity-1) (entity-radius entity-2)))))

(defmethod collide ((entity-1 entity) (entity-2 entity))
  (clear-display *black*)
  (draw-circle (int-position entity-1) (entity-radius entity-1) :color *green*)
  (draw-circle (int-position entity-2) (entity-radius entity-2) :color *green*)
  (draw-line (int-position entity-1)
             (point :x (+ (x (int-position entity-1)) 
                          (x (entity-velocity entity-1)))
                    :y (+ (y (int-position entity-1))
                          (y (entity-velocity entity-1)))))
  (draw-line (int-position entity-2)
             (point :x (+ (x (int-position entity-2))
                          (x (entity-velocity entity-2)))
                    :y (+ (y (int-position entity-2))
                          (y (entity-velocity entity-2)))))
  ;Line between centers
  (draw-line (int-position entity-1) (int-position entity-2) :color *yellow*)
  
  ; Finding the new axes
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
         (distance (sqrt (+ (expt dx 2) (expt dy 2))))
         (midpoint (point :x (+ (x (entity-position entity-1))
                                (* (/ distance 2) (cos angle)))
                          :y (+ (y (entity-position entity-1))
                                (* (/ distance 2) (sin angle)))))
         (p1 (point :x (+ (x midpoint)
                          (* (/ distance 2) (cos perp)))
                    :y (+ (y midpoint)
                          (* (/ distance 2) (sin perp)))))
         (p2 (point :x (- (x midpoint)
                          (* (/ distance 2) (cos perp)))
                    :y (- (y midpoint)
                          (* (/ distance 2) (sin perp))))))
    (draw-circle midpoint 10 :color *yellow*)
    (draw-line p1 p2 :color *yellow*)
    (draw-line (int-position entity-1)
               (point :x (+ (x (int-position entity-1))
                            (* vc-1 (cos angle)))
                      :y (+ (y (int-position entity-1))
                            (* vc-1 (sin angle))))
               :color *red*)
    (draw-line (int-position entity-1)
               (point :x (+ (x (int-position entity-1))
                            (* vn-1 (cos perp)))
                      :y (+ (y (int-position entity-1))
                            (* vn-1 (sin perp))))
               :color *red*)
    (draw-line (int-position entity-2)
               (point :x (+ (x (int-position entity-2))
                            (* vc-2 (cos angle)))
                      :y (+ (y (int-position entity-2))
                            (* vc-2 (sin angle))))
               :color *red*)
    (draw-line (int-position entity-2)
               (point :x (+ (x (int-position entity-2))
                            (* vn-2 (cos perp)))
                      :y (+ (y (int-position entity-2))
                            (* vn-2 (sin perp))))
               :color *red*)
))

    
    
        
