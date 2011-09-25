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
    :initform 10
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