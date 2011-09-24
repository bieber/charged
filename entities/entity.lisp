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
    :initform (point :x 0 :y 0)
    :accessor entity-position)
   (radius
    :documentation "Radius in pixels.  Entities are circular by default"
    :initarg radius
    :initform 10
    :accessor radius))
  (:documentation "The base entity class for Charged"))

(defgeneric draw (entity &optional surface)
  (:documentation "Draws an entity at its position"))

(defmethod draw ((entity entity) &optional (surface *default-surface*))
  (draw-filled-circle (entity-position entity) (radius entity)
                      :color *green*))