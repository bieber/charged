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

;; This represents circular entities, with basic collision detection and
;; elastic collision support

(defclass circle (entity)
  ((radius
    :documentation "Radius in pixels."
    :initarg :radius
    :initform 10
    :accessor circle-radius))
  (:documentation "Base class for circular entities."))

; Method implementations

(defmethod draw ((entity circle) &optional (surface *default-surface*))
  (draw-filled-circle (int-position entity) (circle-radius entity)
                      :color *green* :surface surface))

(defmethod collisionp ((entity-1 circle) (entity-2 circle))
  (if (eq entity-1 entity-2)
      nil
      (within-range (entity-position entity-1) (entity-position entity-2)
                    (+ (circle-radius entity-1) (circle-radius entity-2)))))
