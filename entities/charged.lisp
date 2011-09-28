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

;; A base class for charged entities.

(defclass charged (entity)
  ((charge 
    :documentation "Charge, positive or negative"
    :initarg :charge
    :initform 0
    :accessor charge))
  (:documentation "Base class for entities with electric charge"))

;; Generic function definitions

(defgeneric electric-force (destination source)
  (:documentation "Applies electric force between two charged objects"))

;; Method definitions

(defmethod electric-force ((destination entity) (source entity))

    (if (not (eq source destination))
        (let* (; Distances and angles
               (dx (- (x (entity-position destination))
                      (x (entity-position source))))
               (dy (- (y (entity-position destination)) 
                      (y (entity-position source))))
               (d (sqrt (+ (expt dx 2) (expt dy 2))))
               (theta (atan dy dx))
               ; Force and change in acceleration
               (force (/ (* (charge source) (charge destination)) (expt d 2)))
               (da-magnitude (/ force (entity-mass destination)))
               (da (vector (* da-magnitude (cos theta))
                           (* da-magnitude (sin theta)))))
          (setf (entity-velocity destination)
                (add-vectors (entity-velocity destination) da)))))

