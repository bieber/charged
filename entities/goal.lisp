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

;; A condition triggered whenever a goal is intersected by another entity
(define-condition goal-reached ()
  ((value :initarg :value :reader goal-scored)))

;; The actual goal class
(defclass goal (circle)
  ((value 
    :documentation "Number of points scored by hitting the goal."
    :initarg :value
    :initform 10
    :reader goal-value))
  (:documentation "A goal, triggers a goal-reached event when intersected"))

;; Method definitions
(defmethod draw ((goal goal) &optional (surface *default-surface*))
  (let ((p1 (point-from-vector (add-vectors (entity-position goal)
                                            (vector 0 (circle-radius goal)))))
        (p2 (point-from-vector (sub-vectors (entity-position goal)
                                            (vector 0 (circle-radius goal))))))
    (draw-circle (int-position goal) (circle-radius goal) :color *green*
                 :surface surface)
    (draw-line p1 p2 :color *green* :surface surface)))
  
  

(defmethod collide ((entity entity) (goal goal))
  (setf (circle-radius goal) 0)
  (signal 'goal-reached :value (goal-value goal)))

(defmethod collide ((goal goal) (entity entity))
  (signal 'goal-reached :value (goal-value goal)))

