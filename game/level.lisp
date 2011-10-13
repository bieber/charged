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

;; Represents a level and its elements

(defclass level ()
  ((boundaries
    :documentation "Boundaries within the level."
    :initform nil
    :initarg :boundaries
    :accessor level-boundaries)
   (entities
    :documentation "Particles and other entities in the level's initial state."
    :initform nil
    :initarg :entities
    :accessor level-entities)
   (goals
    :documentation "The level's goals."
    :initform nil
    :initarg :goals
    :accessor level-goals)
   (toolbox
    :documentation "Entities the user is allowed to add to the level."
    :initform nil
    :initarg :toolbox
    :accessor level-toolbox)
   (goal-total
    :documentation "Number of goal points necessary to complete the level."
    :initform 0
    :accessor level-goal-total)
   (goal-points
    :documentation "Number of goal points scored in current state."
    :initform 0
    :accessor level-goal-points))
  (:documentation "Represents a single game level."))

(defun clamp (x low high)
  (min high (max low x)))

;; After initialization, sum goal points.
;; Optional goal-proportion keyword argument specifies proportion of total 
;; points which must be scored to complete the level.
(defmethod initialize-instance :after ((level level) &key (goal-proportion 1.0))
  (setq goal-proportion (clamp goal-proportion 0 1.0))
  (setf (level-goal-total level)
        (floor (* goal-proportion 
                  (reduce #'+ (mapcar #'goal-value (level-goals level)))))))



