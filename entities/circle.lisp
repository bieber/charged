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

(defmethod collide ((entity-1 circle) (entity-2 circle))
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
