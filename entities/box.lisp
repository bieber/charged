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

;; This is a simple box class.  Boxes are specified by their position (center) 
;; width, height, and angle.  Box collisions can't affect their rotation, only
;; their velocity

(defclass box (entity) 
  ((size
    :documentation "Width and height of rectangle as a 2-vector."
    :initarg :size
    :initform #(0 0)
    :accessor box-size)
   (angle
    :documentation "Angle of the box in radians."
    :initarg :angle
    :initform 0
    :accessor box-angle))
  (:documentation "Base class for box-shaped entities."))

; Method implementations

(defmethod draw ((box box) &optional (surface *default-surface*))
  (let ((points (mapcar #'(lambda (v)
                            (add-vectors (rotate-vector-ccw v (box-angle box))
                                         (entity-position box)))
                        (combinations (plus-minus (/ (x (box-size box)) 2))
                                      (plus-minus (/ (y (box-size box)) 2))))))
    (destructuring-bind (p1 p2 p3 p4) (mapcar #'point-from-vector points)
      (draw-filled-polygon (list p1 p2 p4 p3)
                           :color *green* :surface surface))))
                    
(defun one-way-rect-collision (box-1 box-2)
  (let* (; Finding coordinates after translating to center box-1 at origin
         (center-1 #(0 0))
         (center-2-pre (vector (- (x (entity-position box-2))
                                  (x (entity-position box-1)))
                               (- (y (entity-position box-2))
                                  (y (entity-position box-1)))))
         ; Finding the angles and coordinates after rotating by -angle of box-2
         (angle-1 (- (box-angle box-1)
                     (box-angle box-2)))
         (angle-2 0)
         (center-2 (rotate-vector-cw center-2-pre (box-angle box-2)))
         ; Bounds of boxes
         (left (- (x center-2)
                  (/ (x (box-size box-2)) 2)))
         (right (+ (x center-2)
                   (/ (x (box-size box-2)) 2)))
         (top (+ (y center-2)
                 (/ (y (box-size box-2)) 2)))
         (bottom (- (y center-2)
                    (/ (y (box-size box-2)) 2)))
         ; Vertices of box-1
         (vertices-1 (mapcar 
                      #'(lambda (v)
                          (rotate-vector-ccw v angle-1))
                      (combinations (plus-minus (/ (x (box-size box-1)) 2))
                                    (plus-minus (/ (y (box-size box-1)) 2))))))
    (reduce #'(lambda (x y) (or x y))
            (mapcar #'(lambda (v) (point-in-rectangle v left right top bottom))
                    vertices-1))))

(defmethod collisionp ((box-1 box) (box-2 box))
  (if (eq box-1 box-2)
      nil
      (or (one-way-rect-collision box-1 box-2)
          (one-way-rect-collision box-2 box-1))))

; When two boxes collide, make sure they separate afterwards to avoid
; awkward behavior

(defmethod collide :after ((box-1 box) (box-2 box))
  (space-out-entities box-1 box-2))