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

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *max-charge* 4000)

(defvar *particles* nil)
(defvar *walls*
  (list (make-instance 'boundary
                       :size #(801 100)
                       :position #(400 -50))
        (make-instance 'boundary 
                       :size #(801 100)
                       :position #(400 649))
        (make-instance 'boundary 
                       :size #(100 601)
                       :position #(-50 300))
        (make-instance 'boundary
                       :size #(100 601)
                       :position #(849 300))
        (make-instance 'boundary
                       :size #(50 600)
                       :position #(400 300)
                       :angle (* 3/5 pi))
        (make-instance 'goal
                       :position #(400 60)
                       :radius 20)))
         

(defun main ()
  (setf *particles* nil)
                                         
  (with-init ()
    (let ((time (sdl-get-ticks)))
      ;Starting the display
      (window 800 600
              :double-buffer t)
      ;Running the game loop
      (with-events ()
        (:quit-event () t)
        (:sdl-video-resize-event (:w w :h h) (resize-window w h))
        (:key-down-event () (setf *particles* nil))
        (:mouse-button-down-event 
         (:x x :y y)
         (push (let ((radius (1+ (random 30)))
                     (charge (- (random (1+ (* 2 *max-charge*))) *max-charge*)))
                 (make-instance 'particle
                                ;:velocity (vector (- (random 500) 250)
                                 ;                 (- (random 500) 250))
                                :velocity #(0 0)
                                :radius radius
                                :position (vector x y)
                                :mass (* pi radius radius)
                                :charge charge))
               *particles*))
        (:idle ()
               (let ((time-diff (- (sdl-get-ticks) time)))
                 (when (/= time-diff 0)
                   (clear-display *black*)
                   (loop for w in *walls* do (draw w))
                   (loop for p in *particles* do
                        (loop for w in *walls* do
                             (when (collisionp p w)
                               (collide p w)))
                        (loop for p2 in *particles* do
                             (electric-force p2 p)
                             (when (collisionp p p2)
                               (collide p p2))
                             (move p (/ time-diff 1000.0)))
                        (draw p)))
                 (update-display)
                 (setf time (sdl-get-ticks))))))))