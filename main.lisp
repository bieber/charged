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

(defvar *particles* nil)
(defvar *collision* nil)

(defun main ()
  (setf *particles* nil)
  (setf *collision* nil)
  (with-init ()
    (let ((time (sdl-get-ticks)))
      ;Starting the display
      (window 1024 768
              :double-buffer t
              :resizable t)
      ;Running the game loop
      (with-events ()
        (:quit-event () t)
        (:sdl-video-resize-event (:w w :h h) (resize-window w h))
        (:mouse-button-down-event 
         (:x x :y y)
         (when (not *collision*)
           (push (make-instance 'entity
                                :velocity (vector (- (random 200) 100)
                                                  (- (random 200) 100))
                                :radius 70
                                :position (vector x y)) *particles*))
         (when *collision*
           (setf *particles* nil)
           (setf *collision* nil)))
                              
        (:idle ()
               (let ((time-diff (- (sdl-get-ticks) time)))
                 (when (/= time-diff 0)
                   (when (not *collision*)
                     (clear-display *black*)
                     (loop for p in *particles* do
                          (move p (/ time-diff 1000.0))
                          (loop for p2 in *particles* do
                               (when (collisionp p p2)
                                 (setf *collision* t)
                                 (collide p p2)))
                          (when (not *collision*)
                            (draw p))))
                   (update-display)
                   (setf time (sdl-get-ticks)))))))))