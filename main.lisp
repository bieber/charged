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

(defun main ()
  (let ((e (make-instance 'entity)))
    (with-init ()
      ;Starting the display
      (window 500 500 
              :double-buffer t
              :resizable t)
      ;Running the game loop
      (with-events ()
        (:quit-event () t)
        (:sdl-video-resize-event (:w w :h h) (resize-window w h))
        (:mouse-motion-event (:x x :y y) 
                             (setf (entity-position e) (point :x x :y y)))
        (:idle ()
               (clear-display *black*)
               (draw e)
               (update-display))))))