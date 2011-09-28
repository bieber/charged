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

;; A charged particle, like a circle but with charge

(defclass particle (charged circle)
  () ; No new slots, just new behavior
  (:documentation "A simple charged particle"))

; Method definitions

(defmethod draw ((particle particle) &optional (surface *default-surface*))
  (let ((particle-color (if (< (charge particle) 0)
                            (color :b (* 255 
                                         (/ (1+ (charge particle))
                                            (- (1+ *max-charge*)))))
                            (color :r (* 255
                                         (/ (1+ (charge particle))
                                            (1+ *max-charge*)))))))
    (draw-filled-circle (point-from-vector (entity-position particle))
                        (circle-radius particle)
                        :surface surface
                        :color particle-color)))