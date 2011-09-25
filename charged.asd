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

(in-package :asdf)

(defsystem "charged"
  :description "Charged: A puzzle game based on Coulomb's law."
  :version "0.1"
  :author "Robert Bieber <robby@bieberphoto.com>"
  :licence "GPLv3"
  :serial t
  :depends-on (lispbuilder-sdl)
  :components ((:file "charged-package")
               (:file "geometry-helpers"
                      :depends-on ("charged-package"))
               (:module "entities"
                        :depends-on ("charged-package"
                                     "geometry-helpers")
                        :components ((:file "entity")
                                     (:file "circle"
                                            :depends-on ("entity"))
                                     (:file "box"
                                            :depends-on ("entity"
                                                         "circle"))))
               (:file "main"
                      :depends-on ("charged-package"
                                   "entities"))))
               