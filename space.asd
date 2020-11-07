;;;; space.asd

(asdf:defsystem #:space
  :description ""
  :author "Stefano Rodighiero <stefano.rodighiero@gmail.com>"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf
               #:xmls
               #:bordeaux-threads
               #:alexandria)
  :components ((:file "package")
               (:file "game-entity")
               (:file "sprites")
               (:file "animations")
               (:file "enemies")
               (:file "player")
               (:file "starfield")
               (:file "sounds")
               (:file "collisions")
               (:file "levels")
               (:file "space")))
