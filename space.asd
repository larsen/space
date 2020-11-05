;;;; space.asd

(asdf:defsystem #:space
  :description "Describe space here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:xmls
               #:bordeaux-threads
               #:alexandria)
  :components ((:file "package")
               (:file "game-entity")
               (:file "sprites")
               (:file "enemies")
               (:file "player")
               (:file "starfield")
               (:file "collisions")
               (:file "levels")
               (:file "space")))
