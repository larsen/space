(in-package #:space)

(defun set-background-music (music-filename)
  (setf *background-music*
        (sdl-mixer:load-music (asdf:system-relative-pathname
                               'space music-filename)))
  (sdl-mixer:play-music *background-music*))
