;;;; space.lisp

(in-package #:space)

(defparameter *window-width* 1024)
(defparameter *window-height* 768)
(defparameter *window* nil)

(defparameter *moving-west* nil)
(defparameter *moving-east* nil)
(defparameter *moving-north* nil)
(defparameter *moving-south* nil)

(defparameter *debug* t)

(defun init ()
  ;; Setup player position and state
  (setf *moving-west* nil)
  (setf *moving-east* nil)
  (setf *moving-north* nil)
  (setf *moving-south* nil)
  (setf *player-missiles* nil)
  (init-star-field)
  (init-player)
  ; (init-enemies)
  ;(setf *timed-actions* (make-hash-table))
  )

(defun draw-string-at (string x y)
  (sdl:draw-string-solid string (sdl:point :x x :y y)
                         :font (sdl:initialise-font sdl:*font-10x20*)
                         :color sdl:*white*))

(defun draw-score ()
	(draw-string-at (format nil "~d" (format nil "~D" 0))
                  60 10))

(defun draw-elapsed-time ()
  (draw-string-at (format nil "~d" (format nil "~D" (floor (elapsed-time))))
                  10 10))

(defun updates ()
  (update-star-field)
  (update-player-pos *player*)
  (update-enemies)
  (update-missiles-pos))

(defun draw-entities ()
  (draw-star-field)
  (draw *player*)
  (draw-missiles)
  (draw-enemies)
  (draw-elapsed-time)
  (draw-score))

(defun main ()
  (with-init ()
    (setf *window* (window *window-width* *window-height*))
    (clear-display (sdl:color :r 0 :g 0 :b 0))
    (sdl:initialise-default-font)
    (update-display)
    (load-sprite-sheet)
    (init)

    (deflevel 1 "Another nice level"
      :at 1 (init-enemies)
      :at 2 (fire-missile *player*))
    
    (reset-time)
    (with-events ()
      (:quit-event () t)
      (:key-up-event (:key key)
                     (case key
                         (:sdl-key-w (setf *moving-north* nil))
                         (:sdl-key-a (setf *moving-east* nil))
                         (:sdl-key-s (setf *moving-south* nil))
                         (:sdl-key-d (setf *moving-west* nil))))
      (:key-down-event (:key key)
                       (case key
                         (:sdl-key-escape (push-quit-event))
                         (:sdl-key-w (setf *moving-north* t))
                         (:sdl-key-a (setf *moving-east* t))
                         (:sdl-key-s (setf *moving-south* t))
                         (:sdl-key-d (setf *moving-west* t))
                         (:sdl-key-space (fire-missile *player*))))
      (:idle
       (progn
         (clear-display (sdl:color :r 0 :g 0 :b 0))
         (updates)
         ; (check-collisions)
         (check-timeout-and-exec!)
         (draw-entities)
         (update-display))))))

(defun run ()
  (bt:make-thread #'main))
