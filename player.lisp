(in-package #:space)

(defparameter *player-velocity* 15)
(defparameter *player-missiles* nil)

(defparameter *player* nil)

(defclass player (game-actor)
  ((score :initarg :score :initform 0 :accessor score)))

(defun init-player ()
  (setf *player*
        (make-instance 'player
                       :x (/ *window-width* 2) :y (- *window-height* 50)
                       :velocity *player-velocity*
                       :sprite "playerShip2_red.png"
                       :bounding-radius 50)))

(defmethod fire-missile ((player player))
  (sdl-mixer:play-sample *fire-missile-snd-fx*)
  (push (make-instance 'player-missile
                       :x (+ (x player)
                             (ceiling (/ (aref (bounding-box player) 0) 2)))
                       :y (y player)
                       :bounding-radius 20)
        *player-missiles*))

(defun update-player-pos (player)
  (when (and *moving-north* (> (y player) 0))
    (decf (y player) (velocity player)))
  (when (and *moving-south* (< (y player) *window-height*))
    (incf (y player) (velocity player)))
  (when (and *moving-east* (> (x player) 0))
    (decf (x player) (velocity player)))
  (when (and *moving-west* (< (x player) *window-width*))
    (incf (x player) (velocity player))))
