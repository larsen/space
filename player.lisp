(in-package #:space)

(defparameter *player-velocity* 15)
(defparameter *player-missiles* nil)
(defparameter *missile-velocity* 5)

(defparameter *player* nil)

(defclass player (game-entity) ())

(defclass player-missile (game-entity) ())

(defun init-player ()
  (setf *player*
        (make-instance 'player
                       :x (/ *window-width* 2) :y (- *window-height* 50)
                       :velocity *player-velocity*
                       :sprite "playerShip1_blue.png"
                       :bounding-radius 20)))

(defun draw-missiles ()
  (loop for m in *player-missiles*
        do (draw m)))

(defun update-player-pos (player)
  (when (and *moving-north* (> (y player) 0))
    (decf (y player) (* 1 (velocity player))))
  (when (and *moving-south* (< (y player) *window-height*))
    (incf (y player) (* 1 (velocity player))))
  (when (and *moving-east* (> (x player) 0))
    (decf (x player) (* 1 (velocity player))))
  (when (and *moving-west* (< (x player) *window-width*))
    (incf (x player) (* 1 (velocity player)))))

(defun update-missile-pos (m)
  (let ((new-position-y (- (y m) (* 1 (velocity m)))))
    (when (> new-position-y 0)
      (setf (y m) new-position-y))))

(defun update-missiles-pos ()
  (loop for m in *player-missiles*
        do (update-missile-pos m)))

(defun fire-missile (player)
  (setf *player-missiles*
        (append *player-missiles*
                (list
                 (make-instance 'player-missile
                                :x (x player) :y (y player)
                                :velocity *missile-velocity*
                                :sprite "laserBlue01.png"
                                :bounding-radius 20)))))
