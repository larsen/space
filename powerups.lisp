(in-package #:space)

(defparameter *powerups* nil)

(defclass powerup (game-entity)
  ((bounding-radius :initform 10)))

(defclass life-powerup (powerup)
  ((sprite :initform "pill_blue.png")))
(defclass speed-powerup (powerup)
  ((sprite :initform "pill_yellow.png")))

(defgeneric apply-powerup (powerup game-actor))
(defmethod apply-powerup ((powerup life-powerup) *player*)
  (decf (damage *player*) 10))

(defmethod apply-powerup ((powerup speed-powerup) *player*)
  (incf (velocity *player*) 5))

(defun random-powerup ()
  (let ((powerup-types '(life-powerup speed-powerup)))
    (make-instance (nth (random (length powerup-types)) powerup-types)
                   :x (random *window-width*)
                   :y (random *window-height*)
                   :velocity 0)))

(defun add-random-powerup ()
  (push (random-powerup) *powerups*))

(defun draw-powerups ()
  (loop for p in *powerups* do (draw p)))
