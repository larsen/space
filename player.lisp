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
  (setf *player-missiles*
        (push (make-instance 'player-missile
                                :x (+ (x player)
                                      (ceiling (/ (aref (bounding-box player) 0) 2)))
                                :y (y player)
                                :bounding-radius 20)
              *player-missiles*)))

(defun draw-missiles ()
  (loop for m in (nconc *player-missiles* *enemy-missiles*)
        do (draw m)))

(defun update-player-pos (player)
  (when (and *moving-north* (> (y player) 0))
    (decf (y player) (velocity player)))
  (when (and *moving-south* (< (y player) *window-height*))
    (incf (y player) (velocity player)))
  (when (and *moving-east* (> (x player) 0))
    (decf (x player) (velocity player)))
  (when (and *moving-west* (< (x player) *window-width*))
    (incf (x player) (velocity player))))

(defun update-missile-pos (m)
  (let ((new-position-y (- (y m) (* 1 (velocity m)))))
    (setf (y m) new-position-y)))

(defun update-missiles ()
  (loop for m in (nconc *player-missiles* *enemy-missiles*)
        do (update-missile-pos m))
  (labels ((remove-damaged-missiles (missiles-list)
             (setf missiles-list
                   (remove-if
                    (lambda (m)
                      (or (reached-maximum-damage? m)
                          (outside-display-area? m)))
                    missiles-list))))
    (remove-damaged-missiles *player-missiles*)
    (remove-damaged-missiles *enemy-missiles*)))
