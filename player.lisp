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
  (setf *player-missiles*
        (append *player-missiles*
                (list
                 (make-instance 'player-missile
                                :x (+ (x player)
                                      (ceiling (/ (aref (bounding-box player) 0) 2)))
                                :y (y player)
                                :bounding-radius 20)))))

(defun draw-missiles ()
  (loop for m in *player-missiles*
        do (draw m))
  (loop for m in *enemy-missiles*
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
    (setf (y m) new-position-y)))

(defun update-missiles ()
  ;; refactor
  (loop for m in *player-missiles*
        do (update-missile-pos m))
  (setf *player-missiles*
        (remove-if
         (lambda (m)
           (or (reached-maximum-damage? m)
               (outside-display-area? m)))
         *player-missiles*))
  (loop for m in *enemy-missiles*
        do (update-missile-pos m))
  (setf *enemy-missiles*
        (remove-if
         (lambda (m)
           (or (reached-maximum-damage? m)
               (outside-display-area? m)))
         *enemy-missiles*)))
