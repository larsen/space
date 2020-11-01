(in-package #:space)

(defparameter *enemy-sprite* nil)
(defparameter *enemy-velocity* 4)

(defparameter *enemies* nil)

(defclass enemy (game-entity)
  ((motion-f :initarg :motion-f :accessor motion-f)))

(defun basic-forward-motion (enemy time)
  (declare (ignore time))
  (let ((new-position
          (vector (x enemy)
                  (+ (y enemy) (* 1 (velocity enemy))))))
    new-position))

(defun sinus-forward-motion (enemy time)
  (let ((new-position
          (vector (+ (x enemy) (ceiling (* 10 (sin (/ time 100)))))
                  (+ (y enemy) (* 1 (velocity enemy))))))
    new-position))

(defun init-enemies ()
  (setf *enemies*
        (loop repeat 5
              for idx from 0
              collect (make-instance 'enemy
                                     :x (+ 50 (* idx 120))
                                     :y (+ (* idx 50) -300)
                                     :velocity *enemy-velocity*
                                     :motion-f #'basic-forward-motion
                                     :sprite "enemyGreen1.png"
                                     :bounding-radius 30))))

(defun enemy-movement (enemy time)
  "Given an ENEMY object and current TIME, computes and update the
  ENEMY position."
  (let ((new-position (funcall (motion-f enemy) enemy time)))
    (when (< (aref new-position 1) *window-height*)
      (setf (x enemy) (aref new-position 0))
      (setf (y enemy) (aref new-position 1)))))

(defun update-enemies ()
  (loop for e in *enemies*
        do (enemy-movement e (sdl-cffi::SDL-get-ticks))))

(defun draw-enemies ()
  (loop for e in *enemies* do (draw e)))
