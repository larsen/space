(in-package #:space)

(defparameter *enemy-sprite* nil)
(defparameter *enemy-velocity* 4)

(defparameter *enemies* nil)
(defparameter *enemy-missiles* nil)

(defclass enemy (game-actor)
  ((motion-f :initarg :motion-f :accessor motion-f)
   (fire-f :initarg :fire-f :accessor fire-f)))

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

(defun random-fire (enemy time)
  (declare (ignore time))
  (if (> (random 1.0) 0.98)
      (fire-missile enemy)))

(defun init-enemies (&optional (enemy-class 'enemy) (enemy-quantity 5))
  (setf *enemies*
        (loop repeat enemy-quantity
              for idx from 0
              collect (make-instance enemy-class
                                     :x (+ 50 (* idx 120))
                                     :y -100
                                     :velocity *enemy-velocity*
                                     :motion-f #'sinus-forward-motion
                                     :fire-f #'random-fire
                                     :sprite "enemyGreen1.png"
                                     :bounding-radius 30))))

(defmethod fire-missile ((enemy enemy))
  (setf *enemy-missiles*
        (append *enemy-missiles*
                (list
                 (make-instance 'enemy-missile
                                :x (+ (x enemy)
                                      (ceiling (/ (aref (bounding-box enemy) 0) 2)))
                                :y (y enemy)
                                :bounding-radius 20)))))

(defun enemy-movement (enemy time)
  "Given an ENEMY object and current TIME, computes and update the
  ENEMY position."
  (let ((new-position (funcall (motion-f enemy) enemy time)))
    (when (< (aref new-position 1) *window-height*)
      (setf (x enemy) (aref new-position 0))
      (setf (y enemy) (aref new-position 1)))))

(defun update-enemies ()
  (loop for e in *enemies*
        do (progn
             (enemy-movement e (sdl-cffi::SDL-get-ticks))
             (funcall (fire-f e) e (sdl-cffi::SDL-get-ticks))))
  (setf *enemies* (remove-if #'reached-maximum-damage? *enemies* )))

(defun draw-enemies ()
  (loop for e in *enemies* do (draw e)))
