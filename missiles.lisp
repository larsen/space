(in-package #:space)

(defclass missile (game-actor)
  ((maximum-damage :initform 100)))

(defclass player-missile (missile)
  ((velocity :initform *missile-velocity*)
   (sprite :initform "laserBlue01.png")))

(defclass enemy-missile (missile)
  ((velocity :initform (* -1 *missile-velocity*))
   (sprite :initform "laserGreen02.png")))

(defun update-missile-pos (m)
  (let ((new-position-y (- (y m) (* 1 (velocity m)))))
    (setf (y m) new-position-y)))

(defun update-missiles ()
  (dolist (m (concatenate 'list *player-missiles* *enemy-missiles*))
    (update-missile-pos m))
  (labels ((out-of-game? (missile)
             (or (reached-maximum-damage? missile)
                 (outside-display-area? missile))))
    (setf *player-missiles*
          (remove-if #'out-of-game? *player-missiles*))
    (setf *enemy-missiles*
          (remove-if #'out-of-game? *enemy-missiles*))))
