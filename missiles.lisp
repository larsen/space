(in-package #:space)

(defclass missile (game-actor)
  ((maximum-damage :initform 100)))

(defclass player-missile (missile)
  ((velocity :initform *missile-velocity*)
   (sprite :initform "laserBlue01.png")))

(defclass enemy-missile (missile)
  ((velocity :initform (* -1 *missile-velocity*))
   (sprite :initform "laserGreen02.png")))

(defun update-missiles ()
  (loop for m in (concatenate 'list *player-missiles* *enemy-missiles*)
        do (update-missile-pos m))
  (labels ((remove-missiles (missiles-list)
             (setf missiles-list
                   (remove-if
                    (lambda (m)
                      (or (reached-maximum-damage? m)
                          (outside-display-area? m)))
                    missiles-list))))
    (remove-missiles *player-missiles*)
    (remove-missiles *enemy-missiles*)))
