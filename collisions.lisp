(in-package #:space)

(defun collision? (entity1 entity2)
  (< (entity-distance entity1 entity2)
     (max (bounding-radius entity1) (bounding-radius entity2))))

(defun check-collisions ()
  (loop for m in *player-missiles*
        do (loop for e in *enemies*
                 do (when *debug*
                      (draw-line (center m) (center e) :color *red*))
                    (when (collision? e m)
                      (incf (damage e) 100)
                      (incf (damage m) 100)
                      (incf (score *player*) 50))))
  (loop for m in *enemy-missiles*
        do (when *debug*
             (draw-line (center m) (center *player*) :color *red*))
           (when (collision? *player* m)
             (incf (damage *player*) 20)
             (incf (damage m) 100))))
