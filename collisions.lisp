(in-package #:space)

(defun collision? (entity1 entity2)
  (< (entity-distance entity1 entity2) 50))

(defun check-collisions ()
  (loop for e in *enemies*
        do (loop for m in *player-missiles*
                 do (if (collision? e m)
                        (print "BOOM!")))))
