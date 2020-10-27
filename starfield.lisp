(in-package #:space)

(defparameter *star-field-size* 2500)
(defparameter *star-field* nil)

(defun init-star-field ()
  (setf *star-field*
        (make-array (list *star-field-size*)
                    :element-type 'vector
                    :initial-contents (loop for i from 1 to *star-field-size*
                                            collect (vector (random *window-width*)
                                                            (random *window-height*))))))

(defmacro with-layered-star-field (ratio variable values &body body)
  "Results in a form that executes BODY in a loop, binding VARIABLE
respectively to the first element of VALUES if the index of the
element processed is above *STAR-FIELD-SIZE* * RATIO, and the second
element otherwise."
  (let ((threshold (gensym)))
    `(loop for star across *star-field*
           for idx from 0
           do (let* ((,threshold (* ,ratio *star-field-size*))
                     (,variable (if (> idx ,threshold)
                                    ,(first values)
                                    ,(second values))))
                ,@body))))

(defun update-star-field ()
  (with-layered-star-field 1/2 velocity (2 1)
    (progn
      (setf (aref star 1) (+ (aref star 1) (* 1 velocity)))
      (when (> (aref star 1) *window-height*)
        (setf (aref star 0) (random *window-width*))
        (setf (aref star 1) 0)))))

(defun draw-star-field ()
  (with-layered-star-field 2/3 color ((color :r 150 :g 150 :b 150)
                                      (color :r 120 :g 120 :b 150))
    (draw-pixel-* (aref star 0) (aref star 1)
                  :color color
                  :surface *default-display*)))
