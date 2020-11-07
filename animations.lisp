(in-package #:space)

(defclass animation ()
  ((frames :initarg :frames :initform (error "You need to define :FRAMES") :accessor frames)
   (current-frame-index :initform 0 :accessor current-frame-index)))

(defgeneric animate (animation entity &key))
(defmethod animate ((animation animation) (entity game-entity) &key (animation-step 0.1))
  (when (not (animation-thread entity))
    (setf (animation-thread entity)
          (bt:make-thread
           (lambda ()
             (loop (progn 
                     (if (< (+ 1 (current-frame-index animation)) (length (frames animation)))
                         (incf (current-frame-index animation))
                         (setf (current-frame-index animation) 0))
                     (sleep animation-step))))))))
