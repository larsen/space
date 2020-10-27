(in-package #:space)

(defclass game-entity ()
  ((pos-x :initarg :x :accessor x)
   (pos-y :initarg :y :accessor y)
   (velocity :initarg :velocity :accessor velocity)
   (sprite :initarg :sprite :accessor sprite)
   (bounding-radius :initarg :bounding-radius :accessor bounding-radius)))

(defgeneric draw (entity))
(defmethod draw ((entity game-entity))
  (let ((pos-x (x entity))
        (pos-y (y entity)))
    (draw-sprite (sprite entity) *default-display* pos-x pos-y)
    (when *debug*
      (destructuring-bind (sx sy width height)
          (gethash (sprite entity) *sprite-sheet-atlas*)
        (declare (ignore sx sy))
        (sdl:draw-rectangle
         (sdl:rectangle-from-edges-* pos-x pos-y
                                     (+ pos-x width)
                                     (+ pos-y height))
		     :color sdl:*red*)
        (when (bounding-radius entity)
          (sdl:draw-circle-* (aref (center entity) 0) (aref (center entity) 1)
                             (bounding-radius entity)
                             :color sdl:*red*))))))

(defgeneric entity-distance (entity1 entity2))
(defmethod entity-distance ((e1 game-entity) (e2 game-entity))
  "Computes the distance between ENTITY1-pos and ENTITY2-pos. They are both
  assumed to be a vector of integers."
  (sdl:distance (vector (x e1) (y e2))
                (vector (x e2) (y e2))))

(defgeneric bounding-box (entity))
(defmethod bounding-box (entity)
  (destructuring-bind (sx sy width height)
      (gethash (sprite entity) *sprite-sheet-atlas*)
    (declare (ignore sx sy))
    (vector width height)))

(defgeneric center (entity))
(defmethod center ((entity game-entity))
  (let ((bbox (bounding-box entity)))
    (vector (ceiling (+ (x entity) (/ (aref bbox 0) 2)))
            (ceiling (+ (y entity) (/ (aref bbox 1) 2))))))
