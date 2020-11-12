(in-package #:space)

(defparameter *missile-velocity* 20)

(defclass game-entity ()
  ((pos-x :initarg :x :accessor x)
   (pos-y :initarg :y :accessor y)
   (velocity :initarg :velocity :accessor velocity)
   (sprite :initarg :sprite :accessor sprite)
   (bounding-radius :initarg :bounding-radius :accessor bounding-radius)))

(defclass game-actor (game-entity)
  ((damage :initarg :damage :initform 0 :accessor damage)
   (maximum-damage :initarg :maximum-damage :initform 200 :accessor maximum-damage)))

(defclass missile (game-actor)
  ((maximum-damage :initform 100)))

(defclass player-missile (missile)
  ((velocity :initform *missile-velocity*)
   (sprite :initform "laserBlue01.png")))

(defclass enemy-missile (missile)
  ((velocity :initform (* -1 *missile-velocity*))
   (sprite :initform "laserGreen02.png")))

(defgeneric draw (entity))
(defmethod draw ((entity game-entity))
  (let ((pos-x (x entity))
        (pos-y (y entity)))
    (draw-sprite (sprite entity) pos-x pos-y)
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

(defun draw-entities (entities)
  (dolist (e entities)
    (draw e)))

(defgeneric entity-distance (entity1 entity2)
  (:documentation "Computes the distance between ENTITY1-pos and
  ENTITY2-pos. They are both assumed to be a vector of integers."))
(defmethod entity-distance ((e1 game-entity) (e2 game-entity))
  (sdl:distance (center e1) 
                (center e2)))

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

(defgeneric outside-display-area? (entity))
(defmethod outside-display-area? (entity)
  (let ((bbox (bounding-box entity)))
    (or (> (y entity) *window-height*)
        (> (x entity) *window-width*)
        (< (y entity) (- 0 (aref bbox 1)))
        (< (x entity) (- 0 (aref bbox 0))))))

(defgeneric reached-maximum-damage? (actor))
(defmethod reached-maximum-damage? ((actor game-actor))
  (>= (damage actor)
      (maximum-damage actor)))

(defgeneric fire-missile (actor))
