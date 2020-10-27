(in-package #:space)

(defparameter *player-sprite* nil)
(defparameter *player-missile-sprite* nil)
(defparameter *enemy-sprite* nil)

(defparameter *sprite-sheet-image* nil)
(defparameter *sprite-sheet-atlas-filename*
  (asdf:system-relative-pathname 'space "assets/sheet.xml"))
(defparameter *sprite-sheet-atlas* nil)

(defun load-sprite-sheet ()
  (setf *sprite-sheet-atlas* (make-hash-table :test #'equal))
  (setf *sprite-sheet-image*
        (load-image (asdf:system-relative-pathname 'space "assets/sheet.png")
         :image-type :PNG
         :force t
         :color-key-at (vector 0 0)))
  (flet ((peek-in-sprite (sprite item)
           (cdr (find item sprite :test #'equal :key #'car))))
    (let* ((sprite-nodes
             (xmls:node-children
              (xmls:parse
               (uiop:read-file-string *sprite-sheet-atlas-filename*))))
           (first-pass
             (loop for n in sprite-nodes
                   collect (loop for attr in (xmls:node-attrs n)
                                 collect `(,(first attr) . ,(second attr))))))
      (loop for sprite in first-pass
            do (setf (gethash (peek-in-sprite sprite "name") *sprite-sheet-atlas*)
                     `(,(parse-integer (peek-in-sprite sprite "x"))
                       ,(parse-integer (peek-in-sprite sprite "y"))
                       ,(parse-integer (peek-in-sprite sprite "width"))
                       ,(parse-integer (peek-in-sprite sprite "height"))))))))

(defun draw-sprite (sprite-name surface x y)
  (destructuring-bind (sx sy width height) (gethash sprite-name *sprite-sheet-atlas*)
    (setf (clip-rect surface)
          (rectangle-from-edges-* x y (+ x width) (+ y height)))
    (sdl:draw-surface-at-* *sprite-sheet-image* (* -1 (- sx x)) (* -1 (- sy y)))
    (setf (clip-rect surface) nil)))
