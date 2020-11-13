;;;; space.lisp

(in-package #:space)

(defparameter *window-width* 1024)
(defparameter *window-height* 768)
(defparameter *window* nil)

(defparameter *moving-west* nil)
(defparameter *moving-east* nil)
(defparameter *moving-north* nil)
(defparameter *moving-south* nil)

(defparameter *background-image* nil)
(defparameter *banner* nil)

(defparameter *background-music* nil)
(defparameter *fire-missile-snd-fx* nil)
(defparameter *fire-missile-snd-fx2* nil)

(defparameter *debug* t)

(defun init ()
  ;; Setup player position and state
  (setf *moving-west* nil)
  (setf *moving-east* nil)
  (setf *moving-north* nil)
  (setf *moving-south* nil)
  (setf *player-missiles* nil)
  (setf *enemies* nil)
  (setf *enemy-missiles* nil)
  (setf *background-image* nil)
  (setf *timed-actions* (make-hash-table))
  (init-star-field)
  (init-player))

(defparameter *ttf-font-kenvector*
  (make-instance 'ttf-font-definition
                 :size 32
                 :filename (asdf:system-relative-pathname
                            'space "assets/kenvector_future.ttf")))

(defun draw-string-at (string x y)
  (sdl:draw-string-solid string (sdl:point :x x :y y)
                         :font (sdl:initialise-font *ttf-font-kenvector*)
                         :color sdl:*white*))

(defun draw-number-with-sprites (n x y &key (padding nil))
  (check-type n (integer 0 *))
  (let* ((fmt (if padding
                  "~6,'0d"
                  "~d"))
         (n-as-string (format nil fmt n)))
    (loop for d across n-as-string
          for idx from 0
          do (draw-sprite
              (format nil "numeral~A.png" d)
              (+ x (* idx 20)) y))))

(defun draw-score ()
  (draw-number-with-sprites (score *player*) 670 10 :padding t))

(defun draw-elapsed-time ()
  (draw-number-with-sprites (floor (elapsed-time)) 10 10))

(defun draw-player-damage-level (player)
  (draw-sprite "buttonBlue.png" 440 10)
  (let* ((max-bar-length 200)
        ;; p damage : max damage = curr : max
        (current-bar-length (/ (* max-bar-length (damage *player*))
                               (maximum-damage player))))
    (draw-box-* 445 15 current-bar-length 29 :color *red*)))

(defun draw-banner ()
  (when *banner*
    (draw-string-at *banner* 50 50)))

(defun set-background (background-filename)
  (setf *background-image*
        (load-image background-filename
                    :image-type :JPG
                    :force t
                    :color-key-at (vector 0 0))))

(defun draw-background ()
  (when *background-image*
    (sdl:draw-surface-at-* *background-image* 0 0)))

(defun updates ()
  (update-star-field)
  (update-player-pos *player*)
  (update-enemies)
  (update-missiles))

(defun draw-world ()
  (draw-background)
  (draw-star-field)
  (draw *player*)
  (draw-entities *player-missiles*)
  (draw-entities *enemy-missiles*)
  (draw-entities *enemies*)
  (draw-elapsed-time)
  (draw-score)
  (draw-player-damage-level *player*)
  (draw-banner))

(defun set-banner (banner)
  (setf *banner* banner))

(defun reset-banner ()
  (setf *banner* nil))

(deflevel level1 "Another nice level"
  :background (asdf:system-relative-pathname 'space "assets/SpaceBackground-4.jpg")
  :background-music (asdf:system-relative-pathname 'space "assets/run_theme.mp3")
  :after 0 (set-banner "Galaxy Sector I") :for 2 :then (reset-banner)
  :after 1 (init-enemies 'enemy 5)
  :after (+ 5 (random 3)) (set-banner "You should have killed the enemies now!") :for 2 :then (reset-banner))

(deflevel level2 "Extra nice level"
  :background (asdf:system-relative-pathname 'space "assets/SpaceBackground-1.jpg")
  :background-music (asdf:system-relative-pathname 'space "assets/orbital-colossus.mp3")
  :after 0 (set-banner "Galaxy Sector II") :for 2 :then (reset-banner)
  :after 1 (init-enemies 'enemy 6)
  :after (+ 5 (random 3)) (set-banner "You should have killed the enemies now!") :for 2 :then (reset-banner))

(defun main ()
  (with-init ()
    (setf *window* (window *window-width* *window-height*))
    (clear-display (sdl:color :r 0 :g 0 :b 0))
    (sdl:initialise-default-font)
    (sdl-mixer:init-mixer :mp3)
    (sdl-mixer:allocate-channels 16)
    (sdl-mixer:open-audio :chunksize 1024 :enable-callbacks nil)

    (setf *fire-missile-snd-fx*
          (sdl-mixer:load-sample (asdf:system-relative-pathname
                                  'space "assets/sfx_laser1.ogg")))
    (setf *fire-missile-snd-fx2*
          (sdl-mixer:load-sample (asdf:system-relative-pathname
                                  'space "assets/sfx_laser2.ogg")))
    (update-display)
    (load-sprite-sheet)
    (init)

    (reset-time)
    (when *background-music*
      (sdl-mixer:play-music *background-music*))

    (with-events ()
      (:quit-event () t)
      (:key-up-event (:key key)
                     (case key
                         (:sdl-key-w (setf *moving-north* nil))
                         (:sdl-key-a (setf *moving-east* nil))
                         (:sdl-key-s (setf *moving-south* nil))
                         (:sdl-key-d (setf *moving-west* nil))))
      (:key-down-event (:key key)
                       (case key
                         (:sdl-key-escape (progn
                                            (sdl-mixer:Close-Audio t)
                                            (push-quit-event)))
                         (:sdl-key-w (setf *moving-north* t))
                         (:sdl-key-a (setf *moving-east* t))
                         (:sdl-key-s (setf *moving-south* t))
                         (:sdl-key-d (setf *moving-west* t))
                         (:sdl-key-space (fire-missile *player*))))
      (:idle
       (progn
         (clear-display (sdl:color :r 0 :g 0 :b 0))
         (updates)
         (check-collisions)
         (when (> (damage *player*)
                  (maximum-damage *player*))
           (push-quit-event))
         (check-timeout-and-exec!)
         (draw-world)
         (update-display))))))

(defun run ()
  (bt:make-thread #'main))
