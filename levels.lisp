(in-package #:space)

;;; Level definition
;;;
;;; What does constitue a level in the game?
;;;
;;; - level number
;;; - elements in the background
;;; - a catchphrase, to print on the screen when player enter the level
;;; - a sort of script, defining what enemies arrive when, and what
;;;   pattern they will follow. Also, when powerups are going to be showed,
;;;   and in what position on the playfield
;;; - question: when does the level end?

;;; Related:
;;; how to make it possible to write functions/procedures
;;; that are going to be executed at a certain moment in time?
;;; Come dovrebbe essere fatta una struttura dati per
;;; contenere routine che devono essere eseguite dopo
;;; uno specifico intervallo di tempo?

(defparameter *last-reset-ticks* nil)
(defparameter *timed-actions* (make-hash-table))

(defun elapsed-time ()
  "Returns the amount of time (in seconds) since the beginning
of the current game, or since the last call to RESET-TIME."
  (/ (- (sdl-cffi::SDL-get-ticks) *last-reset-ticks*) 1000))

(defun reset-time ()
  "Resets in-game time. Immediately after this call, a
call to ELAPSED-TIME would return 0."
  (setf *last-reset-ticks* (sdl-cffi::SDL-get-ticks)))

(defun check-timeout-and-exec! ()
  "Checks elapsed time and executes any approprioate routine that was
  installed via AFTER! or via deflevel. The routines that were run are then removed
  from the structure."
  (let ((current-elapsed-time (elapsed-time)))
    (loop for interval being the hash-key of *timed-actions*
          when (<= interval current-elapsed-time)
          do (loop for action = (pop (gethash interval *timed-actions*))
                   while action
                   do (eval action)))))

(defmacro after! (interval form)
  `(push ',form (gethash ,interval *timed-actions*)))

(defmacro deflevel (number catchphrase &rest script)
  (let ((directive (gensym))
        (param1 (gensym))
        (param2 (gensym)))
    `(progn
       (after! 0 (print (format nil "Level ~A: ~A" ,number ,catchphrase)))
       (loop for (,directive ,param1 ,param2) on ',script by #'cdddr
             do (case ,directive
                  ;; FIXME it would be nice to use the after! macro
                  ;; here as well
                  (:at (push ,param2 (gethash ,param1 *timed-actions*)))
                  (otherwise (error "Not supported level directive!")))))))

