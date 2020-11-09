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
(defparameter *timed-actions* nil)

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

;; Level language interpreter

(defun parse-script (script)
  (case (car script)
    (:background (parse-set-background-filename (cdr script)))
    (:background-music (parse-set-background-music (cdr script)))
    (:after (parse-after (cdr script)))
    (otherwise nil)))

(defun parse-after (clauses)
  "Parses the :AFTER command arguments.
The :AFTER keyword accepts two forms:
1. :AFTER INTERVAL FORM - This executes FORM when INTERVAL seconds
are passed since the start of the level
2. :AFTER INTERVAL FORM :FOR INTERVAL2 :THEN FORM2 - This
executes a separate thread after INTERVAL seconds. In the thread
first we evaluate FORM, then we wait for INTERVAL2 seconds, then
we evaluate FORM2. This can be used to implement behaviours that
need to execute temporarily, and independent of the game actions."
  (let ((interval (car clauses))
        (form (cadr clauses)))
    (if (eql :for (caddr clauses))
        ;; Second variant
        (destructuring-bind (for interval2 then form2 &rest rest) (cddr clauses)
          (declare (ignore for then))
          `((push '(bt:make-thread (lambda ()
                                     ,form
                                     (sleep ,interval2)
                                     ,form2))
                  (gethash ,interval *timed-actions*))
            ,@(parse-script rest)))
        ;; First variant
        `((push ',form (gethash ,interval *timed-actions*))
          ,@(parse-script (cddr clauses))))))

(defun parse-set-background-music (clauses)
  (let ((background-music-filename (car clauses)))
    `((set-background-music ,background-music-filename)
      ,@(parse-script (cdr clauses)))))

(defun parse-set-background-filename (clauses)
  (let ((background-filename (car clauses)))
    `((set-background ,background-filename)
      ,@(parse-script (cdr clauses)))))

(defmacro deflevel (name catchphrase &rest script)
  `(defun ,(alexandria:symbolicate '#:run-level- name) ()
     (reset-time)
     (reset-banner)
     (setf *timed-actions* (make-hash-table))
     (after! 0 (print (format nil "Level ~A: ~A" ',name ,catchphrase)))
     ,@(parse-script script)))
