;;;; game.lisp

(in-package #:game)

(defparameter *unit* 16
  ;; used to make shapes consistent
  "One unit is 16 pixels.")

(defun units (n)
  (* *unit* n))

(defparameter *width* 640)
(defparameter *height* 480)

(defclass player (node)
   ((height :initform (units 3))
    (width :initform (units 3))
    (color :initform "cyan")
    (speed :initform 7)))

(defun wasd-direction ()
  (cond
    ((and (keyboard-down-p :s)
	  (keyboard-down-p :d))
     :downright)
    ((and (keyboard-down-p :s)
	  (keyboard-down-p :a))
     :downleft)
    ((and (keyboard-down-p :w)
	  (keyboard-down-p :d))
     :upright)
    ((and (keyboard-down-p :w)
	  (keyboard-down-p :a))
     :upleft)
    ((keyboard-down-p :a) :left)
    ((keyboard-down-p :d) :right)
    ((keyboard-down-p :w) :up)
    ((keyboard-down-p :s) :down)
    (t nil)))

(defmethod update ((player player))
  (with-slots (speed) player
    (awhen (or (arrow-keys-direction)
	       (wasd-direction)) 
	(move player (direction-heading it)  speed))))

(defclass shooter (buffer)
   ((player :initform (make-instance 'player))
    (background-color :initform "black")
    (width :initform *width*)
    (height :initform *height*)))

(defmethod initialize-instance :after ((shooter shooter) &key)
   (bind-event shooter '(:r :control) 'start-game))

(defmethod start-game ((shooter shooter))
   (with-slots (player) shooter
     (with-buffer shooter
       (insert player)
       (move-to player 80 280))))

(defun shooter ()
  ;; Configure the screen dimensions
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  ;; Allow resizing of window and scaling
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (with-session
    (open-project :game)
    ;; this indexes everything defined with DEFRESOURCE
    (let ((shooter (make-instance 'shooter)))
      ;; start the buffer running
      (switch-to-buffer shooter)
      (start-game shooter))))
