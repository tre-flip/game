;;;; classes.lisp

(in-package :game.classes)

(defclass player (node)
   ((height :initform (units 3))
    (width :initform (units 3))
    (color :initform "cyan")
    (speed :initform 7))
  (:documentation "Represents a player object."))

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
