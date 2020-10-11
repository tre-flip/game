;;;; classes.lisp

(in-package :game)


;;;;;;;;;;;;;;;;
;; INTERFACES ;;
;;;;;;;;;;;;;;;;

(defclass destructible ()
  ())

(defclass one-shot-killer ()
  ())

(defgeneric bounce (objcect)
  (:documentation "A method for bouncing objects."))

;;;;;;;;;;;;;;
;; BAD GUYS ;;
;;;;;;;;;;;;;;

(defclass bot (destructible one-shot-killer node)
  ((height :initform (units 3))
   (width :initform (units 3))
   (color :initform "red")
   (speed :initform 1)
   (heading :initform (direction-heading :left))))

(defmethod update ((bot bot))
  (with-slots (speed heading) bot
      (move bot heading speed)))


;;;;;;;;;;;;
;; BULLET ;;
;;;;;;;;;;;;

(defclass bullet (node)
  ((height :initform (units 1))
   (width :initform (units 1))
   (color :initform "white")
   (speed :initform (units 1))
   (heading :initform (direction-heading :right))))

(defmethod update :after ((bullet bullet))
  (with-slots (speed heading) bullet
    (move bullet heading speed)))

;;;;;;;;;;;;
;; PLAYER ;;
;;;;;;;;;;;;

(defclass player (destructible node)
  ((height :initform (units 2))
   (width :initform (units 2))
   (color :initform "white")
   (speed :initform 5)
   (current-speed :initform 7)
   (heading :initform -1.5))
  (:documentation "Represents a player object."))

(defmethod update ((player player))
  (with-slots (speed current-speed heading) player
    ;; controlled movement -> write a separate function for movement and call it here
    (aif (keyboard-direction)
      (progn
	(setf heading  (direction-heading it))
	(setf current-speed speed)
	(move player (direction-heading it) speed))
      ;; if no key pressed -> move smoothly
      (progn
	(when (> current-speed 0)
	  (decf current-speed))
	(move player heading current-speed)))))

;;;;;;;;;;
;; WALL ;;
;;;;;;;;;;

(defclass wall (node)
  ((color :initform "black"))
  (:documentation "A wall doesn't let objects move away from the screen."))

(defun make-wall (x y width height)
   (let ((wall (make-instance 'wall)))
     (resize wall width height)
     (move-to wall x y)
     wall))

(defun make-border (x y width height)
  "This function MAKE-BORDER returns a buffer with four walls."
   (let ((left x)
 	(top y)
 	(right (+ x width))
 	(bottom (+ y height)))
     (with-new-buffer
       ;; top wall
       (insert (make-wall left top (- right left) (units 1)))
       ;; bottom wall
       (insert (make-wall left bottom (- right left (units -1)) (units 1)))
       ;; left wall
       (insert (make-wall left top (units 1) (- bottom top)))
       ;; right wall
       (insert (make-wall right top (units 1) (- bottom top (units -1))))
       ;; send it all back
       (current-buffer))))


;;;;;;;;;;;;;;;;;
;; GAME-BUFFER ;;
;;;;;;;;;;;;;;;;;

(defclass game-buffer (buffer)
  ((bot-cooldown :initform 240)
   (bot-cooldown-max :initform 180)
   (fire-cooldown :initform 240)
   (player :initform (make-instance 'player))
   (background-color :initform "black")
   (width :initform *width*)
   (height :initform *height*))
  (:documentation "Main game buffer."))

(defmethod update :after ((game-buffer game-buffer))
  (with-slots (bot-cooldown-max bot-cooldown fire-cooldown player) game-buffer
    (if (> fire-cooldown 0)
	(decf fire-cooldown)
	(progn
	  (insert (make-instance 'bullet)
		  (x player)
		  (y player))
	  (setf fire-cooldown bot-cooldown-max)))
    (if (> bot-cooldown 0)
	(decf bot-cooldown)
	(progn
	  (insert (make-instance 'bot)
		  *width*
		  (random *height*))
	  (setf bot-cooldown 180)
	  (decf bot-cooldown-max 3)))))

;; initialisation of the main game-buffer
(defmethod initialize-instance :after ((game-buffer game-buffer) &key)
   (bind-event game-buffer '(:r :control) 'start-game))

(defmethod start-game ((game-buffer game-buffer))
   (with-slots (player) game-buffer
     (with-buffer game-buffer
       (insert player)
       (paste-from game-buffer (make-border 0 0 (- *width* (units 1)) (- *height* (units 1))))
       (move-to player 80 280))))

;;;;;;;;;;;;;;;;
;; COLLISIONS ;;
;;;;;;;;;;;;;;;;

(defmethod collide ((player player) (wall wall))
  (with-slots (current-speed heading speed) player
    (move player (opposite-heading heading) speed)
    (setf heading (opposite-heading heading))))

(defmethod collide :after ((destructible destructible) (killer one-shot-killer))
  (destroy destructible))

(defmethod collide :after ((bullet bullet) (destructible destructible))
  (destroy bullet)
  (destroy destructible))

(defmethod collide :after ((destructible destructible) (bullet bullet))
  (destroy bullet)
  (destroy destructible))
