;;;; input.lisp

(in-package :game)

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

(defun keyboard-direction ()
  (or (wasd-direction)
      (arrow-keys-direction)))
