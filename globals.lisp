;;;; globals.lisp

(in-package :game.globals)

(defparameter *unit* 16
  ;; used to make shapes consistent
  "One unit is 16 pixels.")

(defun units (n)
  (* *unit* n))

(defparameter *width* 640)
(defparameter *height* 480)
