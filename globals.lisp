;;;; globals.lisp

(in-package :game)

(defparameter *unit* 4
  ;; used to make shapes consistent
  "One unit is 16 pixels.")

(defun units (n)
  (* *unit* n))

(defparameter *width* 640)
(defparameter *height* 480)
