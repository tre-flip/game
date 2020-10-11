;;;; package.lisp

(defpackage #:game.globals
  (:use #:cl #:xelf #:anaphora)
  (:export #:*unit*
	   #:units
	   #:*width*
	   #:*height*)
  (:documentation "Global variables and utils."))

(defpackage #:game.input
  (:use #:cl #:xelf #:anaphora)
  (:export #:keyboard-direction)
  (:documentation "Functions for getting input from mouse and keyboard."))

(defpackage #:game.classes
  (:use #:cl #:xelf #:anaphora #:game.input #:game.globals)
  (:documentation "Classes and methods for in-game objects."))

(defpackage #:game
  (:use #:cl #:xelf #:anaphora)
  (:documentation "Top-level interface."))
