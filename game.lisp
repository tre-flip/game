;;;; game.lisp

(in-package #:game)

(defun main ()
  ;; Configure the screen dimensions
  (setf *screen-height* *height*)
  (setf *screen-width* *width*)
  ;; Allow resizing of window and scaling
  (setf *resizable* t)
  (setf *scale-output-to-window* t)
  (setf *frame-rate* 60)
  (with-session
    (open-project :game)
    ;; this indexes everything defined with DEFRESOURCE
    (let ((game-buffer (make-instance 'game-buffer)))
      ;; start the buffer running
      (switch-to-buffer game-buffer)
      (start-game game-buffer))))
