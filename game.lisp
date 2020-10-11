;;;; game.lisp

(in-package #:game)

(defun main ()
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
