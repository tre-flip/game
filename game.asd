;;;; game.asd

(asdf:defsystem #:game
  :description "Describe game here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("xelf" "anaphora" "trivia" "arrow-macros")
  :components ((:file "package")
               (:file "game")))
