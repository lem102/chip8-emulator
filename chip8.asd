(asdf:defsystem "chip8"
  :depends-on ("sdl2")
  :components ((:file "display")
               (:file "chip8")))
