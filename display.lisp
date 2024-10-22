(defpackage :display
  (:use :cl)
  (:export #:clear
           #:draw
           #:render))

(in-package :display)

(defconstant display-width 64)
(defconstant display-height 32)

(defun create-blank-display ()
  (make-array (list display-width display-height) :initial-element nil :adjustable nil))

(defparameter display (create-blank-display)
  "Representation of the display, 64 by 32 pixels, each pixel can either be on or off (t or nil).")

(defun clear ()
  (setq display (create-blank-display)))

(defun draw (sprite-data x y)
  "Draw the SPRITE-DATA to the screen at X and Y.

SPRITE-DATA should be a sequence of bytes. Each byte corresponds to
one row of a sprite.

Returns t if a pixel was erased, nil otherwise."
  (let (pixel-erased)
    (loop for sprite-row in (coerce sprite-data 'list)
          for row from (mod y display-height) below 32
          do (loop for sprite-pixel in (map 'list
                                            (lambda (char)
                                              (= 1 (digit-char-p char)))
                                            (format nil "~8,'0b" sprite-row))
                   for column from (mod x display-width) below 64
                   do (let ((display-pixel (aref display column row))
                            (new-display-pixel (not (equal sprite-pixel
                                                           (aref display
                                                                 column
                                                                 row)))))
                        (unless pixel-erased
                          (setq pixel-erased (and display-pixel (not new-display-pixel))))
                        (setf (aref display column row)
                              new-display-pixel))))
    pixel-erased))

(defun render (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)
  
  (sdl2:set-render-draw-color renderer 255 255 255 255)

  (dotimes (y 32)
    (dotimes (x 64)
      (when (aref display x y)
        (sdl2:render-fill-rect renderer (sdl2:make-rect (* x 20) (* y 20) 20 20)))))

  (sdl2:render-present renderer))
