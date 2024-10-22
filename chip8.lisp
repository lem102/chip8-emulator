(defpackage :chip8
  (:use :cl)
  (:export #:main))

(in-package :chip8)

                                      ; variable registers

(defparameter registers (make-array '(16) :initial-element 0 :adjustable nil)
  "16 8-bit registers, referred to as V0 through VF.")

(defun vr (x)
  "Get the value of the variable register X."
  (aref registers x))

(defun (setf vr) (value x)
  "Set the value of the variable register X."
  (setf (aref registers x) value))

                                      ; hardware state

(defparameter stack '())

(defparameter program-counter #x200
  "Index of the current instruction in memory.")

(defun increment-program-counter ()
  (setq program-counter (+ program-counter 2))
  (when (>= program-counter +memory-size+)
    (setq program-counter (- program-counter +memory-size+))))

(defparameter *index-register* 0)

(defparameter *delay-timer* 0)

(defparameter *sound-timer* 0)

                                      ; memory

(defconstant +memory-size+ 4096
  "Size of memory in bytes.")

(defparameter *memory* #())

(defparameter *font* #(#xF0 #x90 #x90 #x90 #xF0 ;; 0
                       #x20 #x60 #x20 #x20 #x70 ;; 1
                       #xF0 #x10 #xF0 #x80 #xF0 ;; 2
                       #xF0 #x10 #xF0 #x10 #xF0 ;; 3
                       #x90 #x90 #xF0 #x10 #x10 ;; 4
                       #xF0 #x80 #xF0 #x10 #xF0 ;; 5
                       #xF0 #x80 #xF0 #x90 #xF0 ;; 6
                       #xF0 #x10 #x20 #x40 #x40 ;; 7
                       #xF0 #x90 #xF0 #x90 #xF0 ;; 8
                       #xF0 #x90 #xF0 #x10 #xF0 ;; 9
                       #xF0 #x90 #xF0 #x90 #x90 ;; A
                       #xE0 #x90 #xE0 #x90 #xE0 ;; B
                       #xF0 #x80 #x80 #x80 #xF0 ;; C
                       #xE0 #x90 #x90 #x90 #xE0 ;; D
                       #xF0 #x80 #xF0 #x80 #xF0 ;; E
                       #xF0 #x80 #xF0 #x80 #x80 ;; F
                       ))

(defparameter *font-address* #x50
  "The byte of memory in which the font sprites begin.")

                                      ; keypad

(defparameter *keypad* nil
  "Array representing the state of the keypad.
Each element can either be `:pressed' or `:released'.")

(defparameter *wait-for-keypress* nil
  "If t, wait until a key is pressed.")

(defun validate-key (key)
  (unless (and (integerp key)
               (< key 16)
               (>= key 0))
    (error (format nil "Invalid key passed: ~a" key))))

(defun key (key)
  (validate-key key)
  (aref *keypad* key))

(defun (setf key) (key-state key)
  (validate-key key)
  (unless (member key-state '(:pressed :released))
    (error "Invalid key-state passed"))
  (when *wait-for-keypress*
    (setq *wait-for-keypress* nil))
  (setf (aref *keypad* key) key-state))

                                      ; instruction helpers

(defun instruction-type (instruction)
  "Read the first nibble of INSTRUCTION."
  (floor (/ instruction #x1000)))

(defun instruction-x (instruction)
  "Read the second nibble of INSTRUCTION."
  (floor (mod (/ instruction #x100) #x10)))

(defun instruction-y (instruction)
  "Read the third nibble of INSTRUCTION."
  (floor (mod (/ instruction #x10) #x10)))

(defun instruction-n (instruction)
  "Read the fourth nibble of INSTRUCTION."
  (floor (mod instruction #x10)))

(defun instruction-nn (instruction)
  "Read the second and third nibbles of INSTRUCTION."
  (floor (mod instruction #x100)))

(defun instruction-nnn (instruction)
  "Read the second, third and fourth nibbles of INSTRUCTION."
  (floor (mod instruction #x1000)))

                                      ; fetch decode execute loop

(defun read-chip8-program (filename)
  (coerce (with-open-file (stream filename :element-type '(unsigned-byte 8))
            (loop for byte = (read-byte stream nil)
                  while byte
                  collect byte))
          'vector))

(defun fetch ()
  (unless *wait-for-keypress*
    (prog1
        (+ (* #x100 (aref *memory* program-counter))
           (aref *memory* (1+ program-counter)))
      (increment-program-counter))))

(defun execute (instruction)
  (if *wait-for-keypress*
      (let ((pressed-key (position :pressed *keypad*)))
        (when pressed-key
          (setf (vr *wait-for-keypress*) pressed-key)
          (setf *wait-for-keypress* nil)))
      (let* ((type (instruction-type instruction))
             (x (instruction-x instruction))
             (y (instruction-y instruction))
             (n (instruction-n instruction))
             (nn (instruction-nn instruction))
             (nnn (instruction-nnn instruction)))
        (cond ((= instruction #x00E0) (display:clear))
              ((= instruction #x00EE) (return-from-subroutine))
              ((= type #x0) (warn "jump to native assembler subroutine is not implemented"))
              ((= type #x1) (jump nnn))
              ((= type #x2) (call-subroutine nnn))
              ((= type #x3) (skip-if-vx-=-nn x nn))
              ((= type #x4) (skip-if-vx-!=-nn x nn))
              ((= type #x5) (skip-if-vx-=-vy x y))
              ((= type #x6) (set-register x nn))
              ((= type #x7) (nn-+-vx x nn))
              ((= type #x8)
               (cond ((= n #x0) (set-vx-to-vy x y))
                     ((= n #x1) (vx-or-vy x y))
                     ((= n #x2) (vx-and-vy x y))
                     ((= n #x3) (vx-xor-vy x y))
                     ((= n #x4) (vx-+-vy x y))
                     ((= n #x5) (vx---vy x y))
                     ((= n #x6) (vy>> x y))
                     ((= n #x7) (vy---vx x y))
                     ((= n #xE) (vy<< x y))))
              ((= type #x9) (skip-if-vx-!=-vy x y))
              ((= type #xA) (set-index-register nnn))
              ((= type #xB) (jump-v0-+-nnn nnn))
              ((= type #xC) (set-vx-to-random x nn))
              ((= type #xD) (draw x y n))
              ((= type #xE)
               (cond ((= nn #x9E) (skip-if-pressed x))
                     ((= nn #xA1) (skip-if-released x))))
              ((= type #xF)
               (cond ((= nn #x07) (set-vx-to-delay-timer x))
                     ((= nn #x0A) (wait-for-keypress x))
                     ((= nn #x15) (set-delay-timer-to-vx x))
                     ((= nn #x18) (set-sound-timer-to-vx x))
                     ((= nn #x1E) (add-to-index x))
                     ((= nn #x29) (set-index-to-font-sprite x))
                     ((= nn #x33) (write-vx-as-bcd x))
                     ((= nn #x55) (write-bytes x))
                     ((= nn #x65) (read-bytes x))))))))

(defun set-vx-to-random (x nn)
  (setf (vr x) (logand (random 256) nn)))

(defun set-index-to-font-sprite (x)
  "Set `*index-register*' to the start of the font sprite for vX."
  (setq *index-register* (+ *font-address* (* 8 (vr x)))))

(defun jump-v0-+-nnn (nnn)
  "Jump to NNN to v0."
  (setq program-counter (+ nnn (vr 0))))

(defun wait-for-keypress (x)
  "Wait until a key is pressed, then store the key in vX."
  (setf *wait-for-keypress* x))

(defun set-vx-to-delay-timer (x)
  "Set vX to the delay timer."
  (setf (vr x) *delay-timer*))

(defun set-delay-timer-to-vx (x)
  "Set the delay timer to vX."
  (setq *delay-timer* (vr x)))

(defun set-sound-timer-to-vx (x)
  "Set the sound timer to vX."
  (setq *sound-timer* (vr x)))

(defun write-vx-as-bcd (x)
  "Write vX as binary coded decimal to the bytes pointed to by the
`*index-register*'."
  (let ((vx (vr x)))
    (setf (aref *memory* *index-register*) (floor (/ vx 100))) 
    (setf (aref *memory* (1+ *index-register*)) (floor (mod (/ vx 10) 10)))
    (setf (aref *memory* (+ 2 *index-register*)) (floor (mod vx 10)))))

(defun set-index-register (nnn)
  "Set the `*index-register*' to NNN."
  (setf *index-register* nnn))

(defun skip-if-vx-!=-vy (x y)
  "Skip the next instruction if vX does not equal vY."
  (unless (= (vr x) (vr y))
    (increment-program-counter)))

(defun vy---vx (x y)
  "Subtract vX from vY, store the result in vX.
Set vF to 0 if there is an underflow, 1 otherwise."
  (let ((underflow (< (vr y) (vr x)))
        (result (- (vr y) (vr x))))
    (setf (vr x) (if underflow (+ 256 result) result))
    (setf (vr #xF) (if underflow 0 1))))

(defun vy<< (x y)
  "Set vX to vY and shift vX one bit to the left.
Afterwards, set vF to the bit shifted out."
  (let ((shifted-out-bit (if (> (vr y) 127) 1 0)))
    (setf (vr x) (mod (ash (vr y) 1) 256))
    (setf (vr #xF) shifted-out-bit)))

(defun vy>> (x y)
  "Set vX to vY and shift vX one bit to the right.
Afterwards, set vF to the bit shifted out."
  (let ((result (ash (vr y) -1))
        (shifted-bit (mod (vr y) 2)))
    (setf (vr x) result)
    (setf (vr #xF) shifted-bit)))

(defun vx-+-vy (x y)
  "Add vX and vY, store the result in vX.
Set vF to 1 if there is an overflow, 0 otherwise."
  (let ((result (+ (vr x) (vr y))))
    (setf (vr x) (mod result 256))
    (setf (vr #xF) (if (> result 255) 1 0))))

(defun vx---vy (x y)
  "Subtract vY from vX, store the result in vX.
Set vF to 0 if there is an underflow, 1 otherwise."
  (let ((underflow (< (vr x) (vr y)))
        (result (- (vr x) (vr y))))
    (setf (vr x) (if underflow (+ 256 result) result))
    (setf (vr #xF) (if underflow 0 1))))

(defun vx-xor-vy (x y)
  "Set vX to the result of bitwise vX XOR vY."
  (setf (vr x) (logxor (vr x) (vr y)))
  (setf (vr #xF) 0))

(defun vx-and-vy (x y)
  "Set vX to the result of bitwise vX AND vY."
  (setf (vr x) (logand (vr x) (vr y)))
  (setf (vr #xF) 0))

(defun vx-or-vy (x y)
  "Set vX to the result of bitwise vX OR vY."
  (setf (vr x) (logior (vr x) (vr y)))
  (setf (vr #xF) 0))

(defun set-vx-to-vy (x y)
  "Set vX to vY"
  (setf (vr x) (vr y)))

(defun nn-+-vx (x nn)
  "Add NN to vX. Do not set the carry bit."
  (let ((result (+ (vr x) nn)))
    (setf (vr x) (mod result 256))))

(defun skip-if-vx-=-nn (x nn)
  "If vX = NN, skip the next instruction."
  (when (= nn (vr x))
    (increment-program-counter)))

(defun skip-if-vx-!=-nn (x nn)
  "If vX != NN, skip the next instruction."
  (unless (= nn (vr x))
    (increment-program-counter)))

(defun skip-if-vx-=-vy (x y)
  "If vX = vY, skip the next instruction."
  (when (= (vr x) (vr y))
    (increment-program-counter)))

(defun return-from-subroutine ()
  "Return from current subroutine.
Pop the `stack', set `program-counter' to the returned value."
  (setf program-counter (pop stack)))

(defun call-subroutine (nnn)
  "Call subroutine that starts at NNN.
Push `program-counter' to the `stack' then set `program-counter' to
NNN."
  (push program-counter stack)
  (setf program-counter nnn))

(defun jump (nnn)
  "Jump to NNN."
  (setq program-counter nnn))

(defun read-bytes (x)
  "Read X bytes from memory starting at the value of the
`*index-register*', writing them to the registers v0 to vX."
  (loop for index from 0 to x
        do (setf (vr index) (aref *memory* *index-register*))
           (incf *index-register*)))

(defun write-bytes (x)
  "Write X bytes to memory starting at the value of the
`*index-register*', reading them from the registers v0 to vX."
  (loop for index from 0 to x
        do (setf (aref *memory* *index-register*) (vr index))
           (incf *index-register*)))

(defun add-to-index (x)
  "Add vX to `*index-register*'."
  (setf *index-register* (+ (vr x) *index-register*)))

(defun set-register (x nn)
  "Set register vX to NN."
  (setf (vr x) nn))

(defun skip-if-pressed (x)
  "Skip next instruction if key in vX is pressed."
  (when (eq :pressed (key (mod (vr x) #x10)))
    (increment-program-counter)))

(defun skip-if-released (x)
  "Skip next instruction if key in the lower nibble of vX is released."
  (when (eq :released (key (mod (vr x) #x10)))
    (increment-program-counter)))

(defun draw (x y n)
  "Draw the sprite starting at `*index-register*' at (vX, vY). If the
display returns true, set vF to 1, otherwise 0."
  (setf (vr #xF)
        (if (display:draw (subseq *memory* *index-register* (+ *index-register* n))
                          (vr x)
                          (vr y))
            1
            0)))

(defun initialise (program)
  "Initialise the chip8 hardware."
  (setq program-counter #x200)
  (setq *memory* (let ((array (make-array (list +memory-size+) :initial-element 0 :adjustable nil)))
                   (dotimes (i 80)
                     (setf (aref array (+ *font-address* i))
                           (aref *font* i)))
                   (dotimes (i (length program))
                     (setf (aref array (+ #x200 i))
                           (aref program i)))
                   array))
  (setq *keypad* (make-array '(16) :initial-element :released :adjustable nil))
  (setq registers (make-array '(16) :initial-element 0 :adjustable nil))
  (setq stack nil)
  (setq *index-register* 0)
  (setq *delay-timer* 0)
  (setq *sound-timer* 0)
  (display:clear)
  (setq *wait-for-keypress* nil))

(defun translate-scancode (scancode)
  (case scancode
    (:scancode-1 #x1)
    (:scancode-2 #x2)
    (:scancode-3 #x3)
    (:scancode-4 #xC)
    (:scancode-q #x4)
    (:scancode-w #x5)
    (:scancode-e #x6)
    (:scancode-r #xD)
    (:scancode-a #x7)
    (:scancode-s #x8)
    (:scancode-d #x9)
    (:scancode-f #xE)
    (:scancode-z #xA)
    (:scancode-x #x0)
    (:scancode-c #xB)
    (:scancode-v #xF)))

(defun main (filename)
  (initialise (read-chip8-program filename))
  
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "chip8" :w 1280 :h 640)
      (sdl2:with-renderer (renderer window)
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (equal (sdl2:scancode keysym) :scancode-escape)
             (sdl2:push-event :quit))
           (let ((key-index (translate-scancode (sdl2:scancode keysym))))
             (when key-index
               (setf (key key-index) :released))))
          (:keydown
           (:keysym keysym)
           (let ((key-index (translate-scancode (sdl2:scancode keysym))))
             (when key-index
               (setf (key key-index) :pressed))))
          (:idle ()
                 (cond ((and (zerop *delay-timer*)
                             (zerop *sound-timer*))
                        (execute (fetch))
                        (display:render renderer))
                       ((not (zerop *delay-timer*))
                        (decf *delay-timer*))
                       (t
                        (decf *sound-timer*)))
                 (sdl2:delay *delay*))
          (:quit () t))))))

(defparameter *delay* 1)

