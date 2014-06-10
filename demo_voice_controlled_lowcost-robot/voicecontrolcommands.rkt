;;;;;;;;;;;;;;;;;;;;;
;; 
;; If for any circumstances the racket file is not working,
;; it is highly recommended uploading the Standard Firmata from Arduino IDE
;; I have not known what is the main reason of the previos setback.
;;

#lang racket
(require "libreroboticsfirmata.rkt")

(define servopin3 3)
(define servopin6 6)

;;;;;;;;;;;;;;;;;;;;;
;; One second spins
(define (moveright)
	  (analog-write! servopin3 158)
	  (analog-write! servopin6 161)
	  (sleep 1)
	  (stop)
)

(define (moveleft)
	  (analog-write! servopin3 146)
	  (analog-write! servopin6 147)
	  (sleep 1)
	  (stop)
)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous spinning
(define (rotateright)
	  (analog-write! servopin3 158)
	  (analog-write! servopin6 161)
)

(define (rotateleft)
	  (analog-write! servopin3 146)
	  (analog-write! servopin6 147)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One second moves
(define (moveforward)
	  (analog-write! servopin3 156)
	  (analog-write! servopin6 149)
	  (sleep 1)
	  (stop)
)

(define (movebackward)
	  (analog-write! servopin3 148)
	  (analog-write! servopin6 158)
	  (sleep 1)
	  (stop)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous moving
(define (goforward)
	  (analog-write! servopin3 156)
	  (analog-write! servopin6 149)
)

(define (gobackward)
	  (analog-write! servopin3 148)
	  (analog-write! servopin6 158)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Freeze Position
(define (stop)
	  (analog-write! servopin3 153)
	  (analog-write! servopin6 154)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hi little robot
(define (hlr)
	(moveright)
	(moveleft)
	(moveforward)
	(movebackward)
	(stop)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; waiting for your commands
(define (pw)
          (printf "\n")
          (printf "   ...      I am waiting for your voice commands!\n")
          (printf "\n")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture words function
(define in (open-input-file "capturedwords"))

(define (process-input str)
  (printf "\n------------------------\n")
  (printf "processing input ~a\n" str)
  (set! str (substring str 8))
  (cond 


  	( (string=? (string-upcase str) "MOVE RIGHT")
          (printf "I'm rotating to the right direction for one second\n")
	  (moveright)
	  (pw))
        ( (string=? (string-upcase str) "MOVE LEFT")
          (printf "I'm rotating to the left direction for one second\n")
	  (moveleft)
	  (pw))
  	( (string=? (string-upcase str) "ROTATE RIGHT")
          (printf "I'm rotating to the right direction\n")
	  (rotateright)
	  (pw))
        ( (string=? (string-upcase str) "ROTATE LEFT")
          (printf "I'm rotating to the left direction\n")
	  (rotateleft)
	  (pw))


  	( (string=? (string-upcase str) "MOVE FORWARD")
          (printf "I'm moving in forward direction for one second\n")
	  (moveforward)
	  (pw))
  	( (string=? (string-upcase str) "MOVE BACKWARD")
          (printf "I'm moving in backward direction for one second\n")
	  (movebackward)
	  (pw))
  	( (string=? (string-upcase str) "GO FORWARD")
          (printf "I'm moving in forward direction\n")
	  (goforward)
	  (pw))
  	( (string=? (string-upcase str) "GO BACKWARD")
          (printf "I'm moving in backward direction\n")
	  (gobackward)
	  (pw))


        ( (string=? (string-upcase str) "STOP RIGHT THERE")
          (printf "I'm freezing\n")
	  (stop)
	  (pw))
        ( (string=? (string-upcase str) "HI LITTLE ROBOT")
          (printf "Hello Friend!\n")
	  (hlr)
	  (pw))


        (else
         (printf "Sorry I cannot understand: ~a\n" str)
         (flush-output)
         )
  )
  )

(define (read-loop)
  (define str (read-line in))
  (unless (eof-object? str)
               (process-input str)
    )
  (read-loop))

(define (start-everything)
  (open-firmata "/dev/ttyACM0")
  (set-pin-mode! servopin3 SERVO_MODE)
  (set-pin-mode! servopin6 SERVO_MODE)
	(moveright)
	(moveleft)
	(moveforward)
	(movebackward)
	(stop)

  (read-loop)
  )

(start-everything)