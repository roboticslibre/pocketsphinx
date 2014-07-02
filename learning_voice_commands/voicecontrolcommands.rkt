;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Racket Script with voice commands for discrete, continuous and cognitive tasks.
;; Xicohtzinco, Mexico. 1st July 2014 
;; 
;; Released under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International 
;; Written by Perez-Xochicale M. A. [https://sites.google.com/site/perezxochicale/]
;;
;; Infos,updates, bug reports, contributions and feedback:
;; https://github.com/LibrERobotics/pocketsphinx
;;
;; To know more about LibrE Robotics, visit our website : 
;; https://sites.google.com/site/librerobotics/
;;
;;
;; NB. If for any circumstances the racket file is not working,
;; it is highly recommended uploading the Standard Firmata from Arduino IDE.
;; I have not known yet what is the main reason of the previous setback.

#lang racket
(require "libreroboticsfirmata.rkt")

(define servopinL 5) ; servo pin Left
(define servopinR 6) ; servo pin Right
(define servopinU 3) ; servo pin for the ULTRASONIC SENSOR


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Freeze Position
(define (stop)
	  (analog-write! servopinL 155)
	  (analog-write! servopinR 153)
)


;;;;;;;;;;;;;;;;;;;;;
;; One second spins
(define (spinright)
	  (analog-write! servopinL 165)
	  (analog-write! servopinR 161)
	  (sleep 1)
	  (stop)
)

(define (spinleft)
	  (analog-write! servopinL 146)
	  (analog-write! servopinR 145)
	  (sleep 1)
	  (stop)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One second moves
(define (moveforward)
	  (analog-write! servopinL 163)
	  (analog-write! servopinR 146)
	  (sleep 1)
	  (stop)
)

(define (moveinreverse)
	  (analog-write! servopinL 148)
	  (analog-write! servopinR 158)
	  (sleep 1)
	  (stop)
)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous spinning
(define (rotateright)
	  (analog-write! servopinL 160)
	  (analog-write! servopinR 156)
)

(define (rotateleft)
	  (analog-write! servopinL 151)
	  (analog-write! servopinR 150)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous moving
(define (goforward)
	  (analog-write! servopinL 163)
	  (analog-write! servopinR 146)
)

(define (goinreverse)
	  (analog-write! servopinL 148)
	  (analog-write! servopinR 158)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servo Ultrasonic Sensor
(define (servoeyes)
	(stop)	;stop the servos for the wheels
	
	(for ([i (in-range 85 45 -3) ])
	     (analog-write! servopinU i)
	     (sleep 0.05)
	)
	(for ([i (in-range 45 125 3) ])
	     (analog-write! servopinU i)
	     (sleep 0.05)
	)
	(for ([i (in-range 125 85 -3) ])
	     (analog-write! servopinU i)
	     (sleep 0.05)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servo Ultrasonic Sensor
(define (servohead)
	(stop)	;stop the servos for the wheels
	
	(for ([i (in-range 180 0 -3) ])
	     (analog-write! servopinU i)
	     (sleep 0.02)
	)

	(for ([i (in-range 0 180 3) ])
	     (analog-write! servopinU i)
	     (sleep 0.02)
	)
	;;center servo position
	(for ([i (in-range 180 85 -3) ])
	     (analog-write! servopinU i)
	     (sleep 0.02)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hi little robot
(define (hlr)
	(moveforward)
	(servoeyes)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; waiting for your commands
(define (waitingmessage)
          (printf "\n")
          (printf "   ...      I am waiting for your voice commands!\n")
          (printf "\n")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Capture words function
;;
(define in (open-input-file "capturedwords"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; FUNCTION process-input string
;;
(define (process-input str)
  (printf "\n------------------------\n")
  (printf "processing input ~a\n" str)
  ;;(set! str (substring str 8)) ;; substrack the blank space after RACKET:_???
  (cond 
  	( (string=? (string-upcase str) "SPIN RIGHT")
          (printf "I'm rotating to the right direction for one second\n")
	  (spinright)
	  (waitingmessage))
        ( (string=? (string-upcase str) "SPIN LEFT")
          (printf "I'm rotating to the left direction for one second\n")
	  (spinleft)
	  (waitingmessage))
  	( (string=? (string-upcase str) "ROTATE RIGHT")
          (printf "I'm rotating to the right direction\n")
	  (rotateright)
	  (waitingmessage))
        ( (string=? (string-upcase str) "ROTATE LEFT")
          (printf "I'm rotating to the left direction\n")
	  (rotateleft)
	  (waitingmessage))


  	( (string=? (string-upcase str) "MOVE FORWARD")
          (printf "I'm moving in forward direction for one second\n")
	  (moveforward)
	  (waitingmessage))
  	( (string=? (string-upcase str) "MOVE IN REVERSE")
          (printf "I'm moving in backward direction for one second\n")
	  (moveinreverse)
	  (waitingmessage))
  	( (string=? (string-upcase str) "GO FORWARD")
          (printf "I'm moving in forward direction\n")
	  (goforward)
	  (waitingmessage))
  	( (string=? (string-upcase str) "GO IN REVERSE")
          (printf "I'm moving in backward direction\n")
	  (goinreverse)
	  (waitingmessage))

        ( (string=? (string-upcase str) "STOP RIGHT THERE")
          (printf "I'm freezing\n")
	  (stop)
	  (waitingmessage))
	  
        ( (string=? (string-upcase str) "HI LITTLE ROBOT")
          (printf "Hello Friend!\n")
	  (hlr)
	  (waitingmessage))

        ( (string=? (string-upcase str) "MOVE YOUR EYES")
          (printf "I am moving my eyes!\n")
	  (servoeyes)
	  (waitingmessage))	  


        ( (string=? (string-upcase str) "MOVE YOUR HEAD")
          (printf "I am moving my head!\n")
	  (servohead)
	  (waitingmessage))	  	  
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;BEGIN OF THE LEARNING ZONE
	  ;http://stackoverflow.com/questions/10467202/writing-out-to-a-file-in-scheme
	  
	  ( (string=? (string-upcase str) "LEARN THIS COMMANDS")
  

	    ;;robotmemory file is cleaned
	    (call-with-output-file "robotmemory" #:exists 'replace
	    (lambda (output-port) (display "" output-port))
	    )  
  
  
	  
	  (printf "\n************************************\n")
	  (printf "************************************\n")
	  (printf "**\n")
          (printf "**     Hi there, I am in the learn mode!\n")
          (printf "**     Please teach me some commands!\n")
          (printf "**\n")
          (printf "\n")
	  
	  (learn-loop)
	  
	  )
	  
	
	  ( (string=? (string-upcase str) "SHOW US WHAT YOU HAVE LEARNT")
          (printf ":::::::::::::::::::::::::::::!  \n")
          (call-with-input-file "robotmemory" read-next-line-iter)
	  (printf ":::::::::::::::::::::::::::::!  \n")
	  (waitingmessage)
	  )	  
	  ;; ENDING OF THE LEARNING ZONE
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  

         (else
         (printf "Sorry I cannot understand: ~a\n" str)
         (waitingmessage)
         (flush-output)
         )
  )
  )

 
	       
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; FUNCTION learn-process-input learnedstring
;;
(define (learn-process-input learnedstring)	
  (printf "\n******************\n")
	  (printf "**\n")
          (printf "**\n")
	  
  (cond
  
	  ( (string=? (string-upcase learnedstring) "MOVE YOUR HEAD")
          
          ;;WRITE/append words into the robotmemory FILE
	  (set! learnedstring (string-append learnedstring "\n")) ;; append a command for a new line
	  (call-with-output-file "robotmemory" #:exists 'append
	    (lambda (output-port)
	      (display learnedstring output-port)))
	      
	  (printf "**  ~a" learnedstring) 
	  (printf "**                   ... has been learnt! \n") 
	  (printf "**\n")
	  (printf "**     Teach me!\n")
          (printf "**\n")
	  
	  )  

	  
	  ( (string=? (string-upcase learnedstring) "MOVE YOUR EYES")
          
          ;;WRITE/append words into the robotmemory FILE
	  (set! learnedstring (string-append learnedstring "\n")) ;; append a command for a new line
	  (call-with-output-file "robotmemory" #:exists 'append
	    (lambda (output-port)
	      (display learnedstring output-port)))
	      
	  (printf "**  ~a" learnedstring) 
	  (printf "**                   ... has been learnt! \n") 
	  (printf "**\n")
	  (printf "**     Teach me!\n")
          (printf "**\n")
	  
	  )
	  
	  ( (string=? (string-upcase learnedstring) "SPIN LEFT")
                    ;;WRITE/append words into the robotmemory FILE
	  (set! learnedstring (string-append learnedstring "\n")) ;; append a command for a new line
	  (call-with-output-file "robotmemory" #:exists 'append
	    (lambda (output-port)
	      (display learnedstring output-port)))
	      
	  (printf "**  ~a" learnedstring) 
	  (printf "**                   ... has been learnt! \n") 
	  (printf "**\n")
	  (printf "**     Teach me!\n")
          (printf "**\n")
	  )
	  
	  ( (string=? (string-upcase learnedstring) "MOVE FORWARD")
                    ;;WRITE/append words into the robotmemory FILE
	  (set! learnedstring (string-append learnedstring "\n")) ;; append a command for a new line
	  (call-with-output-file "robotmemory" #:exists 'append
	    (lambda (output-port)
	      (display learnedstring output-port)))
	      
	  (printf "**  ~a" learnedstring) 
	  (printf "**                   ... has been learnt! \n") 
	  (printf "**\n")
	  (printf "**     Teach me!\n")
          (printf "**\n")
	  )
	  
	  ( (string=? (string-upcase learnedstring) "MOVE IN REVERSE")
                    ;;WRITE/append words into the robotmemory FILE
	  (set! learnedstring (string-append learnedstring "\n")) ;; append a command for a new line
	  (call-with-output-file "robotmemory" #:exists 'append
	    (lambda (output-port)
	      (display learnedstring output-port)))
	      
	  (printf "**  ~a" learnedstring) 
	  (printf "**                   ... has been learnt! \n") 
	  (printf "**\n")
	  (printf "**     Teach me!\n")
          (printf "**\n")
	  )	  

	  ( (string=? (string-upcase learnedstring) "SPIN RIGHT")
                    ;;WRITE/append words into the robotmemory FILE
	  (set! learnedstring (string-append learnedstring "\n")) ;; append a command for a new line
	  (call-with-output-file "robotmemory" #:exists 'append
	    (lambda (output-port)
	      (display learnedstring output-port)))
	      
	  (printf "**  ~a" learnedstring) 
	  (printf "**                   ... has been learnt! \n") 
	  (printf "**\n")
	  (printf "**     Teach me!\n")
          (printf "**\n")
	  )	  
	  
  	  ( (string=? (string-upcase learnedstring) "OK THAT IS ENOUGH")
          (printf "** That is just great! \n")
          (printf "**\n")
	  (printf "************************************\n")
	  (printf "************************************\n")
	  (waitingmessage)
	  (read-loop)
	  )	
	  
	 (else
         (printf "Sorry I cannot understand: ~a\n" learnedstring)
         (flush-output)
         )  
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;FUNCTION: READ A FILE LINE BY LINE
;;http://rosettacode.org/wiki/Read_a_file_line_by_line#Racket
  
(define (read-next-line-iter file)
	   (let ((line (read-line file)))
	     (unless (eof-object? line)
	       ;(display line)
	       (printf ":::   ~a " line)
	  
	(cond
	( (string=? (string-upcase line) "MOVE YOUR HEAD")  (servohead))
	( (string=? (string-upcase line) "MOVE YOUR EYES")  (servoeyes))
	( (string=? (string-upcase line) "SPIN LEFT")       (spinleft))
	( (string=? (string-upcase line) "SPIN RIGHT")      (spinright))
	( (string=? (string-upcase line) "MOVE FORWARD")      (moveforward))
	( (string=? (string-upcase line) "MOVE IN REVERSE")      (moveinreverse))
	(else (read-loop) )  
	)
	              
      (newline) ;; read a new line
      (read-next-line-iter file)))       
)
	      
  
;;;;;;;;;;;;;;;;;;;;;;;  
;;READ LOOP  
(define (read-loop)
  (define str (read-line in))
  (unless (eof-object? str) ;; if eof-object? is true do process-input str
               (process-input str)
    )
  (read-loop))
  
;;;;;;;;;;;;;;;;;;;;;;;  
;;LEARN LOOP  
(define (learn-loop)
  (define learnedstring (read-line in))
  (unless (eof-object? learnedstring)
               (learn-process-input learnedstring)
    )
  (learn-loop))
  

  
  
;;;;;;;;;;;;;;;;;;;;;;;  
;;INITIALIZATION SETUP
(define (initialization)
  
  ;setup the port and pin mode 
  (open-firmata "/dev/ttyACM0")
  (set-pin-mode! servopinL SERVO_MODE)
  (set-pin-mode! servopinR SERVO_MODE)
  (set-pin-mode! servopinU SERVO_MODE)

  ;;robotmemory file is cleaned
  (call-with-output-file "robotmemory" #:exists 'replace
  (lambda (output-port) (display ' output-port)))  
  
  
  ;;initialization movements
  (servohead)
  (stop)

  ;;go to read loop
  (read-loop)
  )

(initialization)
