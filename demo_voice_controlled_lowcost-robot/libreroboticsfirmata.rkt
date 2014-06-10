#lang racket
;*----------------------------------*
;*        >>>  Firmata  <<<         *
;*   >>>  DrRacket Interface  <<<   *
;*                                  *
;*       Christophe Scholliers      *
;*        cfscholl@vub.ac.be        * 
;*    SOFT Software Languages Lab   *
;*               2011               *
;*----------------------------------*

;ACKNOWLEDGEMENTS
;Thanks to the work and advice of Franco Raimondi,
;we have successfully created a useful version that 
;works with our hacked servos

;MODIFICATIONS
;Added Line 33	 analog-write! ;; LINE ADDED SO AS TO WORK WITH THE SERVO
;Added Lines 230-237 

;TODO
;learn more about racked programming 

;SOURCE
;http://jura.mdx.ac.uk/mdxracket/index.php/Firmata_rkt_code
;
;;;;;June2014@LibreRobotics 



; ; updates - bob fields - 26.10.12
; Fixed but in set-pin! and clear-pin! where new state wasn't being saved in DIGITAL-IO-PINS
; Use  SET-PIN-MODE in set-pin-mode! instead of literal value.

(provide is-pin-set?
         is-arduino-pin-set?
         set-pin!
         clear-pin!
         set-pin-mode!
         report-analog-pin!
         report-digital-port!
         set-arduino-pin!
         clear-arduino-pin!
         read-analog-pin
         request-version
         open-firmata
         close-firmata
         on-button-pressed
	 analog-write! ;; LINE ADDED SO AS TO WORK WITH THE SERVO
         INPUT_MODE
         OUTPUT_MODE
         ANALOG_MODE
         PWM_MODE
         SERVO_MODE
         ON
         OFF)

; bit-operations
(require file/sha1)
; Needed for setting the terminal baudrate
(require racket/system)
; Needed for keeping track of the current pins
(require rnrs/bytevectors-6)

;----------------------------------------------------------------------
; Firmata Protocol Constants
;----------------------------------------------------------------------
(define DIGITAL-MESSAGE  #x90) ;;send data for a digital port
(define ANALOG-MESSAGE   #xE0) ;;send data for an analog pin (or PWM)
(define REPORT-ANALOG    #xC0) ;;enable analog input by pin #
(define REPORT-DIGITAL   #xD0) ;;enable digital input by port
(define SET-PIN-MODE     #xF4) ;;set a pin to INPUT/OUTPUT/PWM/etc
(define REPORT-VERSION   #xF9) ;;report firmware version
(define SYSTEM-RESET     #xFF) ;;reset from MIDI
(define START-SYSEX      #xF0) ;;start a MIDI SysEx message
(define END-SYSEX        #xF7) ;;end a MIDI SysEx message

(define INPUT_MODE          0)
(define OUTPUT_MODE         1)
(define ANALOG_MODE         2)
(define PWM_MODE            3)
(define SERVO_MODE          4)
(define ON                  1)
(define OFF                 0)

(define BAUDRATE        57600)
(define ANALOG-PINS        16) ;;;Firmata supports up to 16 analog pins
(define DIGITAL-PORTS      16) ;;;Firmata supports up to 16 digital ports

;----------------------------------------------------------------------
; Module constants to keep track of the input and output ports and
; the thread that is started for reading, these constants should not
; leak to the user of the library.
;----------------------------------------------------------------------
(define in                null) 
(define out               null)
(define read-thread       null)
(define registrations      '())

;----------------------------------------------------------------------
; Datastructures to keep track of the pins
;----------------------------------------------------------------------
(define ANALOG-IO-PINS (make-vector ANALOG-PINS))

;Every port is configured as a byte
(define DIGITAL-IO-PINS (make-bytevector DIGITAL-PORTS))

;----------------------------------------------------------------------
; Abstractions to update the internal datastructures
;----------------------------------------------------------------------
(define (update-analog-pin! pin value)
  (vector-set! ANALOG-IO-PINS pin value))

;Used by the programmer to read the latest value of an analog pin
(define (read-analog-pin pin)
  (vector-ref ANALOG-IO-PINS pin))

;Internal function to update the DIGITAL IO PINS
(define (update-digital-port! port value)
  (when (<= value 255)
    (for-each (lambda (p)
                (let ((pin (car p))
                      (f (cdr p)))
                  (when (and 
                         ;it is the same port
                         (= port (quotient pin 8))
                         ;it is currently not set
                         (not (is-arduino-pin-set? pin))
                         ;in the update it is set
                         (is-bit-set? (remainder pin 8) value))
                    (f))))
              registrations)
    (bytevector-u8-set! DIGITAL-IO-PINS port value)))

(define (is-bit-set? bit value)
  (> (bitwise-and 
      value
     ;Create the number where only the pin to be set is 1 = 1 << pin
       (bitwise-and 
        (arithmetic-shift 1 bit)
        #xFF)) 0))

;Used by the programmer to verify that a pin is set on a certain port
(define (is-pin-set? port pin)
   (is-bit-set? pin (bytevector-u8-ref DIGITAL-IO-PINS port)))

(define (read-loop)
  (process-input (read-byte in))
  (read-loop))

;TODO implement sysex messages
(define (process-input data)
  (cond
    ((= (bitwise-and data #xF0) ANALOG-MESSAGE)
     (let ((lsb (read-byte in))
           (msb (read-byte in))
           (analog-pin (bitwise-and data #xF)))
       (update-analog-pin! analog-pin (bitwise-ior (arithmetic-shift msb 8) lsb))))
    
    ((= (bitwise-and data #xF0) DIGITAL-MESSAGE)
     (let ((lsb (read-byte in))
           (msb (read-byte in))
           (port (bitwise-and data #xF)))
       (update-digital-port! port (bitwise-ior (arithmetic-shift msb 8) lsb))))
    
    ((= data REPORT-VERSION) 
     (let ((major (read-byte in))
           (minor (read-byte in)))
       (printf "FIRMATA VERSION DETECTED ~a.~a"  major minor)))))

(define (close-firmata) 
  (when (not (null? read-thread)) 
    (kill-thread read-thread)
    (set! read-thread null)
    (close-input-port in)
    (close-output-port out)
    (set! in null)
    (set! out null)
    (printf "DrRacket-Firmata closed .... \n")))
 
(define (open-firmata port-name)
  (set! out (open-output-file port-name #:mode 'binary #:exists 'append))
  (set! in  (open-input-file  port-name #:mode 'binary))
  (sleep 3)
;;  (if (system (string-append  "stty -f " port-name " 57600 cs8 cread clocal"))
;; Franco 130424: invalid argument -f when using RPi
  (if (system (string-append  "stty -F " port-name " 57600 cs8 cread clocal"))
      (begin 
        (set! read-thread (thread (lambda ()  (read-loop))))
        #t)
      (error "Failed to open the connection with " port-name " verify if your microcontroller is plugged in correctly")))
 
;----------------------------------------------------------------------
; Firmata Control Messages
;----------------------------------------------------------------------
(define (set-pin! port pin)
  (let* ((old-value (bytevector-u8-ref DIGITAL-IO-PINS port))
         (new-value (bitwise-ior old-value (arithmetic-shift 1 pin))))
    (write-byte (bitwise-ior DIGITAL-MESSAGE port) out)
    (write-byte (bitwise-and new-value #x3F) out)
    (write-byte (arithmetic-shift new-value -7) out)
    (bytevector-u8-set!  DIGITAL-IO-PINS port new-value); added - bob fields - 26.10.12
    (flush-output out)))

(define (clear-pin! port pin)
  (let* ((old-value (bytevector-u8-ref DIGITAL-IO-PINS port))
         (new-value (bitwise-and old-value (bitwise-not (arithmetic-shift 1 pin)))))
    (write-byte (bitwise-ior DIGITAL-MESSAGE port) out)
    (write-byte (bitwise-and new-value #x3F) out)
    (write-byte (arithmetic-shift new-value -7) out)
    (bytevector-u8-set!  DIGITAL-IO-PINS port new-value); added - bob fields - 26.10.12
    (flush-output out)))

; set-pin-mode!
;   pin-number (0-127)
;   mode (INPUT/OUTPUT/ANALOG/PWM/SERVO, 0/1/2/3/4)
(define (set-pin-mode! pin mode)
  (write-byte SET-PIN-MODE out)
  (write-byte pin out)
  (write-byte mode out)
  (flush-output out))
  
; report-analog-pin!
;  pin-number (0-16)
;  mode enable = 1 disable = 0
(define (report-analog-pin! pin-number mode) 
  (write-byte (bitwise-ior REPORT-ANALOG pin-number) out)
  (write-byte mode out)
  (flush-output out))
  
; report-digital-port!
;  port-number (0-16)
;  mode enable = 1 disable = 0
(define (report-digital-port! port-number mode) 
  (write-byte (bitwise-ior REPORT-DIGITAL port-number) out)
  (write-byte mode out)
  (flush-output out))

; request-version
(define (request-version)
  (write-byte REPORT-VERSION out)
  (flush-output out))



;; LINE ADDED SO AS TO WORK WITH THE SERVO
;; Franco 25.11.2013: to write PWM pins
(define (analog-write! pin value)
   (write-byte (bitwise-ior ANALOG-MESSAGE (bitwise-and pin #x0F)) out)
   (write-byte (bitwise-and value #x7F) out)
   (write-byte (arithmetic-shift value -7) out)
   (flush-output out)
  )


;----------------------------------------------------------------------
; Firmata Control Messages
;----------------------------------------------------------------------
(define (set-arduino-pin! pin)
  (set-pin! (quotient pin 8) (remainder pin 8)))

(define (clear-arduino-pin! pin)
  (clear-pin! (quotient pin 8) (remainder pin 8)))

(define (is-arduino-pin-set? pin)
  (is-pin-set? (quotient pin 8) (remainder pin 8)))

(define (on-button-pressed pin lambda)
  (set! registrations (append registrations (list (cons pin lambda))))
  (display registrations))
