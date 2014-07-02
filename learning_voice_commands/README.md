A Low-Cost Robot that is capable of learning voice commands  (Using Arduino, pocketsphinx, firmata and racket)
=======================

###Instruccions to run the applications

Open two terminals in which you are going to run each of the following commands:

	$./main -lm lm.lm -dict dic.dic > capturedwords
	
	$ racket voicecontrolcommands.rkt


###Voice Commmands (corpus)

move forward
move in reverse  
["move backward" has been changed to "move in reverse" command since there is poor recogntion! "0 out of 10"]
spin left   [spin to turn; nonetheless, has not been well recognised]
spin right  [spin to turn; it works well]
move your eyes
move your head

go forward
go backward
rotate left
rotate right
stop right there

hi little robot

learn this commands
OK that is enough
show us what you have learnt



### Main References

http://www.rmnd.net/speech-recognition-on-raspberry-pi-with-sphinx-racket-and-arduino/

http://www.pirobot.org/blog/0022/

