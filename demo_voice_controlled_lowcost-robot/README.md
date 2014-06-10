A Demo Version of a Voice Controlled Low Cost Robot Using Arduino, Pocketsphinx, Firmata and Racket
========================

## How to compile and run

build the main.cpp file with 
$ make

Create vocabulary file (corpus) so as to compile it into special dictionary and pronunciation files. Go to http://www.speech.cs.cmu.edu/tools/lmtool-new.html
in order to upload your corpus file, click the "Compile Knowledge Base" button, then download the resulting compressed tarball that contains all the language model files.  


Extract *.lm and *.dic files into the same irectory of the project. Then
renamed to lm.lm and dic.dic.


Then you are going to open two terminals in which the following commands will be runned:

$./main -lm lm.lm -dict dic.dic > capturedwords
$racket voicecontrolcommands.rkt


NB. If for any circumstances the racket file is not working, it is highly recommended uploading the Standard Firmata from Arduino IDE. (I have not known what is the main reason of the previuos setback)




Additionally, the following command was runned on the video so as to produce a text to voice sentence:
$ espeak -ven-f3 -s160 "Voice Commands for  Low-Cost Robot with pocketsphinx and racket"


## Vocabulary Commmands (corpus)

move forward
move backward
move left
move right

go forward
go backward
rotate left
rotate right

stop right there

hi little robot

### Main References
http://www.rmnd.net/speech-recognition-on-raspberry-pi-with-sphinx-racket-and-arduino/
http://www.pirobot.org/blog/0022/


### Acknowledgements
Franco Raimondi [http://www.rmnd.net/]


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
http://www.speech.cs.cmu.edu/tools/product/1402334130_25650/

Sphinx knowledge base generator [lmtool.3]

Your request for a Sphinx knowledge base appears to have been successfully processed!
The base name for this set is 2794. TAR2794.tgz is the compressed version. 
Note that this set of files is internally consistent and is best used together. 

IMPORTANT: Please download these files as soon as possible; they will be deleted in approximately a half hour.

SESSION 1402334130_25650
[_INFO_] Found corpus: 10 sentences, 22 unique words
[_INFO_] Found 0 words in extras  (0)
[_INFO_] Language model completed  (0)
[_INFO_] Pronounce completed  (0)
[_STAT_] Elapsed time: 0.007 sec
Please include these messages in bug reports.
 Name                    Size  Description
 2794.dic                206    Pronunciation Dictionary
 2794.lm                 2.0K   Language Model
 2794.log_pronounce      167    Log File
 2794.sent               219    Corpus (processed)
 2794.vocab               70    Word List
 TAR2794.tgz             1.2K   COMPRESSED TARBALL
Apache/2.2.22 (Ubuntu) Server at www.speech.cs.cmu.edu Port 80

++++
June2014@LibreRobotics 

