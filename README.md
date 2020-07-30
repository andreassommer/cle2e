# cle2e
Tool for reading Heidelberg OCT (optical coherence tomography) files in E2E data format and extracting the images

### Author: Andreas Sommer - github@andreas-sommer.eu

## Usage:
Use Common-Lisp to run, preferably SBCL.

1) Start SBCL
2) Compile and load the cle2e-package:  
   <code>(load (compile-file "cle2e.lisp"))</code>
3) Change to the package  
   <code>(in-package :cle2e)</code>
4) Extract the images from the e2e-file "data.e2e"  
   <code>(process-e2e-file "data.e2e")</code>  
   The programm will auto-generate BMP-files according to the pattern <patientname>-<birthday>-<side>-<type>-<runningnumber>.bmp

If anything goes wrong, increasing the verbosity level might help to figure out the source of trouble:  
<code>(choose-verbosity :max)</code>

##

Reverse-engineering of the E2E file format was done by Christian Heine.  
See https://bitbucket.org/uocte/uocte/wiki/Heidelberg%20File%20Format  
