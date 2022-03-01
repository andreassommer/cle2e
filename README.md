# cle2e
Tool for reading Heidelberg OCT (optical coherence tomography) files in E2E data format and extracting the images

### Author: Andreas Sommer - github@andreas-sommer.eu

## Usage:
Use Common-Lisp to run, preferably SBCL.

1) Start SBCL

3) Compile and load the cle2e-package:  

        (load (compile-file "cle2e.lisp"))

3) Change to the package  

        (in-package :cle2e)
        
4) Extract the images from the e2e-file "data.e2e"  

        (process-e2e-file "data.e2e")

   The programm will generate BMP files according to the pattern  `<patientname>-<birthday>-<side>-<type>-<runningnumber>.bmp`


If anything goes wrong, increasing the verbosity level might help to figure out the source of trouble:  

    (choose-verbosity :max)

   
## References

Reverse-engineering of the E2E file format was done by Christian Heine.  
See https://bitbucket.org/uocte/uocte/wiki/Heidelberg%20File%20Format  
