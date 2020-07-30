;; cle2e - Tool for reading Heidelberg OCT files in E2E data format
;;         and extracting the images.
;;
;; Based on a documentation of the E2E file format of "uocte",
;; https://bitbucket.org/uocte/uocte/wiki/Heidelberg%20File%20Format
;; queried on December 3, 2016
;;
;; Author: Andreas Sommer  (email@andreas-sommer.eu)
;;         December 2016
;;



;; define the package
(defpackage cle2e
  (:use :common-lisp)
  (:export   ;;; Functions
             ;;; Macros
   ))

;; enter the package
(in-package :cle2e)



;; special variables
(defvar *verbosity* NIL)
(defvar *filestream* NIL)
(defparameter *entrypoints* ())
(defparameter *storage* (make-list 0))
(setf *print-right-margin* 120)

;; setup verbosity
;;  :H ;; header
;;  :D ;; main directory
;;  :E ;; directory entries
;;  :C ;; chunks
;;  :P ;; patient info
;;  :INFO ;; infos
;;  :SEEK ;; file seek
;;  :K ;; contour data
;;  :I ;; image data
;;  :L ;; laterality
;;  :S ;; slice data
(defun choose-verbosity (lvl)
  (setf *verbosity*
        (case lvl
          (:max  '(:H :D :E :C :P :INFO :SEEK :K :I :L :S))
          (:min  '(:INFO))
          (otherwise lvl))))
(choose-verbosity '(:P :I :L :K :INFO :S))
(choose-verbosity '(:INFO :P :L))

;; Generate a 32-bit single-float from an unsigned-integer*32
(defun make-float-from-u32 (bits)
  "Stolen from the SBCL source."
  (cond
    ((zerop bits) 0.0)             ;; IEEE float special cases
    ((= bits #x-80000000) -0.0)    ;; IEEE float special cases
    (t (let* ((sign (ecase (ldb (byte 1 31) bits)
                      (0  1.0)
                      (1 -1.0)))
              (iexpt (ldb (byte 8 23) bits))
              (expt (if (zerop iexpt) ; denormalized
                        -126
                        (- iexpt 127)))
              (mant (* (logior (ldb (byte 23 0) bits)
                               (if (zerop iexpt)
                                   0
                                   (ash 1 23)))
                       (expt 0.5 23))))
         (* sign (expt 2.0 expt) mant)))))



;; FIXME: This function is strange...
#+nil
(defun make-float-from-uf16 (bits)
  "Modified from the SBCL source."
  (cond
    ((zerop bits) 0.0)             ;; IEEE float special cases
    (t (let* ((iexpt (ldb (byte 6 10) bits))
              (expt  (- iexpt 25))
              (mant  (* (logior (ldb (byte 10 0) bits)
                                (if (zerop iexpt) 0 (ash 1 10)))
                        (expt 0.5 25))))
         (* (expt 2.0 expt) mant)))))



(defun ldexp (x exp)
  (* x (expt 2 exp)))

;; Also delivers strange numbers
#-nil
(defun make-float-from-uf16 (bits)
  "Adopted from uocte."
  ;; operator float() const
  ;; {
  ;;   if (e == 0)
  ;;     return ldexp(m / float(1<<10), -62);
  ;;   else if (e == 63)
  ;;     return 0.0f;
  ;;   else
  ;;     return ldexp(1.0f + m / float(1<<10), int(e)-63);
  ;; }
  (let*  ((mant  (ldb (byte 10 0) bits))
          (iexpt (ldb (byte 6 10) bits)))
    (cond
      ((= iexpt 63)   0.0d0)
      ((zerop iexpt)  (ldexp (/ mant (ash 1 10))  -62))
      (t              (ldexp (1+ (/ mant (ash 1 10))) (- iexpt 63))))))





(defun trimstr (string)
  "Trims a 0-terminated string list."
  (cond
    ((stringp string)  (string-trim '(#\Nul) string))
    ((listp   string)  (trimstr (concatenate 'string string)))
    (t  (error "unknown thing: ~a" string))))


;; couple of stream access tools
(defun read-chr (s)
  (code-char (read-byte s)))
(defun read-u8 (s)
  (read-byte s))
(defun read-u16 (s)  ;; little endian
  (let ((u16 0))
    (setf (ldb (byte 8 0) u16) (read-byte s))
    (setf (ldb (byte 8 8) u16) (read-byte s))
    u16))
(defun read-u32 (s)  ;; little endian
  (let ((u32 0))
    (declare (type (unsigned-byte 32) u32))
    (setf (ldb (byte 8  0) u32) (read-byte s))
    (setf (ldb (byte 8  8) u32) (read-byte s))
    (setf (ldb (byte 8 16) u32) (read-byte s))
    (setf (ldb (byte 8 24) u32) (read-byte s))
    u32))
(defun read-f32 (s)
  (make-float-from-u32 (read-u32 s)))
(defun read-uf16 (s)
  (make-float-from-uf16 (read-u16 s)))






(defun decode-birthdate (birthdate)
  (decode-julian-date (birthdate-to-juliandate birthdate)))

(defun birthdate-to-juliandate (birthdate)
  (- (/ birthdate 64) 14558805))

(defun decode-julian-date (JD)
  "FROM: http://quasar.as.utexas.edu/BillInfo/JulianDatesG.html"
  (let* ((Q   (+ JD 0.5))
         (Z   (floor Q))
         (W   (floor (/ (- Z 1867216.25) 36524.25)))
         (X   (floor (/ W 4)))
         (A   (- (+ Z 1 W) X))
         (B   (+ A 1524))
         (C   (floor (/ (- B 122.1) 365.25)))
         (D   (floor (* 365.25 C)))
         (E   (floor (/ (- B D) 30.6001)))
         (F   (floor (* 30.6001 E)))
         (day   (truncate (+ (- B D F) (- Q Z))))
         (month (if (<= (- E 1) 12)
                    (- E 1)
                    (- E 13)))
         (year  (if (<= month 2)
                    (- C 4715)
                    (- C 4716))))
    (values year month day)))



;; message reporter
(defun show (type itemname &optional formatstr &rest content)
  ;; verbosity enabled?
  (when (null *verbosity*)
    (return-from show))
  ;; empty line
  (when (member type *verbosity*)
    (if (equalp itemname :crlf)
        ;; just an empty line
        (format t "~%")
        ;; specific output
        (let ((fmt  (format nil "~~&  ~a~~18,1t: ~a" itemname formatstr)))
          (apply #'format t fmt content)))))



;; main function
(defun process-e2e-file (e2efilespec)
  ;; small error check
  (unless (probe-file e2efilespec)
    (error "File ~a does not exist." e2efilespec))
  ;; empty the storage
  (setf *storage* NIL)
  ;; if a file is opened, close it
  (when *filestream* (close *filestream*))
  ;; open the file
  (setf *filestream* (open e2efilespec :direction :input :element-type '(unsigned-byte 8)))
  ;; process file
  (format t "~&Processing file...~&")
  (format t "HEADER:")
  (read-e2e-header *filestream*)
  (format t "MAIN-DIRECTORY:")
  (multiple-value-bind (num_entries entrypoint)
      (read-e2e-main-directory *filestream*)
    (declare (ignore num_entries))
    (format t "DIRCHUNKS:")
    (read-e2e-directory-chunks *filestream* entrypoint)
    (format t "CHUNKS:")
    (decode-chunks *filestream*)
    )
  (close *filestream*)
  (format t "Done!"))



;; read the header
(defun read-e2e-header (stream)
  ;;  // header
  ;;  char[12]            magic1
  ;;  u32                 version ("0x64")
  ;;  u16[9]              9x "0xffff"
  ;;  u16                 "0x0"
  (let ((magic1  (loop for i from 1 to 12 collecting (read-chr stream)))
        (version (read-u32 stream))
        (markers (loop for i from 1 to 9  collecting (read-u16 stream)))
        (endmark (read-u16 stream)))
    ;; display if not silent
    (progn
      (show :H "Magic 1"       "~s" magic1)
      (show :H "Version 0x64"  "0x~x" version)
      (show :H "9x 0xffff"     "~{0x~x ~}" markers)
      (show :H "stop 0x0"      "0x~x" endmark)
      (show :H :crlf))))



;; read the directory
(defun read-e2e-main-directory (stream)
  ;;  // main directory
  ;;  char[12]            magic2
  ;;  u32                 version ("0x64")
  ;;  u16[9]              9x "0xffff"
  ;;  u16                 "0x0"
  ;;  u32                 num_entries
  ;;  u32                 current
  ;;  u32                 "0x0"
  ;;  u32                 ?
  ;;
  (let ((magic2   (loop for i from 1 to 12 collecting (read-chr stream)))
        (version  (read-u32 stream))
        (markers  (loop for i from 1 to 9 collecting (read-u16 stream)))
        (stop1    (read-u16 stream))
        (num_entries (read-u32 stream))
        (current     (read-u32 stream))
        (stop2       (read-u32 stream))
        (unknown     (read-u32 stream)))
    ;; informational output
    (progn
      (show :D "Magic 2"       "~s" magic2)
      (show :D "Version 0x64"  "0x~x" version)
      (show :D "9x 0xffff"     "~{0x~x ~}" markers)
      (show :D "stop 0x0"      "0x~x" stop1)
      (show :D "num_entries"   "~d" num_entries)
      (show :D "current"       "~d" current)
      (show :D "stop 0x0"      "0x~x" stop2)
      (show :D "unknown"       "0x~x" unknown)
      (show :SEEK "--filepos--"   "~d" (file-position stream))
      (show :D :crlf))
    ;; return num_entries and current
    (values num_entries current)))



;; read the directory chunks
(defun read-e2e-directory-chunks (stream current)
  ;;  // traverse list of directory chunks
  ;;  do
  ;;    seek(current)
  ;;    char[12]            magic3
  ;;    u32                 version ("0x64")
  ;;    u16[9]              9x "0xffff"
  ;;    u16                 "0x0"
  ;;    u32                 num_entries
  ;;    u32                 ?
  ;;    u32                 prev
  ;;    u32                 ?
  ;;
  ;;    for i in num_entries..1
  ;;      u32               pos
  ;;      u32               start
  ;;      u32               size
  ;;      u32               "0x0"
  ;;      u32               patient_id
  ;;      u32               study_id
  ;;      u32               series_id
  ;;      u32               slice_id
  ;;      u16               ?
  ;;      u16               "0x0"
  ;;      u32               type
  ;;      u32               ?
  ;;      if start > pos
  ;;        push (start,size) to stack S
  ;;
  ;;    current = prev
  ;;  while current != 0
  (setf *entrypoints* NIL)
  (loop for k from 1 until (= current 0)
     doing
     ;; move to file position
       (show :SEEK "--seeking--" "~d" current)
     ;;(debugshow stream)
       (file-position stream current)
       (let ((dir-magic3    (loop for i from 1 to 12 collecting (read-chr stream)))
             (dir-version   (read-u32 stream))
             (dir-markers   (loop for i from 1 to 9 collecting (read-u16 stream)))
             (dir-stop1     (read-u16 stream))
             (dir-num_entries (read-u32 stream))
             (dir-unknown1  (read-u32 stream))
             (dir-prev      (read-u32 stream))
             (dir-unknown2  (read-u32 stream)))
         ;; informational output
         (progn
           (show :D "DIRECTORYENTRY" "~d" k)
           (show :D "Magic 3"       "~s"        dir-magic3)
           (show :D "Version 0x64"  "0x~x"      dir-version)
           (show :D "9x 0xffff"     "~{0x~x ~}" dir-markers)
           (show :D "stop 0x0"      "0x~x"      dir-stop1)
           (show :D "num_entries"   "~d"        dir-num_entries)
           (show :D "unknown 1"     "0x~x"      dir-unknown1)
           (show :D "prev"          "~d"        dir-prev)
           (show :D "unknown 2"     "0x~x"      dir-unknown2)
           (show :SEEK "--filepos--"   "~d" (file-position stream))
           (show :D :crlf))
         ;; process chunk: store entrypoints
         (loop for i from dir-num_entries downto 1
            doing
            ;;(debugshow stream :count 44)
              (show :SEEK "--filepos--"  "~d"  (file-position stream))
              (let ((pos         (read-u32 stream))
                    (start       (read-u32 stream))
                    (size        (read-u32 stream))
                    (stop1       (read-u32 stream))  ;0x0
                    (patientID   (read-u32 stream))
                    (studyID     (read-u32 stream))
                    (seriesID    (read-u32 stream))
                    (sliceID     (read-u32 stream))
                    (unknown1    (read-u16 stream))
                    (stop2       (read-u16 stream))  ;0x0
                    (type        (read-u32 stream))
                    (unknown2    (read-u32 stream)))
                (progn
                  (show :E "----CHUNK----"  "#~d"  i)
                  (show :E "pos in file" "~d"   pos)
                  (show :E "start"       "~d"   start)
                  (show :E "size"        "~d"   size)
                  (show :E "stop 0x0"    "0x~x" stop1)
                  (show :E "patientID"   "0x~x" patientID)
                  (show :E "studyID"     "0x~x" studyID)
                  (show :E "seriesID"    "0x~x" seriesID)
                  (show :E "sliceID"     "~d"   sliceID)
                  (show :E "unknown"     "0x~x" unknown1)
                  (show :E "stop 0x0"    "0x~x" stop2)
                  (show :E "type"        "0x~x" type)
                  (show :E "unknown"     "0x~x" unknown2)
                  (show :E :crlf))
                ;; collect entry points and sizes
                (if (> start pos)
                    (progn (push (cons start size) *ENTRYPOINTS*)
                           (show :E "STORE ENTRY"  "start=~d" start))
                    (show :E "IGNORE ENTRY" "start=~d < ~d=pos" start pos))
                (show :E :crlf)
                )) ;; closes the inner loop

         ;; move to previous chunk position and cycle
         (setf current dir-prev))))



(defun debugshow (stream &key (count 80) (seekpos (file-position stream)))
  (file-position stream seekpos)
  (loop for i from 1 to count
     collecting (read-chr stream) into data
     finally (format t "~&@~a: ~a~&" seekpos (concatenate 'string data)))
  (file-position stream seekpos))



(defun decode-chunks (stream)
  (loop for (start . size) in *entrypoints*
     doing
       (decode-chunk-at-position stream start size)))



(defun decode-chunk-at-position (stream start size)
  (declare (ignorable size))
  (show :SEEK "--seeking--" "~d" start)
  (file-position stream start)
  (let ((magic4    (loop for i from 1 to 12 collecting (read-chr stream)))
        (unknown1  (read-u32 stream))
        (unknown2  (read-u32 stream))
        (pos       (read-u32 stream))
        (size      (read-u32 stream))
        (stop      (read-u32 stream)) ;0x0
        (patientID (read-u32 stream))
        (studyID   (read-u32 stream))
        (seriesID  (read-u32 stream))
        (sliceID   (read-u32 stream))
        (ind       (read-u16 stream))
        (unknown3  (read-u16 stream))
        (type      (read-u32 stream))
        (unknown4  (read-u32 stream)))
    ;; informational output
    (progn
      (show :C "---------"  "~a"   "")
      (show :C "magic"      "~s"   magic4)
      (show :C "unknown"    "~d"   unknown1)
      (show :C "unknown"    "~d"   unknown2)
      (show :C "pos"        "~d"   pos)
      (show :C "size"       "~d"   size)
      (show :C "stop 0x0"   "0x~x" stop)
      (show :C "patientID"  "0x~x" patientID)
      (show :C "studyID"    "0x~x" studyID)
      (show :C "seriesID"   "0x~x" seriesID)
      (show :C "sliceID"    "~d"   sliceID)
      (show :C "ind"        "~d"   ind)
      (show :C "unknown"    "0x~x" unknown3)
      (show :C "type"       "0x~x" type)
      (show :C "unknown"    "0x~x" unknown4)
      (show :C :crlf))
    ;; switch the type
    (case type
      (#x00000009  (multiple-value-bind (given-name surname birthdate sex)
                       (read-and-decode-chunk-patient-info stream)
                     (show :C "chunk type" "~a" "PATIENT-DATA")
                     (store-patientData patientID given-name surname birthdate sex)
                     ))
      (#x0000000B  (multiple-value-bind (laterality)
                       (read-and-decode-chunk-laterality   stream)
                     (show :C "chunk type" "~a" "LATERALITY")
                     (store-laterality laterality)))
      (#x40000000  (multiple-value-bind (image width height imagetype)
                       (read-and-decode-chunk-image-data   stream)
                     (show :C "chunk type" "~a ~a" "IMAGE" imagetype)
                     (store-slice sliceID
                                  :patientID patientID
                                  :image image
                                  :image-width width
                                  :image-height height
                                  :image-type imagetype)))
      (#x00002723  (multiple-value-bind (data width)
                       (read-and-decode-chunk-contour-data stream)
                     (store-slice sliceID
                                  :patientID patientID
                                  :contour data
                                  :contour-width width)))
      (otherwise   (show :C "chunk type" "~a 0x~x" "UNKNOWN" type)))
    ))




(defun store-patientData (patientID given-name surname birthdate sex)
  (show :INFO "Storing patient data" "ID=~d  name=~s" patientID (trimstr surname))
  (unless (null (getf *storage* :patientID))
    (cerror "PatientID present! What shall I do?" "Overwrite!")
    (show :INFO "Overwriting patientID" "~a by ~a" (getf *storage* :patientID) patientID))
  (setf (getf *storage* :patientID)  patientID
        (getf *storage* :given-name) given-name
        (getf *storage* :surname)    surname
        (getf *storage* :birthdate)  birthdate
        (getf *storage* :sex)        sex))

(defun store-laterality (laterality)
  (show :INFO "Storing laterality" "~a" laterality)
  (setf (getf *storage* :laterality) laterality))

(defun store-slice (sliceID &rest args)
  (show :S "Storing sliceID" "0x~x" sliceID)
  ;; get the slices list
  (let* ((slicelist (getf *storage* :slices))
         (slice     (getf slicelist sliceID)))
    ;; update the slice
    (loop for (type data) on args by #'cddr doing
         (setf (getf slice type) data))
    ;; store the slice in the slices list
    (setf (getf slicelist sliceID) slice)
    ;; store the slices list
    (setf (getf *storage* :slices) slicelist)))




(defun read-and-decode-chunk-patient-info (stream)
  ;; // type == 0x00000009 - patient info
  ;; u8[31]                given name
  ;; u8[66]                surname
  ;; u32                   birthdate
  ;; u8                    sex ("M" or "F")
  (let ((given-name  (loop for i from 1 to 31 collecting (read-chr stream)))
        (surname     (loop for i from 1 to 66 collecting (read-chr stream)))
        (birthdate   (read-u32 stream))
        (sex         (read-chr stream)))
    ;; informational output
    (progn
      (show :P "given name"  "~s"  (concatenate 'string given-name))
      (show :P "surname"     "~s"  (concatenate 'string surname))
      (show :P "birthdate"   "~a (~a)"  birthdate (multiple-value-list (decode-birthdate birthdate)))
      (show :P "sex"         "~s"  sex))
    ;; return results
    (values given-name surname (multiple-value-list (decode-birthdate birthdate)) sex)))


(defun read-and-decode-chunk-laterality (stream)
  ;; // type == 0x0000000B - laterality
  ;; u8[14]                ?
  ;; u8                    laterality ('L' or 'R')
  ;; u8[?]                 ?
  (let* ((unknown1   (loop for k from 1 to 14 collecting (read-u8 stream)))
         (laterality (read-chr stream)))
    (declare (ignorable unknown1))
    (show :L "Laterality"  "~s" laterality)
    (values laterality)))
  


(defun read-and-decode-chunk-image-data (stream)
  ;; // type == 0x40000000 - image data
  ;; u32                   size
  ;; u32                   type ("0x02010201" for fundus, "0x02200201" for tomogram)
  ;; u32                   ?
  ;; u32                   width
  ;; u32                   height
  ;; if ind == 0
  ;;   u8[height][width]   raw fundus image
  ;; else
  ;;   uf16[height][width] raw tomogram slice image
  (let* ((Tfundus   #x02010201)
         (Ttomogram #x02200201)
         (size     (read-u32 stream))
         (type     (read-u32 stream))
         (unknown  (read-u32 stream))
         (width    (read-u32 stream))
         (height   (read-u32 stream)))
    (labels ((get-type-symbol (type)
               (cond
                 ((= type Tfundus)   :Fundus)
                 ((= type Ttomogram) :Tomogram)
                 (t                  :unknown))))
      ;; informational output
      (progn
        (show :I "size"     "~d" size)
        (show :I "type"     "0x~x  (~a)" type (get-type-symbol type))
        (show :I "unknown"  "0x~x" unknown)
        (show :I "width"    "~d" width)
        (show :I "height"   "~d" height))
      ;; read data
      (show :INFO "found image data of type" "~s" (get-type-symbol type))
      (let (image)
        (cond
          ;; FUNDUS: read as is
          ((= type Tfundus)
           (progn (setf image (make-array (list height width)))
                  (loop for i from 0 below (* height width) doing
                       (setf (row-major-aref image i) (read-u8 stream)))))
          ;; TOMOGRAM: swap dimensions and bottom-up
          ((= type Ttomogram)
           (progn (setf image (make-array (list width height)))
                  (loop for x downfrom (1- width) to 0 doing
                       (loop for y from 0 below height doing
                            (setf (aref image x y) (read-uf16 stream)))) ;; uf16 <-- but keep original here
                  (rotatef width height))))
        (values image width height (get-type-symbol type))))))


(defun read-and-decode-chunk-contour-data (stream)
  ;; // type == 0x00002723 - contour data
  ;; u32                   ?
  ;; u32                   id
  ;; u32                   ?
  ;; u32                   width
  ;; f32[width]            contour depth in tomogram pixels for current slice
  (let* ((unknown1   (read-u32 stream))
         (id         (read-u32 stream))
         (unknown2   (read-u32 stream))
         (width      (read-u32 stream))
         (data       (loop for i from 1 to width collecting (read-f32 stream))))
    ;; informational output
    (progn
      (show :K "unknown1"  "0x~x" unknown1)
      (show :K "id"        "~d"   id)
      (show :K "unknown2"  "0x~x" unknown2)
      (show :K "width"     "~d"   width)
      (show :K :crlf))
    ;; return results
    (values data width)))








(defun write-chr (stream chr)
  (write-byte (char-code chr) stream))
(defun write-u8 (stream u8)
  (write-byte (ldb (byte 8 0) u8) stream))
(defun write-u16 (stream u16)
  (write-byte (ldb (byte 8 0) u16) stream)
  (write-byte (ldb (byte 8 8) u16) stream))
(defun write-u32 (stream u32)
  (write-byte (ldb (byte 8  0) u32) stream)
  (write-byte (ldb (byte 8  8) u32) stream)
  (write-byte (ldb (byte 8 16) u32) stream)
  (write-byte (ldb (byte 8 24) u32) stream))



(defun write-BMP-fileheader (stream &key (filesize 0) (offbits (+ 14 40)))
  ;;  FieldName Bytes Description
  ;;  bfType      2   The characters "BM"
  ;;  bfSize      4   The size of the file in bytes
  ;;  bfReserved1 2   Unused - must be zero
  ;;  bfReserved2 2   Unused - must be zero
  ;;  bfOffBits   4   Offset to start of Pixel Data
  (file-position stream 0) ;; seek file header
  (write-chr stream #\B)
  (write-chr stream #\M)
  (write-u32 stream filesize)
  (write-u16 stream 0)
  (write-u16 stream 0)
  (write-u32 stream offbits))


(defun write-BMP-imageheader (stream  width height)
  ;; FieldName   Bytes Description
  ;; biSize          4 Header Size - Must be at least 40
  ;; biWidth         4 Image width in pixels
  ;; biHeight        4 Image height in pixels
  ;; biPlanes        2 Must be 1
  ;; biBitCount      2 Bits per pixel - 1, 4, 8, 16, 24, or 32
  ;; biCompression   4 Compression type (0 = uncompressed)
  ;; biSizeImage     4 Image Size - may be zero for uncompressed images
  ;; biXPelsPerMeter 4 Preferred resolution in pixels per meter
  ;; biYPelsPerMeter 4 Preferred resolution in pixels per meter
  ;; biClrUsed       4 Number Color Map entries that are actually used
  ;; biClrImportant  4 Number of significant colors
  (file-position stream 14) ;; seek after the file header
  (write-u32 stream 40)   ;; image header size
  (write-u32 stream width)
  (write-u32 stream height)
  (write-u16 stream 1)    ;; planes must be 1
  (write-u16 stream 24)   ;; bits per pixel
  (write-u32 stream 0)    ;; 0 = no compression
  (write-u32 stream 0)    ;; 0 = image size for uncompressed images
  (write-u32 stream 0)    ;; pixels per meter in x-dir (0 = no preference)
  (write-u32 stream 0)    ;; pixels per meter in y-dir (0 = no preference)
  (write-u32 stream 0)    ;; 0 = no color map
  (write-u32 stream 0)    ;; number of significant colors (0 = all)
  )



(defun make-filename (type laterality sliceid)
  (let* ((surname    (trimstr (getf *storage* :surname)))
         (given-name (trimstr (getf *storage* :given-name)))
         (birthdate  (getf *storage* :birthdate))
         )
    (format nil "~a-~a-~d-~d-~d-~a-~a.bmp"
            surname
            given-name
            (first birthdate) (second birthdate) (third birthdate)
            laterality
            (cond
              ((equalp type :fundus) "Fundus")
              ((equalp type :tomogram)  (format nil "Tomogram-~2,'0d" sliceid))
              (t  (error "Unknown type ~a" type))))))



(defun write-BMP-from-storage ()
  (loop for (sliceid slice) on (getf *storage* :slices) by #'cddr
     doing
       (let* ((laterality (getf *storage* :laterality))
              (type       (getf slice :image-type))
              (width      (getf slice :image-width))
              (height     (getf slice :image-height))
              (image      (getf slice :image))
              (weights    (case type
                            (:fundus   '(1.0 1.0 1.0))   ;; B/W color
                            (:tomogram '(0.0 1.0 0.4)))) ;; greenish color
              (filename   (make-filename type laterality sliceid)))
         ;; informational output
         (show :INFO "Writing slice ID" "~a to ~a" sliceid filename)
         ;; process tomogram image
         (when (equalp type :tomogram)
           (let* ((newimg   (make-array (array-dimensions image)))
                  (pxcount  (array-total-size image)))
             (dotimes (i pxcount)
               (setf (row-major-aref newimg i)
                     (expt (row-major-aref image i) 0.25d0)))
             (let* ((maxval   (loop for i from 0 below pxcount maximizing (row-major-aref newimg i)))
                    (scale    (/ 255.0d0 maxval)))
               (loop for i from 0 below pxcount doing
                  (setf (row-major-aref newimg i)
                        (floor (* scale  (row-major-aref newimg i))))))
             (setf image newimg)
             (defparameter *newimg* newimg)))
         ;; output
         (with-open-file (s filename :direction :output :element-type 'unsigned-byte :if-exists :supersede)
           (write-BMP s image width height :weights weights)))))




(defun write-BMP (stream image width height &rest rest)
  (write-BMP-fileheader stream)                ;; write file header
  (write-BMP-imageheader stream width height)  ;; write image header
  (apply #'write-BMP24-image stream image width height rest)       ;; write image
  (write-BMP-fileheader stream :filesize (file-position stream)))  ;; update header

(defun write-BMP24-image (stream image width height &key (weights '(1.0 1.0 1.0)))
  (loop for y from 0 below height doing
       (loop for x from 0 below width doing
            (let ((pixel (aref image y x)))
              (write-u8 stream (floor (* pixel (elt weights 0)))) ;; red
              (write-u8 stream (floor (* pixel (elt weights 1)))) ;; green
              (write-u8 stream (floor (* pixel (elt weights 2)))) ;; blue
              )))
  (values (* 3 width height))) ;; return the number of written bytes
