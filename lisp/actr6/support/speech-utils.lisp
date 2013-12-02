;;;-*- Mode: Lisp; Package: CCL -*-

;;;  speech-utils.lisp
;;;
;;;  Some simple functions demonstrating use of
;;;  the new Macintosh Speech Manager
;;;
;;;  Bill Andersen (waander@cs.umd.edu)
;;;  University of Maryland
;;;  Department of Computer Science
;;;
;;;  August, 1993
;;;
;;;  Notes: To use these functions, you need to install the Macintosh
;;;  Speech Manager.  You can get this from ftp.apple.com.  You also
;;;  need the MCL interface file speech.lisp, which is available from
;;;  cambridge.apple.com.  Don't forget to call (reindex-interfaces)
;;;  after you get the interface file.
;;;
;;; Function documentation:
;;;
;;; SPEECH-AVAILABLE-P ()                                            [FUNCTION]
;;;    Returns T if the Speech Manager is installed, else NIL.
;;;
;;; SPEAK-STRING (s)                                                 [FUNCTION]
;;;    Speaks the string in the (crummy) default voice.
;;;
;;; SPEECH-BUSY-P ()                                                 [FUNCTION]
;;;    Returns T if text is currently being spoken, else NIL.
;;;
;;; COUNT-VOICES ()                                                  [FUNCTION]
;;;    Returns the number of voice resources defined in the system.
;;;
;;; SAMPLE-VOICES ()                                                 [FUNCTION]
;;;    Goes through the list of all voice resources defined in the
;;;    system and speaks the description text associated with each
;;;    one.
;;;
;;; SPEAK-IN-VOICE (index string)                                    [FUNCTION]
;;;    Speaks <string> in the voice specified by <index>.  Index must be
;;;    in the range of voice resource indexes known to the Speech Manager.
;;;    There is no limit to the length of the string which can be spoken.
;;;

(in-package :ccl)

(export '(speech-available-p 
          speak-string 
          speech-busy-p 
          count-voices 
          sample-voices 
          speak-in-voice))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Error Handling Stuff
;;;

(defun handle-speech-error (err)
  (let ((string 
         (cond ((eql err #$paramErr)
                "Parameter error")
               ((eql err #$memFullErr)
                "Not enough memory to speak")
               ((eql err #$nilHandleErr)
                "Handle argument is nil")
               ((eql err #$siUnknownInfoType)
                "Feature not implemented on synthesizer")
               ((eql err #$noSynthFound)
                "Could not find the specified speech synthesizer")
               ((eql err #$synthOpenFailed)
                "Could not open another speech synthesizer channel")
               ((eql err #$synthNotReady)
                "Speech synthesizer is still busy speaking")
               ((eql err #$bufTooSmall)
                "Output buffer is too small to hold result")
               ((eql err #$voiceNotFound)
                "Voice resource not found")
               ((eql err #$incompatibleVoice)
                "Specified voice cannot be used with synthesizer")
               ((eql err #$badDictFormat)
                "Problem with pronunciation dictionary")
               ((eql err #$invalidComponentID)
                "Invalid SpeechChannel parameter")
               (t nil))))
    (if string
      (error "~a, (code = ~d)" string err)
      (error "An unknown speech error occurred, (code = ~d)" err))))

(defmacro with-spch-errors (form)
  (let ((code (gensym)))
    `(let ((,code ,form))
       (unless (eql ,code #$noErr)
         (handle-speech-error ,code))
       ,code)))

(defmacro check-for-speech ()
  `(unless (speech-available-p) 
     (error "The Speech Manager is not installed on this machine.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface Functions
;;;

;; SPEECH-AVAILABLE-P
;;
;; Returns T if the Speech Manager is installed, else NIL.

(defun speech-available-p ()
  "Returns T if the Speech Manager is installed, else NIL."
  (%stack-block ((result 8))
    (let ((err (#_Gestalt #$gestaltSpeechAttr result)))
      (if (or (/= err #$noErr) 
              (not (logand (%get-signed-long result)
                           (ash 1 #$gestaltSpeechMgrPresent))))
        nil
        t))))

;; SPEAK-STRING
;;
;; Speaks the string in the (crummy) default voice.

(defun speak-string (s)
  "Speaks the string in the (crummy) default voice."
  (check-for-speech)
  (check-type s string)
  (with-pstrs ((pstr s))
    (with-spch-errors (#_SpeakString pstr))))      

;; SPEECH-BUSY-P
;;
;; Returns T if text is currently being spoken, else NIL.

(defun speech-busy-p ()
  "Returns T if text is currently being spoken, else NIL."
  (check-for-speech)
  (= (#_SpeechBusy) 1))

;; COUNT-VOICES
;;
;; Returns the number of voice resources defined in the system.

(defun count-voices ()
  "Returns the number of voice resources defined in the system."
  (check-for-speech)
  (%stack-block ((voiceCount 2))
    (with-spch-errors (#_CountVoices voiceCount))
    (%get-word voiceCount)))

;; SAMPLE-VOICES
;;
;; Goes through the list of all voice resources defined in the
;; system and speaks the description text associated with each
;; one.

(defun sample-voices ()
  "Goes through the list of all voice resources defined in the
system and speaks the description text associated with each
one."
  (check-for-speech)
  (rlet ((voice :VoiceSpec)
         (info  :VoiceDescription)
         (chan  :SpeechChannel))
    (loop
      for i from 1 to (count-voices)
      do (progn
           (with-spch-errors 
             (#_GetIndVoice i voice))
           (with-spch-errors 
             (#_GetVoiceDescription voice info (record-length :VoiceDescription)))
           (with-spch-errors
             (#_NewSpeechChannel voice chan))
           (let ((string (format nil "Voice #~d: ~a. ~a." i
                                  (rref info :VoiceDescription.name)
                                  (rref info :VoiceDescription.comment))))
             (format t "~&~a~%" string)
             (with-pstrs ((pstr string))
               (unwind-protect
                 (progn
                   (with-spch-errors
                     (#_SpeakText (%get-ptr chan) (%inc-ptr pstr 1) (%get-byte pstr)))
                   (loop while (= (#_SpeechBusy) 1)))
                 (with-spch-errors
                   (#_DisposeSpeechChannel (%get-ptr chan))))))))))

;; SPEAK-IN-VOICE
;;
;; Speaks <string> in the voice specified by <index>.  Index must be
;; in the range of voice resource indexes known to the Speech Manager.
;; There is no limit to the length of the string which can be spoken.

(defun speak-in-voice (index string)
  "Speaks <string> in the voice specified by <index>.  Index must be
in the range of voice resource indexes known to the Speech Manager.
There is no limit to the length of the string which can be spoken."
  (check-for-speech)
  (check-type string string)
  (count-voices)                        ; not sure if I have to call this but
  ; it didn't work once until I did
  (rlet ((voice :VoiceSpec)
         (chan  :SpeechChannel))
    (with-spch-errors 
      (#_GetIndVoice index voice))
    (with-spch-errors
      (#_NewSpeechChannel voice chan))
    (let* ((length (length string))
           (strptr (#_NewPtr length)))
      (%put-cstring strptr string)
      (unwind-protect
        (progn
          (with-spch-errors
            (#_SpeakText (%get-ptr chan) strptr length))
          (loop while (= (#_SpeechBusy) 1)))
        (with-spch-errors
          (#_DisposeSpeechChannel (%get-ptr chan)))
        (#_DisposePtr strptr)))))       ; mdb changed from _DisposPtr


(provide "SPEECH-UTILS")
