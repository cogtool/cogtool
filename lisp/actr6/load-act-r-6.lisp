;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : load-act-r-6.lisp
;;; Version     : 1.0
;;; 
;;; Description : Top level loader for the whole ACT-R 6 system.
;;; 
;;; Bugs        : ???
;;;
;;; To do       : [-] Test in a variety of Lisps for issues with the
;;;             :     logical hostname stuff.
;;;             : [ ] Now, look into using the clisp version in other
;;;             :     lisps because it seems cleaner/more generic than
;;;             :     the ones I put toghether...
;;; 
;;; ----- History -----
;;;
;;; 2004.10.26 Dan
;;;             : Creation.
;;;             :
;;;             : Realized that require doesn't compile things automatically
;;;             : in all cases, so added my own require-compiled that does.
;;; 2004.12.10 Dan
;;;             : Fixed the make-package for the packaged version (for use
;;;             : with ACL at least).
;;;             : Reduced the lines to max of 80 chars.
;;; 2005.01.02 Dan
;;;             : Changed it so that it loads the "core modules" in a specific
;;;             : order and then all other modules.
;;; 2005.01.12 Dan
;;;             : * Added the tools directory to the set.
;;; 2005.01.23 Dan
;;;             : * Fixed the Lispworks binary extension check. Don't think it
;;;             :   still needs the old one...
;;; 2005.01.29 Dan
;;;             : * Added a feature check into compile-and-load to force it
;;;             :   to recompile if :actr-recompile is on the features list.
;;; 2005.02.01 Dan
;;;             : * This time, the Lispworks feature checks should be set
;;;             :   properly for OSX (thanks to Chris Sims).
;;; 2005.02.25 Dan
;;;             : * Removed the ~\newline usages because that causes problems
;;;             :   when a Lisp only wants to see native new lines there.
;;; 2005.04.14 Dan
;;;             : * Changed compile-and-load so that it throws an error if the
;;;             :   file it is passed has a non-"lisp" extension. - need to
;;;             :   verify that in other Lisps to make sure it works right.
;;; 2005.07.07 Dan
;;;             : * Fixed the packaged loading for Lispworks now too.
;;; 2005.08.10 Dan
;;;             : * Added a new directory to the set (commands) in place of
;;;             :   where modules was and then moved modules to after the
;;;             :   devices.
;;;             : * Now, there's basically a directory to auto-load in all
;;;             :   resonable locations, and I can better distribute files 
;;;             :   that were all jammed into tools.
;;;             : * Updated the version to 1.0.
;;; 2005.08.16 Dan
;;;             : * Added a flag to indicate whether things have been loaded
;;;             :   previously or not and actually throw an error if this
;;;             :   file is attempted to be loaded more than once.
;;; 2005.09.16 Dan
;;;             : * Added the appropriate feature checks to work "right" with
;;;             :   ACL 7's IDE i.e. load the devices and package things in
;;;             :   cg-user when necessary.
;;; 2005.10.18 Dan
;;;             : * Added the logical host setup for CMUCL.
;;;             : * Moved the smart-load function here and generalized it so
;;;             :   that framework and core-modules don't need to have 
;;;             :   their own special versions.
;;;             : * Also converted those specific loaders to essentially just
;;;             :   file lists now.
;;; 2005.11.01 Dan
;;;             : * Added a new compile-and-load so that things can be loaded
;;;             :   into MCL 5/5.1 (the versions that have the split open/load
;;;             :   Mac/Unix file menu options) without having to convert all
;;;             :   the files first.  This file needs to be loaded as a Unix
;;;             :   file and the rest should take care of itself.
;;; 2005.11.07 Dan
;;;             : * Realized that since the environment is loaded from tools
;;;             :   that there's no way to add patches to the environment
;;;             :   in an "auto load" directory because things in tools may
;;;             :   be loaded before the environment.  So, I've added yet
;;;             :   another directory from which files are loaded automatically.
;;;             :   The other-files directory is now scanned and .lisp files
;;;             :   are loaded as the last step of the load process.
;;; 2005.12.13 Dan
;;;             : * Changed the logical host setup for ACL because it turns
;;;             :   out that the host-namestring always ends up nil and doesn't
;;;             :   actually capture the drive info which causes problems if
;;;             :   the ACT-R sources are on a different drive than the ACL
;;;             :   assumed default.
;;; 2006.01.04 Dan
;;;             : * Added the switches so that it'll load under CMUCL in OS X
;;;             :   (with ppc).
;;; 2006.06.29 Dan
;;;             : * Added components provided by Don Morrison to allow it to be 
;;;             :   loaded into CLisp v2.38 - the CLisp logical host, tighter 
;;;             :   handling of the logical pathnames in general (other Lisps
;;;             :   didn't mind logical namestrings in places where a pathname
;;;             :   designator was required), and a shadowing of the CLisp
;;;             :   execute function.
;;; 2006.08.31 Dan
;;;             : * Replaced the *already-loaded-act-r-6-files* variable as
;;;             :   the reloading test with a feeature check for :act-r-6.0
;;;             :   which is now placed on the *features* list.
;;; 2007.01.17 Dan
;;;             : * Added the support necessary to load into SBCL.
;;;             : * Required changing all calls to logical-pathname in the
;;;             :   directory calls to translate-logical-pathname which should
;;;             :   work for all Lisps (SHOULD!).
;;;             : * NOTE that this doesn't work with SBCL 1.0 under Windows
;;;             :   because a bug in their directory command doesn't
;;;             :   recognize the wildcards but they've addressed that so future
;;;             :   versions of SBCL for Windows should work.
;;; 2007.02.02 Dan
;;;             : * Changed the uses for the packaged-actr so that all Lisps
;;;             :   use "COMMON-LISP" - should work, but this is still an
;;;             :   an experimental feature.
;;; 2007.02.05 Dan
;;;             : * Added the hack for the broken directory command in SBCL for
;;;             :   Windows so that it can load all of the files now.
;;; 2007.02.26 Dan
;;;             : * Added an appropriate fasl extension for LispWorks 5 on
;;;             :   x86 Macs.
;;; 2007.07.24 Dan
;;;             : * Finally added the right extension for LispWorks 5 for
;;;             :   Windows.
;;; 2007.08.03 Dan
;;;             : * Fixed the feature checks so that LispWorks 5 gets the
;;;             :   right value for Windows...
;;; 2007.09.10 Dan
;;;             : * Putting the LispWorks device file pointers in to allow use
;;;             :   of the beta device interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Using logical pathnames a directory structure for ACT-R 6 can be created
;;; that allows users to add or remove files from a specific directory within
;;; the system, and through the use of require and provide also remove the
;;; need to edit a "load order" file.
;;;
;;; The organization has 5 directories in the act-r6 directory: 
;;;  - framework contains the core code of the system which has its own
;;;              load file and is not supposed to be modified by users
;;;  - devices contains folders to hold the specific device interface and uwi
;;;            files for a particular lisp
;;;            each supported lisp should have a directory in the device
;;;            directory that contains one or two files which should be 
;;;            named device.lisp and uwi.lisp.   The device.lisp file should
;;;            contain the appropriate device interface methods and the 
;;;            uwi.lisp
;;;            file should contain the specific GUI functions that support
;;;            the AGI (ACT-R GUI interface) calls.
;;;
;;;           NOTE: This is one thing that will require changing this
;;;                 load file to add the specific switch and directory name for
;;;                 a new device definition set.
;;;  - support this is where one should place files that may be needed by 
;;;            a particular module or for other special purposes.  These files
;;;            are only loaded when made explicit (or implicit with require).
;;;  - core-modules this is where the core modules of the system are located.
;;;            These modules are referenced explicitly in the loader and 
;;;            if they exist are loaded in a specified order.  They consist
;;;            of the modules that were part of ACT-R 5 (though not always
;;;            implemented that way): Declarative, Goal, Procedural, Vision,
;;;            Motor, Audio, and Speech.  They are loaded in that order if
;;;            they exist.
;;;  - modules this is where any other modules of the system are to be placed.
;;;            All files with a .lisp extension in this folder will be loaded
;;;            in no particular order.  Thus, there should be no dependencies
;;;            among these modules.  Any code that may be needed by more than
;;;            one module should go in the support directory where it can
;;;            be indicated with a require in the module file.
;;;          
;;;   See the declarative (in modules) and central-parameters (in support)
;;;   or vision (in modules) and dmi (in support) for examples of the require/
;;;   provide usage.  
;;;   
;;;   NOTE: require isn't necessairily going to compile the required file,
;;;   so using the require-compiled function below is recommended.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; The logical hostname "ACT-R6" can be used as a relative reference for the
;;; directory where the ACT-R 6 folders are located.
;;;
;;;
;;; require-compiled (code-module pathname)
;;;
;;; code-module is a string that designates some code that needs to be loaded
;;;             which should have a corresponding (provide code-module)
;;; pathname is the pathname to where code-module can be found.
;;;
;;; Similar to the function require this will determine if the requested
;;; code-module has been loaded and if not will compile and load the file
;;; specified by pathname.  This differs from the normal require function
;;; in that the pathname is mandatory and it does not search through any
;;; implementation defaults to find the code-module.  However, it does still
;;; depend on a provide call existing in the code-module file so that
;;; it only loads the necessary file the first time it is required.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; The idea is for a system where people can just drop in new modules without
;;; having to edit or change any of the existing code.  In practice, that
;;; may not work all the time (with things like name conflicts) but should
;;; be useable.  Name conflicts could probably be eliminated through some
;;; sort of module packaging scheme, but that seems to complicate module
;;; creation and could lead to some potentially nasty debugging issues.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (make-package :act-r 
                               :use '("COMMON-LISP-USER" 
                                      "COMMON-LISP"
                                      #+:allegro "EXCL"
                                      #+:allegro-ide "COMMON-GRAPHICS-USER"
                                      #+:common-graphics "COMMON-GRAPHICS-USER"))


;;; Basically a hack for ACL 7 so that I don't have to touch every file!

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  #+(and :allegro :ide (not :allegro-ide))
    (push :allegro-ide *features*))

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


#+:act-r-6.0 (error "The ACT-R 6 load file should only be loaded once.")
#-:act-r-6.0 (pushnew :act-r-6.0 *features*)


;; Clisp has an implementation-specific function execute that conflicts with
;; the generic function execute in ACT-R, so shadow it
#+:clisp (defpackage "COMMON-LISP-USER" (:shadow "EXECUTE"))

;; SBCL has a function called reset we need to shadow and there's an issue
;; with their defconstat because it throws an error if you compile and then
;; load a file (it's fine with the compiled file later, but that first time
;; is a problem).

#+:sbcl (defpackage "COMMON-LISP-USER" (:shadow "RESET" "DEFCONSTANT"))
#+:sbcl (defmacro defconstant (name value &optional documentation)
          `(sb-c::%defconstant ',name ,value ',documentation (sb-c:source-location)))

;;; The Windows version of SBCL doesn't properly handle wild cards in the
;;; directory command so this hacks around that for now sufficiently to load
;;; the ACT-R files...

#+(and :sbcl :win32) 
  (defpackage "COMMON-LISP-USER" (:shadow "DIRECTORY"))
#+(and :sbcl :win32) 
(eval-when (:load-toplevel :execute)
  (defun directory (pathname &key)
    ;(format t "Calling the new directory for ~S~%" pathname)
    (if (not (string= (pathname-type pathname) "*"))
        (let* ((new-path (make-pathname :host (pathname-host pathname)
                                        :device (pathname-device pathname)
                                        :directory (pathname-directory pathname)
                                        :defaults "*.*"))
               (new-val (cl::directory new-path))
               (res (remove-if-not (lambda (x) 
                                     (string-equal (pathname-type x) (pathname-type pathname)))
                                   new-val)))
          ;(format t "Returning ~S from directory of new-path: ~s which returns ~s.~%" res new-path new-val)
          res)
      (cl::directory pathname))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create the logical host "ACT-R6" relative to the current location

#+:allegro (setf (logical-pathname-translations "ACT-R6")
             (list (list "**;*.*" (let ((name (namestring *load-truename*))
                                        (file (file-namestring *load-truename*)))
                                    (subseq name 0 (- (length name) (length file)))))))


#+:digitool (setf (logical-pathname-translations "ACT-R6")
  (list (list "**;*.*" (concatenate 'string
                         (host-namestring *load-truename*)
                         (directory-namestring *load-truename*) "**:"))))

#+:openmcl (setf (logical-pathname-translations "ACT-R6")
  (list (list "**;*.*" (concatenate 'string
                         (host-namestring *load-truename*)
                         (directory-namestring *load-truename*) "**/"))))

#+:lispworks (setf (logical-pathname-translations "ACT-R6")
               (list (list "**;*.*" 
                           (concatenate 'string
                             (format nil "~A" (make-pathname
                                               :host 
                                               (pathname-host *load-truename*)
                                               :directory 
                                               (pathname-directory 
                                                *load-truename*))) 
                             "**/*.*"))))

;; just copied the lispworks one for now...
#+:cmu (setf (logical-pathname-translations "ACT-R6")
               (list (list "**;*.*" 
                           (concatenate 'string
                             (format nil "~A" (make-pathname
                                               :host 
                                               (pathname-host *load-truename*)
                                               :directory 
                                               (pathname-directory 
                                                *load-truename*))) 
                             "**/*.*"))))

#+(or :clisp :sbcl) (setf (logical-pathname-translations "ACT-R6")
                      `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define the file extension (the pathname type) for compiled and source files
;;; in the currently supported systems

(unless (boundp '*.lisp-pathname*)
  (defvar *.lisp-pathname* 
      (make-pathname :type "lisp")))

(unless (boundp '*.fasl-pathname*)
  (defvar *.fasl-pathname* 
    #+:allegro (make-pathname :type "fasl")
    #+:sbcl (make-pathname :type "fasl")
    #+:clisp (make-pathname  :type "fas")
    #+(and :linux :cmu) (make-pathname :type "x86f")
    #+(and :ppc :cmu) (make-pathname :type "ppcf")
    #+(and :lispworks :win32 (not :lispworks5)) (make-pathname :type "fsl")
    #+(and :lispworks :win32 :lispworks5) (make-pathname :type "ofasl")
    #+(and :lispworks :unix (not :macosx)) (make-pathname :type "ufsl")
    #+(and :lispworks :macosx (not :x86)) (make-pathname :type "nfasl")
    #+(and :lispworks :macosx :x86) (make-pathname :type "xfasl")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define some functions for compiling and loading files

;;; compile-and-load (pathname)
;;;
;;; pathname a file pathname (or pathname string) if the file already
;;;          has a type specified, then it is ignored and the defaults
;;;          of lisp for source and system-dependent binary types are
;;;          used.
;;; 
;;; If a source file (.lisp) exists for the specified pathname then if there
;;; is no binary file (determined by *.fasl-pathname*), the binary is
;;; older than the source file, or the feature :act-r-recompile is set then 
;;; compile the source file into a binary and load it.  
;;;
;;; Based on the smart-load function from the ACT-R loader.


;;; Specific loader for the newer MCL 5/5.1

#+(and :ccl-4.3.5 :ccl-5.0) 
(defun compile-and-load (pathname)
  (when (pathname-type pathname) ;; throw away the type to allow for
                                 ;; the merging with a binary type
    (if (string-equal (pathname-type pathname) "lisp")
        (setf pathname (make-pathname :host (pathname-host pathname)
                                      :directory (pathname-directory pathname)
                                      :device (pathname-device pathname)
                                      :name (pathname-name pathname)))
      (error "To compile a file it must have a .lisp extension")))
  
  (let* ((srcpath (merge-pathnames pathname *.lisp-pathname*))
         (binpath (merge-pathnames pathname *.fasl-pathname*)))
    (unless (probe-file srcpath)
      (error "File ~S does not exist" srcpath))
    (when (or (member :actr-recompile *features*)
              (not (probe-file binpath))
              (> (file-write-date srcpath) (file-write-date binpath)))
      (compile-file srcpath :output-file binpath :external-format :unix))
    (load binpath)))
  
#-(and :ccl-4.3.5 :ccl-5.0) 
(defun compile-and-load (pathname)
  (when (pathname-type pathname) ;; throw away the type to allow for
                                 ;; the merging with a binary type
    (if (string-equal (pathname-type pathname) "lisp")
        (setf pathname (make-pathname :host (pathname-host pathname)
                                      :directory (pathname-directory pathname)
                                      :device (pathname-device pathname)
                                      :name (pathname-name pathname)))
      (error "To compile a file it must have a .lisp extension")))
  
  (let* ((srcpath (merge-pathnames pathname *.lisp-pathname*))
         (binpath (merge-pathnames pathname *.fasl-pathname*)))
    (unless (probe-file srcpath)
      (error "File ~S does not exist" srcpath))
    (when (or (member :actr-recompile *features*)
              (not (probe-file binpath))
              (> (file-write-date srcpath) (file-write-date binpath)))
      (compile-file srcpath :output-file binpath))
    (load binpath)))
  


;;; SMART-LOAD      [Function]
;;; Date        : 99.12.21
;;; Description : Loads binary version of a specified file.  Of course, the 
;;;             : said binary version might not exist or be older than the 
;;;             : source version, in which case the source file is compiled 
;;;             : before loading.
;;;             : Updated to add an option parameter to determine whether
;;;             : to just warn of a missing file or to throw an error.


(defun smart-load (this-files-dir file &optional (error? nil))
  "Loads binary <file> in directory <this-files-dir> or compiles and loads 
   source version"
  (let* ((srcpath (merge-pathnames  
                   (merge-pathnames file *.lisp-pathname*)
                   this-files-dir))
         )
    (if (not (probe-file srcpath))
        (if error? 
            (error "File ~S does not exist" srcpath)
          (format *error-output* "File ~S does not exist" srcpath)))
    (compile-and-load srcpath)))


;;; require-compiled (code-module pathname)
;;;
;;; code-module is a string that designates some code that needs to be loaded
;;;             which should have a corresponding (provide code-module) in it
;;; pathname is the pathname to where that code-module can be found (including
;;;          the file's name).
;;;
;;; Similar to the function require this will determine if the requested
;;; code-module has been loaded and if not will compile and load the file
;;; specified by pathname.  This differs from the normal require function
;;; in that the pathname is mandatory and it does not search through any
;;; implementation defaults to find the code-module.  However, it does still
;;; depend on a provide call existing in the code-module file so that
;;; it only loads the necessary file the first time it is required.

(defmacro require-compiled (code-module pathname)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (member ,code-module *modules* :test #'string=)
       (compile-and-load (translate-logical-pathname ,pathname)))))                       


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load any special system support files here

#+(and :mcl (not :openmcl)) (require 'quickdraw)

#+:ccl-5.0
(when (osx-p)
  (load "ACT-R6:support;CFBundle.lisp"))

#+:allegro (when (or (eq :case-sensitive-lower *current-case-mode*)
                     (eq :case-sensitive-upper *current-case-mode*))
             (unless 
                 (yes-or-no-p 
                  "WARNING: you are using a case sensitive Lisp.  ACT-R may not load or run correctly.  Continue anyway?")
               (break)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load the framework's loader file (it is order dependent)

(smart-load (translate-logical-pathname "ACT-R6:framework;") "framework-loader.lisp")

(dolist (the-file *file-list)
  (smart-load (translate-logical-pathname "ACT-R6:framework;") the-file t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load the core modules 

(smart-load (translate-logical-pathname "ACT-R6:core-modules;") "core-loader.lisp")

(dolist (the-file *file-list)
  (smart-load (translate-logical-pathname "ACT-R6:core-modules;") the-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; First, load any additional extensions.

(dolist (file (directory (translate-logical-pathname "ACT-R6:commands;*.lisp")))
  (compile-and-load file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indicate that there is a device available so that it can be loaded 
;;; When a new device is added it should be included with a switch below

(defvar *device-interface-pathname* nil)

;;; Here are the devices that are defined

#+:allegro-ide (setf *device-interface-pathname* "ACT-R6:devices;acl;")

#+:digitool (setf *device-interface-pathname* "ACT-R6:devices;mcl;")

#+:lispworks (setf *device-interface-pathname* "ACT-R6:devices;lw;")


;;; Load the virtual device

(compile-and-load (translate-logical-pathname "ACT-R6:devices;virtual;device.lisp"))
(compile-and-load (translate-logical-pathname "ACT-R6:devices;virtual;uwi.lisp"))

;;; Load any Lisp specific device that's defined

(when *device-interface-pathname*
  (if (probe-file (merge-pathnames *device-interface-pathname* "device.lisp"))
      (compile-and-load (merge-pathnames *device-interface-pathname* 
                                         "device.lisp"))
    (format t 
        "################~%#### No Device file found in ~S ####~%##############"
      *device-interface-pathname*))
  (if (probe-file (merge-pathnames *device-interface-pathname* "uwi.lisp"))
      (compile-and-load (merge-pathnames *device-interface-pathname* 
                                         "uwi.lisp"))
    (format t 
        "#################~%#### No uwi file found in ~S ####~%################"
      *device-interface-pathname*)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; After the modules and devices files are done load any files in the 
;;; modules, tools and then finally the other-files drectories.

(dolist (file (directory (translate-logical-pathname "ACT-R6:modules;*.lisp")))
  (compile-and-load file))

(dolist (file (directory (translate-logical-pathname "ACT-R6:tools;*.lisp")))
  (compile-and-load file))

(dolist (file (directory (translate-logical-pathname "ACT-R6:other-files;*.lisp")))
  (compile-and-load file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print a conformation message to let the user know ACT-R has been loaded
;;; along with the version numbers of all the modules.

(format t "~%##################################~%")
(mp-print-versions )
(format t "~%######### Loading of ACT-R 6 is complete #########~%")



#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
