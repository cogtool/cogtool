;; netica demo
(defpackage "NETICA-DEMO" (:use "CL" "EXT" "FFI" "NETICA"))
(in-package "NETICA-DEMO")

(defvar *x-step* 100)
(defvar *y-step* 100)

(netica:start-netica)
(defparameter *net* (netica:make-net :name "AsiaEx"))
(defparameter *visit-asia*
  (netica:make-node :name "VisitAsia" :net *net*
                    :x (* 1d0 *x-step*) :y (* 1d0 *y-step*)
                    :cpt '((#() . #(0.01f0 0.99f0)))
                    :states '("visit" "no_visit")))
(defparameter *tuberculosis*
  (netica:make-node :name "Tuberculosis" :net *net*
                    :x (* 1d0 *x-step*) :y (* 2d0 *y-step*)
                    :parents (list *visit-asia*)
                    :cpt '((#("visit") . #(0.05f0 0.95f0))
                           (#("no_visit") . #(0.01f0 0.99f0)))
                    :states '("present" "absent")))
(defparameter *smoking*
  (netica:make-node :name "Smoking" :net *net*
                    :x (* 7d0 *x-step*) :y (* 1d0 *y-step*)
                    :cpt '((#() . #(0.5f0 0.5f0)))
                    :states '("smoker" "nonsmoker")))
(defparameter *cancer*
  (netica:make-node :name "Cancer" :net *net*
                    :x (* 7d0 *x-step*) :y (* 2d0 *y-step*)
                    :title "Lung Cancer" :parents (list *smoking*)
                    :cpt '((#("smoker") . #(0.1f0 0.9f0))
                           (#("nonsmoker") . #(0.01f0 0.99f0)))
                    :states '("present" "absent")))
(defparameter *tb-or-ca*
  (netica:make-node :name "TbOrCa" :net *net*
                    :x (* 4d0 *x-step*) :y (* 3d0 *y-step*)
                    :title "Tuberculosis or Cancer"
                    :parents (list *tuberculosis* *cancer*)
                    :cpt '((#("present" "present") . #(1f0 0f0))
                           (#("present" "absent") . #(1f0 0f0))
                           (#("absent" "present") . #(1f0 0f0))
                           (#("absent" "absent") .  #(0f0 1f0)))
                    :states '("true" "false")))
(defparameter *xray*
  (netica:make-node :name "XRay" :net *net*
                    :x (* 4d0 *x-step*) :y (* 4d0 *y-step*)
                    :parents (list *tb-or-ca*)
                    :cpt '((#("true") . #(0.98f0 0.02f0))
                           (#("false") . #(0.05f0 0.95f0)))
                    :states '("abnormal" "normal")))

(format t "~&Compiling net...~%")
(netica:CompileNet_bn *net*)
(netica:check-errors)

(format t "~&Original probabilities:~%")
(netica:get-beliefs *tuberculosis* :verbose t)

(netica:enter-finding *net* "XRay" "abnormal")
(format t "~&Given an abnormal X-ray:~%")
(netica:get-beliefs *tuberculosis* :verbose t)

(netica:enter-finding *net* "VisitAsia" "visit")
(format t "~&Given an abnormal X-ray and a visit to Asia:~%")
(netica:get-beliefs *tuberculosis* :verbose t)

(netica:enter-finding *net* "Cancer" "present")
(format t "~&Given abnormal X-ray, Asia visit, and lung cancer:~%")
(netica:get-beliefs *tuberculosis* :verbose t)

(netica:save-net *net* :file "asia")

(netica:net-info *net*)

;; termination
(netica:DeleteNet_bn *net*)
(netica:close-netica)
