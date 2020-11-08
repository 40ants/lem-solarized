
(defpackage lem-solarized/dark
  (:use #:cl
        #:lem-solarized/colors)
  (:import-from #:lem-theme/spec
                #:defspec)
  (:import-from #:lem-solarized/light)
  (:import-from #:lem-theme/theme
                #:background-mode))
(in-package lem-solarized/dark)


(defvar *color-inversion*
  (list (cons +base03+ +base3+)
        (cons +base02+ +base2+)
        (cons +base01+ +base1+)
        (cons +base00+ +base0+)
        (cons +base0+ +base00+)
        (cons +base1+ +base01+)
        (cons +base2+ +base02+)
        (cons +base3+ +base03+)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass solarized-dark (lem-solarized/light::solarized-light)
    ;; Base theme will invert light color theme into the dark
    ;; automatically
    ((background-mode :initform :dark))))


(defmethod lem-theme/spec:make-color ((theme solarized-dark) value)
  (let ((new-color (assoc value *color-inversion*
                          :test #'equal)))
    (if new-color
        (cdr new-color)
        value)))


;; We need this defspec, because it updates Lem's internal
;; color-theme representation
(defspec (solarized-dark))