(defpackage lem-solarized/colors
  (:use :cl)
  (:export #:+base03+
           #:+base02+
           #:+base01+
           #:+base00+
           #:+base0+
           #:+base1+
           #:+base2+
           #:+base3+
           #:+yellow+
           #:+orange+
           #:+red+
           #:+magenta+
           #:+violet+
           #:+blue+
           #:+cyan+
           #:+green+
           #:+all-colors+
           #:change-colors))
(in-package lem-solarized/colors)


;; Dark backgrounds
(defparameter +base03+ "#002b36")
(defparameter +base02+ "#073642")

;; Content colors
(defparameter +base01+ "#586e75")
(defparameter +base00+ "#657b83")
(defparameter +base0+ "#839496")
(defparameter +base1+ "#93a1a1")

;; Light backgrounds
(defparameter +base2+ "#eee8d5")
(defparameter +base3+ "#fdf6e3")

;; Additional palette
(defparameter +yellow+ "#b58900")
(defparameter +orange+ "#cb4b16")
(defparameter +red+ "#dc322f")
(defparameter +magenta+ "#d33682")
(defparameter +violet+ "#6c71c4")
(defparameter +blue+ "#268bd2")
(defparameter +cyan+ "#2aa198")
(defparameter +green+ "#859900")

(defparameter +all-colors+
  '(+base03+
    +base02+
    +base01+
    +base00+
    +base0+
    +base1+
    +base2+
    +base3+
    +yellow+
    +orange+
    +red+
    +magenta+
    +violet+
    +blue+
    +cyan+
    +green+))


#+lem-ncurses
(defun change-colors ()
  ;; Here we'll change a part of the palette at the end.
  ;; I hope this will not break something.
  (loop for idx upfrom (- (length lem.term::*colors*)
                          (length +all-colors+)
                          1)
        for color in +all-colors+
        for parsed = (lem:parse-color (symbol-value color))
        do (apply #'lem.term:term-set-color idx parsed)))