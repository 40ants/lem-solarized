(defpackage lem-solarized/light
  (:use #:cl
        #:lem-solarized/colors)
  (:import-from #:lem-theme/spec
                #:defspec)
  (:import-from #:lem-theme/theme
                #:theme
                #:background-mode
                #:background
                #:foreground))
(in-package lem-solarized/light)


(defclass solarized-light (theme)
  ((background-mode :initform :light)
   (background :initform +base3+)
   (foreground :initform +base03+)))


(defmethod lem-theme/theme:all-colors ((theme solarized-light))
  +all-colors+)

(defmethod lem-theme/theme:on-load ((theme solarized-light))
  (lem-solarized/colors:change-colors))


(defspec (solarized-light)
  (lem:cursor :fg :background :bg +base00+)
  (lem:syntax-warning-attribute :fg +red+)
  (lem:syntax-string-attribute :fg +cyan+)
  (lem:syntax-comment-attribute :fg +base01+)
  (lem:syntax-keyword-attribute :fg +green+)
  (lem:syntax-constant-attribute :fg +cyan+)
  (lem:syntax-function-name-attribute :fg +blue+)
  (lem:syntax-variable-attribute :fg +blue+)
  (lem:syntax-type-attribute :fg +red+)
  (lem:region :fg +base1+ :bg +base3+ :reverse-p t)
  (lem:minibuffer-prompt-attribute :fg +cyan+ :bold-p t)
  ;; Builtin functions and Lisp keyword symbols:
  (lem:syntax-builtin-attribute :fg +green+)
  (lem:modeline :fg +base01+ :bg +base2+ :bold-p t)
  (lem:modeline-inactive :fg +base00+ :bg +base2+))


(defspec (solarized-light)
  (lem.listener-mode:listener-prompt-attribute :fg +blue+))

(defspec (solarized-light)
  (LEM-LISP-MODE:INSPECTOR-VALUE-ATTRIBUTE :fg +cyan+)
  (LEM-LISP-MODE:INSPECTOR-ACTION-ATTRIBUTE :fg +red+))
