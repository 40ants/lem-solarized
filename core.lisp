(defpackage lem-solarized
  (:use :cl)
  (:nicknames :lem-solarized/cores))
(in-package lem-solarized)

(defparameter +base03+ "#002b36")
(defparameter +base02+ "#073642")
(defparameter +base01+ "#586e75")
(defparameter +base00+ "#657b83")
(defparameter +base0+ "#839496")
(defparameter +base1+ "#93a1a1")

(defparameter +base2+ "#eee8d5")
(defparameter +base3+ "#fdf6e3")

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-color (mode symbol)
    (case mode
      (:light symbol)
      (:dark (case symbol
               (+base03+ '+base3+)
               (+base02+ '+base2+)
               (+base01+ '+base1+)
               (+base00+ '+base0+)
               (+base0+ '+base00+)
               (+base1+ '+base01+)
               (+base2+ '+base02+)
               (+base3+ '+base03+)
               (t symbol)))))


  (defun make-attribute (definition mode default-foreground default-background)
    "Replaces "
    (loop with next-is-color = nil
          with result = nil
          with has-fg = nil
          with has-bg = nil
          for item in definition
          do (cond
               (next-is-color
                (push (make-color mode item)
                      result)
                (setf next-is-color nil))
               ((eql item :fg)
                (push :foreground
                      result)
                (setf next-is-color t
                      has-fg t))
               ((eql item :bg)
                (push :background
                      result)
                (setf next-is-color t
                      has-bg t))
               (t (push item
                        result)))
          finally (when (or has-fg has-bg)
                    (unless has-fg
                      (push :foreground
                            result)
                      (push (make-color mode default-foreground)
                            result))
                    (unless has-bg
                      (push :background
                            result)
                      (push (make-color mode default-background)
                            result)))
                  (return
                    (nreverse result)))))


(defmacro define-solarized (mode)
  (unless (member mode '(:light :dark))
    (error "Bad mode ~A. Should be :light or :dark" mode))

  (let* ((theme-name (format nil "solarized-~A"
                             (string-downcase
                              (symbol-name mode))))
         (foreground '+base03+)
         (background '+base3+)
         (rules `((lem:cursor :fg ,background :bg +base00+)
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
                  ;; Lisp mode
                  (LEM.LISTENER-MODE:LISTENER-PROMPT-ATTRIBUTE :fg +blue+)
                  (lem:modeline :fg +base01+ :bg +base2+)
                  (lem:modeline-inactive :fg +base00+ :bg +base2+)
                  )))
    `(lem:define-color-theme ,theme-name ()
       (lem:display-background-mode ,mode)
       (lem:foreground ,(make-color mode foreground))
       (lem:background ,(make-color mode background))

       ,@(loop for definition in rules
               collect (make-attribute definition mode foreground background)))))


(define-solarized :light)
(define-solarized :dark)


(lem:define-command test-theme () ()
  (asdf:load-system :lem-solarized)
  (lem:load-theme "solarized-light"))


(defun change-colors ()
  ;; Here we'll change a part of the palette at the end.
  ;; I hope this will not break something.
  (loop for idx upfrom (- (length lem.term::*colors*)
                          (length +all-colors+)
                          1)
        for color in +all-colors+
        for parsed = (lem:parse-color (symbol-value color))
        do (apply #'lem.term:term-set-color idx parsed)))


(lem:define-command solarized-light () ()
                    (change-colors)
                    (lem:load-theme "solarized-light"))


(lem:define-command solarized-dark () ()
                    (change-colors)
                    (lem:load-theme "solarized-dark"))


(lem:define-command solarized-colors () ()
  "Draws a solarized palette in a separate buffer.
   Useful to check if all colors are distinguishable from
   each other."
                    (let* ((buffer (lem:make-buffer "*colors*"))
                           (max-length
                            (loop for color in +all-colors+
                               for name = (symbol-name color)
                               maximizing (length name)))
                           (left-format (format nil " ~~~A@A " max-length))
                           (right-format (format nil " ~~~AA " max-length)))
    
                      (setf (lem:buffer-read-only-p buffer)
                            nil)
    
                      (lem:switch-to-buffer buffer)
                      (lem:erase-buffer)
                      (loop for color in +all-colors+
                         for value = (symbol-value color)
                         for name = (symbol-name color)
                         do (lem:insert-string
                             (lem:current-point)
                             (format nil left-format name)
                             :attribute
                             (lem:make-attribute :background :black
                                                 :foreground value))
                           (lem:insert-string
                            (lem:current-point)
                            (format nil right-format name)
                            :attribute
                            (lem:make-attribute :background value
                                                :foreground :black))
                           (lem:insert-character
                            (lem:current-point)
                            #\Newline))
                      (setf (lem:buffer-read-only-p buffer)
                            t)))


;; (PROGN
;;   (SETF (GETHASH "solarized-light" LEM::*COLOR-THEMES*)
;;         (LEM::MAKE-COLOR-THEME :SPECS
;;                                (LIST
;;                                 (LIST 'LEM:DISPLAY-BACKGROUND-MODE :LIGHT)
;;                                 (LIST 'LEM:FOREGROUND +BASE03+)
;;                                 (LIST 'LEM:BACKGROUND +BASE3+)
;;                                 (LIST 'LEM:CURSOR :FOREGROUND +BASE3+
;;                                       :BACKGROUND +BASE00+)
;;                                 (LIST 'LEM:SYNTAX-WARNING-ATTRIBUTE
;;                                       :FOREGROUND +RED+ :BACKGROUND +BASE3+)
;;                                 (LIST 'LEM-BASE:SYNTAX-STRING-ATTRIBUTE
;;                                       :FOREGROUND +CYAN+ :BACKGROUND +BASE3+)
;;                                 (LIST 'LEM-BASE:SYNTAX-COMMENT-ATTRIBUTE
;;                                       :FOREGROUND +BASE01+ :BACKGROUND
;;                                       +BASE3+)
;;                                 (LIST 'LEM-BASE:SYNTAX-KEYWORD-ATTRIBUTE
;;                                       :FOREGROUND +GREEN+ :BACKGROUND +BASE3+)
;;                                 (LIST 'LEM-BASE:SYNTAX-CONSTANT-ATTRIBUTE
;;                                       :FOREGROUND +CYAN+ :BACKGROUND +BASE3+)
;;                                 (LIST 'LEM-BASE:SYNTAX-FUNCTION-NAME-ATTRIBUTE
;;                                       :FOREGROUND +BLUE+ :BACKGROUND +BASE3+)
;;                                 (LIST 'LEM-BASE:SYNTAX-VARIABLE-ATTRIBUTE
;;                                       :FOREGROUND +BLUE+ :BACKGROUND +BASE3+)
;;                                 (LIST 'LEM-BASE:SYNTAX-TYPE-ATTRIBUTE
;;                                       :FOREGROUND +RED+ :BACKGROUND +BASE3+)
;;                                 (LIST 'LEM:SYNTAX-BUILTIN-ATTRIBUTE
;;                                       :FOREGROUND +GREEN+ :BACKGROUND
;;                                       +BASE2+))
;;                                :PARENT NIL)))