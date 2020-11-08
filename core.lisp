(defpackage lem-solarized
  (:use :cl)
  (:nicknames #:lem-solarized/core)
  (:import-from #:lem-solarized/commands
                #:solarized-light
                #:solarized-dark)
  (:export #:solarized-light
           #:solarized-dark))
(in-package lem-solarized)
