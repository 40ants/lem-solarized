# lem-solarized

This is a color theme for the [Lem](https://github.com/cxxxr/lem) ediitor.

It uses [Solarized palette](https://ethanschoonover.com/solarized/) to define light and dark themes:

![](https://github.com/altercation/solarized/raw/master/img/solarized-vim.png)

## Installation

```lisp
(unless (member "ultralisp" (ql-dist:all-dists)
                :key 'ql-dist:name
                :test 'string=)
  (ql-dist:install-dist "http://dist.ultralisp.org/"
                        :prompt nil))

;; Load the extension
(ql:quickload :lem-solarized)

;; Enable the theme
(uiop:symbol-call :lem-solarized :solarized-light)
```
