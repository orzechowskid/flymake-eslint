# flymake-eslint
Flymake backend for Javascript using eslint

Download to `~/.emacs.d/lisp`, then edit `~/.emacs` or equivalent:
```lisp
(add-to-list 'load-path "~/.emacs.d/lisp")
(require "flymake-eslint.el")
(add-hook 'web-mode-hook ; or whatever the mode-hook is for your mode of choice
  (lambda ()
    (flymake-mode) ; or some other mode like eglot-mode which enables flymake-mode automatically
    (flymake-eslint-enable)))
```
