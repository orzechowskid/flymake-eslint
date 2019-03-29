# flymake-eslint
Flymake backend for Javascript using eslint

## Installation

0. Make sure `eslint` is installed and present on your emacs `exec-path`.  For Linux systems `exec-path` usually equals your `$PATH` environment variable; for other systems, you're on your own.
1. Download and place inside `~/.emacs.d/lisp`
2. Edit `~/.emacs` or equivalent:
```lisp
(add-to-list 'load-path "~/.emacs.d/lisp")
(require "flymake-eslint.el")
```

## Usage

also in `~/.emacs` or wherever:
```lisp
(add-hook 'web-mode-hook ; or whatever the mode-hook is for your mode of choice
  (lambda ()
    (flymake-mode) ; or some other mode like eglot-mode which enables flymake-mode automatically
    (flymake-eslint-enable)))
```
## Customization

useful variables are members of the `flymake-eslint` group and can be viewed and modified with the command `M-x customize-group [RET] flymake-eslint [RET]`.

## Bugs

a lot.  this isn't ready for use yet.
