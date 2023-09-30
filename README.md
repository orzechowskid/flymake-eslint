# flymake-eslint
Flymake backend for Javascript using eslint

[![MELPA](https://melpa.org/packages/flymake-eslint-badge.svg)](https://melpa.org/#/flymake-eslint)

## Installation

0. Make sure `eslint` is installed and present on your emacs `exec-path`.  For Linux systems `exec-path` usually equals your `$PATH` environment variable; for other systems, you're on your own.
1. Install:
  - from MELPA: `M-x package-install [RET] flymake-eslint [RET]`
  - manually: download and place inside `~/.emacs.d/lisp` then edit `~/.emacs` or equivalent:
  ```lisp
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (require "flymake-eslint.el")
  ```
2. Enable:
```lisp
(add-hook 'web-mode-hook ; or whatever the mode-hook is for your mode of choice
  (lambda ()
    (flymake-eslint-enable)))
```
## Customization

useful variables are members of the `flymake-eslint` group and can be viewed and modified with the command `M-x customize-group [RET] flymake-eslint [RET]`.

```lisp
(defcustom flymake-eslint-executable-name "eslint"
  "Name of executable to run when checker is called.  Must be present in variable `exec-path'."
  :type 'string
  :group 'flymake-eslint)

(defcustom flymake-eslint-executable-args nil
  "Extra arguments to pass to eslint."
  :type 'string
  :group 'flymake-eslint)

(defcustom flymake-eslint-show-rule-name t
  "Set to t to append rule name to end of warning or error message, nil otherwise."
  :type 'boolean
  :group 'flymake-eslint)
  
(defcustom flymake-eslint-defer-binary-check nil
  "Set to t to bypass the initial check which ensures eslint is present.

Useful when the value of variable `exec-path' is set dynamically and the location of eslint might not be known ahead of time."
  :type 'boolean
  :group 'flymake-eslint)

(defcustom flymake-eslint-project-root nil
  "Buffer-local.  Set to a filesystem path to use that path as the current working directory of the linting process."
  :type 'string
  :group 'flymake-eslint)
  
(defcustom flymake-eslint-prefer-json-diagnostics nil
  "Try to use the JSON diagnostic format when runnin ESLint.
This gives more accurate diagnostics but requires having an Emacs
version with JSON support."
  :type 'boolean
  :group 'flymake-eslint)
```

## Bugs

yes

## See Also

[flymake-stylelint](https://github.com/orzechowskid/flymake-stylelint)

## License

MIT
