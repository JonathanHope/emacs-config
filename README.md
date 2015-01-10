# Emacs Configuration

## Summary

This is my emacs configuration. It is primarily used for clojure development. The keyboard shortcuts have been remapped to more CUA friendly ones where possible. Much of the functionality is inspired by more modern editors like sublime text.

## Requirements

* Emacs 24.4

## Installation

Check this project out into ~/.emacs.d.

## Packages Used

* Undo Tree
* Projectile
* Helm
* Multiple Cursors
* Magit
* Clojure Mode
* Clojure Mode Extra Font Locking
* Paredit
* Rainbow delimeters
* Cider
* Company
* Markdown Mode
* Expand region
* Ace jump mode

## Keymap

### General

* **ESCAPE** Cancel
* **Hold SHIFT** Mark region
* **UP** Up line
* **DOWN** Down line
* **LEFT** Forward character
* **RIGHT** Backward character
* **CTRL + LEFT** Forward word
* **CTRL + RIGHT** Backward word
* **RETURN** New line
* **CTRL + s** Save buffer
* **CTRL + o** Open file
* **CTRL + w** Close emacs
* **CTRL + f** Regex search
* **CTRL + SHIFT + f** Regex search backwards
* **RETURN** Next search match
* **SHIFT + RETURN** Previous search match
* **CTRL + z** Undo
* **CTRL + y** Redo
* **CTRL + c** Copy
* **CTRL + x** Cut
* **CTRL + v** Paste
* **CTRL + b** Change buffer
* **CTRL + g** Ace search mode
* **CTRL + SHIFT + g** Go to line
* **CTRL + p** Open file in project
* **CTRL + SHIFT + p** Execute command
* **F1** Close other windows
* **F2** Split window horizontally
* **F3** Split window vertically
* **F4** Start terminal
* **F6** Git status
* **CTRL + /** Toggle comment line
* **CTRL + SHIFT + /** Toggle comment region.
* **CTRL + RETURN** Mark region rectangle.
* **CTRL + SHIFT + c** Add cursors to start of selected lined
* **CTRL + CLICK** Add cursor
* **CTRL + SHIFT + a** Add cursor to all like this
* **CTRL +S SHIFT + r** Duplicate line
* **CTRL + SHIFT + UP** Move line of text up
* **CTRL + SHIFT + DOWN** Move line of text down
* **TAB** Indent region
* **SHIFT + TAB** Unindent a region
* **CTRL + k** Cut line
* **CTRL + d** Delete line
* **CTR + BACKSPACE** Delete word
* **BACKSPACE** Delete character
* **SHIFT + BACKSPACE** Delete region
* **CTRL + =** Expand region
* **CTRL + -** Contract region
* **CTRL + i** Auto indent
* **CTRL + SHIFT + u** Uppercase region
* **CTRL + SHIFT + l** Lowercase region

### Clojure

* **CTRL + SHIFT + 0** Forward slurp
* **CTRL + SHIFT + ]** Forward barf
* **CTRL + SHIFT + 9** Backward slurp
* **CTRL + SHIFT + [** Backward barf
* **ALT + SHIFT + 9** Wrap in parentheses
* **CTRL + UP** Last repl command
* **CTRL + DOWN** Previous repl command
* **CTRL + SHIFT + RIGHT** Forward sexp
* **CTRL + SHIFT + LEFT** Backward sexp
* **CTRL + SHIFT + k** Cut sexp
* **CTRL + SHIFT + d** Delete sexp
* **ALT + x + e** Evaluate selected sexp
* **ALT + x + n** Set repl namespace to buffers
* **ALT + x + c** Load buffer into repl

## TODO

* Remap CTRL + UP and CTRL + DOWN to ALT + UP AND ALT + DOWN.
