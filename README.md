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
* **CTRL + SPACE** Set mark
* **UP** Up line
* **DOWN** Down line
* **LEFT** Forward character
* **RIGHT** Backward character
* **CTRL + LEFT** Forward word
* **CTRL + RIGHT** Backward word
* **CTRL + UP** Forward five lines.
* **CTRL + DOWN** Backward five lines.
* **RETURN** New line
* **CTRL + s** Save buffer
* **CTRL + o** Open file
* **CTRL + w** Close emacs
* **CTRL + f** Regex search
* **CTRL + SHIFT + f** Regex search backwards
* **RETURN** Next search match
* **SHIFT + RETURN** Previous search match
* **CTRL + a** Select all
* **CTRL + z** Undo
* **CTRL + y** Redo
* **CTRL + c** Copy
* **CTRL + x** Cut
* **CTRL + v** Paste
* **CTRL + b** Change buffer
* **CTRL + g** Ace search mode
* **CTRL + SHIFT + g** Go to line
* **CTRL + p** Open file 1in project
* **CTRL + SHIFT + p** Execute command
* **F1** Close other windows
* **F2** Split window horizontally
* **F3** Split window vertically
* **F4** Start terminal
* **F6** Git status
* **CTRL + /** Toggle comment line
* **CTRL + SHIFT + /** Toggle comment region.
* **CTRL + RETURN** Mark region rectangle.
* **CTRL + CLICK** Add cursor
* **CTRL + SHIFT + a** Add cursor to all like this
* **CTRL +S SHIFT + r** Replicate line
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
* **CTRL + SHIFT + RIGHT** Forward sexp
* **CTRL + SHIFT + LEFT** Backward sexp
* **CTRL + SHIFT + k** Cut sexp
* **CTRL + SHIFT + d** Delete sexp
* **ALT + UP** Last repl command
* **ALT + DOWN** Previous repl command
* **ALT + x + e** Evaluate selected sexp
* **ALT + x + n** Set repl namespace to buffers
* **ALT + x + c** Load buffer into repl

## TODO
