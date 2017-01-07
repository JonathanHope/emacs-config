# Emacs Configuration

## Summary

This is my emacs configuration. It is primarily used for clojure development. The keyboard shortcuts have been remapped to more CUA friendly ones where possible. Much of the functionality is inspired by more modern editors like sublime text.

## Requirements

* Emacs 24.4

## Installation

Check this project out into ~/.emacs.d. 
On windows be sure to set your home: setx HOME C:/Users/you.
On windows you can add a Magit context menu using the following startup: C:\Program Files\emacs\bin\runemacs.exe --eval (revert-default-directory) --eval (magit-status) --eval (delete-other-windows)

## Keymap

### General - Editing

#### Selections

* **CTRL + SPACE** Set mark
* **CTRL + RETURN** Mark region rectangle.
* **CTRL + a** Select all
* **CTRL + CLICK** Add cursor
* **CTRL + SHIFT + a** Add cursor to all like this
* **CTRL + =** Expand region
* **CTRL + -** Contract region

#### Adding lines

* **RETURN** New line

#### Undo/Redo

* **CTRL + z** Undo
* **CTRL + y** Redo

#### Cut/Copy/Paste

* **CTRL + c** Copy
* **CTRL + x** Cut
* **CTRL + v** Paste
* **CTRL + k** Cut line

#### Comments

* **CTRL + /** Toggle comment line
* **CTRL + SHIFT + /** Toggle comment region.

#### Lines

* **CTRL +S SHIFT + r** Replicate line
* **CTRL + SHIFT + UP** Move line of text up
* **CTRL + SHIFT + DOWN** Move line of text down

#### Indentation

* **TAB** Indent region
* **SHIFT + TAB** Unindent a region
* **CTRL + i** Auto indent

#### Deletions

* **CTRL + d** Delete line
* **CTR + BACKSPACE** Delete word
* **BACKSPACE** Delete character
* **SHIFT + BACKSPACE** Delete region

#### Casing 

* **CTRL + SHIFT + u** Uppercase region
* **CTRL + SHIFT + l** Lowercase region

### General - Navigation

#### Canceling

* **ESCAPE** Cancel

#### Directional Movement

* **UP** Up line
* **DOWN** Down line
* **LEFT** Forward character
* **RIGHT** Backward character
* **CTRL + LEFT** Forward word
* **CTRL + RIGHT** Backward word
* **CTRL + UP** Forward five lines
* **CTRL + DOWN** Backward five lines

#### Regex Searching

* **CTRL + f** Regex search
* **CTRL + SHIFT + f** Regex search backwards
* **RETURN** Next search match
* **SHIFT + RETURN** Previous search match

#### Buffer Switching

* **CTRL + b** Change buffer

#### High Level Navigation

* **CTRL + g** Ace search mode
* **CTRL + SHIFT + g** Go to line
* **CTRL + p** Go to file in project

* **CTRL + SHIFT + p** Execute command

### General - Files

* **CTRL + s** Save file
* **CTRL + o** Open file
* **CTRL + w** Close file

### General - Windowing

#### Cose Emacs

* **CTRL + SHIFT + w** Close emacs

#### Adding and Removing Windows

* **F1** Close other windows
* **F2** Split window horizontally
* **F3** Split window vertically

#### Launching Emacs Modes

* **F4** Start terminal
* **F5** Launch Org-mode
* **F6** Launch magit

#### Launch Contextual Menu

* **CTRL + TAB** Launch contextual menu

### Clojure - Editing

* **CTRL + SHIFT + 0** Forward slurp
* **CTRL + SHIFT + ]** Forward barf
* **CTRL + SHIFT + 9** Backward slurp
* **CTRL + SHIFT + [** Backward barf
* **ALT + SHIFT + 9** Wrap in parentheses
* **CTRL + SHIFT + RIGHT** Forward sexp
* **CTRL + SHIFT + LEFT** Backward sexp
* **CTRL + SHIFT + k** Cut sexp
* **CTRL + SHIFT + d** Delete sexp

### Clojure - REPL

* **F7** Launch REPL
* **ALT + UP** Last repl command
* **ALT + DOWN** Previous repl command
* **ALT + x + e** Evaluate selected sexp
* **ALT + x + n** Set repl namespace to buffers
* **ALT + x + c** Load buffer into repl

### Magit - General

* **ALT + c** Commit with the entered commit message

### Org-mode - Editing

* **ALT + RETURN** Add heading/item
* **ALT + SHIFT + RETURN** Add todo/checkbox
* **TAB** Next field in table
* **SHIFT + TAB** Previous field in table
* **ALT + LEFT** Demote headline
* **ALT + RIGHT** Promote headline
* **ALT + UP** Move item up
* **ALT + DOWN** Move item down
* **SHIFT + LEFT** Toggle todo status and toggle list style
* **SHIFT + RIGHT** Toggle todo status and toggle list style
* **SHIFT + UP** Toggle todo priority
* **SHIFT + DOWN** Toggle todo priority
* **TAB** Toggle visibility