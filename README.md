# Emacs Configuration

## Summary

This is my Emacs configuration. The keyboard shortcuts have been remapped to more CUA friendly ones where possible. Much of the functionality is inspired by more modern editors like Sublime Text.

## Requirements

* Emacs 25.1

## Installation

### General

Check this project out into ~/.emacs.d.

On windows be sure to set your home: setx HOME C:/Users/you.

The command "emacs --eval (revert-default-directory) --eval (magit-status) --eval (delete-other-windows)" can be used as an alise or a context menu item to launch magit.

On Windows be sure to install grep/find through Cygwin and add them to your PATH.

### Clojure

Install Java.

Install Leiningen.

Add {:user {:plugins [[cider/cider-nrepl "0.15.0-snapshot"]]}} to your ~/.lein/profiles.clj.

### Markdown

To preview markdown be sure to install MultiMarkdown (https://github.com/fletcher/MultiMarkdown-5/releases). Add it to your path on Windows.

### SQL

Install Java.

Add ~/.emacs.d/lib/jisql/ to the path.

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
* **CTRL + SHIFT + f Regex search in project
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

#### Close Emacs

* **CTRL + SHIFT + w** Close emacs

#### Adding and Removing Windows

* **F1** Close other windows
* **F2** Split window horizontally
* **F3** Split window vertically

### Launch Apps Menu

* **CTRL + TAB** Launch apps menu

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

* **ALT + UP** Last repl command
* **ALT + DOWN** Previous repl command

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
