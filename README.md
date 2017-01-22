# Emacs Configuration

## Summary

This is my Emacs configuration. The keyboard shortcuts have been remapped to more CUA friendly ones where possible. Much of the functionality is inspired by more modern editors like Sublime Text.

## Requirements

* Emacs 25.1

## Installation

### General

Check this project out into ~/.emacs.d.

On windows be sure to set your home: setx HOME C:/Users/you.

Git needs to be installed. On windows just install it from https://git-scm.com/download/win. On windows make sure that the git paths with git.exe, find.exe, and grep.exe are at the very top of the system wide path.

The command "emacs --eval (revert-default-directory) --eval (magit-status) --eval (delete-other-windows)" can be used as an alias or a context menu item to launch magit.

On windows 10 the shortcut C-) will not work unless you change some default language settings. Use the following command: powershell -Command Set-ItemProperty -Path 'HKCU:\Keyboard Layout\Toggle' -Name HotKey -Value 3.

It uses the pragmata fonts.

On Linux you need to install ispell and on Windows you need to install aspell (http://aspell.net/win32/). On windows add aspell to your path.

Ag needs to be installed. On windows run the following commands in admin powershell CLI: Set-ExecutionPolicy RemoteSigned, iwr https://chocolatey.org/install.ps1 -UseBasicParsing | iex, choco install ag.

### Clojure

Install Java.

Install Leiningen.

Add {:user {:plugins [[cider/cider-nrepl "0.15.0-snapshot"]]}} to your ~/.lein/profiles.clj.

### Markdown

Install node.js and npm. Add them to your path on windows.

Install livedown with the following command: npm install -g livedown.

### JavaScript

Install node.js and npm. Add them to your path on windows.

## Keymap

### General - Editing

#### Selections

* **CTRL + SPACE** Set mark
* **CTRL + RETURN** Mark region rectangle
* **CTRL + d** Select word
* **CTRL + l** Select line
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
* **CTRL + SHIFT + v** Paste from kill ring
* **CTRL + k** Kill line

#### Comments

* **CTRL + /** Toggle comment

#### Lines

* **CTRL + SHIFT + d** Duplicate line
* **CTRL + j** Join line
* **CTRL + SHIFT + UP** Move line of text up
* **CTRL + SHIFT + DOWN** Move line of text down

#### Indentation

* **CTRL + i** Auto indent

#### Deletions

* **CTRL + BACKSPACE** Delete word
* **BACKSPACE** Delete character

#### Casing 

* **CTRL + SHIFT + u** Uppercase region
* **CTRL + SHIFT + l** Lowercase region

#### Structural Editing

* **CTRL + SHIFT + 0** Forward slurp
* **CTRL + SHIFT + ]** Forward barf
* **CTRL + SHIFT + 9** Backward slurp
* **CTRL + SHIFT + [** Backward barf
* **ALT + SHIFT + 9** Wrap in parentheses
* **CTRL + SHIFT + RIGHT** Forward sexp
* **CTRL + SHIFT + LEFT** Backward sexp
* **CTRL + SHIFT + k** Kill sexp

#### Code Folding

* **ALT + RETURN** Toggle code folding

#### Multiple Cursors

* **CTRL + SHFIT + c** Add cursors to lines
* **CTRL + SHFIT + a** Mark all like this
* **CTRL + CLICK** Add cursor

#### Snippets

* **TAB** Expand snippet

#### Completion

* **UP** Previous candidate
* **DOWN** Next candidate
* **ENTER** Select candidate

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
* **CTRL + UP** Up five lines
* **CTRL + DOWN** Down five lines

#### Regex Searching

* **CTRL + f** Regex search
* **CTRL + SHIFT + f** Regex search in project
* **CTRL + h** Regex search and replace

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

* **CTRL + SHIFT + TAB** Launch apps menu

#### Launch Contextual Menu

* **CTRL + TAB** Launch contextual menu

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
