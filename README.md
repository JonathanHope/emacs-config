# Emacs Configuration

This is my emacs configuration. It is primarily used for clojure development. The keyboard shortcuts have been remapped to more CUA friendly ones where possible. Much of the functionality is inspired by more moder editors like sublime text.

## Packages Used

* Undo Tree
* Projectile
* Helm
* Multiple Cursors
* Clojure Mode
* Clojure Mode Extra Font Locking
* Paredit
* Rainbow delimeters.
* Cider
* Company.
* Markdown Mode.

## Keymap

### General

* **Escape** Cancel
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
* **CTRL + v** Paste
* **CTRL + b** Change buffer
* **CTRL + g** Go to line
* **CTRL + p** Open file in project
* **CTRL + SHIFT + p** Execute command
* **F1** Close other windows
* **F2** Split window horizontally
* **F3** Split window vertically
* **F4** Start terminal
* **CTRL + /** Toggle comment line
* **CTRL + SHIFT + /** Toggle comment region.
* **CTRL + RETURN** Mark region rectangle.
* **CTRL + SHIFT + c** Add cursors to start of selected lined
* **CTRL + CLICK** Add cursor
* **CTRL + SHIFT + a** Add cursor to all like this
* **CTRL + d** Duplicate line
* **CTRL + SHIFT + UP** Move line of text up
* **CTRL + SHIFT + DOWN** Move line of text down
* **TAB** Indent region
* **SHIFT + TAB** Unindent a region
* **CTRL + k** Delete line
* **CTR + BACKSPACE** Delete word
* **BACKSPACE** Delete character


### Clojure

* **CTRL + SHIFT +09** Forward slurp
* **CTRL + SHIFT + ]** Forward barf
* **CTRL + SHIFT + 9** Backward slurp
* **CTRL + SHIFT + [** Backward barf
* **ALT + SHIFT + 9** Wrap in parentheses.
* **CTRL + UP** Last repl command
* **CTRL + DOWN** Previous repl command
* **CTRL + SHIFT + RIGHT** Forward sexp
* **CTRL + SHIFT + LEFT** Backward sexp
* **CTRL + SHIFT + k** Kill sexp
* **CTRL + x + e** Evaluate sexp
* **CTRL + x + n** Set repl namespace to buffers.
* **CTRL + x + c** Load buffer into repl.
