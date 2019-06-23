
# Table of Contents

1.  [Mainspring - An Emacs Configuration](#orgfa29f39)
    1.  [Philosophy](#org3dd299d)
    2.  [Installation](#orge9f6a70)
    3.  [Keymap](#org136abef)
        1.  [General - Editing](#org741a278)
        2.  [General - Navigation](#orgfc0299f)
        3.  [General - Files](#orge9edaf6)
        4.  [General - Windowing](#org9e463bb)
        5.  [Menus](#orga828c8c)
        6.  [Magit](#org073af9f)
        7.  [Org mode](#orgd607d15)
        8.  [Calc Mode](#org0685fd6)


<a id="orgfa29f39"></a>

# Mainspring - An Emacs Configuration

Mainspring is a highly opinionated Emacs configuration. For more information see the philosophy section below.


<a id="org3dd299d"></a>

## Philosophy

-   **Use CUA key bindings:** CUA bindings are used by most applications on most operating systems. Rather than spend the time to make every other application behave like Vim or Emacs or increase my cognitive overhead by constantly switching between different key binding styles I simply use CUA style key bindings in emacs.
-   **Per mode functionality should be discoverable:** It is impossible to remember a bunch of mode specific key bindings for a lot of modes. Key bindings that are for a specific mode should generally be discoverable in a visual way.
-   **Reward imprecision:** Using techniques like fuzzy matching everywhere, automatic whitespace cleanup, and automatic indenting means that less time can be spent thinking about perfectly entering and formatting data and more time can be spent thinking about solving the problem at hand.
-   **Window management should be simple:** Instead of having to configure complicated workspaces window management should be simple and intuitive.
-   **Startup time matters:** Having to run an emacs server is an unnecessary complication. Instead effort should be made for it start quickly.
-   **Stability matters:** If something is useful but not stable I won't bring it in until it becomes stable.
-   **Performance matters:** If something is useful but impacts performance too much in a negative way I won't bring it in until is performant.
-   **Use structural editing:** Structural editing, navigation, and selection should be implemented wherever possible as it is incredibly useful.
-   **Less is more:** The fewer things to distract the user from the actual act of editing text the better. Things like tree views and scrollbars are not necessary.
-   **Looks matter:** I spend way too much time editing text in my life, the text editor should be a nice place to live.


<a id="orge9f6a70"></a>

## Installation

You obviously need Emacs. On Linux use whatever package manager is provided by your OS. On Windows the recommended distribution can be found here: <https://ntemacs.sourceforge.io/>. Whatever Emacs you install should have been compiled with image support.

Some of the configuration relies on the idea of a home directory. You get this for free on Linux but not on Windows. Run the following command on Windows to set your home directory: setx HOME C:/Users/you.

Git and a number of standard Linux tools are required. On Linux all you have to do is install git using your distributions package manager. On the Windows side run this installer: <https://git-scm.com/download/win>. Then make sure the path to git.exe and find.exe are the first entry in the system wide path variable. Projectile will not work if that is not the case.

On windows 8 and 10 the shortcut C-) will not work unless you change some default language settings. Use the following command: powershell -Command Set-ItemProperty -Path 'HKCU:\Keyboard Layout\Toggle' -Name HotKey -Value 3. See more about this here: <https://superuser.com/a/631324>.

Hunspell is required for spell checking. It can be installed on Linux using your distributions package manager. It can be found for windows here: <https://sourceforge.net/projects/ezwinports/>.

Ripgrep is required for textual searching. It can be installed on Linux using your distributions package manager. It can be found for windows here: <https://github.com/BurntSushi/ripgrep/releases>.

Graphviz is required for graphs in org mode. It can be installed on Linux using your distributions package manager. It can be found for windows here: <http://www.graphviz.org/download/>.

Latex is required for formulas in org documents. It can be installed on Linux using your distributions package manager. It can be found for windows here: <http://www.tug.org/texlive/acquire-netinstall.html>.

Octave is required for octave blocks in org documents. It can be installed on Linux using your distributions package manager. It can be found for windows here: <https://www.gnu.org/software/octave/download.html>.

It uses the pragmata fonts by default. These are non-free fonts.


<a id="org136abef"></a>

## Keymap

Because the shortcuts for this config are so drastically different than standard emacs I detail them here.


<a id="org741a278"></a>

### General - Editing

1.  Selections

    -   **CTRL + SPACE:** Set mark
    -   **CTRL + RETURN:** Mark region rectangle
    -   **CTRL + d:** Select word
    -   **CTRL + l:** Select line
    -   **CTRL + a:** Select all
    -   **CTRL + =:** Expand region
    -   **CTRL + -:** Contract region

2.  Undo/Redo

    -   **CTRL + z:** Undo
    -   **CTRL + y:** Redo

3.  Cut/Copy/Paste

    -   **CTRL + c:** Copy
    -   **CTRL + x:** Cut
    -   **CTRL + SHIFT + x:** Cut line
    -   **CTRL + v:** Paste
    -   **CTRL + SHIFT + v:** Paste from kill ring

4.  Comments

    -   **CTRL + /:** Toggle comment

5.  Lines

    -   **RETURN:** New line
    -   **CTRL + SHIFT + d:** Duplicate line
    -   **CTRL + j:** Join line
    -   **CTRL + SHIFT + UP:** Move line of text up
    -   **CTRL + SHIFT + DOWN:** Move line of text down

6.  Indentation

    -   **TAB:** Auto indent

7.  Deletions

    -   **CTRL + BACKSPACE:** Delete word
    -   **BACKSPACE:** Delete character
    -   **CTRL + k:** Delete a line

8.  Casing

    -   **CTRL + SHIFT + u:** Uppercase region
    -   **CTRL + SHIFT + l:** Lowercase region

9.  Structural Editing

    -   **CTRL + SHIFT + 0:** Forward slurp
    -   **CTRL + SHIFT + ]:** Forward barf
    -   **CTRL + SHIFT + 9:** Backward slurp
    -   **CTRL + SHIFT + [:** Backward barf
    -   **ALT + SHIFT + 9:** Wrap in parentheses
    -   **CTRL + SHIFT + k:** Kill sexp

10. Code Folding

    -   **ALT + RETURN:** Toggle code folding

11. Multiple Cursors

    -   **CTRL + SHFIT + c:** Add cursors to lines
    -   **CTRL + SHFIT + a:** Mark all like this

12. Snippets

    -   **TAB:** Expand snippet


<a id="orgfc0299f"></a>

### General - Navigation

1.  Cancellation

    -   **ESCAPE:** Cancel

2.  Directional Movement

    -   **UP:** Up line
    -   **DOWN:** Down line
    -   **LEFT:** Forward character
    -   **RIGHT:** Backward character
    -   **CTRL + LEFT:** Forward symbol
    -   **CTRL + RIGHT:** Backward symbol
    -   **CTRL + UP:** Up five lines
    -   **CTRL + DOWN:** Down five lines
    -   **SHIFT + LEFT:** Beginning of line
    -   **SHIFT + RIGHT:** End of line
    -   **HOME:** Beginning of buffer
    -   **END:** End of buffer

3.  Regex Searching

    -   **CTRL + f:** Regex search
    -   **CTRL + SHIFT + f:** Regex search in project
    -   **CTRL + h:** Regex search and replace
    -   **CTRL + SHIFT + h:** Regex search in project.

4.  Structural Navigation

    -   **CTRL + SHIFT + RIGHT:** Forward sexp
    -   **CTRL + SHIFT + LEFT:** Backward sexp

5.  High Level Navigation

    -   **CTRL + g:** Go to line
    -   **CTRL + p:** Go to file in project
    -   **CTRL + SHIFT + p:** Execute command
    -   **F12:** Jump to definition
    -   **SHFIT + F12:** Jump back


<a id="orge9edaf6"></a>

### General - Files

-   **CTRL + s:** Save file
-   **CTRL + SHIFT + s:** Save all files
-   **CTRL + o:** Open file
-   **CTRL + w:** Close file


<a id="org9e463bb"></a>

### General - Windowing

1.  Closing

    -   **CTRL + SHIFT + w:** Close emacs
    -   **ESCAPE:** Close popup window

2.  Adding and Removing Windows

    -   **CTRL + b:** Change buffer
    -   **ALT + SHIFT + 1:** Close other windows
    -   **ALT + SHIFT + 2:** Split window horizontally
    -   **ALT + SHIFT + 3:** Split window vertically
    -   **ALT + 1:** Select window 1
    -   **ALT + 2:** Select window 2
    -   **ALT + 3:** Select window 3
    -   **ALT + 4:** Select window 4
    -   **ALT + 5:** Select window 5
    -   **ALT + 6:** Select window 6
    -   **ALT + 7:** Select window 7
    -   **ALT + 8:** Select window 8
    -   **ALT + 9:** Select window 9
    -   **ALT + TAB:** Select next window
    -   **ALT + LEFT:** Select window to left
    -   **ALT + RIGHT:** Select window to right
    -   **ALT + UP:** Select window to up
    -   **ALT + DOWN:** Select window to down


<a id="orga828c8c"></a>

### Menus

-   **CTRL + SHIFT + TAB:** Launch apps menu
-   **CTRL + TAB:** Launch contextual menu


<a id="org073af9f"></a>

### Magit

-   **ALT + c:** Commit with the entered commit message
-   **?:** Show shortcuts


<a id="orgd607d15"></a>

### Org mode

-   **ALT + RETURN:** Add heading/item
-   **ALT + SHIFT + RETURN:** Add todo/checkbox
-   **TAB:** Next field in table
-   **SHIFT + TAB:** Previous field in table
-   **ALT + LEFT:** Demote headline
-   **ALT + RIGHT:** Promote headline
-   **ALT + UP:** Move item upto
-   **ALT + DOWN:** Move item down
-   **SHIFT + LEFT:** Toggle todo status and toggle list style
-   **SHIFT + RIGHT:** Toggle todo status and toggle list style
-   **SHIFT + UP:** Toggle todo priority
-   **SHIFT + DOWN:** Toggle todo priority
-   **TAB:** Toggle visibility
-   **SHIFT + ESC:** Exit source editing


<a id="org0685fd6"></a>

### Calc Mode

-   **0-9:** Start entering a number. Seperate with **:** for a fraction. Use **.** for floats.
-   **RETURN:** Duplicate the last entry on the stack.
-   **TAB:** Swap the last two entries on the stack.
-   **BACKSPACE:** Pop the last entry off of the stack.
-   **+:** Add last two entries on the stack.
-   **-:** Subtract last two entries on the stack.
-   **\*:** Multiply last two entries on the stack.
-   **/:** Divide last two entries on the stack.
-   **/:** Divide second to last entry of the stack to the last entry of the stack power.
