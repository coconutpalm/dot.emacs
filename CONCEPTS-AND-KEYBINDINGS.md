# Emacs concepts and keybindings

`S-key` - means shift+key
`C-key` - means control+key
`M-key` - means "meta", which is Alt on Mac/Win/Lin
`s-key` - means "super", the Cmd or Windows key

`ESC` - has been reprogrammed to exit just about everything that can be exited.  If that fails, try `C-g` or sometimes simply `q`.  Or use `C-x C-b` to go to another open buffer/file.

## Non-standard Emacs key assignments

This Emacs maintains standard Emacs keybindings except for the following:

`C-x C-c` - No longer exits Emacs.  It's too easy to fat-finger.  You're welcome.  Use `C-x C-q` instead.  (A helpful message is displayed if you forget.)

`C-s` - saves rather than incrementally searching forward.  `C-f` now incrementally searches forward.  The original command bound to `C-f` is left unbound.

`ESC` - For historical reasons, the Escape key is normally used as a prefix key that is an alternative to typing Alt key combinations.  E.g.: `ESC x` one after the other would mean the same thing as typing `M-x`.

I've reprogrammed `ESC` to mean "exit the current thing" just about everywhere that makes sense per modern sensibilities.

## Searching / filtering

Emacs is heavily geared toward dealing with large amounts of information; as a result the user interface for opening and selecting things is heavily oriented toward searching, filtering, and selecting.

## Prefix keybindings

`M-x` - the command palette.  Once it opens start typing to filter.
`C-h` - Help commands are under this prefix.
`C-c`, `C-x` - Most compound commands start with one of these.

## Every-day work

### File management

`C-x C-f` - Open a file.
`C-x C-s` or simply `C-s` - Save current file.
`C-x k` - Close the current file.

`C-\` - Toggle file browser on the left.  Right-click the browser area to manage the directories that are displayed there.  `TAB` to expand or collapse entries.  `ENTER` selects.

`C-t` - Open a terminal in the same directory as the current file.  `C-d` or `exit` will exit the underlying shell program and close the terminal window.

### Windows and buffers

Emacs calls "window panes" windows.  What is more commonly called a window is called the "frame" in Emacs.

`C-x 2` - Split the current window horizontally into two separate windows displaying the same buffer.
`C-x 3` - Split the current window vertically...
`C-x 0` - Close the current window pane (but not the current file or buffer).
`C-x 1` - Close all windows (panes) except for the current one.

`C-x 5 2` - Open a new frame (displaying the same buffer as the current window)
`C-x 5 0` - Close the current frame (but not the file or buffer)

`s-<left/right/up/down>` - Move cursor/focus to the window (pane) in the indicated direction

A "window" (that is to say a pane) displays a buffer associated with an open file or one of Emacs's scratch areas.

Another way to say the same thing is that buffers contain data to be displayed and/or edited.  The set of open buffers does **not** correspond with the set of tabs and open windows.

You can select among the current editing-related buffers using `C-x C-b`.  `C-<left-click>` lets you choose among all open buffers.

When you kill a buffer (or close a file) using `C-x k` the current window will be reused for another open buffer unless there are no more buffers left to kill.

### Tabs and tab groups

`M-<left>`, `M-<right>` - Next and previous tab

Open buffers are categorized into groups according to their type.  Only buffers in the same group are represented using tabs across the top of the window.

To change buffer groups, click the down arrow at the far left of the tab bar or use `C-x C-b` to change buffers to a file in a different group (which will refresh the set of tabs displayed across the top).

The buffer group categorization rules are defined inside `init.el`.

### Cursor movement and editing

All the cursor movement keys you're used to using on Windows or Linux (including shift+movement to select) should behave as you expect.  If they don't, it's a bug and I'll try to fix it.

In addition:

`S-C-x` - cut to the "clipboard" (called the kill ring in Emacs)
`S-C-c` - copy to the "clipboard" (called the kill ring in Emacs)
`S-C-v` - paste

`C-f` - Incrementally-search forward from the cursor position
`C-r` - Incrementally-search backward from the cursor position

`C-k` - Delete (kill) the text up to the end of the current line to the clipboard/kill ring.  Repeatedly pressing `C-k` cuts successive lines, adding to the clipboard each time.

`C-d` - Delete the character to the right of the cursor
`C-<delete>` - Delete the word to the right of the cursor; on macs, this behaves according to muscle-memory regardless of the label on the physical key.
`C-<backspace>` - Delete the word to the left of the cursor; on macs, this behaves according to muscle-memory regardless of the label on the physical key.

#### <tab>

`tab` - expands and collapses outline-like things.  The file browser.  Files in the Git Status view.  Outline headings.  You get the idea.

`tab` also invokes content-assist/autocomplete.  For example, to fix a misspelled word, put your cursor after the last character and hit `tab`.

## Misc features

`M-x cider-` - Cider is the Clojure language server.  Try `cider-jack-in`, ...  Google `cider clojure` for (much) more information.

`C-x g` - git status.  Then '?' for command help.

`F12` - Pomodor timer.  ESC exits.  ? lists the other commands.

`S-C-d` - Deft notes

`C-c b` - Prefix for web browser commands
