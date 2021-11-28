# Jet - A Structural JSON editor

<!-- toc GFM -->

* [Features](#features)
* [Keymaps](#keymaps)
* [Installation](#installation)
* [Usage](#usage)
* [Roadmap/Known bugs](#roadmapknown-bugs)

<!-- tocstop -->

Jet is a structural editor for JSON.

I.e. an editor which is aware of the *structure* of JSON and allows you to manipulate it directly.
The document is _always_ in a valid state.

https://user-images.githubusercontent.com/6439644/143655548-3c556ea8-7673-4439-8624-15b4b503001f.mov


# Features

* [x] Structurally sound editing, never outputs invalid JSON.
* [x] Copy/Cut/Paste JSON subtrees
* [x] Subtree folding so you can focus on what's important.
* [x] Transpose values around each other in lists.
* [x] Undo/redo system, everyone makes mistakes
* [x] Save functionality


# Keymaps

Press `?` to see the key map, which should feel familiar to vim users.

# Installation

```shell
cabal update && cabal install jet
```

# Usage

```shell
# Open a file for editing. Use ctrl-s to save back to the file.
# The edited file is output to stdout even if unsaved.
jet myfile.json 

# Using jet in a pipeline for quick in-line edits.
cat myfile.json | jet > result.json
```

# Roadmap/Known bugs

- [ ] Figure out why vty needs two keystrokes to quit for some reason.
- [ ] Allow cut/paste of _keys_ of objects.
- [ ] Allow inserting when empty key already exists
- [ ] Add search
- [ ] Improved visibility around copy/paste with highlighting
- [ ] Increment/decrement commands for integers.
