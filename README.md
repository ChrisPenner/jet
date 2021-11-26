# structural-json

<!-- toc GFM -->

* [Features](#features)
* [Keymaps](#keymaps)
* [Roadmap/Known bugs](#roadmapknown-bugs)

<!-- tocstop -->

A structural editor for JSON.

I.e. an editor which is aware of the *structure* of JSON and allows you to manipulate it directly.
The document is _always_ in a valid state.

https://user-images.githubusercontent.com/6439644/143650775-6478eb4d-e67f-42e7-aae9-53ef8e0173b1.mov


# Features

* [x] Structurally sound editing, never outputs invalid JSON.
* [x] Copy/Cut/Paste JSON subtrees
* [x] Subtree folding so you can focus on what's important.
* [x] Transpose values around each other in lists.
* [x] Undo system, everyone makes mistakes


# Keymaps

Press `?` to see the key map, which should be familiar for vim users.

# Roadmap/Known bugs

- [ ] Redo
- [ ] Add caching to renders
- [ ] Improve Performance on big files
- [ ] Allow cut/paste of _keys_ of objects.
- [ ] Allow inserting when empty key already exists
- [ ] Allow saving files in-place
- [ ] Add search
- [ ] Improved visibility around copy/paste with highlighting
