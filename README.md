# Cargame-Ada

This is a barebones 3D game engine in want of a game. It began as a port to Ada
of a simple car game that I made for a computer graphics course in early 2017.

The source codebase was C++ and was basically hacked together to meet a
deadline. I'm doing things more deliberately here, but many things I did in C++
don't have drop-in solutions for Ada (such as an Obj parser). So I'm
implementing all that myself.

More recently, some experimental things have been happening here, such as the
implementation of an Entity Component System.

## Running this yourself

The game depends on some assets and will crash if it can't find them. Those
assets are deliberately not packaged at the moment since I'm unsure of the
copyright on the ones I have. At the very least, you'll want one or two 3D
models in the `.obj` format. You *may* need a `.jpg` for a texture, too.

## Building

After cloning this repository, run:
- `git submodule update --init`
- `cd dep/OpenGLAda`
- `make`
- `cd -`

Then, depending on platform:

### Debian-like linux
- `sudo apt install gnat-6 gprbuild libgl-dev libglfw3-dev libxi-dev libxinerama-dev libxcursor-dev`
- `make`

### Windows
- Install GNAT. AdaCore's GNAT GPL works, but GNU GNAT should work too (it does
  on Linux).
- Download GLFW3 dll's from http://www.glfw.org/download.html and put them in
  the `obj` folder.
- Run `make`.
- Should *just work*.
