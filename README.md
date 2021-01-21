# Cargame-Ada

I'm in me mum's car! Broom broom.

This is basically the same project as
[DlangVulkanTriangle](https://github.com/esthermations/DlangVulkanTriangle)
except it's in Ada and it uses OpenGL instead of Vulkan. It doesn't actually
really have anything to do with cars anymore, it's basically just a game engine
I hack away at in lieu of anything better to do, eheh. 😇

## Building

- `git clone https://github.com/esthermations/cargame-ada`
- `git submodule update --init`
- Build the dependencies in `dep` (probably just cd there and run `make`)
- Run `gprbuild` in the project root

You may or may not need other libraries like OpenGL or GLFW etc etc. Install
those however you normally would and chuck the library in the `obj`
folder if the compiler/linker doesn't seem to find them.
