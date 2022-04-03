# 🚗 Cargame-Ada

I'm in me mum's car! Broom broom.

This is a game engine written in Ada that I am perpetually re-writing to try
stuff out. It has an OpenGL renderer that *theoretically* supports textures,
but lately I'm mostly just placing 3D models in the scene and playing with the
actual engine logic itself.

At the moment I'm working on an Entity Component System that achieves
dependency ordering by storing its Components in thread-safe state machines
that distinguish between "Stale" and "Fresh" states, "Fresh" meaning "having
been this frame". (I may later have a frame-independent definition for
fresh/stale). Jobs that want fresh data will have to wait until another job has
updated the data to the fresh state, so hopefully it all just... sorts itself
out. That's the hope!

## Architecture

At time of writing, a high-level view of the project is:

 - **Cargame** :
    - Cargame.**Globals** : Global program state. I mostly try to keep
      "application-level" state in here, more specific stuff goes in Gameplay
      or Renderer etc.
    - Cargame.**Engine** :
        - ECS : The Entity Component System
        - Obj_Parser : A Wavefront OBJ parser
        - Models : Data types for renderable models
    - Cargame.**Renderer** : Responsible for interfacing with OpenGL.
        - Systems : Procedures meant to be run by the ECS
        - Uniforms : Declarations of OpenGL uniform values
    - Cargame.**Gameplay** : Gameplay logic lives in here. The idea is that
      this is where I set up how the game itself actually works and plays.
        - Systems : Procedures meant to be run by the ECS
        - Components : Components (Per-Entity data) to be managed by the ECS
    - Cargame.**Types** : A mess; mostly validity-checked types used for linear
      algebra.
    - Cargame.**Util** : Utility stuff, mostly logging and debug helpers.


## Building & Running

This project use [Alire](https://github.com/alire-project/alire) to build and
it's all pretty straightforward. Just install that and run `alr run` in the
root directory.

