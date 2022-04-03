with GL;
with GL.Uniforms;                    use GL.Uniforms;
with GL.Types;                       use GL.Types; use GL.Types.Singles;

with Cargame.Types;                  use Cargame.Types;
with Cargame.Engine.ECS;             use Cargame.Engine.ECS;
with Cargame.Gameplay.Components;
with Cargame.Util;

--  esthero:
--
--  I plan to clean up a lot of content from cargame.models and cargame.types
--  and bring them in here, so all the GL stuff happens under one API.

package Cargame.Renderer is

   procedure Init;
   --  Initialise the renderer state so we can start rendering Entities.

   function  Initialised return Boolean;

   procedure Enqueue_For_Rendering (E : in Entity)
   --  Call this if you'd like to render the given entity with the next call to
   --  Render_Enqueued_Entities.
      with Pre => Initialised;

   procedure Render_Enqueued_Entities;
   --  Call this to issue drawcalls for all enqueued entities. Clears the queue
   --  when completed.

   procedure Handle_Window_Resize;
   --  Call this when the viewport has changed size.

   procedure Clear_Back_Buffer;
   --  Call this before each frame to clear the buffer before we draw into it.

   procedure Swap_Buffers;
   --  Call this to swap the backbuffer to the front after rendering is done.

   -------------
   --  Camera --
   -------------

   package Camera is
      procedure Set_Position (Pos : in Valid_Vector3);
      procedure Set_Target   (Tgt : in Valid_Vector3);
      function  Get_Position return Valid_Vector3;
      function  Get_Target   return Valid_Vector3;
   end Camera;

   package State is
      Ready_To_Render : Cargame.Util.Semaphore;
   end State;

end Cargame.Renderer;