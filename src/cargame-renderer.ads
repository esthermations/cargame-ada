with GL;
with GL.Uniforms;              use GL.Uniforms;
with GL.Types;                 use GL.Types; use GL.Types.Singles;

with Cargame.Types;            use Cargame.Types;
with Cargame.Generic_Uniforms; use Cargame.Generic_Uniforms;

with Cargame.ECS;      use Cargame.ECS;
with Cargame.Gameplay; use Cargame.Gameplay;

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
      with Pre => Initialised and
                  Components.Model.Has (E) and
                  Components.Rotation.Has (E) and
                  Components.Position.Has (E) and
                  Components.Render_Scale.Has (E);

   procedure Render_Enqueued_Entities;
   --  Call this to issue drawcalls for all enqueued entities. Clears the queue
   --  when completed.

   procedure Handle_Window_Resize;
   --  Call this when the viewport has changed size.

   -------------
   --  Camera --
   -------------

   package Camera is
      procedure Set_Position (Pos : in Valid_Vector3);
      procedure Set_Target   (Tgt : in Valid_Vector3);
      function  Get_Position return Valid_Vector3;
      function  Get_Target   return Valid_Vector3;
   end Camera;

   ----------------
   --  Uniforms  --
   ----------------

   package Uniforms is
      pragma Style_Checks (Off);
         package Projection is new Generic_Uniform ("Projection", Matrix4, Set_Single);
         package View       is new Generic_Uniform ("View"      , Matrix4, Set_Single);
         package Model      is new Generic_Uniform ("Model"     , Matrix4, Set_Single);
      pragma Style_Checks (On);
   end Uniforms;

end Cargame.Renderer;