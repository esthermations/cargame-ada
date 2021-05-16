with GL.Objects.Programs; use GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Buffers;
with GL.Files;
with GL.Toggles;

with Cargame.Models;
with Cargame.Globals;
with Cargame.Util;

package body Cargame.Renderer is

   --  Internal state

   Render_Queue : Entity_Set := (others => False);

   --  Internal function declarations

   procedure Internal_Render (E : in ECS.Entity);

   --  Function definitions

   function Initialised return Boolean is (Globals.Shader.Initialized);

   procedure Init is
      use GL.Objects.Shaders;
      Vert_Shader : Shader (Kind => GL.Objects.Shaders.Vertex_Shader);
      Frag_Shader : Shader (Kind => GL.Objects.Shaders.Fragment_Shader);

      Vert_Path : constant String := "../src/shaders/vert.glsl";
      Frag_Path : constant String := "../src/shaders/frag.glsl";
   begin
      Util.Log ("Compiling shaders.");

      GL.Objects.Programs.Initialize_Id (Globals.Shader);
      GL.Objects.Shaders.Initialize_Id  (Vert_Shader);
      GL.Objects.Shaders.Initialize_Id  (Frag_Shader);

      GL.Files.Load_Shader_Source_From_File (Vert_Shader, Vert_Path);
      GL.Files.Load_Shader_Source_From_File (Frag_Shader, Frag_Path);

      Compile (Vert_Shader);
      Compile (Frag_Shader);

      if not Vert_Shader.Compile_Status then
         Util.Log_Error ("Failed to compile Vertex shader.");
         Util.Log (Vert_Shader.Info_Log);
         pragma Assert (False, "Failed to compile Vertex shader.");
      end if;

      if not Frag_Shader.Compile_Status then
         Util.Log_Error ("Failed to compile Fragment shader.");
         Util.Log (Frag_Shader.Info_Log);
         pragma Assert (False, "Failed to compile Fragment shader.");
      end if;

      Globals.Shader.Attach (Vert_Shader);
      Globals.Shader.Attach (Frag_Shader);
      Globals.Shader.Link;

      if not Globals.Shader.Link_Status then
         Util.Log_Error ("Failed to link shaders!");
         Util.Log (Globals.Shader.Info_Log);
         pragma Assert (False, "Failed to link shaders.");
      end if;

      Globals.Shader.Use_Program;
      GL.Toggles.Enable (GL.Toggles.Depth_Test);

      Util.Log ("Shader loaded.");
   end Init;

   -----------------------------
   --  Enqueue_For_Rendering  --
   -----------------------------

   procedure Enqueue_For_Rendering (E : in Entity) is
   begin
      Render_Queue (E) := True;
   end Enqueue_For_Rendering;

   procedure Render_Enqueued_Entities is
   begin
      for E in Entity loop
         if Render_Queue (E) then
            Internal_Render (E);
         end if;
      end loop;
      Render_Queue := (others => False); --  Clear queue
   end Render_Enqueued_Entities;

   -----------------------
   --  Internal_Render  --
   -----------------------

   procedure Internal_Render (E : in ECS.Entity) is
      M   : constant Cargame.Models.Model := Components.Model.Value (E);
      Pos : constant Valid_Vector3        := Components.Position.Value (E);
      Rot : constant Radians              := Components.Rotation.Value (E);
      Scl : constant Single               := Components.Render_Scale.Value (E);

      Object_Transform : constant Matrix4 :=
         Translate (Rotate (Scale (Identity4, Scl), Rot), Pos);

      use GL.Objects.Vertex_Arrays, GL.Objects.Buffers;
   begin
      Uniforms.Object_Transform.Set_And_Send (Object_Transform);

      Bind (M.Vao);
      Bind (Array_Buffer, M.Vertex_Buffer);
      Bind (Array_Buffer, M.Normal_Buffer);

      for Mtl of M.Materials loop
         Draw_Arrays (Mode       => Triangles,
                      First      => Mtl.First_Index,
                      Count      => Mtl.Num_Indices);
      end loop;
   end Internal_Render;

   package body Uniforms is
      procedure Set_Vector3_Wrapper (U : in Uniform; Vec : in Vector3) is
         use GL;
      begin
         Set_Single (U, Vec (X), Vec (Y), Vec (Z));
      end Set_Vector3_Wrapper;
   end Uniforms;

end Cargame.Renderer;
