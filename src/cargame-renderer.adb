with GL.Objects.Programs; use GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Buffers;
with GL.Files;
with GL.Toggles;
with GL.Window;

with Cargame.Config;
with Cargame.Globals;
with Cargame.Engine.Models;
with Cargame.Util;

with Cargame.Renderer.Uniforms;

package body Cargame.Renderer is

   --  Internal state

   Program      : GL.Objects.Programs.Program;
   Render_Queue : Entity_Set := (others => False);

   View_Matrix_Needs_Update       : Boolean := False;
   Projection_Matrix_Needs_Update : Boolean := False;

   --  Internal function declarations

   procedure Internal_Render (E : in Entity);
   function  Internal_Calculate_Projection
      return Matrix4;

   --  Function definitions

   function Initialised
      return Boolean
      is (Program.Initialized);

   ------------
   --  Init  --
   ------------

   procedure Init is
      use GL.Objects.Shaders;
      Vert_Shader : Shader (Kind => GL.Objects.Shaders.Vertex_Shader);
      Frag_Shader : Shader (Kind => GL.Objects.Shaders.Fragment_Shader);

      Vert_Path : constant String := Cargame.Config.Vertex_Shader_Path;
      Frag_Path : constant String := Cargame.Config.Fragment_Shader_Path;
   begin
      Util.Log ("Compiling shaders.");

      GL.Objects.Programs.Initialize_Id (Program);
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

      Program.Attach (Vert_Shader);
      Program.Attach (Frag_Shader);
      Program.Link;

      if not Program.Link_Status then
         Util.Log_Error ("Failed to link shaders!");
         Util.Log (Program.Info_Log);
         pragma Assert (False, "Failed to link shaders.");
      end if;

      Program.Use_Program;
      GL.Toggles.Enable (GL.Toggles.Depth_Test);

      Util.Log ("Shader loaded.");

      Cargame.Renderer.Uniforms.Projection.Find_Location (Program);
      Cargame.Renderer.Uniforms.View.Find_Location       (Program);
      Cargame.Renderer.Uniforms.Model.Find_Location      (Program);

      Projection_Matrix_Needs_Update := True;
      View_Matrix_Needs_Update       := True;

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
      --  Update View, if needed
      if View_Matrix_Needs_Update then
         Uniforms.View.Set_And_Send
            (Look_At (Camera_Pos => Camera.Get_Position,
                      Target_Pos => Camera.Get_Target,
                      Up         => Cargame.Config.Up_Vector));
      end if;

      --  Update Projection, if needed
      if Projection_Matrix_Needs_Update then
         Cargame.Renderer.Uniforms.Projection.Set_And_Send
            (Internal_Calculate_Projection);
         GL.Window.Set_Viewport
            (0, 0, Int (Globals.Window.Width), Int (Globals.Window.Height));
      end if;

      --  Render each entity
      for E in Entity loop
         if Render_Queue (E) then
            Internal_Render (E);
         end if;
      end loop;

      --  Clear queue
      Render_Queue := (others => False);
   end Render_Enqueued_Entities;

   ----------------------------
   --  Handle_Window_Resize  --
   ----------------------------

   procedure Handle_Window_Resize is
   begin
      Projection_Matrix_Needs_Update := True;
   end Handle_Window_Resize;

   -----------------------
   --  Internal_Render  --
   -----------------------

   procedure Internal_Render (E : in Entity) is
      use Cargame.Engine.Models;
      use Cargame.Gameplay;
      M   : constant Model         := Components.Model.Value (E);
      Pos : constant Valid_Vector3 := Components.Position.Value (E);
      Rot : constant Radians       := Components.Rotation.Value (E);
      Scl : constant Single        := Components.Render_Scale.Value (E);

      Transform : constant Matrix4 :=
         Translate (Rotate (Scale (Identity4, Scl), Rot), Pos);

      use GL.Objects.Vertex_Arrays, GL.Objects.Buffers;
   begin
      Uniforms.Model.Set_And_Send (Transform);

      Bind (M.Vao);
      Bind (Array_Buffer, M.Vertex_Buffer);
      Bind (Array_Buffer, M.Normal_Buffer);

      for Mtl of M.Materials loop
         Draw_Arrays (Mode  => Triangles,
                      First => Mtl.First_Index,
                      Count => Mtl.Num_Indices);
      end loop;
   end Internal_Render;

   -------------------------------------
   --  Internal_Calculate_Projection  --
   -------------------------------------

   function Internal_Calculate_Projection
      return Matrix4
   is
   begin
      return Types.Perspective_Matrix
         (View_Angle   => Types.Degrees (Config.Vertical_FoV),
          Aspect_Ratio => Globals.Window.Aspect_Ratio,
          Near         => Config.Near_Plane,
          Far          => Config.Far_Plane);
   end Internal_Calculate_Projection;

   --------------
   --  Camera  --
   --------------

   package body Camera is

      Position : Valid_Vector3 := Config.Initial_Camera_Position;
      Target   : Valid_Vector3 := Config.Initial_Camera_Target;

      procedure Set_Position (Pos : in Valid_Vector3) is
      begin
         Position := Pos;
         View_Matrix_Needs_Update := True;
      end Set_Position;

      procedure Set_Target (Tgt : in Valid_Vector3) is
      begin
         Target := Tgt;
         View_Matrix_Needs_Update := True;
      end Set_Target;

      function Get_Position return Valid_Vector3 is (Position);
      function Get_Target   return Valid_Vector3 is (Target);
   end Camera;

end Cargame.Renderer;
