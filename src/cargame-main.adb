with Ada.Command_Line;
with Ada.Directories;
with Ada.Real_Time;             use Ada.Real_Time;
with Ada.Text_IO;               use Ada.Text_IO;

with GL;                        use GL;
with GL.Buffers;                use GL.Buffers;
with GL.Files;
with GL.Objects.Programs;       use GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;  use GL.Objects.Vertex_Arrays;
with GL.Objects.Buffers;        use GL.Objects.Buffers;
with GL.Objects.Shaders;        use GL.Objects.Shaders;
with GL.Objects.Textures;
with GL.Toggles;
with GL.Types;                  use GL.Types; use GL.Types.Singles;

with Glfw;
with Glfw.Input;                use Glfw.Input;
with Glfw.Windows;         
with Glfw.Windows.Context;      use Glfw.Windows.Context;
with Glfw.Windows.Hints;        use Glfw.Windows.Hints;

with Cargame.Globals;           use Cargame.Globals;
with Cargame.Models;            use Cargame.Models;
with Cargame.Types;             use Cargame.Types;
with Cargame.Uniforms;
with Cargame.Gameplay;          use Cargame.Gameplay;
with Cargame.Util;
with Cargame.Texture_Loader;
with Cargame.ECS;

--------------------------------------------------------------------------------

procedure Cargame.Main is

   Frame_T0 : Time;
   Frame_T1 : Time;

   --  Top_Score : Single := 0.0;

   Log_Task : Util.Log_Task;

   Last_Frame_Deadline_Miss : Time := Clock;

   Asteroids : array (1 .. 50) of ECS.Entity;

   use all type ECS.Enabled_Components;

begin

   Ada.Directories.Set_Directory
      (Ada.Directories.Containing_Directory (Ada.Command_Line.Command_Name));

   Util.Log ("Running from " & Ada.Directories.Current_Directory);

   ------------------------------
   --  Set up GLFW and OpenGL  --
   ------------------------------

   Util.Log ("Creating a window.");

   Glfw.Init; --  Initialises both GLFW and OpenGL.

   Set_Minimum_OpenGL_Version (Major => 3, Minor => 3);
   Set_Forward_Compat (True);
   Set_Profile (Core_Profile);

   Globals.Window.Init (Object => Globals.Window.Ptr,
                        Title  => "Skeltal Racing",
                        Width  => Globals.Window.Width,
                        Height => Globals.Window.Height);

   pragma Assert (Globals.Window.Object.Initialized,
                  "Failed to initialise main window.");

   Make_Current (Globals.Window.Ptr);
   Set_Swap_Interval (0);

   Set_Color_Clear_Value (Globals.Background_Colour);

   Enable_Callbacks :
   declare
      use Globals.Window;
      use Glfw.Windows;
   begin
      Object.Enable_Callback (Callbacks.Key);
      Object.Enable_Callback (Callbacks.Size);
      Object.Enable_Callback (Callbacks.Mouse_Position);
      Object.Enable_Callback (Callbacks.Mouse_Button);
   end Enable_Callbacks;

   -----------------------------------
   --  Set up GL_Program (shaders)  --
   -----------------------------------

   Util.Log ("Compiling shaders.");

   Shader_Setup :
   declare
      Vertex_Shader   : Shader (Kind => GL.Objects.Shaders.Vertex_Shader);
      Fragment_Shader : Shader (Kind => GL.Objects.Shaders.Fragment_Shader);

      Vertex_Path   : constant String := "../src/shaders/vert.glsl";
      Fragment_Path : constant String := "../src/shaders/frag.glsl";
      use Ada.Directories;
   begin
      pragma Assert (Exists (Vertex_Path),   "Couldn't find " & Vertex_Path);
      pragma Assert (Exists (Fragment_Path), "Couldn't find " & Fragment_Path);

      Initialize_Id (GL_Program);
      Initialize_Id (Vertex_Shader);
      Initialize_Id (Fragment_Shader);

      GL.Files.Load_Shader_Source_From_File (Vertex_Shader, Vertex_Path);
      GL.Files.Load_Shader_Source_From_File (Fragment_Shader, Fragment_Path);

      Compile (Vertex_Shader);
      Compile (Fragment_Shader);

      if not Vertex_Shader.Compile_Status then
         Util.Log_Error ("Failed to compile Vertex shader.");
         Util.Log (Vertex_Shader.Info_Log);
         pragma Assert (False, "Failed to compile Vertex shader.");
      end if;

      if not Fragment_Shader.Compile_Status then
         Util.Log_Error ("Failed to compile Fragment shader.");
         Util.Log (Fragment_Shader.Info_Log);
         pragma Assert (False, "Failed to compile Fragment shader.");
      end if;

      GL_Program.Attach (Vertex_Shader);
      GL_Program.Attach (Fragment_Shader);
      GL_Program.Link;

      if not GL_Program.Link_Status then
         Put_Line ("Log: " & GL_Program.Info_Log);
         pragma Assert (False, "Failed to link shaders.");
      end if;
   end Shader_Setup;

   GL_Program.Use_Program;
   GL.Toggles.Enable (GL.Toggles.Depth_Test);

   Util.Log ("Shader loaded.");

   ---------------------------
   --  Initialise entities  --
   ---------------------------

   Log_Task.Start ("Initialising entities.");

   Player_Model   := Create_Model_From_Obj ("../src/models/Barrel02.obj");
   Asteroid_Model := Create_Model_From_Obj ("../src/models/Barrel02.obj");

   Player_Entity := ECS.Manager.New_Entity;
   Util.Log ("Player_Entity = " & Player_Entity'Img);
   ECS.Manager.Add_Component (Player_Entity, ECS.Player);
   ECS.Manager.Add_Component (Player_Entity, ECS.Position);
   ECS.Manager.Add_Component (Player_Entity, ECS.Rotation);
   ECS.Manager.Add_Component (Player_Entity, ECS.Velocity);
   ECS.Manager.Add_Component (Player_Entity, ECS.Acceleration);
   ECS.Manager.Add_Component (Player_Entity, ECS.Render_Scale);
   ECS.Manager.Add_Component (Player_Entity, ECS.Object_Matrix);
   ECS.Manager.Add_Component (Player_Entity, ECS.CamObj_Matrix);
   ECS.Manager.Add_Component (Player_Entity, ECS.Normal_Matrix);

   Gameplay.Components.Position.Set     (Player_Entity, (0.0, 0.0, 0.0));
   Gameplay.Components.Velocity.Set     (Player_Entity, (0.0, 0.0, 0.0));
   Gameplay.Components.Acceleration.Set (Player_Entity, (0.0, 0.0, 0.0));
   Gameplay.Components.Rotation.Set     (Player_Entity, Radians (0.0));
   Gameplay.Components.Render_Scale.Set (Player_Entity, 10.0);

   for I in Asteroids'Range loop
      Asteroids (I) := ECS.Manager.New_Entity;
      Util.Log ("Making planet " & I'Img & " = " & Asteroids (I)'Img);
      ECS.Manager.Add_Component (Asteroids (I), ECS.Position);
      ECS.Manager.Add_Component (Asteroids (I), ECS.Rotation);
      ECS.Manager.Add_Component (Asteroids (I), ECS.Rotational_Speed);
      ECS.Manager.Add_Component (Asteroids (I), ECS.Render_Scale);
      ECS.Manager.Add_Component (Asteroids (I), ECS.Object_Matrix);
      ECS.Manager.Add_Component (Asteroids (I), ECS.CamObj_Matrix);
      ECS.Manager.Add_Component (Asteroids (I), ECS.Normal_Matrix);

      Gameplay.Components.Position.Set         (Asteroids (I), (Single ((I rem 49) - 24) / 10.0, 0.0, Single (I) / 100.0));
      Gameplay.Components.Rotation.Set         (Asteroids (I), Radians (0.0));
      Gameplay.Components.Rotational_Speed.Set (Asteroids (I), Radians (if I mod 2 = 0 then 1 else (-1)) * Radians (0.01));
      Gameplay.Components.Render_Scale.Set     (Asteroids (I), 10.0);
   end loop;

   ECS.Manager.Register_System 
      (Name => "Tick_Position",
       Proc         => Gameplay.Systems.Tick_Position'Access, 
       Run_Interval => Frames (1), 
       Components   => ECS.Position & ECS.Velocity);

   ECS.Manager.Register_System 
      (Name => "Tick_Velocity",
       Proc         => Gameplay.Systems.Tick_Velocity'Access, 
       Run_Interval => Frames (1), 
       Components   => ECS.Velocity & ECS.Acceleration);

   ECS.Manager.Register_System 
      (Name => "Tick_Player_Actions",
       Proc         => Gameplay.Systems.Tick_Player_Actions'Access, 
       Run_Interval => Frames (1), 
       Components   => ECS.Player & ECS.Acceleration);

   ECS.Manager.Register_System 
      (Name => "Tick_Rotation",
       Proc         => Gameplay.Systems.Tick_Rotation'Access, 
       Run_Interval => Frames (1), 
       Components   => ECS.Rotation & ECS.Rotational_Speed);

   ECS.Manager.Register_System
      (Name => "Tick_Object_Matrix",
       Proc         => Gameplay.Systems.Tick_Object_Matrix'Access,
       Run_Interval => Frames (1),
       Components   => ECS.Position & ECS.Rotation & ECS.Render_Scale);
   
   ECS.Manager.Register_System
      (Name => "Tick_CamObj_Matrix",
       Proc         => Gameplay.Systems.Tick_CamObj_Matrix'Access,
       Run_Interval => Frames (1),
       Components   => +ECS.Object_Matrix);

   ECS.Manager.Register_System
      (Name => "Tick_Normal_Matrix",
       Proc         => Gameplay.Systems.Tick_Normal_Matrix'Access,
       Run_Interval => Frames (1),
       Components   => +ECS.CamObj_Matrix);

   Log_Task.Complete;

   ---------------------------
   --  Initialise uniforms  --
   ---------------------------

   Util.Log ("Initialising uniforms.");

   Uniforms.Projection.Initialise (GL_Program);
   --  There's a dedicated procedure for correctly updating the projection:
   Globals.Window.Update_Projection; 

   Uniforms.Camera_Transform.Initialise
     (GL_Program,
      Value => Look_At (Camera_Position => Globals.Camera_Position,
                        Target_Position =>
                           (Initial_Player_Position +
                            Vector3'(Z => 2.0, others => 0.0)),
                        Up => (Y => 1.0, others => 0.0)));

   Uniforms.Object_Transform.Initialise (GL_Program, Identity4);
   Uniforms.CamObj_Transform.Initialise (GL_Program, Identity4);
   Uniforms.Normal_Transform.Initialise (GL_Program, Identity3);

   Uniforms.Diffuse_Map.Initialise  (GL_Program, Globals.Diffuse_Map_ID);
   Uniforms.Specular_Map.Initialise (GL_Program, Globals.Specular_Map_ID);

   Uniforms.Material_Ambient.Initialise   (GL_Program);
   Uniforms.Material_Shininess.Initialise (GL_Program);
   Uniforms.Light_Position.Initialise     (GL_Program, Initial_Player_Position);
   Uniforms.Light_Ambient.Initialise      (GL_Program, (others => 1.0));
   Uniforms.Light_Diffuse.Initialise      (GL_Program, (others => 1.0));
   --  Uniforms.Light_Specular.Initialise     (GL_Program);

   -----------------------------------
   --  Initialise default material  --
   -----------------------------------

   --  TODO(2019-02-04): There's related code in cargame-models.adb for
   --  applying this material. Ensure there's no overlap, and maybe tidy these
   --  two up.

   Util.Log ("Loading default material.");

   Set_Default_Material :
   declare
   begin
      Default_Texture := Texture_Loader.Load_Texture
         ("../src/models/default_texture.png");
      pragma Assert (Default_Texture.Initialized);

      Default_Material :=
         (Name             => To_Material_Name ("Default material"),
          Diffuse_Texture  => Default_Texture,
          Specular_Texture => Default_Texture,
          others           => <>);
   end Set_Default_Material;

   ----------------------
   --  Main game loop  --
   ----------------------

   Log_Task.Start ("Entering game loop.");

   Util.Log ("Frame deadline is " & Util.Image (Globals.Frame_Interval));

   Game_Loop :
   while not Globals.Window.Object.Should_Close loop

      -------------------
      -- Set deadlines --
      -------------------

      --  For timing accuracy, evaluating Clock should be the first thing done
      --  every frame.
      Frame_T0 := Clock;

      Next_Frame_Time := Frame_T0 + Frame_Interval;

      if Frame_T0 > Globals.Next_Input_Poll_Time then
         Glfw.Input.Poll_Events;
         Globals.Next_Input_Poll_Time := 
            Globals.Next_Input_Poll_Time + Globals.Input_Poll_Interval;
      end if;

      -----------------------
      --  Gameplay update  --
      -----------------------

      Globals.Frame_Number := Globals.Frame_Number + 1;

      ECS.Manager.Run_Systems;

      --------------
      --  Render  --
      --------------

      Clear (Buffer_Bits'(Depth => True, Color => True, others => <>));

      declare
         Pos : constant Position_Type := Gameplay.Components.Position.Get (Player_Entity);
         Vel : constant Velocity_Type := Gameplay.Components.Velocity.Get (Player_Entity);

         function Camera_Offset return Position_Type is (Position_Type (Vel));
      begin
         Uniforms.Camera_Transform.Set
            (Look_At (Camera_Position => (Pos + Vector3'(0.0, 2.0, -2.0)),
                      Target_Position => (Pos + Vector3'(0.0, 0.0, +2.0) +
                                          Camera_Offset),
                      Up              => (Y => 1.0, others => 0.0)));
         
         Gameplay.Player_Model.Render (E => Player_Entity);
      end;

      for E of Asteroids loop
         Gameplay.Asteroid_Model.Render (E);
      end loop;

      GL.Flush;
      Swap_Buffers (Globals.Window.Ptr);

      -----------------------------
      --  Performance analytics  --
      -----------------------------

      Frame_T1 := Clock;

      if Frame_T1 > Next_Frame_Time then
         declare
            Now : constant Time := Clock;
         begin
            Util.Log_Warning
               ("Missed frame deadline by "
                   & Util.Image (Frame_T1 - Next_Frame_Time)
                   & ". (last happened " 
                   & Util.Image (Now - Last_Frame_Deadline_Miss) 
                   & " ago)");
            Last_Frame_Deadline_Miss := Now;
         end;
      end if;


      -------------
      --  Vsync  --
      -------------

      delay until Next_Frame_Time;

   end loop Game_Loop;

   Log_Task.Complete;

   Globals.Window.Destroy (Globals.Window.Ptr);
   Put_Line ("Thanks for playing!");

   Glfw.Shutdown;

end Cargame.Main;
