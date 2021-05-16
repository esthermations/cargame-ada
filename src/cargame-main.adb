with Ada.Command_Line;
with Ada.Directories;
with Ada.Real_Time;             use Ada.Real_Time;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Float_Random;

with GL;                        use GL;
with GL.Buffers;                use GL.Buffers;
with GL.Objects.Programs;       use GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types;                  use GL.Types; use GL.Types.Singles;
with GL.Uniforms;

with Glfw;
with Glfw.Input;                use Glfw.Input;
with Glfw.Windows;
with Glfw.Windows.Context;      use Glfw.Windows.Context;
with Glfw.Windows.Hints;        use Glfw.Windows.Hints;

with Cargame.Globals;
with Cargame.Gameplay;          use Cargame.Gameplay;
with Cargame.Models;            use Cargame.Models;
with Cargame.Types;             use Cargame.Types;
with Cargame.Util;
with Cargame.ECS;
with Cargame.Renderer;

procedure Cargame.Main is

   --  Timings

   Program_Epoch   : constant Time := Clock;
   Next_Frame_Time : Time := Program_Epoch;
   Frame_T0        : Time;
   Frame_T1        : Time;

   --  Models

   Player_Model   : Model;
   Asteroid_Model : Model;

   Float_Generator : Ada.Numerics.Float_Random.Generator;

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

   Cargame.Renderer.Init;

   ---------------------------
   --  Initialise entities  --
   ---------------------------

   Player_Model   := Create_Model_From_Obj ("../src/models/Barrel02.obj");
   Asteroid_Model := Create_Model_From_Obj ("../src/models/Barrel02.obj");

   Player := ECS.New_Entity;
   Components.Controlled_By_Player.Set (Player, True);
   Components.Position.Set             (Player, Origin);
   Components.Velocity.Set             (Player, (others => 0.0));
   Components.Acceleration.Set         (Player, (others => 0.0));
   Components.Render_Scale.Set         (Player, 10.0);
   Components.Object_Matrix.Set        (Player, Identity4);
   Components.CamObj_Matrix.Set        (Player, Identity4);
   Components.Normal_Matrix.Set        (Player, Identity3);
   Components.Model.Set                (Player, Player_Model);
   Components.Rotation.Set             (Player, Radians (0.0));

   for I in Asteroids'Range loop
      Asteroids (I) := ECS.New_Entity;

      --  Set position
      declare
         package R renames Ada.Numerics.Float_Random;
         Position : Valid_Vector3;
         Factor : constant Single := 50.0;
      begin
         Position (X) := Factor * Single (R.Random (Float_Generator) - 0.5);
         Position (Y) := Factor * Single (R.Random (Float_Generator) - 0.5);
         Position (Z) := Factor * Single (R.Random (Float_Generator) - 0.5);
         Components.Position.Set (Asteroids (I), Position);
      end;

      --  Set everything else
      Components.Rotation.Set         (Asteroids (I), Radians (0.0));
      Components.Model.Set            (Asteroids (I), Asteroid_Model);
      Components.Rotational_Speed.Set (Asteroids (I), Radians (0.01));
      Components.Render_Scale.Set     (Asteroids (I), 10.0);
      Components.Object_Matrix.Set    (Asteroids (I), Identity4);
      Components.CamObj_Matrix.Set    (Asteroids (I), Identity4);
      Components.Normal_Matrix.Set    (Asteroids (I), Identity3);
   end loop;

   ---------------------------
   --  Initialise uniforms  --
   ---------------------------

   Util.Log ("Initialising uniforms.");

   GL.Uniforms.Set_Single (Globals.Shader.Uniform_Location ("u_Projection"),
                           Globals.Window.Calculate_Projection);

   GL.Uniforms.Set_Single (Globals.Shader.Uniform_Location ("u_View"),
                           Look_At (Camera_Pos => (0.0, 2.0, -2.0),
                                    Target_Pos => Origin,
                                    Up         => (0.0, 1.0, 0.0)));

   ----------------------
   --  Main game loop  --
   ----------------------

   Util.Log ("Frame deadline is " & Util.Image (Globals.Frame_Interval));

   Game_Loop :
   while not Globals.Window.Object.Should_Close loop

      -------------------
      -- Set deadlines --
      -------------------

      Frame_T0 := Clock;

      --  NOTE: We set Next_Frame_Time each frame which means that a slow frame
      --  doesn't reduce the deadline for subsequent frames.
      Next_Frame_Time := Frame_T0 + Globals.Frame_Interval;

      ------------
      --  Tick  --
      ------------

      Globals.Frame_Number := @ + 1;

      Glfw.Input.Poll_Events;
      ECS.Run_All_Systems;
      Renderer.Render_Enqueued_Entities;

      -----------------------------
      --  Performance analytics  --
      -----------------------------

      Frame_T1 := Clock;

      if Frame_T1 > Next_Frame_Time then
         Util.Log_Warning ("Missed frame deadline by " &
                           Util.Image (Frame_T1 - Next_Frame_Time));
      end if;

      -------------
      --  Vsync  --
      -------------

      delay until Next_Frame_Time;

   end loop Game_Loop;

   Globals.Window.Destroy (Globals.Window.Ptr);
   Glfw.Shutdown;

   Put_Line ("Thanks for playing!");

end Cargame.Main;
