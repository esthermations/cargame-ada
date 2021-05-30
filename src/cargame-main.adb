with Ada.Command_Line;
with Ada.Directories;
with Ada.Real_Time;                           use Ada.Real_Time;
with Ada.Text_IO;                             use Ada.Text_IO;
with Ada.Numerics.Float_Random;

with GL;                                      use GL;
with GL.Buffers;                              use GL.Buffers;
with GL.Objects.Programs;                     use GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types;                                use GL.Types;
with GL.Uniforms;

with Glfw;
with Glfw.Input;                              use Glfw.Input;
with Glfw.Windows;
with Glfw.Windows.Context;                    use Glfw.Windows.Context;
with Glfw.Windows.Hints;                      use Glfw.Windows.Hints;

with Cargame.Config;
with Cargame.Globals;
with Cargame.Gameplay;                        use Cargame.Gameplay;
with Cargame.Gameplay.Systems;
with Cargame.Gameplay.Components;

with Cargame.Types;                           use Cargame.Types;
with Cargame.Util;
with Cargame.Engine.ECS;
with Cargame.Engine.Models;                   use Cargame.Engine.Models;

with Cargame.Renderer;

procedure Cargame.Main is

   use GL.Types.Singles;

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

   Set_Color_Clear_Value (Config.Clear_Colour);

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

   ---------------------------
   --  Initialise renderer  --
   ---------------------------

   Cargame.Renderer.Init;

   ---------------------------
   --  Initialise entities  --
   ---------------------------

   Player_Model   := Create_Model_From_Obj (Config.Player_Model_Path);
   Asteroid_Model := Create_Model_From_Obj (Config.Asteroid_Model_Path);

   Player := ECS.New_Entity;
   Components.Controlled_By_Player.Mgr.Update (Player, True);
   Components.Position.Mgr.Update             (Player, Origin);
   Components.Velocity.Mgr.Update             (Player, (others => 0.0));
   Components.Acceleration.Mgr.Update         (Player, (others => 0.0));
   Components.Render_Scale.Mgr.Update         (Player, 10.0);
   Components.Object_Matrix.Mgr.Update        (Player, Identity4);
   Components.CamObj_Matrix.Mgr.Update        (Player, Identity4);
   Components.Normal_Matrix.Mgr.Update        (Player, Identity3);
   Components.Model.Mgr.Update                (Player, Player_Model);
   Components.Rotation.Mgr.Update             (Player, Radians (0.0));

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
         Components.Position.Mgr.Update (Asteroids (I), Position);
      end;

      --  Set everything else
      Components.Rotation.Mgr.Update         (Asteroids (I), Radians (0.0));
      Components.Model.Mgr.Update            (Asteroids (I), Asteroid_Model);
      Components.Rotational_Speed.Mgr.Update (Asteroids (I), Radians (0.01));
      Components.Render_Scale.Mgr.Update     (Asteroids (I), 10.0);
      Components.Object_Matrix.Mgr.Update    (Asteroids (I), Identity4);
      Components.CamObj_Matrix.Mgr.Update    (Asteroids (I), Identity4);
      Components.Normal_Matrix.Mgr.Update    (Asteroids (I), Identity3);
   end loop;

   -----------------------
   --  Set ECS Systems  --
   -----------------------

   declare
      package ECS renames Cargame.Engine.ECS;
      use Cargame.Gameplay.Systems;
   begin
      --  NOTE: The dependency ordering of these systems is manually set here
      --  just by arranging these function calls. Maybe we could do something
      --  cleverer.
      ECS.Systems.Register_System (Tick_Camera'Access);
      ECS.Systems.Register_System (Tick_Position'Access);
      ECS.Systems.Register_System (Tick_Rotation'Access);
      ECS.Systems.Register_System (Tick_Object_Matrix'Access);
      ECS.Systems.Register_System (Render'Access);
   end;

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

      ECS.Systems.Run_All_Systems;

      Renderer.Clear_Back_Buffer;
      Renderer.Render_Enqueued_Entities;
      Renderer.Swap_Buffers;

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
