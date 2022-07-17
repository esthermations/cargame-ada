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
with Cargame.Renderer.Systems;

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

   declare
      Controlled_By_Player : Components.Controlled_By_Player.Data_T := Components.Controlled_By_Player.Mgr.Get;
      Position             : Components.Position.Data_T             := Components.Position.Mgr.Get;
      Velocity             : Components.Velocity.Data_T             := Components.Velocity.Mgr.Get;
      Acceleration         : Components.Acceleration.Data_T         := Components.Acceleration.Mgr.Get;
      Render_Scale         : Components.Render_Scale.Data_T         := Components.Render_Scale.Mgr.Get;
      Object_Matrix        : Components.Object_Matrix.Data_T        := Components.Object_Matrix.Mgr.Get;
      Normal_Matrix        : Components.Normal_Matrix.Data_T        := Components.Normal_Matrix.Mgr.Get;
      Model                : Components.Model.Data_T                := Components.Model.Mgr.Get;
      Rotation             : Components.Rotation.Data_T             := Components.Rotation.Mgr.Get;
      Rotational_Speed     : Components.Rotational_Speed.Data_T     := Components.Rotational_Speed.Mgr.Get;
      Look_At_Target       : Components.Look_At_Target.Data_T       := Components.Look_At_Target.Mgr.Get;

      Player : constant ECS.Entity := ECS.New_Entity;
      Camera : constant ECS.Entity := ECS.New_Entity;
   begin
      Controlled_By_Player (Player) := True;
      Position             (Player) := Origin;
      Velocity             (Player) := (others => 0.0);
      Acceleration         (Player) := (others => 0.0);
      Render_Scale         (Player) := 10.0;
      Object_Matrix        (Player) := Identity4;
      Normal_Matrix        (Player) := Identity3;
      Model                (Player) := Player_Model;
      Rotation             (Player) := Radians (0.0);

      --  Set camera...
      Controlled_By_Player (Camera) := False;
      Position             (Camera) := Valid_Vector3'(0.0, 0.0, -1.0);
      Look_At_Target       (Camera) := Position (Player).Val;

      --  Set asteroids...
      for I in Asteroids'Range loop
         Asteroids (I) := ECS.New_Entity;

         --  Set position
         declare
            package R renames Ada.Numerics.Float_Random;
            Pos    : Valid_Vector3;
            Factor : constant Single := 50.0;
         begin
            Pos (X) := Factor * Single (R.Random (Float_Generator) - 0.5);
            Pos (Y) := Factor * Single (R.Random (Float_Generator) - 0.5);
            Pos (Z) := Factor * Single (R.Random (Float_Generator) - 0.5);
            Position (Asteroids (I)).Val := Pos;
         end;

         --  Set everything else
         Rotation         (Asteroids (I)) := Radians (0.0);
         Model            (Asteroids (I)) := Asteroid_Model;
         Rotational_Speed (Asteroids (I)) := Radians (0.01);
         Render_Scale     (Asteroids (I)) := 10.0;
         Object_Matrix    (Asteroids (I)) := Identity4;
         Normal_Matrix    (Asteroids (I)) := Identity3;
      end loop;

      Components.Controlled_By_Player.Mgr.Set (Controlled_By_Player);
      Components.Position.Mgr.Set             (Position);
      Components.Velocity.Mgr.Set             (Velocity);
      Components.Acceleration.Mgr.Set         (Acceleration);
      Components.Render_Scale.Mgr.Set         (Render_Scale);
      Components.Object_Matrix.Mgr.Set        (Object_Matrix);
      Components.Normal_Matrix.Mgr.Set        (Normal_Matrix);
      Components.Model.Mgr.Set                (Model);
      Components.Rotation.Mgr.Set             (Rotation);
      Components.Rotational_Speed.Mgr.Set     (Rotational_Speed);
      Components.Look_At_Target.Mgr.Set       (Look_At_Target);
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

      Glfw.Input.Poll_Events;

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
      Globals.Frame_Number.Increment;

   end loop Game_Loop;

   Globals.Window.Destroy (Globals.Window.Ptr);
   Glfw.Shutdown;

   Put_Line ("Thanks for playing!");

end Cargame.Main;
