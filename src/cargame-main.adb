with Ada.Command_Line;
with Ada.Directories;
with Ada.Real_Time;                           use Ada.Real_Time;
with Ada.Text_IO;                             use Ada.Text_IO;

with GL;                                      use GL;
with GL.Buffers;                              use GL.Buffers;

with Glfw;
with Glfw.Input;                              use Glfw.Input;
with Glfw.Windows;
with Glfw.Windows.Context;                    use Glfw.Windows.Context;
with Glfw.Windows.Hints;                      use Glfw.Windows.Hints;

with Cargame.Config;
with Cargame.Globals;
with Cargame.Gameplay;                        use Cargame.Gameplay;

with Cargame.Types;                           use Cargame.Types;
with Cargame.Util;

with Cargame.Renderer;

procedure Cargame.Main is

   --  Timings

   Program_Epoch   : constant Time := Clock;
   Next_Frame_Time : Time := Program_Epoch;
   Frame_T0        : Time;
   Frame_T1        : Time;

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

   Cargame.Gameplay.Init;

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
