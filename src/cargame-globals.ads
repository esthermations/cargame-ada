with Ada.Real_Time;        use Ada.Real_Time;
with GL;                   use GL;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types.Colors;      use GL.Types.Colors;
with GL.Types;             use GL.Types; use GL.Types.Singles;
with Glfw.Input;           use Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows;         use Glfw.Windows;

--
--  NOTE:
--
--  This package was originally intended to just contain globals willy-nilly as
--  needed. That turns out to be a bit complicated due to circular unit
--  dependencies, and this is one of the only packages in the program that
--  doesn't depend on any other Cargame modules. That means this should
--  probably be called "Program_Globals", since it can't really contain any
--  game logic. Most of the game logic globals are in Cargame.Types for the
--  time being.
--

package Cargame.Globals is

   ----------------------------------------------------------------------------
   --  Fundamental timings and intervals

   Program_Epoch          : Time               := Clock;
   Next_Frame_Time        : Time               := Clock;
   Target_FPS             : constant Positive  := 120;
   Frame_Interval         : constant Time_Span := (Seconds (1) / Target_FPS);
   Seconds_Per_Frame      : constant Single    :=
      (Single (1.0) / Single (Target_FPS));

   Next_Input_Poll_Time   : Time               := Clock;
   Input_Polls_Per_Second : constant Positive  := (Target_FPS / 2);
   Input_Poll_Interval    : constant Time_Span :=
      (Seconds (1) / Input_Polls_Per_Second);

   --  Unit of time, so we can write stuff like
   --  "Input_Poll_Interval := Frames (5);"
   function Frames (Num : in Natural) return Time_Span is
      (Num * Globals.Frame_Interval);

   Frame_Number : Natural := 0;
   --  Incremented in the game loop.

   ----------------------------------------------------------------------------
   --  Rendering state

   Background_Colour : constant Color := 
      (R => 0.1, G => 0.4, B => 0.4, A => 1.0);

   Vertical_FoV      : Single                       := 60.0; 
   --  Actually degrees, but Degrees is defined in Cargame.Types and that'd be a
   --  circular dependency... I do miss D's forward references and lack of 
   --  header files sometimes.

   GL_Program        : GL.Objects.Programs.Program;
   Near_Plane        : constant                     := 1.0;
   Far_Plane         : constant                     := 200.0;

   Camera_Position   : Vector3                      := (0.0, 1.0, -1.0);

   Diffuse_Map_ID    : constant Int := 0;
   Specular_Map_ID   : constant Int := 1;

   Default_Texture   : GL.Objects.Textures.Texture;

   function Rendering_Context_Initialised return Boolean;

   ----------------------------------------------------------------------------
   --  Window state

   package Window is

      type Main_Window_Type is new Glfw.Windows.Window with null record;

      use Glfw.Windows.Callbacks;

      overriding procedure Key_Changed
         (Object   : not null access Main_Window_Type;
          Key      : Keys.Key;
          Scancode : Keys.Scancode;
          Action   : Keys.Action;
          Mods     : Keys.Modifiers);

      overriding procedure Size_Changed
         (Object   : not null access Main_Window_Type;
          Width    : Natural;
          Height   : Natural);

      overriding procedure Mouse_Position_Changed
         (Object   : not null access Main_Window_Type;
          X, Y     : Mouse.Coordinate);

      overriding procedure Mouse_Button_Changed
         (Object   : not null access Main_Window_Type;
          Button   : Mouse.Button;
          State    : Button_State;
          Mods     : Keys.Modifiers);

      Object : aliased Main_Window_Type;
      Ptr    : constant access Main_Window_Type := Object'Access;
      Width  : Glfw.Size := 1280;
      Height : Glfw.Size := 720;

      function Aspect_Ratio return Single is
         (Single (Width) / Single (Height)) with Inline;

      procedure Update_Projection;

   end Window;

   ----------------------------------------------------------------------------
   --  Input state

   package Mouse is
      X, Y : Glfw.Input.Mouse.Coordinate;
      Button_States : array (Glfw.Input.Mouse.Button) of Button_State :=
         (others => Released);

      function Position return Vector2 is 
         (Vector2'(GL.X => Single (X), GL.Y => Single (Y)));

      function Normalised_Position_From_Centre return Vector2;
      --  Return a vector with values normalised to [-1,1], where (0,0) is the
      --  centre of the screen, (1,1) is bottom-right, and (-1,-1) is top-left.
   end Mouse;

end Cargame.Globals;

