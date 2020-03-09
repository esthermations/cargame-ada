with GL.Window;
with Cargame.Types;

with Cargame.Uniforms;
with Cargame.Globals;
-- with Cargame.Types;

with Glfw;

with Cargame.Gameplay;
with Cargame.Util;

package body Cargame.Globals is

   function Rendering_Context_Initialised return Boolean is
      (Globals.Window.Object.Initialized and Globals.GL_Program.Initialized);

   ----------------------------------------------------------------------------
   --  Window 

   package body Window is

      -------------------------------------------------------------------------
      procedure Key_Changed (Object   : not null access Main_Window_Type;
                             Key      : Glfw.Input.Keys.Key;
                             Scancode : Glfw.Input.Keys.Scancode;
                             Action   : Glfw.Input.Keys.Action;
                             Mods     : Glfw.Input.Keys.Modifiers)
      is
         pragma Unreferenced (Scancode, Mods); 

         use Glfw.Input.Keys;
         use Cargame.Gameplay.Controls;

         pragma Assert (Object = Globals.Window.Ptr,
                        "Received input from a non-main window?");

         --  Two things called "Action" here, so...
         Game_Action : constant Any_Control_Action := Key_To_Action (Key);
         Key_Action  : Glfw.Input.Keys.Action renames Action;
      begin
         if Game_Action in Player_Action then

            Gameplay.Player.Throttle (Game_Action).Is_Active := 
               (Key_Action in Press | Repeat);

         else 

            case Game_Action is
               when Quit => Globals.Window.Object.Set_Should_Close (True);
               when others => null;
            end case;

         end if;
      end Key_Changed;

      -------------------------------------------------------------------------
      procedure Size_Changed (Object        : not null access Main_Window_Type;
                              Width, Height : Natural)
      is
         pragma Unreferenced (Object);
      begin
         Util.Log ("Window resized to: " 
                      & Natural'Image (Width) & " x " & Natural'Image (Height)
                      & " (aspect: " & Single'Image (Aspect_Ratio) & ")");

         Globals.Window.Width  := Glfw.Size (Width);
         Globals.Window.Height := Glfw.Size (Height);

         Window.Update_Projection;
         GL.Window.Set_Viewport (0, 0, Int (Width), Int (Height));

      end Size_Changed;

      -------------------------------------------------------------------------
      procedure Mouse_Position_Changed
         (Object : not null access Main_Window_Type;
          X, Y   : Glfw.Input.Mouse.Coordinate)
      is
         pragma Unreferenced (Object);
      begin
         Globals.Mouse.X := X;
         Globals.Mouse.Y := Y;
      end Mouse_Position_Changed;

      -------------------------------------------------------------------------
      procedure Mouse_Button_Changed
         (Object : not null access Main_Window_Type;
          Button : Glfw.Input.Mouse.Button;
          State  : Glfw.Input.Button_State;
          Mods   : Glfw.Input.Keys.Modifiers)
      is
         pragma Unreferenced (Object, Mods);
      begin
         Globals.Mouse.Button_States (Button) := State;
      end Mouse_Button_Changed;

      -------------------------------------------------------------------------
      procedure Update_Projection is
      begin
         Uniforms.Projection.Set
            (Types.Perspective_Matrix
                (View_Angle   => Types.Degrees (Globals.Vertical_FoV),
                 Aspect_Ratio => Window.Aspect_Ratio,
                 Near         => Globals.Near_Plane,
                 Far          => Globals.Far_Plane));
      end Update_Projection;

   end Window;

   package body Mouse is

      function Normalised_Position_From_Centre return Vector2 is
         Pos : constant Vector2 := Mouse.Position;
         Half_Screen_Width : constant Single := (Single (Window.Width) / 2.0);
         Half_Screen_Height : constant Single := (Single (Window.Height) / 2.0);
         Ret : constant Vector2 := 
            (GL.X => (Pos (GL.X) - Half_Screen_Width) / Half_Screen_Width,
             GL.Y => (Pos (GL.Y) - Half_Screen_Height) / Half_Screen_Height);
      begin
         return Ret;
      end Normalised_Position_From_Centre;
   end Mouse;

end Cargame.Globals;
