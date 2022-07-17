with GL.Window;
with Glfw;

with Cargame.Types;
with Cargame.Globals;
with Cargame.Gameplay;
with Cargame.Renderer; use Cargame.Renderer;

package body Cargame.Globals is

   protected body Frame_Number
   is

      function Get return Frame is (Value);

      entry Increment is
         Value := @ + 1;
      end Increment;

   end Frame_Number;

   --------------
   --  Window  --
   --------------

   package body Window is

      -------------------------------------------------------------------------
      overriding
      procedure Key_Changed (Object   : not null access Main_Window_Type;
                             Key      : Glfw.Input.Keys.Key;
                             Scancode : Glfw.Input.Keys.Scancode;
                             Action   : Glfw.Input.Keys.Action;
                             Mods     : Glfw.Input.Keys.Modifiers)
      is
         pragma Unreferenced (Scancode, Mods);

         use Glfw.Input.Keys;
         use Cargame.Gameplay.Controls;

         --  Two things called "Action" here, so...
         Game_Action : constant Any_Control_Action := Key_To_Action (Key);
         Key_Action  : Glfw.Input.Keys.Action renames Action;
      begin
         if Game_Action in Player_Action then
            Gameplay.Controls.Player_Is_Requesting_Action (Game_Action) :=
               (if Key_Action in Press | Repeat then True else False);
         else
            case Game_Action is
               when Quit => Globals.Window.Object.Set_Should_Close (True);
               when others => null; --  Key not bound to anything
            end case;
         end if;
      end Key_Changed;

      -------------------------------------------------------------------------
      overriding
      procedure Size_Changed (Object        : not null access Main_Window_Type;
                              Width, Height : Natural)
      is
         pragma Unreferenced (Object);
      begin
         Globals.Window.Width  := Glfw.Size (Width);
         Globals.Window.Height := Glfw.Size (Height);
         Renderer.Handle_Window_Resize;
      end Size_Changed;

      -------------------------------------------------------------------------
      overriding
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
      overriding
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

   end Window;

   -------------
   --  Mouse  --
   -------------

   package body Mouse is

      function Normalised_Position_From_Centre return Vector2 is
         Pos : constant Vector2 := Mouse.Position;
         Half_Screen_W : constant Single := (Single (Window.Width)  / 2.0);
         Half_Screen_H : constant Single := (Single (Window.Height) / 2.0);
         Ret : constant Vector2 :=
            (GL.X => (Pos (GL.X) - Half_Screen_W) / Half_Screen_W,
             GL.Y => (Pos (GL.Y) - Half_Screen_H) / Half_Screen_H);
      begin
         return Ret;
      end Normalised_Position_From_Centre;
   end Mouse;

end Cargame.Globals;