with GL;
with GL.Types;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;

with Cargame.Globals;
with Cargame.Types;
with Cargame.Models;
with Cargame.ECS;

package Cargame.Gameplay is

   use Cargame.Models, Cargame.Types;
   use GL, GL.Types, GL.Types.Singles; 

   package Components is
      use ECS;
      package Position         is new ECS.Generic_Component_Store (Position_Type, Kind => Position);
      package Velocity         is new ECS.Generic_Component_Store (Velocity_Type, Kind => Velocity);
      package Rotation         is new ECS.Generic_Component_Store (Radians, Kind => Rotation);
      package Rotational_Speed is new ECS.Generic_Component_Store (Radians, Kind => Rotational_Speed); 
      package Render_Scale     is new ECS.Generic_Component_Store (Single,  Kind => Render_Scale);
      package Object_Matrix    is new ECS.Generic_Component_Store (Matrix4, Kind => Object_Matrix);
      package CamObj_Matrix    is new ECS.Generic_Component_Store (Matrix4, Kind => CamObj_Matrix);
      package Normal_Matrix    is new ECS.Generic_Component_Store (Matrix3, Kind => Normal_Matrix);
   end Components;

   package Systems is
      procedure Tick_Position      (E : in ECS.Entity);
      procedure Tick_Rotation      (E : in ECS.Entity);
      procedure Tick_Object_Matrix (E : in ECS.Entity);
      procedure Tick_CamObj_Matrix (E : in ECS.Entity);
      procedure Tick_Normal_Matrix (E : in ECS.Entity);
   end Systems;

   ----------------------------------------------------------------------------
   --  Controls

   package Controls is

      use Glfw.Input.Keys;

      type Any_Control_Action is
         (No_Action,
          --  Movement actions:
          Accelerate, Decelerate, 
          Strafe_Left, Strafe_Right, Strafe_Up, Strafe_Down, 
          --  Rotate actions:
          Yaw_Left, Yaw_Right, Roll_Left, Roll_Right, Pitch_Up, Pitch_Down,
          --  Menu actions
          Quit);

      subtype Control_Action is Any_Control_Action range 
         Any_Control_Action'Succ (No_Action) .. Any_Control_Action'Last;

      subtype Player_Action is Control_Action range Accelerate .. Pitch_Down;
      subtype Movement_Action is Control_Action range Accelerate .. Strafe_Down;
      subtype Rotate_Action is Control_Action range Yaw_Left .. Pitch_Down;

      Unbound : constant Key := Unknown; 

      function Key_To_Action (K : in Key) return Any_Control_Action;

      Control_Mapping : constant array (Control_Action) of Key :=
         (Accelerate   => W, --Up,
          Decelerate   => S, --Down,
          Strafe_Left  => A, --Left,
          Strafe_Right => D, --Right,
          Strafe_Up    => Space,
          Strafe_Down  => Left_Control,
          Yaw_Left     => Q,
          Yaw_Right    => E,
          Roll_Left    => Unbound,
          Roll_Right   => Unbound,
          Pitch_Up     => Unbound,
          Pitch_Down   => Unbound,
          Quit         => Escape);

   end Controls;

   ----------------------------------------------------------------------------
   --  Throttle

   --  This models movement in a direction "spinning up" or "winding down" when
   --  a button is pressed or released (TC.Is_Active). Throttle_Amount exists in
   --  the range from 0.0 .. 1.0, so can be used to "scale" the Max_Speed of an
   --  entity just by multiplying (TC.Value * Max_Speed). Set TC.Step to adjust 
   --  how quickly it spins up or slows down.

   subtype Throttle_Amount is Single range 0.0 .. 1.0;

   type Throttle_Control is tagged record
      Value     : Throttle_Amount := 0.0;
      Step      : Throttle_Amount := 0.01;
      Is_Active : Boolean         := False;
   end record;

   procedure Tick (T : in out Throttle_Control);

   ----------------------------------------------------------------------------
   --  Entities 

   package Player is

      Position : Position_Type := Origin;
      Rotation : Radians;

      Rotational_Velocity : Radians := Radians (0.0);
      --  TODO: 3-dimensional rotational velocity. How do you store 3D rotations
      --  anyway? Quaternions?

      Max_Speed    : constant Single := 0.1; 
      --  This is max speed in all directions simultaneously. The sum of the
      --  absolute components of the player's velocity should never exceed this
      --  number.

      Velocity     : Velocity_Type := (others => 0.0); 
      --  Current velocity.

      Model        : Models.Model;
      Render_Scale : constant Single := 0.2;

      Throttle : array (Controls.Player_Action) of Throttle_Control; 

      procedure Tick with Pre => Globals.Rendering_Context_Initialised;
      --   with Post => (Magnitude (Velocity) <= Max_Speed);
      --
      --  Ideally that postcondition would be true, but it isn't always. 
      --  TODO: be a clever girl and figure out why. It's probably not that
      --  complicated, but writing code while tired is a bit like that.

      function Camera_Movement_Offset return Vector3 is (20.0 * Velocity);

   end Player;

   package Planet is
      Model               : Models.Model;
      Rotation            : Radians := Radians (0.0);
      Rotational_Velocity : Radians := Radians (0.01);
      Render_Scale        : constant Single := 20.0;
      Position            : Position_Type := (X => 0.0, Y => 0.0, Z => 2.0);

      procedure Tick with Global => (Input  => Rotational_Velocity,
                                     In_Out => Rotation);
   end Planet;

end Cargame.Gameplay;
