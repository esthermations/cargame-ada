with GL;
with GL.Types;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;

with Cargame.Types;
with Cargame.Models;
with Cargame.ECS;

package Cargame.Gameplay is


   use Cargame.Models, Cargame.Types;
   use GL, GL.Types, GL.Types.Singles; 

   ----------------------------------------------------------------------------
   --  Gameplay setup 

   Player_Entity : ECS.Entity;

   Player_Acceleration_Tick : constant Single := 0.001;

   Initial_Player_Position : Position_Type := Origin;

   Player_Model   : Models.Model;
   Asteroid_Model : Models.Model;

   ----------------------------------------------------------------------------
   --  ECS

   package Components is
      use ECS;
      package Position         is new ECS.Generic_Component_Store (Position_Type,     Kind => Position);
      package Velocity         is new ECS.Generic_Component_Store (Velocity_Type,     Kind => Velocity);
      package Acceleration     is new ECS.Generic_Component_Store (Acceleration_Type, Kind => Acceleration);
      package Rotation         is new ECS.Generic_Component_Store (Radians,           Kind => Rotation);
      package Rotational_Speed is new ECS.Generic_Component_Store (Radians,           Kind => Rotational_Speed); 
      package Render_Scale     is new ECS.Generic_Component_Store (Single,            Kind => Render_Scale);
      package Object_Matrix    is new ECS.Generic_Component_Store (Matrix4,           Kind => Object_Matrix);
      package CamObj_Matrix    is new ECS.Generic_Component_Store (Matrix4,           Kind => CamObj_Matrix);
      package Normal_Matrix    is new ECS.Generic_Component_Store (Matrix3,           Kind => Normal_Matrix);
   end Components;

   package Systems is
      procedure Tick_Position       (E : in ECS.Entity);
      procedure Tick_Velocity       (E : in ECS.Entity);
      procedure Tick_Player_Actions (E : in ECS.Entity);
      procedure Tick_Rotation       (E : in ECS.Entity);
      procedure Tick_Object_Matrix  (E : in ECS.Entity);
      procedure Tick_CamObj_Matrix  (E : in ECS.Entity);
      procedure Tick_Normal_Matrix  (E : in ECS.Entity);
   end Systems;

   ----------------------------------------------------------------------------
   --  Controls

   package Controls is

      use Glfw.Input.Keys;

      type Any_Control_Action is
         (No_Action,
          --  Menu actions
          Quit,
          --  Movement actions:
          Accelerate, Decelerate, 
          Strafe_Left, Strafe_Right, Strafe_Up, Strafe_Down, 
          --  Rotate actions:
          Yaw_Left, Yaw_Right, Roll_Left, Roll_Right, Pitch_Up, Pitch_Down);

      subtype Control_Action is Any_Control_Action range 
         Any_Control_Action'Succ (No_Action) .. Any_Control_Action'Last;

      subtype Player_Action is Control_Action range Accelerate .. Pitch_Down;

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


      --  Should this be a component?
      type Player_Action_Boolean_Array is array (Player_Action) of Boolean;
      Player_Is_Requesting_Action : Player_Action_Boolean_Array := 
         (others => False);

   end Controls;

end Cargame.Gameplay;
