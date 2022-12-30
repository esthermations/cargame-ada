with Cargame.Engine.Models;

package Cargame.Gameplay.Components is

   package Controlled_By_Player is new ECS.Component (Boolean            , "Controlled_By_Player");
   package Position             is new ECS.Component (Types.Valid_Vector3, "Position"            );
   package Velocity             is new ECS.Component (Types.Valid_Vector3, "Velocity"            );
   package Acceleration         is new ECS.Component (Types.Valid_Vector3, "Acceleration"        );
   package Rotation             is new ECS.Component (Types.Radians      , "Rotation"            );
   package Rotational_Speed     is new ECS.Component (Types.Radians      , "Rotational_Speed"    );

end Cargame.Gameplay.Components;
