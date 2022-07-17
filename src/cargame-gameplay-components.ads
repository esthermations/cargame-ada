with Cargame.Engine.Models;

package Cargame.Gameplay.Components is
   subtype Cargame_Model is Cargame.Engine.Models.Model;

   package Controlled_By_Player is new ECS.Component (Boolean            , "Controlled_By_Player");
   package Position             is new ECS.Component (Types.Valid_Vector3, "Position"            );
   package Velocity             is new ECS.Component (Types.Valid_Vector3, "Velocity"            );
   package Acceleration         is new ECS.Component (Types.Valid_Vector3, "Acceleration"        );
   package Model                is new ECS.Component (Cargame_Model      , "Model"               );
   package Rotation             is new ECS.Component (Types.Radians      , "Rotation"            );
   package Rotational_Speed     is new ECS.Component (Types.Radians      , "Rotational_Speed"    );
   package Render_Scale         is new ECS.Component (Single             , "Render_Scale"        );
   package Object_Matrix        is new ECS.Component (Matrix4            , "Object_Matrix"       );
   package Normal_Matrix        is new ECS.Component (Matrix3            , "Normal_Matrix"       );
   package Look_At_Target       is new ECS.Component (Types.Valid_Vector3, "Look_At_Target"      );

end Cargame.Gameplay.Components;
