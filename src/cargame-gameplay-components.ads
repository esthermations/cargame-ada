with Cargame.Engine.Models;

package Cargame.Gameplay.Components is
   subtype Cargame_Model is Cargame.Engine.Models.Model;

   package Controlled_By_Player is new ECS.Component (Boolean);
   package Position             is new ECS.Component (Types.Valid_Vector3);
   package Velocity             is new ECS.Component (Types.Valid_Vector3);
   package Acceleration         is new ECS.Component (Types.Valid_Vector3);
   package Model                is new ECS.Component (Cargame_Model);
   package Rotation             is new ECS.Component (Types.Radians);
   package Rotational_Speed     is new ECS.Component (Types.Radians);
   package Render_Scale         is new ECS.Component (Single);
   package Object_Matrix        is new ECS.Component (Matrix4);
   package CamObj_Matrix        is new ECS.Component (Matrix4);
   package Normal_Matrix        is new ECS.Component (Matrix3);
end Cargame.Gameplay.Components;
