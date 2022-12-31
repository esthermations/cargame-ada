with Cargame.Types;
with Cargame.Engine.Models;
with Cargame.Engine.ECS;

package Cargame.Renderer.Components is
   use Cargame.Engine;

   package Model          is new ECS.Component (Models.Model       , "Model");
   package Render_Scale   is new ECS.Component (Single             , "Render_Scale");
   package Object_Matrix  is new ECS.Component (Matrix4            , "Object_Matrix");
   package Normal_Matrix  is new ECS.Component (Matrix3            , "Normal_Matrix");
   package Look_At_Target is new ECS.Component (Types.Valid_Vector3, "Look_At_Target");
end Cargame.Renderer.Components;

