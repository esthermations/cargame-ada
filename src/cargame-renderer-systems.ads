with Cargame.Engine;
with Cargame.Engine.ECS;
with Cargame.Globals;

package Cargame.Renderer.Systems is

   package Kernels is
      procedure Update_Renderer_State;
      procedure Render;
   end Kernels;

   package Update_Renderer_State is new Cargame.Engine.ECS.System
      (Kernel => Kernels.Update_Renderer_State,
       After  => Cargame.Engine.Gameplay_Update_Done);

   package Render is new Cargame.Engine.ECS.System
      (Kernel => Kernels.Render,
       After  => Update_Renderer_State.Done);

end Cargame.Renderer.Systems;