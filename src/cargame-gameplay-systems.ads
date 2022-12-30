with Cargame.Engine.ECS;

package Cargame.Gameplay.Systems is

   package Kernels is
      procedure Tick_Camera;
      procedure Tick_Position;
      procedure Tick_Rotation;
      procedure Tick_Object_Matrix;
   end Kernels;

   package Tick_Camera is new Cargame.Engine.ECS.System
      (Kernel => Kernels.Tick_Camera, After => []);

   package Tick_Position is new Cargame.Engine.ECS.System
      (Kernel => Kernels.Tick_Position, After => []);

   package Tick_Rotation is new Cargame.Engine.ECS.System
      (Kernel => Kernels.Tick_Rotation, After => []);

   package Tick_Object_Matrix is new Cargame.Engine.ECS.System
      (Kernel => Kernels.Tick_Object_Matrix,
       After  => [Tick_Position.Done, Tick_Rotation.Done]);

end Cargame.Gameplay.Systems;
