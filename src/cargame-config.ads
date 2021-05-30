--  Configurable constant values you may want to change. This is essentially a
--  config file for the project.

with GL.Types;        use GL.Types, GL.Types.Singles;
with GL.Types.Colors; use GL.Types.Colors;

package Cargame.Config is

   Vertex_Shader_Path   : constant String := "./src/shaders/vert.glsl";
   Fragment_Shader_Path : constant String := "./src/shaders/frag.glsl";

   Player_Model_Path    : constant String := "./src/models/Barrel02.obj";
   Asteroid_Model_Path  : constant String := "./src/models/Barrel02.obj";

   Max_Entities : constant := 100;
   --  Maximum number of entities we can spawn in the game. Storage for these
   --  is preallocated, so if you set this too high the program may crash.
   --  Hooray!

   Max_Systems  : constant := 10;
   --  Maximum number of systems the ECS will allow you to register.

   Initial_Camera_Position : constant Vector3 := (0.0, 2.0, -2.0);
   Initial_Camera_Target   : constant Vector3 := (0.0, 0.0, 0.0);
   Up_Vector               : constant Vector3 := (0.0, 1.0, 0.0);

   Camera_Position_Offset  : constant Vector3 := (0.0, 2.0, -2.0);
   --  This is added to the player's position to give the camera's position.

   Camera_Target_Offset    : constant Vector3 := (0.0, 0.0, +2.0);
   --  This is added to the player's position to give the camera's target.

   Clear_Colour            : constant Color := (0.1, 0.4, 0.4, 1.0);
   Vertical_FoV            : constant       := 60.0;
   Near_Plane              : constant       := 1.0;
   Far_Plane               : constant       := 200.0;

end Cargame.Config;