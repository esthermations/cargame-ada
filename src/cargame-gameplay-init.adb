with Cargame.Engine.ECS;
with Cargame.Renderer.Components;
with Ada.Numerics.Float_Random;

separate (Cargame.Gameplay)
procedure Init is

   Float_Generator : Ada.Numerics.Float_Random.Generator;

   package RC renames Cargame.Renderer.Components;
   package GC renames Cargame.Gameplay.Components;

   Controlled_By_Player : GC.Controlled_By_Player.Data_T;
   Position             : GC.Position            .Data_T;
   Velocity             : GC.Velocity            .Data_T;
   Acceleration         : GC.Acceleration        .Data_T;
   Rotation             : GC.Rotation            .Data_T;
   Rotational_Speed     : GC.Rotational_Speed    .Data_T;
   Look_At_Target       : GC.Look_At_Target      .Data_T;

   Render_Scale         : RC.Render_Scale        .Data_T;
   Object_Matrix        : RC.Object_Matrix       .Data_T;
   Normal_Matrix        : RC.Normal_Matrix       .Data_T;
   Model                : RC.Model               .Data_T;

   Player : constant ECS.Entity := ECS.New_Entity;
   Camera : constant ECS.Entity := ECS.New_Entity;

begin

   Player_Model   := Create_Model_From_Obj (Config.Player_Model_Path);
   Asteroid_Model := Create_Model_From_Obj (Config.Asteroid_Model_Path);

   Controlled_By_Player.Set (Player, True);
   Position            .Set (Player, Origin);
   Velocity            .Set (Player, (others => 0.0));
   Acceleration        .Set (Player, (others => 0.0));
   Render_Scale        .Set (Player, 10.0);
   Object_Matrix       .Set (Player, Identity4);
   Normal_Matrix       .Set (Player, Identity3);
   Model               .Set (Player, Player_Model);
   Rotation            .Set (Player, Radians (0.0));

   --  Set camera...
   Controlled_By_Player.Set (Camera, False);
   Position            .Set (Camera, Valid_Vector3'(0.0, 0.0, -1.0));
   Look_At_Target      .Set (Camera, Position.Elements (Player));

   --  Set asteroids...
   for I in Asteroids'Range loop
      Asteroids (I) := ECS.New_Entity;

      --  Set position
      declare
         package R renames Ada.Numerics.Float_Random;
         Pos    : Valid_Vector3;
         Factor : constant Single := 50.0;
      begin
         Pos (X) := Factor * Single (R.Random (Float_Generator) - 0.5);
         Pos (Y) := Factor * Single (R.Random (Float_Generator) - 0.5);
         Pos (Z) := Factor * Single (R.Random (Float_Generator) - 0.5);
         Position.Set (Asteroids (I), Pos);
      end;

      --  Set everything else
      Rotation        .Set (Asteroids (I), Radians (0.0));
      Model           .Set (Asteroids (I), Asteroid_Model);
      Rotational_Speed.Set (Asteroids (I), Radians (0.01));
      Render_Scale    .Set (Asteroids (I), 10.0);
      Object_Matrix   .Set (Asteroids (I), Identity4);
      Normal_Matrix   .Set (Asteroids (I), Identity3);
   end loop;

   Components.Controlled_By_Player.Mgr.Lock;
   Components.Position.Mgr.Lock;
   Components.Velocity.Mgr.Lock;
   Components.Acceleration.Mgr.Lock;
   Components.Render_Scale.Mgr.Lock;
   Components.Object_Matrix.Mgr.Lock;
   Components.Normal_Matrix.Mgr.Lock;
   Components.Model.Mgr.Lock;
   Components.Rotation.Mgr.Lock;
   Components.Rotational_Speed.Mgr.Lock;
   Components.Look_At_Target.Mgr.Lock;

   --  These could easily be done in parallel. Whatever happened to that
   --  `parallel` block that was going to be in Ada 2022?
   Components.Controlled_By_Player.Mgr.Set (Controlled_By_Player);
   Components.Position.Mgr.Set             (Position);
   Components.Velocity.Mgr.Set             (Velocity);
   Components.Acceleration.Mgr.Set         (Acceleration);
   Components.Render_Scale.Mgr.Set         (Render_Scale);
   Components.Object_Matrix.Mgr.Set        (Object_Matrix);
   Components.Normal_Matrix.Mgr.Set        (Normal_Matrix);
   Components.Model.Mgr.Set                (Model);
   Components.Rotation.Mgr.Set             (Rotation);
   Components.Rotational_Speed.Mgr.Set     (Rotational_Speed);
   Components.Look_At_Target.Mgr.Set       (Look_At_Target);

   Components.Controlled_By_Player.Mgr.Unlock;
   Components.Position.Mgr.Unlock;
   Components.Velocity.Mgr.Unlock;
   Components.Acceleration.Mgr.Unlock;
   Components.Render_Scale.Mgr.Unlock;
   Components.Object_Matrix.Mgr.Unlock;
   Components.Normal_Matrix.Mgr.Unlock;
   Components.Model.Mgr.Unlock;
   Components.Rotation.Mgr.Unlock;
   Components.Rotational_Speed.Mgr.Unlock;
   Components.Look_At_Target.Mgr.Unlock;

end Init;
