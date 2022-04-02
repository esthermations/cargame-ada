with Cargame.Engine.ECS; use Cargame.Engine.ECS;

with Cargame.Types;    use Cargame.Types;
with Cargame.Globals;  use Cargame.Globals;
with Cargame.Renderer; use Cargame.Renderer;
with Cargame.Config;

with Cargame.Gameplay; use Cargame.Gameplay;
with Cargame.Gameplay.Components;

with GL;               use GL;
with GL.Types;         use GL.Types;          use GL.Types.Singles;
with GL.Buffers;       use GL.Buffers;
with GL.Objects;
with GL.Objects.Textures;

package body Cargame.Gameplay.Systems is

   use Cargame.Gameplay.Components;

   procedure Tick_Camera is
      Pos : Position.Option;
   begin
      Position.Mgr.Read_Fresh (Cargame.Gameplay.Player, Pos);
      Renderer.Camera.Set_Position (Pos.Val + Config.Camera_Position_Offset);
      Renderer.Camera.Set_Target   (Pos.Val + Config.Camera_Target_Offset);
   end Tick_Camera;

   procedure Tick_Position is
      Old_Pos : Position.Option;
      Vel     : Velocity.Option;

      function Should_Update
         return Boolean
         is (Old_Pos.Is_Set and then Vel.Is_Set);
   begin
      for E in Entity loop
         Position.Mgr.Read_Stale (E, Old_Pos);
         Velocity.Mgr.Read_Fresh (E, Vel);
         if Should_Update then
            Position.Mgr.Update (E, Old_Pos.Val + Vel.Val);
         end if;
      end loop;
      Position.Mgr.Finished_Update;
   end Tick_Position;

   procedure Tick_Rotation is
      Old_Rot : Rotation.Option;
      Rot_Spd : Rotational_Speed.Option;

      function Should_Update
         return Boolean
         is (Old_Rot.Is_Set and Rot_Spd.Is_Set);
   begin
      for E in Entity loop
         Rotation.Mgr.Read_Stale         (E, Old_Rot);
         Rotational_Speed.Mgr.Read_Fresh (E, Rot_Spd);
         if Should_Update then
            Rotation.Mgr.Update (E, Old_Rot.Val + Rot_Spd.Val);
         end if;
      end loop;
      Rotation.Mgr.Finished_Update;
   end Tick_Rotation;

   procedure Tick_Object_Matrix is
      Scl     : Render_Scale.Option;
      Rot     : Rotation.Option;
      Pos     : Position.Option;

      function Should_Update
         return Boolean
         is (Scl.Is_Set and then Rot.Is_Set and then Pos.Is_Set);
   begin
      for E in Entity loop
         Render_Scale.Mgr.Read_Fresh (E, Scl);
         Rotation.Mgr.Read_Fresh (E, Rot);
         Position.Mgr.Read_Fresh (E, Pos);

         if Should_Update then
            declare
               Mtx : Matrix4 := Identity4;
            begin
               Scale     (Mtx, Scl.Val);
               Rotate    (Mtx, Rot.Val);
               Translate (Mtx, Pos.Val);
               Object_Matrix.Mgr.Update (E, Mtx);
            end;
         end if;
      end loop;
      Object_Matrix.Mgr.Finished_Update;
   end Tick_Object_Matrix;

   procedure Render is
      Mdl : Model.Option;
      Rot : Rotation.Option;
      Pos : Position.Option;
      Scl : Render_Scale.Option;

      function Should_Render
         return Boolean
         is (Mdl.Is_Set and then
             Rot.Is_Set and then
             Pos.Is_Set and then
             Scl.Is_Set);
   begin
      for E in Entity loop
         Model.Mgr.Read_Fresh (E, Mdl);
         Rotation.Mgr.Read_Fresh (E, Rot);
         Position.Mgr.Read_Fresh (E, Pos);
         Render_Scale.Mgr.Read_Fresh (E, Scl);
         if Should_Render then
            Cargame.Renderer.Enqueue_For_Rendering (E);
         end if;
      end loop;
   end Render;

end Cargame.Gameplay.Systems;
