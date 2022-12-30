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

package body Cargame.Gameplay.Systems.Kernels is

   use Cargame.Gameplay.Components;

   task body Tick_Camera is
      Pos             : Position.Data_T;
      Player_Position : Position.Datum_T;
   begin
      Position.Mgr.Wait_Until_Updated;
      Pos := Position.Mgr.Get;

      Player_Position := Pos (Gameplay.Player);
      pragma Assert (Player_Position.Is_Set);

      Renderer.Camera.Set_Position (Player_Position.Val + Config.Camera_Position_Offset);
      Renderer.Camera.Set_Target   (Player_Position.Val + Config.Camera_Target_Offset);

      Tick_Camera_Completed.Signal;
   end Tick_Camera;


   task body Tick_Position is
      Pos : Position.Data_T := Position.Mgr.Get;
      Vel : Velocity.Data_T;
   begin
      Velocity.Mgr.Wait_Until_Updated;
      Vel := Velocity.Mgr.Get;

      for E in Entity loop
         if Pos (E).Is_Set and Vel (E).Is_Set then
            Pos (E).Val := @ + Vel (E).Val;
         end if;
      end loop;

      Position.Mgr.Set (Pos);
   end Tick_Position;


   task body Tick_Rotation is
      Rot : Rotation        .Data_T := Rotation.Mgr.Get;
      Spd : Rotational_Speed.Data_T;
   begin
      Rotational_Speed.Mgr.Wait_Until_Updated;
      Spd := Rotational_Speed.Mgr.Get;

      for E of Union (Rot.Entities, Spd.Entities) loop
         Rot.Elements (E) := @ + Spd.Elements (E);
      end loop;

      Rotation.Mgr.Set (Rot);
   end Tick_Rotation;


   task body Tick_Velocity is
      Vel : Velocity    .Data_T := Velocity.Mgr.Get;
      Acc : Acceleration.Data_T;
   begin
      Acceleration.Wait_Until_Updated;
      Acc := Acceleration.Mgr.Get;

      for E of Union (Vel.Entities, Acc.Entities) loop
         Vel.Elements (E) := @ + Acc.Elements (E);
      end loop;

      Velocity.Mgr.Set (Vel);
   end Tick_Velocity;


   task body Tick_Object_Matrix is
      Scl : Render_Scale.Data_T;
      Rot : Rotation.Data_T;
      Pos : Position.Data_T;
      Obj : Object_Matrix.Data_T;
   begin
      Render_Scale.Mgr.Wait_Until_Updated;
      Rotation.Mgr.Wait_Until_Updated;
      Position.Mgr.Wait_Until_Updated;

      Scl := Render_Scale.Mgr.Get;
      Rot := Rotation.Mgr.Get;
      Pos := Position.Mgr.Get;

      for E in Entity loop
         if Scl (E).Is_Set and Rot (E).Is_Set and Pos (E).Is_Set then
            declare
               Mtx : Matrix4 := Identity4;
            begin
               Scale     (Mtx, Scl.Val);
               Rotate    (Mtx, Rot.Val);
               Translate (Mtx, Pos.Val);
               Obj (E) := (Val => Mtx, Is_Set => True);
            end;
         end if;
      end loop;

      Object_Matrix.Mgr.Set (Obj);
   end Tick_Object_Matrix;

end Cargame.Gameplay.Systems.Kernels;
