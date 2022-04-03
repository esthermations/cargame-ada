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
      Pos             : Position.Data_T;
      Player_Position : Position.Datum_T;
   begin
      Position.Mgr.Read_Fresh (Pos);

      Player_Position := Pos (Gameplay.Player);
      pragma Assert (Player_Position.Is_Set);

      Renderer.Camera.Set_Position (Player_Position.Val + Config.Camera_Position_Offset);
      Renderer.Camera.Set_Target   (Player_Position.Val + Config.Camera_Target_Offset);
   end Tick_Camera;

   procedure Tick_Position is
      Pos : Position.Data_T;
      Vel : Velocity.Data_T;
   begin
      Position.Mgr.Read_Stale (E, Pos);
      Velocity.Mgr.Read_Fresh (E, Vel);

      for E in Entity loop
         if Pos (E).Is_Set and Vel (E).Is_Set then
            Pos (E).Val := @ + Vel (E).Val;
         end if;
      end loop;

      Position.Mgr.Write_Fresh (Pos);
   end Tick_Position;

   procedure Tick_Rotation is
      Rot : Rotation.Data_T;
      Spd : Rotational_Speed.Data_T;
   begin
      Rotation.Mgr.Read_Stale         (Rot);
      Rotational_Speed.Mgr.Read_Fresh (Spd);

      for E in Entity loop
         if Rot (E).Is_Set and Spd (E).Is_Set then
            Rot (E).Val := @ + Spd (E).Val;
         end if;
      end loop;

      Rotation.Mgr.Write_Fresh (Rot);
   end Tick_Rotation;


   procedure Tick_Velocity is
      Vel : Velocity.Data_T;
      Acc : Acceleration.Data_T;
   begin
      Velocity.Mgr.Read_Stale (Old);
      Acceleration.Mgr.Read_Fresh (Acc);

      for E in Vel'Range loop
         if Vel (E).Is_Set and then Acc (E).Is_Set then
            Vel (E).Val := @ + Acc (E).Val;
         end if;
      end loop;

      Velocity.Mgr.Write_Fresh (Vel);
   end Tick_Velocity;


   procedure Tick_Object_Matrix is
      Scl : Render_Scale.Data_T;
      Rot : Rotation.Data_T;
      Pos : Position.Data_T;
      Obj : Object_Matrix.Data_T;
   begin
      Render_Scale.Mgr.Read_Fresh (Scl);
      Rotation.Mgr.Read_Fresh     (Rot);
      Position.Mgr.Read_Fresh     (Pos);

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

      Object_Matrix.Mgr.Discard_Stale_Data;
      Object_Matrix.Mgr.Write_Fresh (Obj);
   end Tick_Object_Matrix;

end Cargame.Gameplay.Systems;
