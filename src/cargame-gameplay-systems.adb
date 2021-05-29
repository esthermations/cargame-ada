with Cargame.Engine.ECS; use Cargame.Engine.ECS;

with Cargame.Types;    use Cargame.Types;
with Cargame.Globals;  use Cargame.Globals;
with Cargame.Renderer; use Cargame.Renderer;

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
      Pos : constant Valid_Vector3 :=
         Position.Value (Cargame.Gameplay.Player);
   begin
      --  FIXME: These offsets should be configurable in Cargame.Config
      Renderer.Camera.Set_Position (Pos + Vector3'(0.0, 2.0, -2.0));
      Renderer.Camera.Set_Target   (Pos + Vector3'(0.0, 0.0, +2.0));
   end Tick_Camera;

   procedure Tick_Position is
      Update : constant Entity_Set := Union ((Position.Q, Velocity.Q));
   begin
      Position.Value := (for E in Entity =>
                           (if Update (E)
                            then @ (E) + Velocity.Value (E)
                            else @ (E)));
   end Tick_Position;

   procedure Tick_Rotation is
      Update : constant Entity_Set :=
         Union ((Rotation.Q, Rotational_Speed.Q));
   begin
      Rotation.Value := (for E in Entity =>
                           (if Update (E)
                            then @ (E) + Rotational_Speed.Value (E)
                            else @ (E)));
   end Tick_Rotation;

   procedure Tick_Object_Matrix is
      Update : constant Entity_Set :=
         Union ((Position.Q, Render_Scale.Q, Rotation.Q));
   begin
      for E in Entity loop
         if Update (E) then
            declare
               Mtx : Matrix4 := Components.Object_Matrix.Value (E);
            begin
               Scale     (Mtx, Components.Render_Scale.Value (E));
               Rotate    (Mtx, Components.Rotation.Value (E));
               Translate (Mtx, Components.Position.Value (E));
               Object_Matrix.Value (E) := Mtx;
            end;
         end if;
      end loop;
   end Tick_Object_Matrix;

   procedure Render is
      package C renames Cargame.Gameplay.Components;

      Should_Render : constant Entity_Set :=
         Union ((C.Model.Q, C.Rotation.Q, C.Position.Q, C.Render_Scale.Q));
   begin
      for E in Entity loop
         if Should_Render (E) then
            Cargame.Renderer.Enqueue_For_Rendering (E);
         end if;
      end loop;
   end Render;

end Cargame.Gameplay.Systems;
