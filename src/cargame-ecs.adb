with Cargame.Globals;  use Cargame.Globals;
with Cargame.Gameplay; use Cargame.Gameplay;
with Cargame.Types;    use Cargame.Types;
with Cargame.Renderer; use Cargame.Renderer;

with GL;               use GL;
with GL.Types;         use GL.Types;          use GL.Types.Singles;
with GL.Buffers;       use GL.Buffers;
with GL.Objects;
with GL.Objects.Textures;

with Glfw.Windows.Context;

package body Cargame.ECS is

   use Cargame.Gameplay.Components;

   function Union (Sets : in Entity_Sets) return Entity_Set is
   begin
      return Ret : Entity_Set := (others => False) do
         for S of Sets loop
            for E in Entity loop
               if S (E) then
                  Ret (E) := True;
               end if;
            end loop;
         end loop;
      end return;
   end Union;

   ------------------
   --  New_Entity  --
   ------------------

   Next_Entity : Entity := Entity'First;

   function New_Entity return Entity is
      pragma Assert (Next_Entity /= Entity'Last,
                     "Exceeded allowable number of entities.");
      Ret : constant Entity := Next_Entity;
   begin
      Next_Entity := Entity'Succ (@);
      return Ret;
   end New_Entity;

   -----------------
   --  Component  --
   -----------------

   package body Component is

      procedure Set (E : in Entity; V : in Data_Type) is
      begin
         Has   (E) := True;
         Value (E) := V;
      end Set;

   end Component;

   ---------------
   --  Systems  --
   ---------------

   --  Declarations

   package Systems is
      procedure Tick_View_Matrix;
      procedure Tick_Position;
      procedure Tick_Rotation;
      procedure Tick_Object_Matrix;
      procedure Render;
   end Systems;

   package body Systems is

      procedure Tick_View_Matrix is
         Pos : constant Valid_Vector3 :=
            Position.Value (Cargame.Gameplay.Player);
      begin
         Uniforms.View_Matrix.Set_Without_Sending
            (Look_At (Camera_Pos => (Pos + Vector3'(0.0, 2.0, -2.0)),
                      Target_Pos => (Pos + Vector3'(0.0, 0.0, +2.0)),
                      Up         => (Y => 1.0, others => 0.0)));
      end Tick_View_Matrix;

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
         Should_Render : constant Entity_Set :=
            Union ((Model.Q, Rotation.Q, Position.Q, Render_Scale.Q));
      begin
         for E in Entity loop
            if Should_Render (E) then
               Cargame.Renderer.Enqueue_For_Rendering (E);
            end if;
         end loop;
      end Render;

   end Systems;

   -----------------------
   --  Run_All_Systems  --
   -----------------------

   procedure Run_All_Systems is
   begin

      --  Clear backbuffer
      Clear (Buffer_Bits'(Depth => True, Color => True, others => <>));

      --  NOTE: The dependency ordering of these systems is manually set here
      --  just by arranging these function calls. Maybe we could do something
      --  cleverer.

      Systems.Tick_View_Matrix;
      Systems.Tick_Position;
      Systems.Tick_Rotation;
      Systems.Tick_Object_Matrix;
      Systems.Render;

      --  Swap backbuffer to front
      Glfw.Windows.Context.Swap_Buffers (Globals.Window.Ptr);

   end Run_All_Systems;

end Cargame.ECS;
