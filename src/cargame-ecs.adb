with Cargame.Globals;  use Cargame.Globals;
with Cargame.Gameplay; use Cargame.Gameplay;
with Cargame.Types;    use Cargame.Types;
with Cargame.Uniforms; use Cargame.Uniforms;

with GL;               use GL;
with GL.Types;         use GL.Types;          use GL.Types.Singles;
with GL.Buffers;       use GL.Buffers;
with GL.Objects;

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
         Is_Present (E) := True;
         Value      (E) := V;
      end Set;

   end Component;

   ---------------
   --  Systems  --
   ---------------

   procedure Render (E : in ECS.Entity);

   procedure Run_All_Systems is
   begin

      --  Clear backbuffer
      Clear (Buffer_Bits'(Depth => True, Color => True, others => <>));

      Tick_View_Matrix :
      declare
         Pos : constant Valid_Vector3 :=
            Position.Get (Cargame.Gameplay.Player);
      begin
         Uniforms.View_Matrix.Set_And_Send
            (Look_At (Camera_Position => (Pos + Vector3'(0.0, 2.0, -2.0)),
                      Target_Position => (Pos + Vector3'(0.0, 0.0, +2.0)),
                      Up              => (Y => 1.0, others => 0.0)));
      end Tick_View_Matrix;

      Tick_Position :
      declare
         Ents : constant Entity_Set := Union ((Position.Q, Velocity.Q));
      begin
         for E in Entity loop
            if Ents (E) then
               Position.Set (E, @ + Velocity.Get (E));
            end if;
         end loop;
      end Tick_Position;

      Tick_Rotation :
      declare
         Ents : constant Entity_Set :=
            Union ((Rotation.Q, Rotational_Speed.Q));
      begin
         for E in Entity loop
            if Ents (E) then
               Rotation.Set (E, @ + Rotational_Speed.Get (E));
            end if;
         end loop;
      end Tick_Rotation;

      Tick_Object_Matrices :
      declare
         Ents : constant Entity_Set :=
            Union ((Position.Q, Render_Scale.Q, Rotation.Q));
      begin
         for E in Entity loop
            if Ents (E) then
               declare
                  Mtx : Matrix4 := Components.Object_Matrix.Get (E);
               begin
                  Scale     (Mtx, Components.Render_Scale.Get (E));
                  Rotate    (Mtx, Components.Rotation.Get (E));
                  Translate (Mtx, Components.Position.Get (E));
                  Components.Object_Matrix.Set (E, Mtx);
               end;
            end if;
         end loop;
      end Tick_Object_Matrices;

      Render :
      declare
         Ents : constant Entity_Set := Model.Q;
      begin
         for E in Ents loop
            if Ents (E) then
               Render (E);
            end if;
         end loop;
      end Render;

      --  Swap backbuffer to front
      Swap_Buffers (Globals.Window.Ptr);

   end Run_All_Systems;

   --------------
   --  Render  --
   --------------

   procedure Render (E : in ECS.Entity) is
      M : Model := ECS.Model.Get (E);
      use GL.Objects.Buffers;
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
   begin
      Send_Updated_Uniforms (Object_Position => Pos,
                             Object_Rotation => Rot,
                             Object_Scale    => Scale);
      Bind (Vao);
      Bind (Array_Buffer, Vertex_Buffer);

      for Mtl of M.Materials loop
         Draw_Arrays (Mode       => Triangles,
                      Index_Type => UInt_Type,
                      First      => Mtl.First_Index,
                      Count      => Mtl.Num_Indices);
      end loop;
   end Render;

end Cargame.ECS;
