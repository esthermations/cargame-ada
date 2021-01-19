with Cargame.Globals; use Cargame.Globals;

package body Cargame.ECS is

   Next_Entity : Entity := Entity'First;

   ------------------
   --  New_Entity  --
   ------------------

   function New_Entity return Entity is
      pragma Assert (Manager.Next_Entity /= Entity'Last,
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

      function Query return Entity_Set is
         Num_Entities : Natural := 0;
      begin
         for E in Entity'Range loop
            if Is_Present (E) then
               Num_Entities := @ + 1;
            end if;
         end loop;

         declare
            Idx : Natural := 0;
            Ret : Entity_Set (Num_Entities);
         begin
            for E in Entity'Range loop
               if Is_Present (E) then
                  Ret (Idx) := E;
                  Idx := @ + 1;
               end if;
            end loop;

            pragma Assert (Idx = Num_Entities);
            return Ret;
         end;
      end Query;

   end Component;


   ---------------
   --  Systems  --
   ---------------

   package body Systems is
      use ECS;

      procedure Run_All_Systems is
      begin

         Tick_Position :
         for E of Union ((Position.Q, Velocity.Q)) loop
            ECS.Position (E) := @ + ECS.Velocity (E);
         end loop;

         Tick_Rotation :
         for E of Union ((Rotation.Q, Rotational_Speed.Q)) loop
            ECS.Rotation (E) := @ + ECS.Rotational_Speed (E);
         end loop;

         Tick_Object_Matrices :
         for E of Union ((Position.Q, Render_Scale.Q, Rotation.Q)) loop
            ECS.Object_Matrix (E) :=
               Translate (Rotate (Scale (Identity4,
                                         ECS.Render_Scale (E)),
                                  ECS.Rotation (E)),
                          ECS.Position (E));
         end loop;

      end Run_All_Systems;

      --------------------------
      --  Tick_CamObj_Matrix  --
      --------------------------

      procedure Tick_CamObj_Matrix (E : in Entity) is
         Obj : constant Matrix4 := Components.Object_Matrix.Get (E);
         Cam : constant Matrix4 := Uniforms.Camera_Transform.Get;
         CamObj : constant Matrix4 := Cam * Obj;
      begin
         Components.CamObj_Matrix.Set (E, CamObj);
      end Tick_CamObj_Matrix;

      --------------------------
      --  Tick_Normal_Matrix  --
      --------------------------

      procedure Tick_Normal_Matrix (E : in Entity) is
         CamObj : constant Matrix4 := Components.CamObj_Matrix.Get (E);
         Normal : constant Matrix3 := Transpose (Inverse (Mat4_To_Mat3 (CamObj)));
      begin
         Components.Normal_Matrix.Set (E, Normal);
      end Tick_Normal_Matrix;

      --------------
      --  Render  --
      --------------

      -------------------------------------------------------------------------
      procedure Render (E : in ECS.Entity) is
         M : Model := ECS.Model.Get (E);
      begin
         use GL.Objects.Buffers;
         use GL.Objects.Textures;
         use GL.Objects.Textures.Targets;

         Send_Updated_Uniforms (Object_Position => Pos,
                                Object_Rotation => Rot,
                                Object_Scale    => Scale);

         Bind (Vao);
         Bind (Array_Buffer, Vertex_Buffer);

         Draw_Arrays (Mode           => Triangles,
                      Index_Type     => UInt_Type,
                      Count          => (Elm_Range.Last - Elm_Range.First));

      end Render;
   end Systems;



end Cargame.ECS;
