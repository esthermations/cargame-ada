with Cargame.Globals; use Cargame.Globals;

package body Cargame.ECS is

   Next_Entity : Entity := Entity'First;

   ------------------
   --  New_Entity  --
   ------------------

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

      procedure Run_All_Systems is
      begin

         Tick_Position :
         for E of Union ((Position.Q, Velocity.Q)) loop
            ECS.Position (E) := @ + ECS.Velocity (E);
         end loop Tick_Position;

         Tick_Rotation :
         for E of Union ((Rotation.Q, Rotational_Speed.Q)) loop
            ECS.Rotation (E) := @ + ECS.Rotational_Speed (E);
         end loop Tick_Rotation;

         Tick_Object_Matrices :
         for E of Union ((Position.Q, Render_Scale.Q, Rotation.Q)) loop
            Scale     (ECS.Object_Matrix (E), ECS.Render_Scale (E));
            Rotate    (ECS.Object_Matrix (E), ECS.Rotation (E));
            Translate (ECS.Object_Matrix (E), ECS.Position (E));
         end loop Tick_Object_Matrices;

      end Run_All_Systems;

      --------------
      --  Render  --
      --------------

      -------------------------------------------------------------------------
      --  procedure Render (E : in ECS.Entity) is
      --     M : Model := ECS.Model.Get (E);
      --  begin
      --     use GL.Objects.Buffers;
      --     use GL.Objects.Textures;
      --     use GL.Objects.Textures.Targets;

      --     Send_Updated_Uniforms (Object_Position => Pos,
      --                            Object_Rotation => Rot,
      --                            Object_Scale    => Scale);

      --     Bind (Vao);
      --     Bind (Array_Buffer, Vertex_Buffer);

      --     Draw_Arrays (Mode           => Triangles,
      --                  Index_Type     => UInt_Type,
      --                  Count => (Elm_Range.Last - Elm_Range.First));

      --  end Render;

end Cargame.ECS;
