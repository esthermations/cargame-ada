package body Cargame.Renderer.Systems is

   procedure Update_Renderer_State is
   begin
      --  Update View, if needed
      if View_Matrix_Needs_Update then
         Uniforms.View.Set_And_Send (
            Look_At (
               Camera_Pos => Camera.Get_Position,
               Target_Pos => Camera.Get_Target,
               Up         => Cargame.Config.Up_Vector
            )
         );
      end if;

      --  Update Projection, if needed
      if Projection_Matrix_Needs_Update then
         Cargame.Renderer.Uniforms.Projection.Set_And_Send (
            Internal_Calculate_Projection
         );
         GL.Window.Set_Viewport (
            0, 0, Int (Globals.Window.Width), Int (Globals.Window.Height)
         );
      end if;

      View_Matrix_Needs_Update       := False;
      Projection_Matrix_Needs_Update := False;

      --  Signal that rendering can start. The Render job will wait for this
      --  before it starts rendering stuff.
      Cargame.Renderer.State.Ready_To_Render.Signal;

   end Update_Renderer_State;

   procedure Render is
      use Cargame.Gameplay.Components;
      Mdl : Model.Data_T;
      Pos : Position.Data_T;
      Rot : Rotation.Data_T;
      Scl : Render_Scale.Data_T;

      Transform : Matrix4 := Identity4;

      use GL.Objects.Vertex_Arrays, GL.Objects.Buffers;
   begin

      Model.Mgr.Read_Fresh        (Mdl);
      Position.Mgr.Read_Fresh     (Pos);
      Rotation.Mgr.Read_Fresh     (Rot);
      Render_Scale.Mgr.Read_Fresh (Scl);

      Util.Log ("Waiting for Ready_To_Render to be signalled...");
      Cargame.Renderer.State.Ready_To_Render.Wait;

      for E in Entity loop
         if Mdl (E).Is_Set and then
            Pos (E).Is_Set and then
            Rot (E).Is_Set and then
            Scl (E).Is_Set
         then
            Scale     (Transform, Scl (E).Val);
            Rotate    (Transform, Rot (E).Val);
            Translate (Transform, Pos (E).Val);

            Uniforms.Model.Set_And_Send (Transform);

            Bind (Mdl (E).Val.Vao);
            Bind (Array_Buffer, Mdl (E).Val.Vertex_Buffer);
            Bind (Array_Buffer, Mdl (E).Val.Normal_Buffer);

            if Mdl (E).Val.Materials.Length > 0 then
               for Mtl of Mdl (E).Val.Materials loop
                  Draw_Arrays (
                     Mode  => Triangles,
                     First => Mtl.First_Index,
                     Count => Mtl.Num_Indices
                  );
               end loop;
            else -- No materials
               pragma Assert (Mdl (E).Val.Num_Vertices > 0);
               Draw_Arrays (
                  Mode  => Triangles,
                  First => 0,
                  Count => Mdl (E).Val.Num_Vertices
               );
            end if;
         end if;
      end loop;
   end Render;



end Cargame.Renderer.Systems;
