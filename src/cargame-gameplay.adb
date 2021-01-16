with GL.Objects.Textures;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Buffers;

with Cargame.Globals;
with Cargame.Uniforms;
with Cargame.Util;

package body Cargame.Gameplay is

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
         use GL.Objects.Buffers;
         use GL.Objects.Textures;
         use GL.Objects.Textures.Targets;

   --     Send_Updated_Uniforms (Object_Position => Pos,
   --                            Object_Rotation => Rot,
   --                            Object_Scale    => Scale);

   --     Bind (Vao);
   --     Bind (Array_Buffer, Vtx_Buf);
   --     Bind (Element_Array_Buffer, Elm_Buf);

   --     Uniforms.Material_Shininess.Set (Shininess);
   --     Uniforms.Material_Ambient.Set (Ambient_Light);

   --     --  FIXME: These should probably be their own systems, right?

   --     if ECS.Components.Diffuse_Texture.Has (E) then
   --        Uniforms.Material_Diffuse.Set (Globals.Diffuse_Map_ID);
   --        Set_Active_Unit (Globals.Diffuse_Map_ID);
   --        Texture_2D.Bind (Components.Diffuse_Light.Get (E));
   --     end if;

   --     if ECS.Components.Specular_Texture.Has (E) then
   --        Uniforms.Material_Specular.Set (Globals.Specular_Map_ID);
   --        Set_Active_Unit (Globals.Specular_Map_ID);
   --        Texture_2D.Bind (Components.Specular_Texture.Get (E));
   --     end if;

   --     Draw_Elements (Mode           => Triangles,
   --                    Index_Type     => UInt_Type,
   --                    Element_Offset => Elm_Range.First,
   --                    Count          => (Elm_Range.Last - Elm_Range.First));

   --     --  Unbind_VAO; --  Uncomment if borked.
   --
   --  end Render;

   ------------------------
   --  Throttle_Control  --
   ------------------------

   procedure Tick (T : in out Throttle_Control) is
   begin
      T.Value := (if T.Is_Active
                  then Single'Min (Throttle_Amount'Last,  T.Value + T.Step)
                  else Single'Max (Throttle_Amount'First, T.Value - T.Step));
   end Tick;

   --------------
   --  Player  --
   --------------

   package body Player is

      ------------
      --  Tick  --
      ------------

      procedure Tick is
         use Gameplay.Controls;
      begin

         ----------------------
         --  Tick throttles  --
         ----------------------

         for T of Throttle loop
            T.Tick;
         end loop;

         ---------------------
         --  Tick Velocity  --
         ---------------------

         declare
            --  Mouse_Pos : constant Vector2 :=
            --     Globals.Mouse.Normalised_Position_From_Centre;

            X_Speed : constant Single :=
               (Throttle (Strafe_Left).Value - Throttle (Strafe_Right).Value);
            Y_Speed : constant Single :=
               (Throttle (Strafe_Up).Value - Throttle (Strafe_Down).Value);
            Z_Speed : constant Single :=
               (Throttle (Accelerate).Value - Throttle (Decelerate).Value);
         begin
            --  TODO : Make throttle up and down push you on a vector depending
            --  on your current rotation.
            --
            --  i.e., Forwards throttle should behave differently in that it
            --  just increases your speed towards your mouse cursor.

            Velocity (X) := Velocity (X) + X_Speed;
            Velocity (Y) := Velocity (Y) + Y_Speed;
            Velocity (Z) := Velocity (Z) + Z_Speed;

            --  TODO: Normalise velocity in a reasonable way
            Velocity := Velocity / 10.0;
         end;

         ---------------------
         --  Tick Position  --
         ---------------------

         Position := Position + Velocity;

         for I in Position'Range loop
            Position (I) := Position (I) + Velocity (I);
         end loop;

         GL.Objects.Vertex_Arrays.Bind (M.Vao);
         Bind (Target => Array_Buffer,         Object => M.Vertex_Buffer);
         Bind (Target => Element_Array_Buffer, Object => M.Index_Buffer);

         Rotational_Velocity :=
            Rotational_Velocity +
            0.001 * (Radians (Throttle (Yaw_Right).Value) -
                     Radians (Throttle (Yaw_Left).Value));

            Uniforms.Material_Shininess.Set (Mtl.Shininess);
            Uniforms.Material_Ambient.Set   (Mtl.Ambient_Light);

            if Mtl.Diffuse_Texture.Initialized then
               Uniforms.Diffuse_Map.Set (Globals.Diffuse_Map_ID);
               Set_Active_Unit (Globals.Diffuse_Map_ID);
               Texture_2D.Bind (Mtl.Diffuse_Texture);
            end if;

            if Mtl.Specular_Texture.Initialized then
               Uniforms.Specular_Map.Set (Globals.Specular_Map_ID);
               Set_Active_Unit (Globals.Specular_Map_ID);
               Texture_2D.Bind (Mtl.Specular_Texture);
            end if;

            GL.Objects.Buffers.Draw_Elements
               (Mode           => Triangles,
                Index_Type     => UInt_Type,
                Element_Offset => Integer (Mtl.First_Index),
                Count          => Mtl.Num_Indices);

         end loop;

      end Render;

   end Systems;

   ----------------
   --  Controls  --
   ----------------

   package body Controls is

      function Key_To_Action (K : in Key) return Any_Control_Action is
      begin
         for Action in Control_Action'Range loop
            if Control_Mapping (Action) = K then
               return Action;
            end if;
         end loop;
         return No_Action;
      end Key_To_Action;

   end Controls;

end Cargame.Gameplay;
