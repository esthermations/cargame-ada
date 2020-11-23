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

      ---------------------
      --  Tick_Position  --
      ---------------------

      procedure Tick_Position (E : in Entity) is
         Pos :          Position_Type := Components.Position.Get (E);
         Vel : constant Velocity_Type := Components.Velocity.Get (E);
      begin
         Pos := Pos + Vel;
         Components.Position.Set (E, Pos);
      end Tick_Position;

      ---------------------
      --  Tick_Velocity  --
      ---------------------

      procedure Tick_Velocity (E : in Entity) is
         Vel :          Velocity_Type     := Components.Velocity.Get (E);
         Acc : constant Acceleration_Type := Components.Acceleration.Get (E);
      begin
         Vel := Vel + Acc;
         Components.Velocity.Set (E, Vel);
      end Tick_Velocity;

      ---------------------------
      --  Tick_Player_Actions  --
      ---------------------------

      procedure Tick_Player_Actions (E : in Entity) is
         use Gameplay.Controls;
         Acc : Acceleration_Type := (0.0, 0.0, 0.0);
         Requested : Player_Action_Boolean_Array 
            renames Player_Is_Requesting_Action;

         Amount : Single renames Gameplay.Player_Acceleration_Tick;
      begin
         for Action in Controls.Player_Action loop
            if Requested (Action) then
               case Action is
                  when Accelerate   => Acc := @ + (0.0, 0.0, +Amount);
                  when Decelerate   => Acc := @ + (0.0, 0.0, -Amount);
                  when Strafe_Left  => Acc := @ + (+Amount, 0.0, 0.0);
                  when Strafe_Right => Acc := @ + (-Amount, 0.0, 0.0);
                  when Strafe_Up    => Acc := @ + (0.0, +Amount, 0.0);
                  when Strafe_Down  => Acc := @ + (0.0, -Amount, 0.0);
                  when others => null;
               end case;
            end if;
         end loop;
         Components.Acceleration.Set (E, Acc);
      end Tick_Player_Actions;

      ---------------------
      --  Tick_Rotation  --
      ---------------------

      procedure Tick_Rotation (E : in Entity) is
         Rot :          Radians := Components.Rotation.Get (E);
         Spd : constant Radians := Components.Rotational_Speed.Get (E);
      begin
         Rot := Rot + Spd;
         Components.Rotation.Set (E, Rot);
      end Tick_Rotation;

      --------------------------
      --  Tick_Object_Matrix  --
      --------------------------

      procedure Tick_Object_Matrix (E : in Entity) is
         Pos : constant Position_Type := Components.Position.Get (E);
         Scl : constant Single        := Components.Render_Scale.Get (E);
         Rot : constant Radians       := Components.Rotation.Get (E);

         Obj : constant Matrix4 := 
            Translate (Rotate (Scale (Identity4, Scl), Rot), Pos);
      begin
         Components.Object_Matrix.Set (E, Obj);
      end Tick_Object_Matrix;

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

         M      : constant Models.Model := Components.Model.Get (E);
         CamObj : constant Matrix4      := Components.CamObj_Matrix.Get (E);
         Normal : constant Matrix3      := Components.Normal_Matrix.Get (E);
      begin

         Uniforms.CamObj_Transform.Set (CamObj);
         Uniforms.Normal_Transform.Set (Normal);

         GL.Objects.Vertex_Arrays.Bind (M.Vao);
         Bind (Target => Array_Buffer,         Object => M.Vertex_Buffer);
         Bind (Target => Element_Array_Buffer, Object => M.Index_Buffer);

         for Mtl of M.Materials loop

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
