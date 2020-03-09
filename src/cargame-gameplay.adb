with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Buffers;

with Cargame.Uniforms;
with Cargame.Util;

package body Cargame.Gameplay is

   --  procedure Register_Systems is
   --     use Cargame.ECS;
   --     use Cargame.ECS.Components;
   --  begin

   --     Register_System ((Proc       => Tick_Position'Access, 
   --                       Run_Every  => Frames (1), 
   --                       Components => Position & Velocity));

   --     Register_System ((Proc       => Tick_Rotation'Access, 
   --                       Run_Every  => Frames (1), 
   --                       Components => Rotation & Rotational_Speed));

   --     --  TODO: More ambitious conversions below.

   --     --  Register_System (Render'Access,
   --     --                   Run_Every  => Frames (1), 
   --     --                   Components => Position & 
   --     --                                 Rotation & 
   --     --                                 Scale &
   --     --                                 Vertex_Array & 
   --     --                                 Vertex_Buffer & 
   --     --                                 Element_Buffer &
   --     --                                 Ambient_Light & 
   --     --                                 Diffuse_Light & 
   --     --                                 Specular_Light);

   --     --  Register_System (Grab_Input'Access,
   --     --                   Run_Every  => Frames (2), 
   --     --                   Components => Keyboard & Mouse);

   --  end Register_Systems;

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

   end Systems;

   --  procedure Render (E : in Entity) is
   --     use Cargame.ECS.Components;
   --     Vao : Vertex_Array_Object := Vertex_Array.Get (E);
   --     Vtx_Buf : Vertex_Buffer   := Vertex_Buffer.Get (E);
   --     Elm_Buf : Element_Buffer  := Element_Buffer.Get (E);

   --     Pos : Position_Type := Position.Get (E);
   --     Rot : Radians       := Rotation.Get (E);
   --     Scale : Single      := Render_Scale.Get (E);

   --     Amb_Light : Vector3 := Ambient_Light.Get (E);
   --     Dif_Light : Vector3;
   --     Spe_Light : Vector3;

   --     Elm_Range : Element_Range := Element_Range.Get (E);

   --     use GL.Objects.Textures;
   --     use GL.Objects.Textures.Targets;
   --  begin

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

         ---------------------
         --  Tick Rotation --
         ---------------------

         Rotational_Velocity := 
            Rotational_Velocity + 
            0.001 * (Radians (Throttle (Yaw_Right).Value) - 
                     Radians (Throttle (Yaw_Left).Value));

         Rotation := Rotation + Rotational_Velocity;

      end Tick;

   end Player;

   --------------
   --  Planet  --
   --------------

   package body Planet is

      procedure Tick is
      begin
         Rotation := Rotation + Rotational_Velocity;
      end Tick;

   end Planet;

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
