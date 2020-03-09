with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Text_IO;             use Ada.Text_IO;

with GL;
with GL.Objects.Textures;
with GL.Objects.Textures.Targets;

with Cargame.Gameplay;
with Cargame.Globals;
with Cargame.Uniforms;
with Cargame.Obj_Parser;
with Cargame.Util; use Cargame.Util;

package body Cargame.Models is

   ----------------------------------------------------------------------------
   function All_Members_Are_Valid (Arr : in Vector3_Array) return Boolean is
      (for all V of Arr => (for all X of V => X'Valid));

   ----------------------------------------------------------------------------
   function All_Members_Are_Valid (Arr : in Vector2_Array) return Boolean is
      (for all V of Arr => (for all X of V => X'Valid));

   ----------------------------------------------------------------------------
   function Get_Bounding_Volume (Verts : in Vector3_Array)
      return Volume3D_Type;

   ----------------------------------------------------------------------------
   function Is_Renderable (M : Model) return Boolean is
      Has_Valid_VAO : constant Boolean := (M.Vao /= Null_Array_Object);
      Has_Indices   : constant Boolean := (M.Num_Indices /= 0);
      Has_Materials : constant Boolean := (M.Materials.Length /= 0);
   begin
      if Has_Valid_VAO and Has_Indices and Has_Materials then
         return True;
      else
         Util.Log_Error ("Model isn't renderable:");
         Util.Log_Error ("Has_Valid_VAO = " & Boolean'Image (Has_Valid_VAO));
         Util.Log_Error ("Has_Indices   = " & Boolean'Image (Has_Indices));
         Util.Log_Error ("Has_Materials = " & Boolean'Image (Has_Materials));
         Util.Log ("All the above must be true for it to be renderable.");
         return False;
      end if;
   end Is_Renderable;

   ----------------------------------------------------------------------------
   function Get_Bounding_Volume (Verts : in Vector3_Array)
      return Volume3D_Type
   is
      Min, Max : Vector3 := (others => 0.0);
   begin
      for Vec of Verts loop
         for I in Vec'Range loop
            if Vec (I) > Max (I) then Max (I) := Vec (I); end if;
            if Vec (I) < Min (I) then Min (I) := Vec (I); end if;
         end loop;
      end loop;

      return Volume3D_Type (Max - Min);
   end Get_Bounding_Volume;

   ----------------------------------------------------------------------------
   function Create_Model (Vertices, Normals : in Vector3_Array;
                          TexCrds           : in Vector2_Array;
                          Indices           : in Int_Array;
                          Materials         : in Vector_Of_Material)
      return Model
   is
      Vao           : Vertex_Array_Object;
      Vertex_Buf    : Buffer;
      Normal_Buf    : Buffer;
      TexCrd_Buf    : Buffer;
      Index_Buf     : Buffer;
      Out_Materials : Vector_Of_Material := Materials;

   begin

      Vao.Initialize_Id;
      pragma Assert (Vao.Initialized);
      Vao.Bind;

      -------------------------
      -- Add vertices to VAO --
      -------------------------

      Vertex_Buf.Initialize_Id;
      pragma Assert (Vertex_Buf.Initialized);

      Bind (Array_Buffer, Vertex_Buf);
      Load_Vector3_Buffer (Target => Array_Buffer,
                           Data   => Vertices,
                           Usage  => Static_Draw);

      Set_Vertex_Attrib_Pointer (Index  => Vertices_Attribute,
                                 Count  => Vector3'Length,
                                 Kind   => Single_Type,
                                 Stride => 0,
                                 Offset => 0);

      Enable_Vertex_Attrib_Array (Vertices_Attribute);

      ------------------------
      -- Add normals to VAO --
      ------------------------

      Normal_Buf.Initialize_Id;
      pragma Assert (Normal_Buf.Initialized);

      Bind (Array_Buffer, Normal_Buf);
      Load_Vector3_Buffer (Target => Array_Buffer,
                           Data   => Normals,
                           Usage  => Static_Draw);

      Set_Vertex_Attrib_Pointer (Index  => Normals_Attribute,
                                 Count  => Vector3'Length,
                                 Kind   => Single_Type,
                                 Stride => 0,
                                 Offset => 0);

      Enable_Vertex_Attrib_Array (Normals_Attribute);

      ------------------------
      -- Add texcrds to VAO --
      ------------------------

      TexCrd_Buf.Initialize_Id;
      pragma Assert (TexCrd_Buf.Initialized,
                     "Failed to init texcrd buffer.");

      Bind (Array_Buffer, TexCrd_Buf);
      Load_Vector2_Buffer (Target => Array_Buffer,
                           Data   => TexCrds,
                           Usage  => Static_Draw);

      Set_Vertex_Attrib_Pointer (Index  => TexCrds_Attribute,
                                 Count  => Vector2'Length,
                                 Kind   => Single_Type,
                                 Stride => 0,
                                 Offset => 0);
      Enable_Vertex_Attrib_Array (TexCrds_Attribute);

      ------------------------
      -- Add indices to VAO --
      ------------------------

      Index_Buf.Initialize_Id;
      pragma Assert (Index_Buf.Initialized,
                     "Failed to init index buffer.");

      Bind (Element_Array_Buffer, Index_Buf);
      Load_Int_Buffer (Target => Element_Array_Buffer,
                       Data   => Indices,
                       Usage  => Static_Draw);

      ---------------------------------------
      -- Add default material if necessary --
      ---------------------------------------

      if Out_Materials.Length = 0 then
         Put_Line ("Appending default materials.");
         pragma Assert
            (Default_Material.Diffuse_Texture.Raw_Id /= 0 and then
                Default_Material.Specular_Texture.Raw_Id /= 0,
             "Default material is uninitialised.");
         declare
            Mtl : Material := Default_Material;
         begin
            pragma Assert
               ((for all I in Indices'First .. Indices'Last
                    => Indices (I) = I),
                "Using default material for non-sequential indices"
                   & " is untested.");
            Mtl.First_Index := Indices'First;
            --  FIXME: There has to be a neater way to get the number of
            --  possible values in a range.
            Mtl.Num_Indices := Indices'Last - Mtl.First_Index;
            Out_Materials.Append (Mtl);
         end;
      else
         Util.Log ("NOT appending default material.");
      end if;

      ------------
      -- Return --
      ------------

      Unbind_VAO;

      return Model'(Vao           => Vao,
                    Materials     => Out_Materials,
                    Dimensions    => Get_Bounding_Volume (Vertices),
                    Vertex_Buffer => Vertex_Buf,
                    Normal_Buffer => Normal_Buf,
                    TexCrd_Buffer => TexCrd_Buf,
                    Index_Buffer  => Index_Buf,
                    Num_Indices   => Indices'Length);
   end Create_Model;

   ----------------------------------------------------------------------------
   function Create_Model_From_Obj (Obj_File_Path : in String) return Model is

      --  This function's main job is to deduplicate the data received from the
      --  obj parser to suit the layout required by glDrawElements. Each unique
      --  combination of GL attributes is given an index. An attribute is
      --  something like a vertex, normal, texcoord, etc. In this case, we're
      --  only dealing with those three.

      Obj : constant Cargame.Obj_Parser.Obj_Data := 
         Cargame.Obj_Parser.Parse (Obj_File_Path);

      subtype Attribute_Index is GL.Types.Size;

      package GL_Attributes is

         --  This is the ENTIRE INTERFACE.

         --  Inputs:
         procedure Add (V, N : in Vector3; T : in Vector2);

         --  Outputs:
         function Get_Vertices return Vector3_Array
            with Pre => Attributes_Are_Valid;
         function Get_Normals return Vector3_Array
            with Pre => Attributes_Are_Valid;
         function Get_TexCrds return Vector2_Array
            with Pre => Attributes_Are_Valid;
         function Get_Indices return Int_Array
            with Pre => Attributes_Are_Valid;

         function Attributes_Are_Valid return Boolean with Ghost;
         --  For preconditions only.

      private

         Vertices        : Vector_Of_Vector3;
         Normals         : Vector_Of_Vector3;
         TexCrds         : Vector_Of_Vector2;
         Attrib_Sequence : Vector_Of_Size;
         --  Naming this is tricky. It IS a list of attrib ID's, but they may
         --  be repeated.
         Next_ID : Attribute_Index := 0;
         --  Indices passed to glDrawElements and such. Starts at 0.
         --  Materials : Vector_Of_Material;

         type FC_Data is record
            V, N : Vector3;
            T    : Vector2;
         end record;

         function "=" (A, B : in FC_Data) return Boolean is
            (A.V = B.V and A.N = B.N and A.T = B.T);

         function Hash (FCD : in FC_Data) return Hash_Type is
            (Ada.Strings.Hash_Case_Insensitive
                (Image (FCD.V) & "," & Image (FCD.N) & "," & Image (FCD.T)));

         package FCD_Map is new Ada.Containers.Hashed_Maps
            (Key_Type        => FC_Data,
             Element_Type    => Attribute_Index,
             Hash            => Hash,
             Equivalent_Keys => "=");

         use FCD_Map;

         Map : FCD_Map.Map;
      end GL_Attributes;

      package body GL_Attributes is

         function Attributes_Are_Valid return Boolean is
         begin
            --  Vertices, Normals and TexCrds are our attributes. Each
            --  attribute must have one of each of these, so their lengths must
            --  be the same. Indices will either be the same length (if every
            --  point on the model is unique, which would be very strange) or
            --  longer, if some attributes are repeated.
            pragma Assert (Vertices.Length =  Normals.Length);
            pragma Assert (Vertices.Length =  TexCrds.Length);
            pragma Assert (Vertices.Length <= Attrib_Sequence.Length);
            return True;
         end Attributes_Are_Valid;

         procedure Add (V, N : in Vector3; T : in Vector2) is
            Tuple : constant FC_Data := (V => V, T => T, N => N);
         begin
            if Map.Find (Tuple) /= No_Element then
               --  Duplicate attribute, so it already has an attrib ID. Just
               --  append that ID.
               Attrib_Sequence.Append (Map.Element (Tuple));
            else
               --  New attribute, so it needs an ID and to have its data added.
               Vertices.Append (V);
               Normals.Append (N);
               TexCrds.Append (T);

               pragma Assert (Vertices.Last_Index = GL_Attributes.Next_ID);
               pragma Assert (Normals.Last_Index = GL_Attributes.Next_ID);
               pragma Assert (TexCrds.Last_Index = GL_Attributes.Next_ID);

               Attrib_Sequence.Append (GL_Attributes.Next_ID);

               Map.Insert (Tuple, GL_Attributes.Next_ID);

               GL_Attributes.Next_ID := GL_Attributes.Next_ID + 1;
            end if;
         end Add;

         function Get_Vertices return Vector3_Array is
            subtype Attrib_ID is Attribute_Index range
               Vertices.First_Index .. Vertices.Last_Index;
         begin
            return Ret : Vector3_Array (Attrib_ID) do
               for I in Attrib_ID loop
                  Ret (I) := Vertices (I);
               end loop;
            end return;
         end Get_Vertices;

         function Get_Normals return Vector3_Array is
            subtype Attrib_ID is Attribute_Index range
               Normals.First_Index .. Normals.Last_Index;
         begin
            return Ret : Vector3_Array (Attrib_ID) do
               for I in Attrib_ID loop
                  Ret (I) := Normals (I);
               end loop;
            end return;
         end Get_Normals;

         function Get_TexCrds return Vector2_Array is
            subtype Attrib_ID is Attribute_Index range
               TexCrds.First_Index .. TexCrds.Last_Index;
         begin
            return Ret : Vector2_Array (Attrib_ID) do
               for I in Attrib_ID loop
                  Ret (I) := TexCrds (I);
               end loop;
            end return;
         end Get_TexCrds;

         function Get_Indices return Int_Array is
            subtype Index is Attribute_Index range
               Attrib_Sequence.First_Index .. Attrib_Sequence.Last_Index;
         begin
            return Ret : Int_Array (Index) do
               for I in Index loop
                  Ret (I) := Attrib_Sequence (I);
               end loop;
            end return;
         end Get_Indices;

      end GL_Attributes;

   begin

      for I in Obj.Vertices.First_Index .. Obj.Vertices.Last_Index loop
         GL_Attributes.Add (V => Obj.Vertices (I),
                            T => Obj.TexCrds  (I),
                            N => Obj.Normals  (I));
      end loop;

      return Create_Model (Vertices  => GL_Attributes.Get_Vertices,
                           Normals   => GL_Attributes.Get_Normals,
                           TexCrds   => GL_Attributes.Get_TexCrds,
                           Indices   => GL_Attributes.Get_Indices,
                           Materials => Obj.Materials);

   end Create_Model_From_Obj;

   ----------------------------------------------------------------------------
   procedure Send_Updated_Uniforms (Object_Position : in Position_Type;
                                    Object_Scale    : in Single := 1.0;
                                    Object_Rotation : in Radians := 0.0)
   is
      Camera_Transform : constant Matrix4 := Uniforms.Camera_Transform.Get;
      Object_Transform : constant Matrix4 :=
         Translate (Rotate (Scale (Identity4, Object_Scale), 
                            Object_Rotation), 
                    Object_Position);
      CamObj_Transform : constant Matrix4 :=
         Camera_Transform * Object_Transform;

      Normal_Transform : constant Matrix3 :=
         Transpose (Inverse (Mat4_To_Mat3 (CamObj_Transform)));
   begin
      for I in Normal_Transform'Range (1) loop
         for J in Normal_Transform'Range (2) loop
            Put (" " & Normal_Transform (I, J)'Img);
         end loop;
         New_Line; --Put (" ; ");
      end loop;
      New_Line;
      New_Line;

      Uniforms.Object_Transform.Set (Object_Transform);
      Uniforms.CamObj_Transform.Set (CamObj_Transform);
      Uniforms.Normal_Transform.Set (Normal_Transform);

   end Send_Updated_Uniforms;

   ----------------------------------------------------------------------------
   procedure Render (M : in Model;
                     E : in ECS.Entity)
   is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use Cargame.Gameplay;

      CamObj : constant Matrix4 := Components.CamObj_Matrix.Get (E);
      Normal : constant Matrix3 := Components.Normal_Matrix.Get (E);
   begin

      --Send_Updated_Uniforms (Object_Position => Position,
      --                       Object_Rotation => Rotation,
      --                       Object_Scale    => Scale);

      Uniforms.CamObj_Transform.Set (CamObj);
      Uniforms.Normal_Transform.Set (Normal);

      Bind (M.Vao);
      Bind (Array_Buffer,         M.Vertex_Buffer);
      Bind (Element_Array_Buffer, M.Index_Buffer);

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

         Draw_Elements (Mode           => Triangles,
                        Index_Type     => UInt_Type,
                        Element_Offset => Integer (Mtl.First_Index),
                        Count          => Mtl.Num_Indices);

      end loop;

      Unbind_VAO;
   end Render;

   ----------------------------------------------------------------------------
   procedure Draw_A_Line (From, To : in Position_Type;
                          Vao : in Vertex_Array_Object;
                          Vbo : in Buffer) 
   is
      Data : aliased constant Vector3_Array (1 .. 2) := (From, To);
   begin
      Vao.Bind;
      Bind (Array_Buffer, Vbo);

      Load_Vector3_Buffer (Target => Array_Buffer,
                           Data   => Data,
                           Usage  => Static_Draw);

      Set_Vertex_Attrib_Pointer (Index  => Vertices_Attribute,
                                 Count  => Vector3'Length,
                                 Kind   => Single_Type,
                                 Stride => 0,
                                 Offset => 0);

      Enable_Vertex_Attrib_Array (Vertices_Attribute);

      Cargame.Uniforms.Drawing_A_Line.Set (1);

      Send_Updated_Uniforms (Object_Position => Origin);

      Draw_Arrays (Mode  => Lines,
                   First => 0,
                   Count => 2);

      Cargame.Uniforms.Drawing_A_Line.Set (0);

      Unbind_VAO;
   end Draw_A_Line;

end Cargame.Models;
