with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Text_IO;             use Ada.Text_IO;

with GL;
with GL.Objects.Textures;

with Cargame.Engine.Obj_Parser;
with Cargame.Util; use Cargame.Util;

package body Cargame.Engine.Models is

   ----------------------------------------------------------------------------
   function Is_Renderable (M : Model) return Boolean is
      Has_Valid_VAO : constant Boolean := (M.Vao /= Null_Array_Object);
      Has_Materials : constant Boolean := (M.Materials.Length /= 0);
   begin
      pragma Assert (Has_Valid_VAO);
      pragma Assert (Has_Materials);
      return True;
   end Is_Renderable;

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

      Set_Vertex_Attrib_Pointer (Index      => Vertices_Attribute,
                                 Count      => Vector3'Length,
                                 Kind       => Single_Type,
                                 Normalized => False,
                                 Stride     => 0,
                                 Offset     => 0);

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

      Set_Vertex_Attrib_Pointer (Index      => Normals_Attribute,
                                 Count      => Vector3'Length,
                                 Kind       => Single_Type,
                                 Normalized => False,
                                 Stride     => 0,
                                 Offset     => 0);

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

      Set_Vertex_Attrib_Pointer (Index      => TexCrds_Attribute,
                                 Count      => Vector2'Length,
                                 Kind       => Single_Type,
                                 Normalized => False,
                                 Stride     => 0,
                                 Offset     => 0);
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

      return Model'(Vao           => Vao,
                    Vertex_Buffer => Vertex_Buf,
                    Normal_Buffer => Normal_Buf,
                    Materials     => Out_Materials);
   end Create_Model;

   ----------------------------------------------------------------------------
   function Create_Model_From_Obj (Obj_File_Path : in String)
      return Model
   is

      --  This function's main job is to deduplicate the data received from the
      --  obj parser to suit the layout required by glDrawElements. Each unique
      --  combination of GL attributes is given an index. An attribute is
      --  something like a vertex, normal, texcoord, etc. In this case, we're
      --  only dealing with those three.

      use Cargame.Engine.Obj_Parser;
      Obj : constant Obj_Data := Parse (Obj_File_Path);

      subtype Attribute_Index is GL.Types.Size;

      package GL_Attributes is

         --  Inputs:
         procedure Add (V, N : in Vector3; T : in Vector2);

         --  Outputs:

         function Get_Vertices
            return Vector3_Array
            with Pre => Attributes_Are_Valid;

         function Get_Normals
            return Vector3_Array
            with Pre => Attributes_Are_Valid;

         function Get_TexCrds
            return Vector2_Array
            with Pre => Attributes_Are_Valid;

         function Get_Indices
            return Int_Array
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

         type FC_Data is record
            V, N : Vector3;
            T    : Vector2;
         end record;

         overriding
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
            Expected_Length : constant Count_Type := Vertices.Length;
         begin
            --  Vertices, Normals and TexCrds are our attributes. Each
            --  attribute must have one of each of these, so their lengths must
            --  be the same. Indices will either be the same length (if every
            --  point on the model is unique, which would be very strange) or
            --  longer, if some attributes are repeated.
            return Normals.Length   = Expected_Length and
                   TexCrds.Length   = Expected_Length and
                   Expected_Length <= Attrib_Sequence.Length;

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

end Cargame.Engine.Models;
