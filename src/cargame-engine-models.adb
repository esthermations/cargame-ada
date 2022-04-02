with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Text_IO;             use Ada.Text_IO;

with GL;
with GL.Objects.Textures;

with Cargame.Engine.Obj_Parser;
with Cargame.Util; use Cargame.Util;

package body Cargame.Engine.Models is

   ----------------------------------------------------------------------------
   function Is_Renderable (M : Model)
      return Boolean
   is
      Has_Valid_VAO : constant Boolean := (M.Vao /= Null_Array_Object);
      Has_Materials : constant Boolean := (M.Materials.Length /= 0);
   begin
      pragma Assert (Has_Valid_VAO);
      pragma Assert (Has_Materials);
      return True;
   end Is_Renderable;

   function Create_Model
      (Data      : in Vertex_Data;
       Indices   : in Int_Array;
       Materials : in Vector_Of_Material)
      return Model
   is
      Vao : Vertex_Array_Object;
      Buf : Buffer;
      Index_Buf : Buffer;
   begin
      Vao.Initialize_Id;
      pragma Assert (Vao.Initialized);
      Vao.Bind;

      Buf.Initialize_Id;
      pragma Assert (Buf.Initialized);

      Bind (Array_Buffer, Buf);
      Load_Single_Buffer
         (Target => Array_Buffer,
          Data   => Data.Data,
          Usage  => Static_Draw);

      case Data.Layout is
         when Position3_Normal3_TexCrd2 =>
      Set_Vertex_Attrib_Pointer
         (Index      => Vertices_Attribute,
          Count      => Vector3'Length,
          Kind       => Single_Type,
          Normalized => False,
          Stride     => 0,
          Offset     => 0);

      Set_Vertex_Attrib_Pointer
         (Index      => Vertices_Attribute,
          Count      => Vector3'Length,
          Kind       => Single_Type,
          Normalized => False,
          Stride     => 0,
          Offset     => 0);



      Enable_Vertex_Attrib_Array (Vertices_Attribute);



   end Create_Model;

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
      Ret  : Model;
      Data : Vertex_Data := Cargame.Engine.Obj_Parser.Parse (Obj_File_Path);
   begin
      Cargame.Engine.Models.Create
   end Create_Model_From_Obj;


end Cargame.Engine.Models;
