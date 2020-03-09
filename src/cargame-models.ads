with GL.Attributes;
with GL.Types;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Buffers;

with Cargame.Types;
with Cargame.Vectors;
with Cargame.ECS;

package Cargame.Models is

   use GL.Attributes;
   use GL.Types;
   use GL.Types.Singles;
   use GL.Objects.Vertex_Arrays;
   use GL.Objects.Buffers;
   use Cargame.Types;
   use Cargame.Vectors;

   Vertices_Attribute : constant Attribute := 0;
   Normals_Attribute  : constant Attribute := 1;
   TexCrds_Attribute  : constant Attribute := 2;

   procedure Send_Updated_Uniforms (Object_Position : in Position_Type;
                                    Object_Scale    : in Single := 1.0;
                                    Object_Rotation : in Radians := 0.0);

   type Model is tagged record
      Materials     : Vector_Of_Material;
      Vao           : Vertex_Array_Object := Null_Array_Object;
      Dimensions    : Volume3D_Type       := (others => 0.0);
      Vertex_Buffer : Buffer;
      Normal_Buffer : Buffer;
      TexCrd_Buffer : Buffer;
      Index_Buffer  : Buffer;
      Num_Indices   : Int := 0;
   end record;

   procedure Render (M : in Model;
                     E : in ECS.Entity);

   function "=" (L, R : in Model) return Boolean is
        ((L.Vao           = R.Vao)           and then
         --  (L.Materials     = R.Materials)     and then
         (L.Vertex_Buffer = R.Vertex_Buffer) and then
         (L.Normal_Buffer = R.Normal_Buffer) and then
         (L.TexCrd_Buffer = R.TexCrd_Buffer) and then
         (L.Index_Buffer  = R.Index_Buffer)  and then
         (L.Num_Indices   = R.Num_Indices));

   function Is_Renderable (M : in Model) return Boolean;

   function All_Members_Are_Valid (Arr : in Vector3_Array) return Boolean;
   function All_Members_Are_Valid (Arr : in Vector2_Array) return Boolean;

   function Create_Model (Vertices, Normals : in Vector3_Array;
                          TexCrds           : in Vector2_Array;
                          Indices           : in Int_Array;
                          Materials         : in Vector_Of_Material)
      return Model

      --  TODO: Add sanity checks for Normals and TexCrds.
      with Pre => (Vertices'Length > 0
                      and then Indices'Length > 0
                      and then Vertices'Length = Normals'Length
                      and then Vertices'Length = TexCrds'Length
                      and then Vertices'Length <= Indices'Length
                      and then All_Members_Are_Valid (Vertices)
                      and then All_Members_Are_Valid (Normals)
                      and then All_Members_Are_Valid (TexCrds)
                      and then (for all Mtl of Materials =>
                                   Mtl.First_Index in Indices'Range)
                      and then (for all Mtl of Materials =>
                                   Mtl.Final_Index in Indices'Range)),
           Post => Is_Renderable (Create_Model'Result);

   function Create_Model_From_Obj (Obj_File_Path : in String) return Model
      with Post => Is_Renderable (Create_Model_From_Obj'Result);


   procedure Draw_A_Line (From, To : in Position_Type;
                          Vao : in Vertex_Array_Object;
                          Vbo : in Buffer) 
      with Pre => Vao.Initialized and Vbo.Initialized;

end Cargame.Models;
