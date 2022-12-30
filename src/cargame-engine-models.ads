with GL.Attributes;
with GL.Types;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Buffers;
with Cargame.Types;
with Cargame.Vectors;

use GL.Attributes;
use GL.Types;
use GL.Types.Singles;
use GL.Objects.Vertex_Arrays;
use GL.Objects.Buffers;
use Cargame.Types;
use Cargame.Vectors;

package Cargame.Engine.Models is

   package Attributes is
      Vertices : constant Attribute := 0;
      Normals  : constant Attribute := 1;
      TexCrds  : constant Attribute := 2;
   end Attributes;

   type Model is tagged record
      Materials     : Vector_Of_Material;
      Vao           : Vertex_Array_Object := Null_Array_Object;
      Vertex_Buffer : GL.Objects.Buffers.Buffer;
      Normal_Buffer : GL.Objects.Buffers.Buffer;
      Num_Vertices  : GL.Types.Size;
   end record;

   overriding
   function "=" (L, R : in Model) return Boolean is (L.Vao = R.Vao);

   function Is_Renderable (M : in Model) return Boolean;

   function Create_Model (Vertices, Normals : in Vector3_Array;
                          TexCrds           : in Vector2_Array;
                          Indices           : in Int_Array;
                          Materials         : in Vector_Of_Material)
      return Model
      with Pre => (Vertices'Length > 0 and then
                   Indices'Length > 0 and then
                   Vertices'Length = Normals'Length and then
                   Vertices'Length = TexCrds'Length and then
                   Vertices'Length <= Indices'Length and then
                   All_Members_Are_Valid (Vertices) and then
                   All_Members_Are_Valid (Normals) and then
                   All_Members_Are_Valid (TexCrds) and then
                   (for all Mtl of Materials =>
                       Mtl.First_Index in Indices'Range) and then
                   (for all Mtl of Materials =>
                       Mtl.Final_Index in Indices'Range)),
           Post => Is_Renderable (Create_Model'Result);

   function Create_Model_From_Obj (Obj_File_Path : in String)
      return Model
      with Post => Is_Renderable (Create_Model_From_Obj'Result);

end Cargame.Engine.Models;
