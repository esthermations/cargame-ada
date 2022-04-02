package Cargame.Engine with Pure is

   ---------------------------------------------------------------------------
   --  Vertex_Data

   type Vertex_Data_Layout is (
      Undefined,
      Position3_Normal3_TexCrd2,
      Position3_Normal3
   );

   Layout_Stride : constant array (Vertex_Data_Layout) of Natural := (
      Undefined                 => 0,
      Position3_Normal3_TexCrd2 => 8,
      Position3_Normal3         => 6
   );

   type Vertex_Data is record
      Data      : Vector_Of_Single;
      Layout    : Vertex_Data_Layout;
      Materials : Vector_Of_Materials;
   end record;

   function Is_Sane (VD : Vertex_Data)
      return Boolean
      is ((VD.Data.Length mod Layout_Stride (VD.Layout) = 0)
          and (for all F of VD.Data => Single'Valid (F)));

   function Num_Elements (VD : in Vertex_Data)
      return Size
      is (VD.Data.Length / Layout_Stride (VD.Layout))
      with Pre => Is_Sane (VD);

   procedure Deduplicate
      (Input   : in  Vertex_Data;
       Output  : out Vertex_Data;
       Indices : out Vector_Of_Size)
   --  Find duplicated vertices (elements) of the input data and produce an
   --  output with no repeated vertices, and a list of indices that allows you
   --  to reconstruct the original layout.
      with Post => Indices.Length mod Layout_Stride (Input.Layout) = 0
                   and Output.Data.Length <= Input.Data.Length;

end Cargame.Engine;