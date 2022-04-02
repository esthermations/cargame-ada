with Ada.Text_IO;           use Ada.Text_IO;

with GL;
with GL.Types;
with GL.Objects.Textures;   use GL.Objects.Textures;

with Cargame.Globals;
with Cargame.Types;
with Cargame.Vectors;       use Cargame.Vectors;
with Cargame.Renderer;

with GNATCOLL.Strings;      use GNATCOLL.Strings;

package Cargame.Engine.Obj_Parser is

   use GL;
   use GL.Types;
   use GL.Types.Singles;
   use Cargame.Types;

   Invalid_Obj_File : exception;
   Invalid_Obj_Data : exception;
   Invalid_Mtl_File : exception;

   ---------------------------------------------------------------------------
   --  Obj_Data

   type Obj_Data (Has_TexCrds : Boolean := True) is record
      Materials : Vector_Of_Material;
      Vertices  : Vector_Of_Vector3;
      Normals   : Vector_Of_Vector3;

      case Has_TexCrds is
         when True   => TexCrds : Vector_Of_Vector2;
         when others => null;
      end case;
   end record;

   ---------------------------------------------------------------------------
   function Is_Valid (Data : in Obj_Data) return Boolean;
   --  This runs the obj data through a series of sanity checks. Returns false
   --  and logs details if it finds any issues, else true.

   ---------------------------------------------------------------------------
   function Parse (File_Path : in String) return Obj_Data
       with Pre  => Renderer.Initialised,
            Post => Is_Valid (Parse'Result);

private

   ---------------------------------------------------------------------------
   --  Helper functions

   ---------------------------------------------------------------------------
   function Read_Single (Str : in XString)
      return Single
      is (Single'Value (Str.To_String))
      with Pre     => Length (Str) /= 0,
           Depends => (Read_Single'Result => Str);

   ---------------------------------------------------------------------------
   function Read_Vector2 (Split_Line : in XString_Array)
      return Valid_Vector2
      is (Vector2'(X => Read_Single (Split_Line (2)),
                   Y => Read_Single (Split_Line (3))))
      with Pre => (Split_Line'Length >= 3 and then
                   Split_Line (1).To_String in "vt");
                    --  "vt" -> texcoord. Technically that makes this
                    --  function "Read_TexCrd" but y'know.

   ---------------------------------------------------------------------------
   function Read_Vector3 (Split_Line : in XString_Array)
      return Valid_Vector3
      is (Vector3'(X => Read_Single (Split_Line (2)),
                   Y => Read_Single (Split_Line (3)),
                   Z => Read_Single (Split_Line (4))))
      with Pre => (Split_Line'Length = 4 and then
                   To_String (Split_Line (1)) in
                      "v" | "vn" | "Ks" | "Kd" | "Ka");

   ---------------------------------------------------------------------------
   procedure Next_Significant_Line (File : in File_Type; Line : out XString)
      with Pre  => Is_Open (File),
           Post => (if Line.Length = 0 then End_Of_File (File));

end Cargame.Engine.Obj_Parser;
