with Ada.Containers; use Ada.Containers;

package Cargame.Obj_Parser.Mtl_Parser is

   function Parse_Mtl (Mtl_File_Path : in String) return Vector_Of_Material
      with Post => Parse_Mtl'Result.Length /= 0;

end Cargame.Obj_Parser.Mtl_Parser;
