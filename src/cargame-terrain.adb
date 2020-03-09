with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Containers;      use Ada.Containers;

with GL;                  use GL;
with GL.Types;            use GL.Types; use GL.Types.Singles;

with Cargame.Renderables; use Cargame.Renderables;
with Cargame.Vectors;     use Cargame.Vectors;
with Cargame.Types;       use Cargame.Types;
with Cargame.Util;

package body Cargame.Terrain is

   ---------------------------------------------------------------------------
   function Generate_Terrain_Segment return Entity.Entity is
      Vertices : Vector_Of_Vector3;
      Normals  : Vector_Of_Vector3;

      N : constant GL.Types.Size := 32;

      --  FIXME: What is this? I believe it's the number of vertices there
      --  are on the horizontal and depth axis of the terrain. In the C++
      --  implementation it was parameterised, but I passed 64.

   begin
      for I in Size (1) .. N loop
         for J in Size (1) .. N loop
            declare
               FI : constant Single := Single (I);
               FJ : constant Single := Single (J);
               FN : constant Single := Single (N);
            begin
               Vertices.Append (((FI + 0.0) / FN, 0.0, (FJ + 0.0) / FN));
               Vertices.Append (((FI + 0.0) / FN, 0.0, (FJ + 1.0) / FN));
               Vertices.Append (((FI + 1.0) / FN, 0.0, (FJ + 1.0) / FN));
               Vertices.Append (((FI + 1.0) / FN, 0.0, (FJ + 1.0) / FN));
               Vertices.Append (((FI + 1.0) / FN, 0.0, (FJ + 1.0) / FN));
               Vertices.Append (((FI + 0.0) / FN, 0.0, (FJ + 0.0) / FN));
            end;
         end loop;
      end loop;

      for I in Vertices.First_Index .. Vertices.Last_Index loop
         Normals.Append ((X => 0.0, Y => 1.0, Z => 0.0));
      end loop;

      pragma Assert (Normals.Length = Vertices.Length,
                    "Must have as many normals as vertices."
                     & " Have "
                     & Count_Type'Image (Normals.Length)
                     & " normals, and "
                     & Count_Type'Image (Vertices.Length)
                     & " vertices.");

      pragma Assert ((for all N of Normals => N = (0.0, 1.0, 0.0)),
                    "Not all Normals are (0, 1, 0)");

      declare
         subtype Vertices_Vector_Index is
            Size range Vertices.First_Index .. Vertices.Last_Index;

         Out_Indices   : Int_Array     (Vertices_Vector_Index);
         Out_Vertices  : Vector3_Array (Vertices_Vector_Index);
         Out_Normals   : Vector3_Array (Vertices_Vector_Index);
         Out_TexCrds   : Vector2_Array (Vertices_Vector_Index);
         Out_Materials : Vector_Of_Material; --  Empty
      begin

         Util.Got_Here ("Setting Out_Indices ("
                       & Integer'Image (Out_Indices'Length) & " )");

         for I in Out_Indices'Range loop
            Out_Indices (I) := I;
         end loop;

         Util.Got_Here ("Setting Out_Vertices ("
                        & Integer'Image (Out_Vertices'Length) & " )");

         for I in Out_Vertices'Range loop
            Out_Vertices (I) := Vertices (I);
         end loop;

         Util.Got_Here ("Setting Out_Normals ("
                        & Integer'Image (Out_Normals'Length) & " )");

         for I in Out_Normals'Range loop
            Out_Normals (I) := (0.0, 1.0, 0.0);
         end loop;

         Util.Got_Here ("Setting Out_TexCrds ("
                        & Integer'Image (Out_TexCrds'Length) & " )");

         for I in Out_TexCrds'Range loop
            Out_TexCrds (I) := (0.0, 0.0);
         end loop;

         return Entity.Entity'(RVAO => Create_Renderable_VAO
                                 (Vertices  => Out_Vertices,
                                  Normals   => Out_Normals,
                                  TexCrds   => Out_TexCrds,
                                  Indices   => Out_Indices,
                                  Materials => Out_Materials),
                               Is_Live => True,
                               others  => <>);
      end;
   end Generate_Terrain_Segment;
   ---------------------------------------------------------------------------

end Cargame.Terrain;
