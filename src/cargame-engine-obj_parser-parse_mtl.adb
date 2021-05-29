with Ada.Directories;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNATCOLL.Strings;              use GNATCOLL.Strings;

with Cargame.Engine.Obj_Parser;
with Cargame.Engine.Texture_Loader; use Cargame.Engine.Texture_Loader;
with Cargame.Util;

--  Input example:
--
--   newmtl p206-tp.bmp_p206-tp.j_p206-tp.jpg
--   Ns 90.196078
--   Ka 0.000000 0.000000 0.000000
--   Kd 0.409600 0.409600 0.409600
--   Ks 0.125000 0.125000 0.125000
--   Ni 1.000000
--   d 1.000000
--   illum 2
--   map_Kd p206-tp.jpg

separate (Cargame.Engine.Obj_Parser)
function Parse_Mtl (Mtl_File_Path : in String)
   return Vector_Of_Material
is
   Mtl_File         : File_Type;
   Line             : XString;
   Split_Line       : XString_Array (1 .. 10);
   Split_Last       : Natural;

   Output_Materials : Vector_Of_Material;
   Current_Material : Material;
   First_Material   : Boolean := True;
begin

   Open (File => Mtl_File,
         Mode => In_File,
         Name => Mtl_File_Path);

   Loop_Over_Mtl_Lines :
   while not End_Of_File (Mtl_File) loop

      Next_Significant_Line (Mtl_File, Line);

      exit Loop_Over_Mtl_Lines when Length (Line) = 0;

      Line.Split (Sep => " ",
                  Omit_Empty => True,
                  Into => Split_Line,
                  Last => Split_Last);

      Parse_Mtl_Token :
      declare

         type Mtl_Token is
            (Newmtl, Map_Kd, Illum, Ni, Ns, Ks, Kd, Ka, Ke, D);

         Token : constant Mtl_Token :=
            Mtl_Token'Value (To_String (Split_Line (1)));

      begin

         case Token is
            when Ka =>
               Current_Material.Ambient_Light  := Get_Vector3 (Split_Line);
            when Kd =>
               Current_Material.Diffuse_Light  := Get_Vector3 (Split_Line);
            when Ks =>
               Current_Material.Specular_Light := Get_Vector3 (Split_Line);
            when Ns =>
               Current_Material.Shininess := Get_Single (Split_Line (2));

            when Map_Kd =>
               --  Diffuse texture. Next token should be path to an image
               --  file.

               pragma Assert
                  (Ada.Directories.Exists (To_String (Split_Line (2))),
                   "Mtl file specified a texture file that I can't find.");

               Initialize_Id (Current_Material.Diffuse_Texture);
               pragma Assert (Current_Material.Diffuse_Texture.Initialized);

               Current_Material.Diffuse_Texture :=
                  Load_Texture (To_String (Split_Line (2)));

               Initialize_Id (Current_Material.Specular_Texture);
               pragma Assert
                  (Current_Material.Specular_Texture.Initialized);

            when Newmtl =>
               --  Material name. Next token should be a string
               --  without spaces

               if not First_Material then
                  Output_Materials.Append (Current_Material);
               end if;

               First_Material := False;
               Current_Material :=
                  (Name   => To_Material_Name (To_String (Split_Line (2))),
                   others => <>);

            when Ni    => null; -- TODO: Index of refraction
            when Illum => null; -- TODO: Illumination model
            when Ke    => null; -- TODO: ???
            when D     => null; -- TODO: Dissolve
         end case;

      end Parse_Mtl_Token;

   end loop Loop_Over_Mtl_Lines;

   --  We append materials when we encounter the next one, which
   --  means the last material won't be appended. Do that here.
   Output_Materials.Append (Current_Material);

   for M of Output_Materials loop
      if not M.Diffuse_Texture.Initialized then
         Util.Log_Warning (M.Printable_Name & " has no texture.");
      end if;
   end loop;

   Close (Mtl_File);

   return Output_Materials;
end Parse_Mtl;
