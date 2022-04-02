with Ada.Directories;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Exceptions;

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

   Util.Log ("Opening " & Mtl_File_Path);

   Open (
      File => Mtl_File,
      Mode => In_File,
      Name => Mtl_File_Path
   );

   Util.Got_Here;

   Loop_Over_Mtl_Lines :
   while not End_Of_File (Mtl_File) loop

      Next_Significant_Line (Mtl_File, Line);

      Util.Log (To_String (Line));

      exit Loop_Over_Mtl_Lines when Length (Line) = 0;

      Line.Split (
         Sep        => " ",
         Omit_Empty => True,
         Into       => Split_Line,
         Last       => Split_Last
      );

      Parse_Mtl_Token :
      declare

         type Mtl_Token is (Newmtl, Map_Kd, Map_Bump, Map_Ks, Illum, Ni, Ns, Ks, Kd, Ka, Ke, D);

         pragma Assert (Split_Last >= 1);

         Token : constant Mtl_Token :=
            Mtl_Token'Value (To_String (Split_Line (1)));

      begin

         case Token is
            when Ka => Current_Material.Ambient_Light  := Read_Vector3 (Split_Line (1 .. Split_Last));
            when Kd => Current_Material.Diffuse_Light  := Read_Vector3 (Split_Line (1 .. Split_Last));
            when Ks => Current_Material.Specular_Light := Read_Vector3 (Split_Line (1 .. Split_Last));
            when Ns => Current_Material.Shininess      := Read_Single (Split_Line (2));

            when Map_Kd =>
               --  Diffuse texture. Next token should be path to an image file.

               if Split_Last > 2 then
                  Util.Log_Error ("This texture filename has a space in it and I can't handle those because I am feeble, and weak");
               elsif not Ada.Directories.Exists (To_String (Split_Line (2))) then
                  Util.Log_Warning ("Couldn't find Material texture file " & To_String (Split_Line (2)) & ", skipping.");
               else
                  Initialize_Id (Current_Material.Diffuse_Texture);
                  pragma Assert (Current_Material.Diffuse_Texture.Initialized);

                  Current_Material.Diffuse_Texture := Load_Texture (To_String (Split_Line (2)));

                  Initialize_Id (Current_Material.Specular_Texture);
                  pragma Assert (Current_Material.Specular_Texture.Initialized);
               end if;

            when Newmtl =>
               --  Material name. Next token should be a string without spaces

               if not First_Material then
                  Output_Materials.Append (Current_Material);
               end if;

               First_Material := False;
               Current_Material := (
                  Name   => To_Material_Name (To_String (Split_Line (2))),
                  others => <>
               );

            when Ni    => null; -- TODO: Index of refraction
            when Illum => null; -- TODO: Illumination model
            when Ke    => null; -- TODO: ???
            when D     => null; -- TODO: Dissolve
            when Map_Bump => null; -- TODO: Bumpmap
            when Map_Ks   => null; -- TODO: Specular texture
         end case;

      end Parse_Mtl_Token;

   end loop Loop_Over_Mtl_Lines;

   --  We append materials when we encounter the next one, which means the last
   --  material won't be appended. Do that here.

   declare
      Have_Viable_Material : Boolean :=
         (for some M of Output_Materials => M.Diffuse_Texture.Initialized);
   begin
      if not Have_Viable_Material then
         Util.Log_Warning ("Material file produced no usable materials");
      else
         Output_Materials.Append (Current_Material);
      end if;
   end;

   Close (Mtl_File);

   return Output_Materials;

exception

   when E : others =>
      Close (Mtl_File); -- Program is going to crash anyway but be nice
      Util.Log_Error (
         "Exception raised while parsing MTL: "
         & Ada.Exceptions.Exception_Message (E)
      );
      raise Invalid_Mtl_File with Ada.Exceptions.Exception_Message (E);

end Parse_Mtl;
