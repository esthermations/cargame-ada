with Ada.Containers;                use Ada.Containers;
with Ada.Directories;               use Ada.Directories;
with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

with GL;                            use GL;
with Cargame.Obj_Parser.Mtl_Parser; use Cargame.Obj_Parser.Mtl_Parser;

with Cargame.Util;                  use Cargame.Util;

package body Cargame.Obj_Parser is

   ---------------------------------------------------------------------------
   --  Declarations

   type Face is array (Integer range 1 .. 3) of Face_Component;
   --  I'm assuming here that faces will be specified as triangles, i.e. '1/2/3'
   --  or '1//3' or something. This isn't actually required by the obj format.

   ---------------------------------------------------------------------------
   function Get_Face (Split_Line : in XString_Array) return Face
      is separate
      with Pre        => Split_Line (1) = "f",
           Global     => null,
           Depends    => (Get_Face'Result => Split_Line);

   ---------------------------------------------------------------------------
   --  Definitions

   ---------------------------------------------------------------------------
   procedure Next_Significant_Line (File : in File_Type; Line : out XString)
   is
      Line_Is_Significant : Boolean;
      use Ada.Characters.Handling;
   begin

      while not End_Of_File (File) loop

         Line.Set (Ada.Text_IO.Get_Line (File));

         for I in Line loop
            if Line (I) in Ascii.CR | Ascii.LF then
               Line (I) := ' ';
            end if;
         end loop;

         Line_Is_Significant := 
            Line.Length > 0 and
            Line (1) /= '#' and
            not (for all C of Line => Is_Space (C) or Is_Control (C));

         if Line_Is_Significant then
            return;
         end if;

      end loop;

      Line.Set ("");
      --  It's intended (and part of the postcondition) that this function
      --  should return an empty string iff End_Of_File (File) is true.
   end Next_Significant_Line;

   -------------
   --  Parse  --
   -------------

   function Parse (File_Path : in String) return Obj_Data is

      Ret : Obj_Data; --  Return value

      --  Unique vertices as specified in the obj file, irrespective of faces.
      --  When we handle the faces, we just copy the values given by the indices
      --  in the face from this array into the output data.
      Unique_Vertices : Vector_Of_Vector3;
      Unique_Normals  : Vector_Of_Vector3;
      Unique_TexCrds  : Vector_Of_Vector2;

      type Obj_Token is (V, VN, VT, MtlLib, UseMtl, S, G, F, O);

      use Material_Names;

      Line                  : XString;
      Split_Line            : XString_Array (1 .. 10);
      Split_Last            : Natural;
      Obj_File              : File_Type;
      Current_Material_Name : Material_Name;
      Original_Directory    : constant String := Current_Directory;

      Log_Task : Util.Log_Task;

   begin
      Set_Directory (Containing_Directory (File_Path));

      Open (File => Obj_File,
            Mode => In_File,
            Name => Simple_Name (File_Path));

      Log_Task.Start ("Parsing obj file: " & File_Path);

      Loop_Over_Obj_Lines :
      loop

         Next_Significant_Line (File => Obj_File, Line => Line);

         exit Loop_Over_Obj_Lines when End_Of_File (Obj_File);

         Line.Split (Sep => " ", 
                     Omit_Empty => True, 
                     Into => Split_Line, 
                     Last => Split_Last);

         pragma Assert (Split_Line'Length >= 1);

         case Obj_Token'Value (To_String (Split_Line (1))) is

            when VN => Unique_Normals.Append (Get_Vector3 (Split_Line (Split_Line'First .. Split_Last)));
            when VT => Unique_TexCrds.Append (Get_Vector2 (Split_Line (Split_Line'First .. Split_Last)));
            when V => Unique_Vertices.Append (Get_Vector3 (Split_Line (Split_Line'First .. Split_Last)));
            when F =>
               declare

                  UV : Vector_Of_Vector3 renames Unique_Vertices;
                  UN : Vector_Of_Vector3 renames Unique_Normals;
                  UT : Vector_Of_Vector2 renames Unique_TexCrds;

                  subtype UV_Range is GL.Types.Size range UV.First_Index .. UV.Last_Index;
                  subtype UN_Range is GL.Types.Size range UN.First_Index .. UN.Last_Index;
                  subtype UT_Range is GL.Types.Size range UT.First_Index .. UT.Last_Index;

                  F : constant Face := Get_Face (Split_Line (Split_Line'First .. Split_Last));
               begin
                  for FC of F loop
                     if (FC.V not in UV_Range) then
                        Util.Log_Error ("Failing on FC.V = " & Int'Image (FC.V));
                        raise Invalid_Obj_Data;
                     end if;
                     if (FC.N not in UN_Range) then
                        Util.Log_Error ("Failing on FC.N = " & Int'Image (FC.N));
                        raise Invalid_Obj_Data;
                     end if;
                     if (FC.T not in UT_Range) then
                        Util.Log_Error ("Failing on FC.T = " & Int'Image (FC.T));
                        raise Invalid_Obj_Data;
                     end if;

                     Ret.Vertices.Append (Unique_Vertices (FC.V));
                     Ret.Normals.Append  (Unique_Normals  (FC.N));
                     Ret.TexCrds.Append  (Unique_TexCrds  (FC.T));
                  end loop;
               end;

            when MtlLib =>

               Ret.Materials := Parse_Mtl (To_String (Split_Line (2)));

            when UseMtl =>

               Handle_UseMtl :
               declare
                  UseMtl_Name : constant Material_Name := 
                     To_Material_Name (To_String (Split_Line (2)));
               begin

                  --  Finish with the current material
                  for M of Ret.Materials loop
                     if Names_Match (M.Name, Current_Material_Name) then
                        M.Num_Indices :=
                           (Ret.Vertices.Last_Index - M.First_Index);
                        Util.Log
                           ("Finished with " & To_String (UseMtl_Name)
                               & ": " & Standard.Ascii.HT --  Horizontal tab
                               & " indices " & Int'Image (M.First_Index)
                               & " .. "
                               & Int'Image (M.First_Index + M.Num_Indices));
                     end if;
                  end loop;

                  --  Set Current_Material_Name to the next material
                  for M of Ret.Materials loop
                     if Names_Match (M.Name, UseMtl_Name) then
                        M.First_Index := (if Ret.Vertices.Length > 0
                                          then Ret.Vertices.Last_Index + 1
                                          else Ret.Vertices.First_Index);
                        Current_Material_Name := M.Name;
                     end if;
                  end loop;
               end Handle_UseMtl;

            when G => null;
            when O => null; --  TODO (wtok, 2018-03-29)
            when S => null; --  TODO (wtok, 2018-03-29)
         end case;
      end loop Loop_Over_Obj_Lines;

      Close (Obj_File);

      Set_Directory (Original_Directory);

      --  We set Num_Indices when we encounter the next material, so that won't
      --  happen for the last material. Do that here.
      pragma Assert
         ((for some M of Ret.Materials => M.Num_Indices = Unset_Num_Indices),
           "All materials have Num_Indices set. That's not supposed to happen.");

      for M of Ret.Materials loop
         if M.Num_Indices = Unset_Num_Indices then
            M.Num_Indices := (Ret.Vertices.Last_Index - M.First_Index);
            Util.Log ("Set Num_Indices for last material to "
                         & Int'Image (M.Num_Indices));
         end if;
      end loop;

      Log_Task.Complete;

      return Ret;

   exception

      when E : others =>
         Util.Log_Error ("Failed to parse obj file: " & File_Path);
         Util.Log_Warning (Ada.Exceptions.Exception_Message (E));
         raise;

   end Parse;

   ---------------------------------------------------------------------------
   function Is_Valid (Data : in Obj_Data) return Boolean is

      Num_Vertices : constant Count_Type := Data.Vertices.Length;
      Num_Normals  : constant Count_Type := Data.Normals.Length;
      Num_TexCrds  : constant Count_Type := Data.TexCrds.Length;

      subtype Valid_Index_Range is GL.Types.Size
         range Data.Vertices.First_Index .. Data.Vertices.Last_Index;

      Valid : Boolean := True;

   begin

      --  Ensure that every vertex has an associated normal and texcoord

      if Num_Normals /= Num_Vertices then
         Util.Log_Error
            ("For some reason there aren't as many normals as vertices.");
         Valid := False;
      end if;

      if Num_TexCrds /= Num_Vertices then
         Util.Log_Error
            ("For some reason there aren't as many texcrds as vertices.");
         Valid := False;
      end if;

      --  Ensure that our various *_Indices vectors are indexed alike

      if (Data.Normals.First_Index /= Data.Vertices.First_Index) then
         Util.Log_Error
            ("Vertices and normals aren't indexed alike");
         Valid := False;
      end if;

      if (Data.TexCrds.First_Index /= Data.Vertices.First_Index) then
         Util.Log_Error
            ("Vertices and texcrds aren't indexed alike");
         Valid := False;
      end if;

      --  Ensure we didn't get any invalid floats (NaN, etc)

      if (for some V of Data.Vertices => not Is_Valid (V)) then
         Util.Log_Error ("Found an invalid vertex");
         Valid := False;
      end if;

      if (for some V of Data.Normals  => not Is_Valid (V)) then
         Util.Log_Error ("Found an invalid normal");
         Valid := False;
      end if;

      if (for some V of Data.TexCrds  => not Is_Valid (V)) then
         Util.Log_Error ("Found an invalid texcrd");
         Valid := False;
      end if;

      --  Ensure our materials are actually associated with our vertices

      for M of Data.Materials loop
         if not (M.First_Index in Valid_Index_Range) then
            Util.Log_Error
               (M.Printable_Name
                   & "'s First_Index ("
                   & Int'Image (M.First_Index)
                   & ") isn't a valid index of Vertices");
            Valid := False;
         end if;

         if not (M.Final_Index in Valid_Index_Range) then
            Util.Log_Error
               (M.Printable_Name
                   & "'s final index ("
                   & Int'Image (M.Final_Index)
                   & ") isn't a valid index of Vertices");
            Valid := False;
         end if;
      end loop;

      return Valid;
   end Is_Valid;

end Cargame.Obj_Parser;
