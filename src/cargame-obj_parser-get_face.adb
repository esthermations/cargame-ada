separate (Cargame.Obj_Parser)
function Get_Face (Split_Line : in XString_Array) return Face is

   ----------------------------------------------------------------------------
   --  Declarations

   function To_Face_Component (Str : in XString) return Face_Component
      with Pre => (Str.Length /= 0);
   --  The given string would be something like "1234/5678/9012". This function
   --  gives us Face_Component'(V => 1234, T => 5678, N => 9012).

   ----------------------------------------------------------------------------
   --  Definitions

   function To_Face_Component (Str : XString) return Face_Component is
      FC      : Face_Component;
      Split   : XString_Array (1 .. 3);
      Unused  : Natural;

      TexCrd_Is_Used : Boolean := True;
   begin

      --  Input: Something like '1/2/3' or '1//3'
      Str.Split (Sep => "/",
                 Omit_Empty => False,
                 Into => Split,
                 Last => Unused);

      TexCrd_Is_Used := not Split (2).Is_Empty;

      if not TexCrd_Is_Used then
         FC.T := Unset_Index;
      end if;

      FC.V := GL.Types.Size'Value (Split (1).To_String);
      FC.N := GL.Types.Size'Value (Split (3).To_String);

      if TexCrd_Is_Used then
         FC.T := GL.Types.Size'Value (Split (2).To_String);
      end if;

      --  Confirm that the obj file complies to spec...
      if 0 in FC.V | FC.N or (TexCrd_Is_Used and 0 in FC.T) then
         raise Invalid_Obj_File
            with "Obj file gave a zero index on a face, which is out of spec.";
      end if;

      --  Subtract 1 to convert to our zero-based indexing
      FC.V := @ - 1;
      FC.N := @ - 1;

      if TexCrd_Is_Used then
         FC.T := @ - 1;
      end if;

      return FC;

   exception

      when E : Constraint_Error =>
         Util.Log_Error ("Was unable to convert this string to a face:");
         Util.Log_Warning ("'" & Str.To_String & "'");
         Util.Log ("We only support triangles with V/T/N or V//N formats.");
         Util.Log ("Further exception info provided below:");
         Util.Log (Ada.Exceptions.Exception_Message (E));
         raise;

   end To_Face_Component;

   ----------------------------------------------------------------------------
   --  Variables

   Ret : Face (1 .. (Split_Line'Length - 1)) :=
      (others => Face_Component'(others => Unset_Index));

begin

   for I in Split_Line'First + 1 .. Split_Line'Last loop
      Ret (I - 1) := To_Face_Component (Split_Line (I));
   end loop;

   return Ret;

end Get_Face;
