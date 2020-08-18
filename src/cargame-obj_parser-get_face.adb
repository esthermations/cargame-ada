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
   begin

      Str.Split (Sep => "/", 
                 Omit_Empty => False, 
                 Into => Split, 
                 Last => Unused);

      --  Subtract 1 to convert to our zero-based indexing
      FC.V := GL.Types.Size'Value (Split (1).To_String);
      FC.T := GL.Types.Size'Value (Split (2).To_String);
      FC.N := GL.Types.Size'Value (Split (3).To_String);

      --  Assert that obj file complies to spec...
      if FC.V = 0 or FC.N = 0 or FC.T = 0 then
         raise Invalid_Obj_File 
            with "Obj file gave a zero index on a face, which is out of spec.";
      end if;

      FC.V := @ - 1;
      FC.T := @ - 1;
      FC.N := @ - 1;

      return FC;

   exception

      when E : Constraint_Error =>
         Util.Log_Error ("Was unable to convert this string to a face:");
         Util.Log_Warning ("'" & Str.To_String & "'");
         Util.Log ("Note that we only support triangles with V/T/N format.");
         Util.Log ("Further exception info provided below:");
         Util.Log (Ada.Exceptions.Exception_Message (E));
         Util.Log_Error ("Crashing.");
         raise;

   end To_Face_Component;

   ----------------------------------------------------------------------------
   --  Variables

   Ret : Face := (others => Face_Component'(V => Unset_Index,
                                            T => Unset_Index,
                                            N => Unset_Index));

begin

   if Split_Line'Length /= 4 then
      raise Invalid_Obj_File with
         "Obj file has faces that aren't triangles. We can't handle those yet.";
   end if;

   for I in (Split_Line'First + 1) .. Split_Line'Last loop
      Ret (I - 1) := To_Face_Component (Split_Line (I));
   end loop;

   return Ret;

end Get_Face;
