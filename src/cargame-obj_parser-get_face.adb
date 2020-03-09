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
      Split   : XString_Array (1 .. 3); -- Triangles only.
      Unused  : Natural;
   begin

      Str.Split (Sep => "/", 
                 Omit_Empty => False, 
                 Into => Split, 
                 Last => Unused);

      FC.V := GL.Types.Size'Value (To_String (Split (1)));
      FC.T := GL.Types.Size'Value (To_String (Split (2)));
      FC.N := GL.Types.Size'Value (To_String (Split (3)));

      pragma Assert (FC.V /= 0 and FC.N /= 0 and FC.T /= 0,
                     "A face line gave a 0 index."
                        & " Obj indices start at 1. What does 0 mean?");

      --  Obj indices start at 1, but our indices start at 0.
      --  XXX: If there's gonna be an off-by-one error, this will cause it.

      FC.V := FC.V - 1;
      FC.N := FC.N - 1;
      FC.T := FC.T - 1;

      return FC;

   exception

      when Constraint_Error =>
         Util.Log_Error ("Was unable to convert this string to a face:");
         Util.Log_Warning (""" & Str.To_String & """);
         Util.Log ("Note that we only support triangles with V/T/N format.");
         Util.Log_Error ("Crashing.");
         raise;

   end To_Face_Component;

   ----------------------------------------------------------------------------
   --  Variables

   Ret : Face := (others => Face_Component'(V => Unset_Index,
                                            T => Unset_Index,
                                            N => Unset_Index));

begin

   pragma Assert (Split_Line'Length = 4, 
                  -- Something like ("f" "1/2/3" "1/2/3" "1/2/3")
                  "Mtl file has faces that aren't triangles."
                     & " We can't handle those yet.");

   for I in (Split_Line'First + 1) .. Split_Line'Last loop
      Ret (I - 1) := To_Face_Component (Split_Line (I));
   end loop;

   return Ret;

end Get_Face;
