--  Perform an EXTREMELY naive transformation to convert the given face into a
--  HOPEFULLY equivalent face, composed entirely of triangles. This algorithm
--  assumes that the given face is convex---i.e., that no line between any of
--  its edges will exit the face. Since this function only operates on indices,
--  not vertices, we have no way of verifying this.
--
--  If it's already a triangle, it will be returned unchanged. Otherwise, the
--  resulting Face will have quite a few more Face_Components in it. It's quite
--  a naive transformation.

separate (Cargame.Obj_Parser)
function Convert_Into_Triangles (F : in Face) return Face is
begin

   --  Simplest case: it's already a triangle. Return right away.
   if F'Length = 3 then
      return F;
   end if;

   --  Handle squares.
   --  1234 -> 123234. That's it.
   if F'Length = 4 then
      return Face'(F (1), F (2), F (3), F (2), F (3), F (4));
   end if;

   --  Other cases...
   raise Unimplemented_Feature
      with "Convert_Into_Triangles can't handle more than squares just yet.";

end Convert_Into_Triangles;