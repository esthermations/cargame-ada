separate (Cargame.Obj_Parser)
function Convert_Into_Triangles (F : in Face) return Face is
begin
    --  Simplest case: it's already a triangle. Return right away.
    if F'Length = 3 then
        Util.Log ("Face is already a triangle.");
        return F;
    end if;

    --  Handle squares. 
    --  1234 -> 123234. That's it.

    if F'Length = 4 then
        return Face'(F (1), F (2), F (3), 
                     F (2), F (3), F (4));
    end if;

    --  Other cases...

    raise Unimplemented_Feature 
        with "Convert_Into_Triangles can't handle more than squares just yet.";

end Convert_Into_Triangles;