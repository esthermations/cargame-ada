with GL;       use GL;
with GL.Types; use GL.Types;

package Cargame.Generic_Vector_Math is

   --  Euclidian magnitude of the vector in 3D space. 
   generic
      type Index_Type is new GL.Index_Homogeneous;
      type Vec_Type is array (Index_Type) of aliased Single;
      with function Sqrt (S : in Single) return Single is <>;
   function Generic_Magnitude (V : in Vec_Type) return Single;

   --  Normalise the vector to have a magnitude of 1
   generic
      type Index_Type is new GL.Index_Homogeneous;
      type Vec_Type is array (Index_Type) of aliased Single;
      with function "/" (V : in Vec_Type; D : in Single) return Vec_Type is <>;
      with function Magnitude (V : in Vec_Type) return Single is <>;
   function Generic_Normalized (V : in Vec_Type) return Vec_Type;

end Cargame.Generic_Vector_Math;