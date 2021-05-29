--  C++ style vectors, not math vectors.

with Ada.Containers.Vectors;

with GL.Types;
with GL.Objects.Textures;
with Cargame.Types;

package Cargame.Vectors is

   use GL.Types;
   use GL.Types.Singles;
   use Cargame.Types;

   ------------------
   -- Vector types --
   ------------------

   package C renames Ada.Containers;
   subtype S is GL.Types.Size;

   pragma Style_Checks (Off);
      package Single_Vectors         is new C.Vectors (S, Single);
      package Vector2_Vectors        is new C.Vectors (S, Vector2);
      package Vector3_Vectors        is new C.Vectors (S, Vector3);
      package Int_Vectors            is new C.Vectors (S, Int);
      package UInt_Vectors           is new C.Vectors (S, UInt);
      package Size_Vectors           is new C.Vectors (S, GL.Types.Size);
      package Texture_Vectors        is new C.Vectors (S, GL.Objects.Textures.Texture);
      package Radians_Vectors        is new C.Vectors (S, Cargame.Types.Radians);
      package Material_Vectors       is new C.Vectors (S, Cargame.Types.Material);
      package Matrix3_Vectors        is new C.Vectors (S, Matrix3);
      package Matrix4_Vectors        is new C.Vectors (S, Matrix4);
      package Face_Component_Vectors is new C.Vectors (S, Face_Component);
   pragma Style_Checks (On);

   subtype Vector_Of_Single         is Single_Vectors.Vector;
   subtype Vector_Of_Vector2        is Vector2_Vectors.Vector;
   subtype Vector_Of_Vector3        is Vector3_Vectors.Vector;
   subtype Vector_Of_Int            is Int_Vectors.Vector;
   subtype Vector_Of_UInt           is UInt_Vectors.Vector;
   subtype Vector_Of_Size           is Size_Vectors.Vector;
   subtype Vector_Of_Texture        is Texture_Vectors.Vector;
   subtype Vector_Of_Radians        is Radians_Vectors.Vector;
   subtype Vector_Of_Material       is Material_Vectors.Vector;
   subtype Vector_Of_Matrix3        is Matrix3_Vectors.Vector;
   subtype Vector_Of_Matrix4        is Matrix4_Vectors.Vector;
   subtype Vector_Of_Face_Component is Face_Component_Vectors.Vector;

   -----------------
   -- Array types --
   -----------------

   type Array_Of_Single is
      array (GL.Types.Size range <>) of aliased Single;
   type Array_Of_Vector2 is
      array (GL.Types.Size range <>) of aliased Vector2;
   type Array_Of_Vector3 is
      array (GL.Types.Size range <>) of aliased Vector3;
   type Array_Of_UInt is
      array (GL.Types.Size range <>) of aliased UInt;
   type Array_Of_Radians is
      array (GL.Types.Size range <>) of aliased Radians;
   type Array_Of_Material is
      array (GL.Types.Size range <>) of aliased Material;
   type Array_Of_Matrix3 is
      array (GL.Types.Size range <>) of aliased Matrix3;
   type Array_Of_Matrix4 is
      array (GL.Types.Size range <>) of aliased Matrix4;
   type Array_Of_Face_Component is
      array (GL.Types.Size range <>) of aliased Face_Component;

   function All_Members_Are_Valid (Arr : in Vector3_Array)
      return Boolean
      is (for all V of Arr => (for all X of V => X'Valid));

   function All_Members_Are_Valid (Arr : in Vector2_Array)
      return Boolean
      is (for all V of Arr => (for all X of V => X'Valid));

end Cargame.Vectors;
