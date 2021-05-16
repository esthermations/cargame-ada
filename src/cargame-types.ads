with Ada.Containers;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Bounded;

with GL;
with GL.Objects.Buffers;
with GL.Objects.Textures;
with GL.Types;

with Cargame.Generic_Vector_Math;

package Cargame.Types is

   use GL;
   use GL.Types;
   use GL.Types.Singles;
   use GL.Objects.Buffers;

   function "=" (L, R : in GL.Objects.Textures.Texture) return Boolean is
      (L.Raw_Id = R.Raw_Id);

   package Single_Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Single);
   use Single_Elementary_Functions;

   function Image (V : in Vector2) return String is
       (" ( " & Single'Image (V (X)) & ", "
              & Single'Image (V (Y)) & " )");

   function Image (V : in Vector3) return String is
       (" ( " & Single'Image (V (X)) & ", "
              & Single'Image (V (Y)) & ", "
              & Single'Image (V (Z)) & " )");

   function Is_Valid (V : Vector3) return Boolean is
      (for all X of V => X'Valid) with Inline;

   function Is_Valid (V : Vector2) return Boolean is
      (for all X of V => X'Valid) with Inline;

   -----------
   -- Units --
   -----------

   type Radians is new Single;
   type Degrees is new Single;

   Radians_Per_Degree : constant Radians := Ada.Numerics.Pi / 180.0;
   Degrees_Per_Radian : constant Degrees := 180.0 / Ada.Numerics.Pi;

   function To_Radians (X : Degrees) return Radians is
      (Radians (X) * Radians_Per_Degree);

   function To_Degrees (X : Radians) return Degrees is
      (Degrees (X) * Degrees_Per_Radian);

   subtype Valid_Vector2 is Vector2 with
      Dynamic_Predicate => Is_Valid (Valid_Vector2);
   subtype Valid_Vector3 is Vector3 with
      Dynamic_Predicate => Is_Valid (Valid_Vector3);

   Origin : constant Valid_Vector3 := (others => 0.0);

   --------------------
   -- Buffer loaders --
   --------------------

   procedure Load_Int_Buffer     is new Load_To_Buffer (GL.Types.Int_Pointers);
   procedure Load_Vector2_Buffer is new Load_To_Buffer (Vector2_Pointers);
   procedure Load_Vector3_Buffer is new Load_To_Buffer (Vector3_Pointers);

   -------------------------
   -- Geometric functions --
   -------------------------

   procedure Look_At (Camera_Pos, Target_Pos : in     Valid_Vector3;
                      Up                     : in     Valid_Vector3;
                      Mtx                    :    out Matrix4);

   function Look_At (Camera_Pos, Target_Pos : in Valid_Vector3;
                     Up                     : in Valid_Vector3)
      return Matrix4;

   function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
       return Matrix4;

   function Perspective_Matrix (View_Angle              : Degrees;
                                Aspect_Ratio, Near, Far : Single)
       return Matrix4
       with Pre => Near > 0.0 and Far > Near;

   function Rotation_Matrix (Angle : Radians; Axis : Vector3) return Matrix4;

   procedure Rotate (Mtx   : in out Matrix4;
                     Angle : in     Radians;
                     Axis  : in     Vector3 := (Y => 1.0, others => 0.0));

   function Rotate (Mtx   : in Matrix4;
                    Angle : in Radians;
                    Axis  : in Vector3 := (Y => 1.0, others => 0.0))
      return Matrix4;

   procedure Translate (Mtx : in out Matrix4; Change : Vector3);
   function  Translate (Mtx : in     Matrix4; Change : Vector3) return Matrix4;

   function Inverse (M : in Matrix4) return Matrix4;
   function Inverse (M : in Matrix3) return Matrix3;

   function  Scale (M : in     Matrix4; Factor : in Single) return Matrix4;
   procedure Scale (M : in out Matrix4; Factor : in Single);

   ------------------------------
   -- Vector utility functions --
   ------------------------------

   function Magnitude is new Cargame.Generic_Vector_Math.Generic_Magnitude
      (Index_Type => GL.Index_3D, Vec_Type => Vector3);

   function Magnitude is new Cargame.Generic_Vector_Math.Generic_Magnitude
      (Index_Type => GL.Index_2D, Vec_Type => Vector2);

   function Normalized is new Cargame.Generic_Vector_Math.Generic_Normalized
      (Index_Type => GL.Index_3D, Vec_Type => Vector3);

   function Normalized is new Cargame.Generic_Vector_Math.Generic_Normalized
      (Index_Type => GL.Index_2D, Vec_Type => Vector2);

   function Mat4_To_Mat3 (M : in Matrix4) return Matrix3 is
      (Matrix3'((M (X, X), M (X, Y), M (X, Z)),
                (M (Y, X), M (Y, Y), M (Y, Z)),
                (M (Z, X), M (Z, Y), M (Z, Z))));

   --------------
   -- Mtl data --
   --------------

   use GL.Objects.Textures;

   Max_Material_Name_Length : constant := 100;
   package Material_Names is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max_Material_Name_Length);
   subtype Material_Name is Material_Names.Bounded_String;

   function To_Material_Name (S : in String) return Material_Name is
      (Material_Names.To_Bounded_String (S))
      with Pre => S'Length < Max_Material_Name_Length;

   --  It seems that 'usemtl' often refers to material names with just
   --  enough of the actual material name to be unambiguous. So we
   --  can't just compare (Got_Name = Have_Name), we have to compare
   --  them up to the length of the shortest string.

   use Material_Names;

   function Names_Match (A, B : in Material_Name) return Boolean
      is (if Length (A) = 0 or Length (B) = 0 then False
          else (if Length (A) > Length (B)
                then (Head (A, Length (B)) = B)
                else (Head (B, Length (A)) = A)));

   Unset_Index       : constant GL.Types.Size := GL.Types.Size'Last;
   Unset_Num_Indices : constant Int           := -1;

   type Material is tagged record
      Name               : Material_Name;
      Ambient_Light      : Vector3 := (others => 0.0);
      Diffuse_Light      : Vector3 := (others => 0.0);
      Specular_Light     : Vector3 := (others => 0.0);
      First_Index        : GL.Types.Size := Unset_Index;
      Num_Indices        : Int           := Unset_Num_Indices;
      Diffuse_Texture    : Texture;
      Specular_Texture   : Texture;
      Shininess          : Single := 8.0;
   end record;

   --  Get the material's name for printing. TODO: Should possibly be renamed.
   function Printable_Name (M : in Material) return String is
      ("Material( " & Material_Names.To_String (M.Name) & " )");

   function Final_Index (M : in Material) return GL.Types.Size is
      (M.First_Index + M.Num_Indices);

   overriding
   function "=" (L, R : in Material) return Boolean is
      (L.Name = R.Name and then
       L.Ambient_Light           = R.Ambient_Light and then
       L.Diffuse_Light           = R.Diffuse_Light and then
       L.Specular_Light          = R.Specular_Light and then
       L.First_Index             = R.First_Index and then
       L.Num_Indices             = R.Num_Indices and then
       L.Diffuse_Texture.Raw_Id  = R.Diffuse_Texture.Raw_Id and then
       L.Specular_Texture.Raw_Id = R.Diffuse_Texture.Raw_Id and then
       L.Shininess               = R.Shininess);

   Default_Material : constant Material :=
      (Name             => To_Material_Name ("Default Material"),
       Ambient_Light    => (others => 0.0),
       Diffuse_Light    => (others => 0.0),
       Specular_Light   => (others => 0.0),
       First_Index      => 0,
       Num_Indices      => 0,
       Diffuse_Texture  => <>,
       Specular_Texture => <>,
       Shininess        => 0.0);

   ---------------------
   --  Face Component --
   ---------------------

   type Face_Component is record
      V, T, N : GL.Types.Size;
   end record;

   overriding
   function "=" (A, B : Face_Component) return Boolean is
      (A.V = B.V and then A.T = B.T and then A.N = B.N);

   use Ada.Containers;
   function Hash (C : Face_Component) return Hash_Type
      with Pre => (Hash_Type'Size = 64 and then
                   C.V < 2#0001_1111_1111_1111_1111_1111# and then
                   C.N < 2#0001_1111_1111_1111_1111_1111# and then
                   C.T < 2#0001_1111_1111_1111_1111_1111#);
   --  We need to smoosh 3 integers into a 64-bit hash type, so each gets 21
   --  bits to work with. If this precondition fails, we have a hash collision.

end Cargame.Types;
