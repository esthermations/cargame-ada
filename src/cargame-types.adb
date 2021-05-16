with Cargame.Quaternions;
with Ada.Numerics.Generic_Real_Arrays;
with Cargame.Util; use Cargame.Util;
with Interfaces;

package body Cargame.Types is

   ----------------------------------------------------------------------------

   package GRA is new Ada.Numerics.Generic_Real_Arrays (Real => Single);

   function GL_To_GRA (M : in Matrix3) return GRA.Real_Matrix;
   function GL_To_GRA (M : in Matrix4) return GRA.Real_Matrix;
   function GRA_To_GL (X : in GRA.Real_Matrix) return Matrix3;
   function GRA_To_GL (X : in GRA.Real_Matrix) return Matrix4;

   ----------------------------------------------------------------------------
   --  Abdridged from OpenGLAda/examples/common/src/maths.adb.
   function Perspective_Matrix (Top, Bottom, Left, Right, Near, Far : Single)
      return Matrix4
   is
      dX  : constant Single := Right - Left;
      dY  : constant Single := Top - Bottom;
      dZ  : constant Single := Far - Near;
      Mtx : Matrix4         := (others => (others => 0.0));
   begin
      Mtx (X, X) := 2.0 * Near / dX;
      Mtx (Z, X) := (Right + Left) / dX;
      Mtx (Y, Y) := 2.0 * Near / dY;
      Mtx (Z, Y) := (Top + Bottom) / dY;
      Mtx (Z, Z) := -(Far + Near) / dZ;
      Mtx (W, Z) := -2.0 * Far * Near / dZ;
      Mtx (Z, W) := -1.0;
      return Mtx;
   end Perspective_Matrix;

   ----------------------------------------------------------------------------
   --  Abdridged from OpenGLAda/examples/common/src/maths.adb.
   function Perspective_Matrix (View_Angle              : Degrees;
                                Aspect_Ratio, Near, Far : Single)
      return Matrix4
   is
      Top    : constant Single :=
         (Near * Tan (Single (0.5 * To_Radians (View_Angle))));
      Bottom : constant Single := -Top;
      Right  : constant Single := Top * Aspect_Ratio;
      Left   : constant Single := -Right;
   begin
      return Perspective_Matrix (Top, Bottom, Left, Right, Near, Far);
   end Perspective_Matrix;

   ----------------------------------------------------------------------------
   --  Abridged from OpenGLAda/examples/common/src/maths.adb.
   procedure Look_At (Camera_Pos, Target_Pos : in     Valid_Vector3;
                      Up                     : in     Valid_Vector3;
                      Mtx                    :    out Matrix4)
   is
      Forward : constant Vector3 := Normalized (Camera_Pos - Target_Pos);
      Side    : constant Vector3 := Normalized (Cross_Product (Up, Forward));
      Up_New  : constant Vector3 := Normalized (Cross_Product (Forward, Side));
   begin
      Mtx := (X => (Side (X), Up_New (X), Forward (X), 0.0),
              Y => (Side (Y), Up_New (Y), Forward (Y), 0.0),
              Z => (Side (Z), Up_New (Z), Forward (Z), 0.0),
              -----
              W => (X => -Dot_Product (Camera_Pos, Side),
                    Y => -Dot_Product (Camera_Pos, Up_New),
                    Z => -Dot_Product (Camera_Pos, Forward),
                    W => 1.0));
   end Look_At;

   ----------------------------------------------------------------------------
   function Look_At (Camera_Pos, Target_Pos : in Valid_Vector3;
                     Up                     : in Valid_Vector3)
      return Matrix4
   is
   begin
      return Ret : Matrix4 do
         Look_At (Camera_Pos, Target_Pos, Up, Ret);
      end return;
   end Look_At;

   ----------------------------------------------------------------------------
   --  Straight copy from OpenGLAda/examples/common/src/maths.adb.
   function Rotation_Matrix (Angle : Radians; Axis : Vector3) return Matrix4 is
      package Quaternions is new Cargame.Quaternions (Single);
      use Quaternions;

      Half_Angle : constant Single     := 0.5 * Single (Angle);
      Sine       : constant Single     := Sin (Half_Angle);
      NQ         : constant Quaternion :=
         Normalized ((A => Cos (Half_Angle),
                      B => Sine * Axis (GL.X),
                      C => Sine * Axis (GL.Y),
                      D => Sine * Axis (GL.Z)));

      --  FIXME: This might be incorrect depending on row-major
      --  or column-major.
      Mtx : constant Matrix4 :=
         (X => (X => (1.0 - 2.0 * (NQ.C * NQ.C + NQ.D * NQ.D)),
                Y => (0.0 + 2.0 * (NQ.B * NQ.C - NQ.A * NQ.D)),
                Z => (0.0 + 2.0 * (NQ.B * NQ.D + NQ.A * NQ.C)),
                W => 0.0),
          Y => (X => (0.0 + 2.0 * (NQ.B * NQ.C + NQ.A * NQ.D)),
                Y => (1.0 - 2.0 * (NQ.B * NQ.B + NQ.D * NQ.D)),
                Z => (0.0 + 2.0 * (NQ.C * NQ.D - NQ.A * NQ.B)),
                W => 0.0),
          Z => (X => (0.0 + 2.0 * (NQ.B * NQ.D - NQ.A * NQ.C)),
                Y => (0.0 + 2.0 * (NQ.C * NQ.D + NQ.A * NQ.B)),
                Z => (1.0 - 2.0 * (NQ.B * NQ.B + NQ.C * NQ.C)),
                W => 0.0),
          W => (X => 0.0, Y => 0.0, Z => 0.0, W => 1.0));
   begin
      return Mtx;
   end Rotation_Matrix;

   ----------------------------------------------------------------------------
   procedure Rotate (Mtx   : in out Matrix4;
                     Angle : in     Radians;
                     Axis  : in     Vector3 := (Y => 1.0, others => 0.0))
   is
      Rot : constant Matrix4 := Rotation_Matrix (Angle, Axis);
   begin
      Mtx := Mtx * Rot;
   end Rotate;

   ----------------------------------------------------------------------------
   function Rotate (Mtx   : in Matrix4;
                    Angle : in Radians;
                    Axis  : in Vector3 := (Y => 1.0, others => 0.0))
      return Matrix4
   is
      Ret : Matrix4 := Mtx;
   begin
      Rotate (Mtx => Ret, Angle => Angle, Axis => Axis);
      return Ret;
   end Rotate;

   ----------------------------------------------------------------------------
   --  Abridged copy from OpenGLAda/examples/common/src/maths.adb.
   procedure Translate (Mtx    : in out Matrix4;
                        Change : in     Vector3) is
   begin
      Mtx (W, X) := Change (X);
      Mtx (W, Y) := Change (Y);
      Mtx (W, Z) := Change (Z);
   end Translate;

   ----------------------------------------------------------------------------
   function Translate (Mtx : in Matrix4; Change : in Vector3) return Matrix4 is
      Mtx_Copy : Matrix4 := Mtx;
   begin
      Translate (Mtx_Copy, Change);
      return Mtx_Copy;
   end Translate;

   ----------------------------------------------------------------------------
   function Scale (M : in Matrix4; Factor : in Single) return Matrix4 is
      Ret : Matrix4 := M;
   begin
      Ret (X, X) := M (X, X) * Factor;
      Ret (Y, Y) := M (Y, Y) * Factor;
      Ret (Z, Z) := M (Z, Z) * Factor;
      return Ret;
   end Scale;

   ----------------------------------------------------------------------------
   procedure Scale (M : in out Matrix4; Factor : in Single) is
      Mtx : constant Matrix4 := M;
   begin
      M := Scale (Mtx, Factor);
   end Scale;

   ----------------------------------------------------------------------------
   function GL_To_GRA (M : in Matrix4) return GRA.Real_Matrix is
      X : GRA.Real_Matrix (1 .. 4, 1 .. 4);
   begin
      for Row in 1 .. 4 loop
         for Column in 1 .. 4 loop
            X (Row, Column) := M (Int_To_Index (Row), Int_To_Index (Column));
         end loop;
      end loop;
      return X;
   end GL_To_GRA;

   ----------------------------------------------------------------------------
   function GRA_To_GL (X : in GRA.Real_Matrix) return Matrix4 is
      M : Matrix4;
   begin
      for Row in 1 .. 4 loop
         for Column in 1 .. 4 loop
            M (Int_To_Index (Row), Int_To_Index (Column)) := X (Row, Column);
         end loop;
      end loop;
      return M;
   end GRA_To_GL;

   ----------------------------------------------------------------------------
   function GL_To_GRA (M : in Matrix3) return GRA.Real_Matrix is
      X : GRA.Real_Matrix (1 .. 3, 1 .. 3);
   begin
      for Row in 1 .. 3 loop
         for Column in 1 .. 3 loop
            X (Row, Column) := M (Int_To_Index (Row), Int_To_Index (Column));
         end loop;
      end loop;
      return X;
   end GL_To_GRA;

   ----------------------------------------------------------------------------
   function GRA_To_GL (X : in GRA.Real_Matrix) return Matrix3 is
      M : Matrix3;
   begin
      for Row in 1 .. 3 loop
         for Column in 1 .. 3 loop
            M (Int_To_Index (Row), Int_To_Index (Column)) := X (Row, Column);
         end loop;
      end loop;
      return M;
   end GRA_To_GL;

   ----------------------------------------------------------------------------
   --  function Transpose (M : in Matrix4) return Matrix4 is
   --        (GRA_To_GL (GRA.Transpose (GL_To_GRA (M))));

   ----------------------------------------------------------------------------
   function Inverse (M : in Matrix4) return Matrix4 is
      (GRA_To_GL (GRA.Inverse (GL_To_GRA (M))));

   ----------------------------------------------------------------------------
   function Inverse (M : in Matrix3) return Matrix3 is
      (GRA_To_GL (GRA.Inverse (GL_To_GRA (M))));

   ----------------------------------------------------------------------------
   function Hash (C : Face_Component) return Hash_Type is
      use Interfaces;
      UV64 : constant Unsigned_64 := Unsigned_64 (C.V);
      UN64 : constant Unsigned_64 := Unsigned_64 (C.N);
      UT64 : constant Unsigned_64 := Unsigned_64 (C.T);
   begin
      return Hash_Type
         (Shift_Left (UV64, 0 * (Hash_Type'Size / 3)) +
          Shift_Left (UN64, 1 * (Hash_Type'Size / 3)) +
          Shift_Left (UT64, 2 * (Hash_Type'Size / 3)));
   end Hash;

end Cargame.Types;
