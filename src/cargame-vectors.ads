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

   package Single_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Single);
   package Vector2_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Vector2);
   package Vector3_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Vector3);
   package Int_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Int);
   package UInt_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, UInt);
   package Size_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, GL.Types.Size);
   package Texture_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, GL.Objects.Textures.Texture);
   package Radians_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Cargame.Types.Radians);
   package Distance_Type_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Cargame.Types.Distance_Type);
   package Position_Type_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Cargame.Types.Position_Type);
   package Velocity_Type_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Cargame.Types.Velocity_Type);
   package Material_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Cargame.Types.Material);
   package Matrix3_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Matrix3);
   package Matrix4_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Matrix4);
   package Face_Component_Vectors is new
      Ada.Containers.Vectors (GL.Types.Size, Face_Component);

   subtype Vector_Of_Single         is Single_Vectors.Vector;
   subtype Vector_Of_Vector2        is Vector2_Vectors.Vector;
   subtype Vector_Of_Vector3        is Vector3_Vectors.Vector;
   subtype Vector_Of_Int            is Int_Vectors.Vector;
   subtype Vector_Of_UInt           is UInt_Vectors.Vector;
   subtype Vector_Of_Size           is Size_Vectors.Vector;
   subtype Vector_Of_Texture        is Texture_Vectors.Vector;
   subtype Vector_Of_Radians        is Radians_Vectors.Vector;
   subtype Vector_Of_Distance_Type  is Distance_Type_Vectors.Vector;
   subtype Vector_Of_Position_Type  is Position_Type_Vectors.Vector;
   subtype Vector_Of_Velocity_Type  is Velocity_Type_Vectors.Vector;
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
   type Array_Of_Distance_Type is
      array (GL.Types.Size range <>) of aliased Distance_Type;
   type Array_Of_Position_Type is
      array (GL.Types.Size range <>) of aliased Position_Type;
   type Array_Of_Velocity_Type is
      array (GL.Types.Size range <>) of aliased Velocity_Type;
   type Array_Of_Material is
      array (GL.Types.Size range <>) of aliased Material;
   type Array_Of_Matrix3 is
      array (GL.Types.Size range <>) of aliased Matrix3;
   type Array_Of_Matrix4 is
      array (GL.Types.Size range <>) of aliased Matrix4;
   type Array_Of_Face_Component is
      array (GL.Types.Size range <>) of aliased Face_Component;

end Cargame.Vectors;
