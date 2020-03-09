with Ada.Numerics;
with GL.Algebra;

--  This is abridged from GNAT's System.Dim.Mks module. That module is licensed
--  under GPL3+.
--
--  TODO: Figure out what my legal requirements regarding that are.
--
--  In the meantime, consider this file a derivative work of that file, also
--  licensed as GPL3+.

generic
   type Base_Type is digits <>;
package Cargame.Units is

   e  : constant := Ada.Numerics.e;
   Pi : constant := Ada.Numerics.Pi;

   --  Dimensioned type Mks_Type

   type Mks_Type is new Base_Type 
      with Dimension_System => 
         ((Unit_Name => Metre,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),
          (Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),
          (Unit_Name => Kilogram, Unit_Symbol => "kg",  Dim_Symbol => 'M'));

   --  SI Base dimensioned subtypes

   subtype Length is Mks_Type
      with Dimension => (Symbol => 'm', Metre  => 1, others => 0);

   subtype Mass is Mks_Type
      with Dimension => (Symbol => "kg", Kilogram => 1, others   => 0);

   subtype Time is Mks_Type
      with Dimension => (Symbol => 's', Second => 1, others => 0);

   --  Initialize SI Base unit values

   --  Turn off the all the dimension warnings for these basic assignments
   --  since otherwise we would get complaints about assigning dimensionless
   --  values to dimensioned subtypes (we can't assign 1.0*m to m).

   pragma Warnings (Off, "*assumed to be*");

   Metres    : constant Length := 1.0;
   Kilograms : constant Mass   := 1.0;
   Seconds   : constant Time   := 1.0;

   pragma Warnings (On, "*assumed to be*");

   --  SI Derived dimensioned subtypes

   subtype Angle is Mks_Type
      with Dimension => (Symbol => "rad", others => 0);

   subtype Area is Mks_Type
      with Dimension => (Metre  => 2, others => 0);


   subtype Force is Mks_Type
      with Dimension => (Symbol   => 'N', 
                         Metre    => 1, 
                         Kilogram => 1, 
                         Second   => -2, 
                         others   => 0);

   subtype Frequency is Mks_Type
      with Dimension => (Symbol => "Hz", Second => -1, others =>  0);

   subtype Speed is Mks_Type
      with Dimension => (Metre  =>  1, Second => -1, others =>  0);

   subtype Volume is Mks_Type
      with Dimension => (Metre  => 3, others => 0);

   pragma Warnings (Off, "*assumed to be*");

   Radians : constant Angle     := 1.0;
   Hertz   : constant Frequency := 1.0;
   Newtons : constant Force     := 1.0;

   --  Distance 

   Micrometres : constant Length := 1.0E-06;
   Millimetres : constant Length := 1.0E-03;
   Centimetres : constant Length := 1.0E-02;
   Kilometres  : constant Length := 1.0E+03;
   Megametres  : constant Length := 1.0E+06;

   --  Mass 

   Micrograms : constant Mass := 1.0E-09;
   Milligrams : constant Mass := 1.0E-06;
   Kilograms  : constant Mass := 1.0;
   Tonnes     : constant Mass := 1.0E+03;

   --  Time 

   Nanoseconds  : constant Time := 1.0E-09;
   Microseconds : constant Time := 1.0E-06;
   Milliseconds : constant Time := 1.0E-03;

   Minutes : constant Time := 60.0 * Seconds;
   Hours   : constant Time := 60.0 * Minutes;
   Days    : constant Time := 24.0 * Hours;
   Years   : constant Time := 365.25 * Days;

   pragma Warnings (On, "*assumed to be*");

   -----------------------------------------------------------------------------

   package Speeds is new GL.Algebra (Element_Type => Speed,
                                     Index_Type   => GL.Types.Size,
                                     Null_Value   => 0.0,
                                     One_Value    => 1.0);

end Cargame.Units;