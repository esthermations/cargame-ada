with GNAT.Source_Info;
with Ada.Real_Time; use Ada.Real_Time;
with GL;            use GL;

package Cargame.Util is

   function Time_Span_Image (TS : in Time_Span) return String;
   function Image (TS : in Time_Span) return String renames Time_Span_Image;

   Int_To_Index : constant array (Integer range 1 .. 4) of Index_Homogeneous :=
      (X, Y, Z, W);

   --------------------
   --  Text colours  --
   --------------------

   type Any_Text_Colour is (Default, Red, Green, Yellow, Blue, Magenta, Cyan);
   subtype Text_Colour is Any_Text_Colour range Red .. Cyan;

   function Coloured (S : in String; Colour : in Text_Colour) return String;

   function Red     (S : in String) return String is (Coloured (S, Red));
   function Green   (S : in String) return String is (Coloured (S, Green));
   function Yellow  (S : in String) return String is (Coloured (S, Yellow));
   function Blue    (S : in String) return String is (Coloured (S, Blue));
   function Magenta (S : in String) return String is (Coloured (S, Magenta));
   function Cyan    (S : in String) return String is (Coloured (S, Cyan));

   ---------------
   --  Logging  --
   ---------------

   procedure Log
      (Message : in String;
       Context : in String := GNAT.Source_Info.Enclosing_Entity);

   procedure Log_Error
      (Message : in String;
       Context : in String := GNAT.Source_Info.Enclosing_Entity);

   procedure Log_Warning
      (Message : in String;
       Context : in String := GNAT.Source_Info.Enclosing_Entity);

   procedure Got_Here
      (Where   : in String := GNAT.Source_Info.Source_Location;
       Context : in String := GNAT.Source_Info.Enclosing_Entity);

end Cargame.Util;
