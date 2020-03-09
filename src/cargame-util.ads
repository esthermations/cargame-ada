with GNAT.Source_Info;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Unbounded;

with GL;            use GL;

package Cargame.Util is

   use Ada.Strings.Unbounded;

   function Time_Span_Image (TS : in Time_Span) return String;
   function Image (TS : in Time_Span) return String is (Time_Span_Image (TS));

   Int_To_Index : constant array (Integer range 1 .. 4) of Index_Homogeneous :=
      (X, Y, Z, W);

   procedure Unbind_VAO;

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

   Num_Log_Tasks_Running : Natural := 0;

   type Log_Task is tagged limited record
      Message : Unbounded_String;
      Context : Unbounded_String;
      Running : Boolean := False;
      Time_Started : Time;
      Time_Stopped : Time;
   end record;

   procedure Start 
      (LT      : in out Log_Task; 
       Message : in     String; 
       Context : in     String := GNAT.Source_Info.Enclosing_Entity)
      with Pre  => not LT.Running and
                   Num_Log_Tasks_Running /= Natural'Last,
           Post => LT.Running and
                   Num_Log_Tasks_Running = Num_Log_Tasks_Running'Old + 1;

   procedure Complete (LT : in out Log_Task)
      with Pre  => LT.Running and 
                   Num_Log_Tasks_Running /= 0,
           Post => not LT.Running and
                   Num_Log_Tasks_Running = Num_Log_Tasks_Running'Old - 1;


end Cargame.Util;
