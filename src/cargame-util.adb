with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

with GL.Objects.Vertex_Arrays;

with GNATCOLL.Strings;

with Cargame.Globals;

package body Cargame.Util is

   function Log_Task_Indent return String is
      (Ada.Strings.Fixed."*" (1 * Num_Log_Tasks_Running, ' '));

   use Standard.Ascii;

   ANSI_Reset_Colours : constant String := ESC & "[39;49m";

   subtype ANSI_Token is String (1 .. String'(ESC & "[1;31m")'Length);

   ANSI_Colour_Prefix : constant array (Text_Colour) of ANSI_Token :=
      (Red     => (ESC & "[1;31m"),
       Green   => (ESC & "[1;32m"),
       Yellow  => (ESC & "[1;33m"),
       Blue    => (ESC & "[1;34m"),
       Magenta => (ESC & "[1;35m"),
       Cyan    => (ESC & "[1;36m"));

   ----------------------------------------------------------------------------

   function Coloured (S : in String; Colour : in Text_Colour) return String is
      (ANSI_Colour_Prefix (Colour) & S & ANSI_Reset_Colours);

   ----------------------------------------------------------------------------
   function Abbreviate_Context (Ctx : in String) return String is
      use GNATCOLL.Strings;
      Context : XString;
      Colour : Text_Colour;
      Split_Context : XString_Array (1 .. 10);
      Split_Last : Positive;
      Module : XString;
   begin
      Module.Set ("Cargame");

      Context.Set (Ctx);
      Context.Split (Sep => ".", Into => Split_Context, Last => Split_Last);

      if Split_Last > 1 then
         Module := Split_Context (2);
      end if;

      --  Dynamically assign a colour :)
      Colour := Text_Colour'Val
         (Module.Length mod ANSI_Colour_Prefix'Length + 1);

      return "[" & Coloured (Module.To_String, Colour) & "]";
   end Abbreviate_Context;

   ----------------------------------------------------------------------------
   procedure Log_Error
      (Message : in String;
       Context : in String := GNAT.Source_Info.Enclosing_Entity)
   is
   begin
      Log (Message => Red (Message),
           Context => Context);
   end Log_Error;

   ----------------------------------------------------------------------------
   procedure Log_Warning
      (Message : in String;
       Context : in String := GNAT.Source_Info.Enclosing_Entity)
   is
   begin
      Log (Message => Yellow (Message),
           Context => Context);
   end Log_Warning;

   ----------------------------------------------------------------------------
   procedure Log
      (Message : in String;
       Context : in String := GNAT.Source_Info.Enclosing_Entity)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error,
                "[Frame" & Globals.Frame_Number'Img & "]"
                   & Log_Task_Indent
                   & Abbreviate_Context (Context)
                   & " " & Message);
   end Log;

   ----------------------------------------------------------------------------
   --  Used for debugging. Since default arguments are evaluated at the
   --  callsite, calling this with no arguments should be sufficiently
   --  informative, but custom messages can be provided if you want.
   procedure Got_Here
     (Where   : in String := GNAT.Source_Info.Source_Location;
      Context : in String := GNAT.Source_Info.Enclosing_Entity)
   is
   begin
      Log (Message => "Got here: " & Where,
           Context => Context);
   end Got_Here;

   ----------------------------------------------------------------------------
   procedure Start
      (LT      : in out Log_Task;
       Message : in     String;
       Context : in     String := GNAT.Source_Info.Enclosing_Entity)
   is
   begin
      LT.Message := To_Unbounded_String (Message);
      LT.Context := To_Unbounded_String (Context);
      LT.Time_Started := Ada.Real_Time.Clock;
      LT.Running := True;

      Log (Message => Cyan ("Task started: ") & Message,
           Context => Context);

      Num_Log_Tasks_Running := Num_Log_Tasks_Running + 1;
   end Start;

   ----------------------------------------------------------------------------
   procedure Complete (LT : in out Log_Task) is
   begin
      LT.Time_Stopped := Ada.Real_Time.Clock;
      LT.Running := False;

      Num_Log_Tasks_Running := Num_Log_Tasks_Running - 1;

      Log (Message =>
              Cyan ("Task completed in " & Time_Span_Image
                       (LT.Time_Stopped - LT.Time_Started)),
           Context => To_String (LT.Context));
   end Complete;

   ----------------------------------------------------------------------------
   procedure Unbind_VAO is
      use GL.Objects.Vertex_Arrays;
   begin
      Bind (Null_Array_Object);
   end Unbind_VAO;

   ----------------------------------------------------------------------------
   function Time_Span_Image (TS : in Time_Span) return String is
      package FIO is new Ada.Text_IO.Float_IO (Float);
      Ret : String (1 .. 7);
      Aft : constant := 3;
      Exp : constant := 0;
   begin
      if TS < Nanoseconds (1) then
         --  Less than nanoseconds. Probably impossible. If it is possible,
         --  hello future people! Do you have flying cars yet? Did AI take over
         --  the world? Are the Maldives still above water? Did world
         --  governments figure out how to legislate to stop apps competing
         --  with well-regulated markets by not holding up worker's rights?
         return "0 nanoseconds";
      elsif TS < Microseconds (1) then
         return Integer'Image (TS / Nanoseconds (1)) & " nanoseconds";
      elsif TS < Milliseconds (1) then
         FIO.Put (To   => Ret,
                  Item => Float (TS /  Nanoseconds (1)) / 1000.0,
                  Aft  => Aft,
                  Exp  => Exp);
         return Ret & " microseconds";
      elsif TS < Seconds (1) then
         FIO.Put (To   => Ret,
                  Item => Float (TS / Microseconds (1)) / 1000.0,
                  Aft  => Aft,
                  Exp  => Exp);
         return Ret & " milliseconds";
      elsif TS < Seconds (60) then
         FIO.Put (To   => Ret,
                  Item => Float (TS / Milliseconds (1)) / 1000.0,
                  Aft  => Aft,
                  Exp  => Exp);
         return Ret & " seconds";
      else
         FIO.Put (To   => Ret,
                  Item => Float (TS / Seconds (1)) / 60.0,
                  Aft  => Aft,
                  Exp  => Exp);
         return Ret & " minutes";
      end if;
   end Time_Span_Image;
end Cargame.Util;
