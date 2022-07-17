with Cargame.Globals;  use Cargame.Globals;
with Cargame.Types;    use Cargame.Types;
with Cargame.Util;

with GL;               use GL;
with GL.Types;         use GL.Types;          use GL.Types.Singles;
with GL.Buffers;       use GL.Buffers;
with GL.Objects;
with GL.Objects.Textures;

with Glfw.Windows.Context;


package body Cargame.Engine.ECS is

   function Union (Sets : in Entity_Sets)
      return Entity_Set
   is
   begin
      return [for E in Entity => (for S of Sets => S (E))];
   end Union;

   ------------------
   --  New_Entity  --
   ------------------

   Next_Entity : Entity := Entity'First;

   function New_Entity
      return Entity
   is
      pragma Assert (Next_Entity /= Entity'Last,
                     "Exceeded allowable number of entities.");
      Ret : constant Entity := Next_Entity;
   begin
      Next_Entity := Entity'Succ (@);
      return Ret;
   end New_Entity;

   -----------------
   --  Component  --
   -----------------

   package body Component is

      procedure Provide (D : in out Data_T; E : in Entity; Elem : in Element_T)
         with Post => D.Present (E) and then D.Elements (E) = Elem
      is
      begin
      end Provide;

      procedure Remove (D : in out Data_T; E : in Entity)
         with Post => not D.Present (E);

      protected body Mgr is

         entry Wait_Until_Updated (Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity)
            when Last_Update /= Globals.Frame_Number
         is
         begin
            Util.Log (Name & ": " & Who_Are_You & " finished waiting");
         end Wait_Until_Updated;

         function Get return Data_T is (Data);

         procedure Set (New_Data : in Data_T; Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity)
         is
         begin
            Util.Log (Name & ": " & Who_Are_You & " gave us new values. Now up-to-date!");
            Data        := New_Data;
            Last_Update := Globals.Frame_Number;
         end Set;

      end Mgr;

   end Component;

   ---------------
   --  Systems  --
   ---------------

   package body Systems is

      type Array_Of_System
         is array (Positive range 1 .. Config.Max_Systems)
         of System;

      Registered_Systems : Array_Of_System;
      Last_System_Index  : Positive := Registered_Systems'First;

      procedure Register_System (S : in System) is
      begin
         pragma Assert (Last_System_Index <= Config.Max_Systems);
         Registered_Systems (Last_System_Index) := S;
         Last_System_Index := @ + 1;
      end Register_System;

      procedure Run_All_Systems is
      begin
         for Sys of Registered_Systems (1 .. Last_System_Index) loop
            Util.Log ("Running system: " & Sys.Name);
            Sys.Kernel.all; --  This is a procedure call.
         end loop;
      end Run_All_Systems;

   end Systems;

end Cargame.Engine.ECS;
