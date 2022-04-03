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
      return Ret : Entity_Set := (others => False) do
         for S of Sets loop
            for E in Entity loop
               if S (E) then
                  Ret (E) := True;
               end if;
            end loop;
         end loop;
      end return;
   end Union;

   ------------------
   --  New_Entity  --
   ------------------

   Next_Entity : Entity := Entity'First;

   function New_Entity return Entity is
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

      task body Mgr is
      begin
         Task_Loop : loop

            Stale_State : loop
               select
                  accept Read_Stale (Ret : out Data_T) do
                     Util.Log (Name & " served stale data. Awaiting update.");
                     Ret := Data;
                     exit Stale_State;
                  end Read_Stale;
               or
                  accept Discard_Stale_Data;
                  exit Stale_State;
               or
                  terminate;
               end select;
            end loop Stale_State;

            Awaiting_Update : loop
               select
                  accept Write_Fresh (New_Data : in Data_T) do
                     Util.Log (Name & " received fresh data, moving to fresh state.");
                     Data := New_Data;
                     exit Awaiting_Update;
                  end Write_Fresh;
               or
                  terminate;
               end select;
            end loop Awaiting_Update;

            Fresh_State : loop
               select
                  accept Read_Fresh (E : in Entity; Ret : out Option) do
                     Ret := Data (E);
                  end Read_Fresh;
               or
                  accept Mark_Stale do
                     Util.Log (Name & " -> Stale");
                  end Mark_Stale;
                  exit Fresh_State;
               or
                  terminate;
               end select;
            end loop Fresh_State;

         end loop Task_Loop;
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
