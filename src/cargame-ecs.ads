with Ada.Containers;
with Ada.Real_Time; use Ada.Real_Time;

with Conts.Maps.Def_Def_Unbounded;
with Conts.Functional.Sets;

with GL.Types;
with Cargame.Globals;
with Cargame.Types;
with Cargame.Models;

package Cargame.ECS is

   --------------
   --  Entity  --
   --------------

   Max_Entities : constant := 100;
   type Entity is new Natural range 0 .. Max_Entities;

   No_Entity : constant Entity := Entity'Last;

   function New_Entity return Entity;

   ------------------------------
   --  Entity Container Types  --
   ------------------------------

   type Entity_Array is array (Positive range <>) of aliased Entity;

   package Entity_Set_Package is new Conts.Functional.Sets (Entity);
   subtype Entity_Set is Entity_Set_Package.Set;
   use Entity_Set_Package;

   -----------------
   --  Component  --
   -----------------

   type Entity_Bool_Array is array (Entity) of Boolean with Pack;

   generic
      type Data_Type is private;
   package Component is
      Value      : array (Entity) of Data_Type;
      Is_Present : Entity_Bool_Array;

      procedure Set (E : in Entity; V : in Data_Type)
         with Post => Is_Present (E) and Value (E) = V;

      function Get (E : in Entity) return Data_Type is (Value (E)) with Inline;

      function Query return Entity_Set
         with Post => (for all E of Query'Result => Is_Present (E)) and
                      (for all E in Entity =>
                        (if Is_Present (E) then Mem (Query'Result, E)));

      function Q return Entity_Set renames Query;
      --  Shorthand for writing systems
   end Component;

   ---------------
   --  Systems  --
   ---------------

   procedure Run_All_Systems;

end Cargame.ECS;