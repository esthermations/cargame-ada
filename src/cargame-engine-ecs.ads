with Cargame.Config;

package Cargame.Engine.ECS is

   --------------
   --  Entity  --
   --------------

   Max_Entities : constant := Cargame.Config.Max_Entities;
   type Entity is new Natural range 0 .. Max_Entities;

   function New_Entity return Entity;

   ------------------------------
   --  Entity Container Types  --
   ------------------------------

   type Entity_Array is array (Positive range <>) of aliased Entity;

   type Entity_Set is array (Entity) of Boolean with Pack;

   type Entity_Sets is array (Positive range <>) of Entity_Set with Pack;

   function Union (Sets : in Entity_Sets) return Entity_Set with
      Post =>
         --  If any given set contains E, result must contain E.
         (for all I in Sets'Range =>
            (for all E in Entity => (if Sets (I)(E) then Union'Result (E))))
         and
         --  If the result has E, then some given set must have E.
         (for all E in Union'Result'Range =>
            (if Union'Result (E) then (for some S of Sets => S (E))));

   -----------------
   --  Component  --
   -----------------

   generic
      type Data_Type is private;
   package Component is
      Value : array (Entity) of Data_Type; --  with Volatile_Components;
      Has   : Entity_Set; --  FIXME: Backwards naming?

      procedure Set (E : in Entity; V : in Data_Type)
         with Post => Has (E) and then Value (E) = V;

      function Q return Entity_Set is (Has);
   end Component;

   ---------------
   --  Systems  --
   ---------------

   procedure Run_All_Systems;

end Cargame.Engine.ECS;