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
      type Element_T is private;
   package Component is

      type Option is
         record
            Is_Set : Boolean;
            Val    : Element_T;
         end record;

      type Data_T is array (Entity) of Option;

      task Mgr is
         --  Stale state

         entry Read_Stale (E : in Entity; Ret : out Option);
         --  Read Stale component data, probably because you plan to use it to
         --  calculate New_Data for calling Update on this component.

         entry Update (E : in Entity; New_Value : in Element_T);
         --  Update the data in this component.

         entry Finished_Update;
         --  Moves us out of the Stale state into the Fresh state.

         --  Fresh state

         entry Read_Fresh (E : in Entity; Ret : out Option);
         --  Read updated data. Will block until Updated is flagged.

         entry Next_Frame;
         --  Indicate that all systems have finished running and we're moving
         --  on to the next frame. Moves us into the Stale state.
      end Mgr;

   private

      Data : Data_T;

   end Component;

   ---------------
   --  Systems  --
   ---------------

   package Systems is
      type System is access procedure;
      procedure Register_System (S : in System);
      procedure Run_All_Systems;
   end Systems;

end Cargame.Engine.ECS;