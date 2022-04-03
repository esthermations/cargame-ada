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
      Name : String;
   package Component is

      type Option is record
         Val    : Element_T;
         Is_Set : Boolean;
      end record;

      subtype Datum_T is Option; -- Yes I'm a nerd ok I use the word "datum"
      type Data_T is array (Entity) of Datum_T;

      task Mgr is
         --  Stale state

         entry Read_Stale (Data : out Data_T);
         --  Read Stale component data, probably because you plan to use it to
         --  calculate New_Data for calling Update on this component.

         entry Discard_Stale_Data;
         --  Used to exit the stale state if you don't care about the previous
         --  values of this component.

         --  Awaiting update state

         entry Write_Fresh (New_Data : in Data_T);
         --  Update the data in this component.

         --  Fresh state

         entry Read_Fresh (Data : out Data_T);
         --  Read updated data. Will block until Updated is flagged.

         entry Mark_Stale;
         --  Indicate that this component is due for an update. Any systems
         --  that want fresh data from it will need to wait until a system has
         --  used Write_Fresh on it.
      end Mgr;

   private

      Data : Data_T;

   end Component;

   ---------------
   --  Systems  --
   ---------------

   package Systems is
      type System is record
         Kernel : access procedure;
         Name   : String (1 .. 25);
      end record;
      procedure Register_System (S : in System);
      procedure Run_All_Systems;
   end Systems;

end Cargame.Engine.ECS;