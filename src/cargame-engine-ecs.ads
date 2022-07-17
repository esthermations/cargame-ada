with Cargame.Config;

with GNAT.Source_Info;

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
   package Component
   is

      type Datum_T is record
         Val    : Element_T;
         Is_Set : Boolean;
      end record;

      type Elements_T is array (Entity) of Element_T;

      type Data_T is tagged record
         Elements : Elements_T;
         Entities : Entity_Set;
      end record;

      procedure Provide (D : in out Data_T; E : in Entity; Elem : in Element_T)
         with Post => D.Present (E) and then D.Elements (E) = Elem;

      procedure Remove (D : in out Data_T; E : in Entity)
         with Post => not D.Present (E);

      protected Mgr is

         entry     Wait_Until_Updated        (Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity);

         procedure Set (New_Data : in Data_T; Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity);
         function  Get return Data_T;

      private

         Data        : Data_T;
         Last_Update : Frame;

      end Mgr;

   end Component;

   --------------
   --  System  --
   --------------

   --  Systems are implemented as tasks :)

end Cargame.Engine.ECS;