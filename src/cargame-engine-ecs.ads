with Cargame.Config;
with Cargame.Globals;
with Cargame.Util;

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
         and then
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

      type Elements_T is array (Entity) of Element_T;
      --  Must be its own decl by language rules

      type Data_T is tagged record
         Elements : Elements_T;
         Entities : Entity_Set;
      end record;

      --  Helper methods for mutating a Data_T in-line. These would generally
      --  be called after Mgr.Read() and before Mgr.Write().
      procedure Set   (D : in out Data_T; E : Entity; Value : Element_T);
      procedure Unset (D : in out Data_T; E : Entity);

      protected Mgr is
         entry Lock   (Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity);
         entry Read   (D : out Data_T);
         entry Write  (D : in Data_T);
         entry Unlock (Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity);
      private
         Data : Data_T;
      end Mgr;

   end Component;

   --------------
   --  System  --
   --------------

   generic
      with procedure Kernel;
      After : Util.Semaphore_Array;
   package System is
      Done : Util.Semaphore;
   private
      task Kernel_Runner;
      --  Kicks off when instantiated. Will wait for the provided semaphores
      --  before executing Kernel.
   end System;

end Cargame.Engine.ECS;