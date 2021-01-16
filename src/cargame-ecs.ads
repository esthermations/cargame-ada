with Ada.Containers;
with Ada.Real_Time; use Ada.Real_Time;

with Conts.Maps.Def_Def_Unbounded;
with Conts.Functional.Sets;


package Cargame.ECS is

   --------------
   --  Entity  --
   --------------

   Max_Entities : constant := 100;
   type Entity is new Natural range 0 .. Max_Entities;
   type Entity_Set is array (Positive range <>) of Entity;

   No_Entity : constant Entity := Entity'Last;

   function New_Entity return Entity;

   -----------------
   --  Component  --
   -----------------

   type Entity_Sets is array (Positive range <>) of Entity_Set;

   function Union (Sets : Entity_Sets) return Entity_Set
      with Pre  => Sets'Length > 0;
   --  TODO: Could have quite thorough postconditions here.

   type Entity_Bool_Array is array (Entity) of Boolean with Pack;

   generic
      type Data_Type is (<>);
   package Component is
      Value      : array (Entity) of Data_Type;
      Is_Present : Entity_Bool_Array;

      function Query return Entity_Set;
      function Q renames Query; -- Shorthand for writing systems
   end Component;

   package Position         is new Component (Position_Type);
   package Velocity         is new Component (Velocity_Type);
   package Rotation         is new Component (Single);
   package Rotational_Speed is new Component (Single);
   package Render_Scale     is new Component (Single);
   package Object_Matrix    is new Component (Matrix4);
   package CamObj_Matrix    is new Component (Matrix4);
   package Normal_Matrix    is new Component (Matrix3);


end Cargame.ECS;