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
   type Entity_Set is array (Positive range <>) of Entity
      with Dynamic_Predicate =>
         (for all I in Entity_Set'Range =>
            (for all J in Entity_Set'Range =>
               (if Entity_Set (I) = Entity_Set (J) then I = J)));

   No_Entity : constant Entity := Entity'Last;

   function New_Entity return Entity;

   -------------------
   --  Entity Sets  --
   -------------------

   package Entity_Set_Package is new Conts.Functional.Sets (Entity);
   subtype Entity_Set is Entity_Set_Package.Set;

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

      function Query return Entity_Set
         with Post => (for all E of Query'Result => Is_Present (E)) and
                      (for all I in Query'Result'Range =>
                         (for all J in Query'Result'Range =>
                            (if Query'Result (I) = Query'Result (J)
                             then I = J)));

      function Q return Entity_Set renames Query;
      --  Shorthand for writing systems
   end Component;

   package Controlled_By_Player is new Component (Boolean);
   package Position             is new Component (Types.Valid_Vector3);
   package Velocity             is new Component (Types.Valid_Vector3);
   package Acceleration         is new Component (Types.Valid_Vector3);
   package Model                is new Component (Models.Model);
   package Rotation             is new Component (Types.Radians);
   package Rotational_Speed     is new Component (Types.Radians);
   package Render_Scale         is new Component (GL.Types.Single);
   package Object_Matrix        is new Component (GL.Types.Singles.Matrix4);
   package CamObj_Matrix        is new Component (GL.Types.Singles.Matrix4);
   package Normal_Matrix        is new Component (GL.Types.Singles.Matrix3);

   ---------------
   --  Systems  --
   ---------------

   package Systems is
      procedure Run_All_Systems;
      procedure Tick_Position      (E : in ECS.Entity);
      procedure Tick_Rotation      (E : in ECS.Entity);
      procedure Tick_Object_Matrix (E : in ECS.Entity);
      procedure Tick_CamObj_Matrix (E : in ECS.Entity);
      procedure Tick_Normal_Matrix (E : in ECS.Entity);
   end Systems;

end Cargame.ECS;