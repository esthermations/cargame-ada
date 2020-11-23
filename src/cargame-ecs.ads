with Ada.Containers;
with Ada.Real_Time; use Ada.Real_Time;

with Conts.Maps.Def_Def_Unbounded;
with Conts.Functional.Sets;

with Cargame.Globals;

package Cargame.ECS is

   --------------
   --  Entity  --
   --------------

   type Any_Entity is new Natural;

   No_Entity : constant Any_Entity := 0;

   subtype Entity is Any_Entity range 1 .. Any_Entity'Last;

   package Entity_Sets is new Conts.Functional.Sets (Element_Type => Entity);
   subtype Entity_Set is Entity_Sets.Set;

   function Hash (E : in Entity) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (E));

   -----------------
   --  Component  --
   -----------------

   type Component_Kind is 
      (

       --  Gameplay-related
       Player,

       --  Physics-related

       Position, 
       Velocity, 
       Acceleration,
       Rotation,
       Rotational_Speed,
       --Hitbox,

       --  Render-related 

       Model,
       Render_Scale,
       Object_Matrix,
       CamObj_Matrix,
       Normal_Matrix
       --Ambient_Light,
       --Diffuse_Light,
       --Specular_Light,
       );

   type Enabled_Components is array (Component_Kind) of Boolean;
   --  I'd love to have `with Pack` on there, but that makes it incompatible
   --  with the "C" calling convention, which is required for storing it in a
   --  vector.

   No_Components : constant Enabled_Components := (others => False);

   --  Two "&" functions are provided for notational convenience. This should
   --  allow constructing lists of enabled components by just chaining them with
   --  "&". Any infix could've worked, but "&" seems the most appropriate.

   function "&" (C1, C2 : in Component_Kind) return Enabled_Components;

   function "&" (EC : in Enabled_Components; C : in Component_Kind) 
      return Enabled_Components is ((EC with delta C => True));

   function "+" (C1 : in Component_Kind) return Enabled_Components;
   --  Unary plus. Allows us to write '+Position' to mean "Enable position".

   --------------
   --  System  --
   --------------

   type System_Proc is access procedure (E : in Entity);
   --  There's no type safety here. You can give the Manager any old procedure
   --  and it'll run it. But you shouldn't.

   ---------------
   --  Manager  --
   ---------------

   package Manager is

      pragma Assertion_Policy (Pre => Check, Post => Check);

      function New_Entity return Entity;

      function Query (Enabled : in Enabled_Components) return Entity_Set
         with Pre => (for some C of Enabled => C),
              Post => (for all C in Enabled'Range =>
                          (for all E of Query'Result => 
                             (if Enabled (C) then Has_Component (E, C))));

      function Query (C : in Component_Kind) return Entity_Set
         with Post => (for all E of Query'Result => Has_Component (E, C));

      function Has_Component (E : in Entity; C : in Component_Kind) 
         return Boolean;

      procedure Add_Component (E : in Entity; C : in Component_Kind)
         with Pre  => not Has_Component (E, C),
              Post => Has_Component (E, C);

      procedure Register_System (Name         : in String;
                                 Proc         : in not null System_Proc;
                                 Components   : in Enabled_Components;
                                 Run_Interval : in Globals.Frames)
         with Pre => Name'Length <= 50 and then
                     Components /= No_Components;

      procedure Run_Systems;

   end Manager;

   ------------------------
   --  Component Stores  --
   ------------------------

   generic
      type Component_Data is private;
      Kind : Component_Kind;
   package Generic_Component_Store is

      pragma Assertion_Policy (Pre => Check, Post => Check);

      function Query return Entity_Set;

      function Has (E : in Entity) return Boolean;

      function Is_Set (E : in Entity) return Boolean 
         with Pre => Has (E);

      function Get (E : in Entity) return Component_Data
         with Pre => Has (E) and then
                     Is_Set (E);

      procedure Set (E : in Entity; Comp : in Component_Data)
         with Pre  => Manager.Has_Component (E, Kind),
              Post => Has (E);

   private

      package Maps is new Conts.Maps.Def_Def_Unbounded
         (Key_Type            => Entity, 
          Element_Type        => Component_Data, 
          Hash                => Hash, 
          Container_Base_Type => Conts.Limited_Base);

      Map : Maps.Map;

   end Generic_Component_Store;

   --  Systems query for entities associated with a set of components.

   --  The problem we're bound to run into here is that "Component_Kind" and the
   --  actual type of the component data will always be separate, and Ada 
   --  generics (let alone runtime) provides no way to generate code to get one 
   --  from the other. So I'll have to manually implement *some* sort of 
   --  Component_Kind -> component mapping, and I should pick where I do that to
   --  minimise the work needed to add new components.



end Cargame.ECS;