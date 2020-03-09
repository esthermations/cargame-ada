with Ada.Containers;
with Ada.Real_Time; use Ada.Real_Time;

with Conts.Maps.Def_Def_Unbounded;
with Conts.Functional.Sets;


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

   --  In order for this ECS library to be generic, this should be a generic
   --  parameter so that the user specifies their list of components, and a
   --  Generic_Component_Store is instantiated for each Component_Kind.
   --
   --  Current idea is for the Manager keep a Component_Kind -> Entity_Set 
   --  mapping, which the individual component stores update when Set is called.
   --  I suppose a System should be a generic procedure, with an execution 
   --  kernel and a set of Enabled_Components.
   --
   --  The execution kernel is technically free to grab components from whatever
   --  stores it wants. I'll look into how to enforce discipline there, but for 
   --  the meantime it's "be careful".

   type Component_Kind is 
      (
       --  Physics-related

       Position, 
       Velocity, 
       Rotation,
       Rotational_Speed,
       --Hitbox,

       --  Render-related 

       Render_Scale,
       Object_Matrix,
       CamObj_Matrix,
       Normal_Matrix
       --Ambient_Light,
       --Diffuse_Light,
       --Specular_Light,
       --Material_Name,
       --Element_Range,
       --Shininess,
       --Specular_Texture,
       --Diffuse_Texture,
       --Vertex_Array,
       --Vertex_Buffer
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

   --  `Enable` is an alternative, in case that doesn't work out. It allows you
   --  to write Enable ((C1, C5, C17)) to provide a list of components.

   -- function Enable (Comps : in array (Component_Kind'Range) of Component_Kind) return Enabled_Components
   -- is
   --    Ret : Enabled_Components := (others => False);
   -- begin
   --    return Ret : Enabled_Components := (others => False) do
   --       for C of Comps loop
   --          Ret (C) := True;
   --       end loop;
   --    end return;
   -- end Enable;

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
         with Post => Has (E);

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

   --------------
   --  System  --
   --------------

   --  Systems are the main problem as far as implementation in Ada goes. It
   --  might be most appropriate to have it just be an interface. But what do we
   --  gain there?
   --
   --  I guess any idea for a System implementation has to improve on just
   --  having a bunch of procedures. Let's try that.

   type System_Proc is access procedure (E : in Entity);
   --  There's no type safety here. You can give the Manager any old procedure
   --  and it'll run it. But you shouldn't.

   -- type System_Trigger is (Regular_Interval, After_Another_System);

   -- type System_Run_Timing (Trigger : in System_Trigger) is record
   --    case Trigger is
   --       when Regular_Interval     => Run_Every : Time_Span;
   --       when After_Another_System => Run_After : System_Proc;
   --    end case;
   -- end record;

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
                                 Run_Interval : in Time_Span)
         with Pre => Name'Length <= 50 and then
                     Components /= No_Components;

      procedure Run_Systems;

   end Manager;

end Cargame.ECS;