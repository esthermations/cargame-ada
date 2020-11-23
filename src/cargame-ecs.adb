with Ada.Strings.Bounded;

with Conts.Vectors.Definite_Unbounded;

with Cargame.Util;

with Cargame.Globals; use Cargame.Globals;

package body Cargame.ECS is

   -----------
   --  "&"  --
   -----------

   function "&" (C1, C2 : in Component_Kind) return Enabled_Components is
      Ret : Enabled_Components := (others => False);
   begin
      Ret (C1) := True;
      Ret (C2) := True;
      return Ret;
   end "&";

   -----------
   --  "+"  --
   -----------

   function "+" (C1 : in Component_Kind) return Enabled_Components is
      Ret : Enabled_Components := (others => False);
   begin
      Ret (C1) := True;
      return Ret;
   end "+";

   -------------------------------
   --  Generic_Component_Store  --
   -------------------------------

   package body Generic_Component_Store is

      -------------
      --  Query  --
      -------------

      function Query return Entity_Set is (Manager.Query (Kind));

      --------------
      --  Is_Set  --
      --------------

      function Is_Set (E : in Entity) return Boolean is (Map.Contains (E));

      -----------
      --  Has  --
      -----------

      function Has (E : in Entity) return Boolean is 
         (Manager.Has_Component (E, Kind));

      -----------
      --  Get  --
      -----------

      --  This is where the separation between Component_Kind and Component is
      --  resolved - we rely on Query to return only Entities for which this Get
      --  function will NOT hit an exception.
      function Get (E : in Entity) return Component_Data is 
      begin
         return Map.Get (E);
      exception
         when others =>
            Util.Log_Error ("Expected Entity " & E'Img & " to have Component " & Kind'Img & ", but it isn't set!");
            raise;
      end Get;

      -----------
      --  Set  --
      -----------
     
      procedure Set (E : in Entity; Comp : in Component_Data) is
      begin
         Map.Set (E, Comp);
      end Set;

   end Generic_Component_Store;

   ---------------
   --  Manager  --
   ---------------

   package body Manager is 

      -----------------
      --  Variables  --
      -----------------

      Next_Entity : Entity := Entity'First;

      type Associated_Entities is array (Component_Kind) of Entity_Set; 
      Entities : Associated_Entities;

      Never : constant Frame := Frame'First;

      package System_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (50);
      subtype System_Name is System_Names.Bounded_String;

      type System is tagged record
         Name     : System_Name;
         Proc     : System_Proc;
         Comps    : Enabled_Components;
         Last_Run : Frame := Never;
         Interval : Frames;
      end record;

      package System_Vectors is new Conts.Vectors.Definite_Unbounded 
         (Index_Type => Positive, 
          Element_Type => System, 
          Container_Base_Type => Conts.Limited_Base);

      Systems : System_Vectors.Vector;

      package Query_Memo is
         use Ada.Containers;
         function Hash (EC : in Enabled_Components) return Hash_Type;

         type Query_Info is record
            Result       : Entity_Set;
            Last_Update  : Time;
            Needs_Update : Boolean;
         end record;

         package Query_Info_Maps is new Conts.Maps.Def_Def_Unbounded
            (Key_Type            => Enabled_Components, 
             Element_Type        => Query_Info, 
             Hash                => Hash, 
             Container_Base_Type => Conts.Limited_Base);

         Map : Query_Info_Maps.Map;
      end Query_Memo;

      -----------------------
      --  Register_System  --
      -----------------------

      procedure Register_System (Name         : in String;
                                 Proc         : in not null System_Proc;
                                 Components   : in Enabled_Components;
                                 Run_Interval : in Frames) 
      is
         Sys : constant System := 
            (Name     => System_Names.To_Bounded_String (Name),
             Proc     => Proc,
             Comps    => Components,
             Last_Run => Never,
             Interval => Run_Interval);
      begin
         Systems.Append (Sys);
         Query_Memo.Map.Set (Components, (Needs_Update => True, others => <>));

         Util.Log ("Registered system:" & Name);
      end Register_System;

      ------------------
      --  New_Entity  --
      ------------------

      function New_Entity return Entity is
         pragma Assert (Manager.Next_Entity /= Entity'Last,
                        "Exceeded allowable number of entities.");
         Ret : constant Entity := Manager.Next_Entity;
      begin
         Manager.Next_Entity := Entity'Succ (Manager.Next_Entity);
         return Ret;
      end New_Entity;

      ---------------------
      --  Add_Component  --
      ---------------------

      procedure Add_Component (E : in Entity; C : in Component_Kind) is
      begin
         Util.Log ("Adding comp " & C'Img & " to entity " & E'Img);
         Entities (C) := Entity_Sets.Add (Entities (C), E);

         for Sys of Systems loop
            if Sys.Comps (C) then
               Util.Log ("Invalidating a query cache.");
               Query_Memo.Map.Set (Sys.Comps, (Needs_Update => True, others => <>));
            end if;
         end loop;
      end Add_Component;

      ---------------------
      --  Has_Component  --
      ---------------------

      function Has_Component (E : in Entity; C : in Component_Kind) 
         return Boolean is (Entity_Sets.Mem (Entities (C), E));

      -------------
      --  Query  --
      -------------

      function Query (C : in Component_Kind) return Entity_Set is (Entities (C));

      package body Query_Memo is
         function Hash (EC : in Enabled_Components) return Hash_Type is
            Ret : Hash_Type := Hash_Type (0);
         begin
            for C in Enabled_Components'Range loop
               if EC (C) then
                  Ret := Ret + Hash_Type (2 ** Component_Kind'Enum_Rep (C));
               end if;
            end loop;
            return Ret;
         end Hash;
      end Query_Memo;

      function Query (Enabled : in Enabled_Components) return Entity_Set is
         Ret : Entity_Set;
      begin
         if Query_Memo.Map.Contains (Enabled)
            and then not Query_Memo.Map (Enabled).Needs_Update
         then
            Ret := Query_Memo.Map (Enabled).Result;
            return Ret;
         else

            Util.Log ("Query memo cache miss. This frame will be slow.");

            --  FIXME: This is extremely slow. I *believe* that
            --  Entity_Sets.Intersection allocates on every call. For the
            --  moment we're memoising it, but that's really not good enough. 
            --  We can't have the frames where a new Query happens 
            --  arbitrarily taking 10x longer.
            --
            --  This might be sped up considerably by tweaking the generic
            --  parameters of Entity_Sets to use a preallocated storage pool.
            --  Failing that, implement our own "set" concept. Will have to 
            --  look into how to do that.

            for Comp in Enabled'Range loop
               if Enabled (Comp) then
                  declare
                     C_Set : constant Entity_Set := Manager.Query (Comp);
                  begin
                     Ret := (if Entity_Sets.Is_Empty (Ret)
                             then C_Set 
                             else Entity_Sets.Intersection (Ret, C_Set));
                  end;
               end if;
            end loop;

            --  Optimisation: Memoise.
            Query_Memo.Map.Set (Enabled, (Result => Ret, 
                                          Needs_Update => False,
                                          Last_Update => Clock));
         end if;
         return Ret;
      end Query;

      -------------------
      --  Run_Systems  --
      -------------------

      procedure Run_Systems is
         --  In theory, this would do some sort of sensible scheduling or 
         --  parallelisation based on the inputs and outputs of the systems. For
         --  the moment, though, that's a bit above my pay grade.

         Ents : Entity_Set;
      begin
         for I in Systems loop
            declare
               Now       : Frame renames Globals.Current_Frame;
               Sys       : System renames Systems (I);
               Scheduled : constant Frame   := Sys.Last_Run + Sys.Interval;
               Due       : constant Boolean := (Sys.Last_Run = Never) or (Now >= Scheduled);
            begin
               if Due then
                  --  Run the thing

                  --Util.Log ("Running System: " & System_Names.To_String (Sys.Name) & " (next due on frame " & Frame'Image (Now + Sys.Interval) & ")");

                  declare
                     New_Val : System := Systems (I);
                  begin
                     New_Val.Last_Run := Now;
                     Systems.Replace_Element (I, New_Val);
                  end;

                  Ents := Manager.Query (Sys.Comps);

                  for E of Ents loop
                     Sys.Proc.all (E);
                  end loop;

              else
                 Util.Log ("NOT running system " 
                           & System_Names.To_String (Sys.Name) 
                           & "(due on frame " & Scheduled'Img & ")");
              end if;
            end;
         end loop;
      end Run_Systems;
   end Manager;

end Cargame.ECS;
