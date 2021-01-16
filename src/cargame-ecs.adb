with Ada.Strings.Bounded;

with Cargame.Util;

with Cargame.Globals; use Cargame.Globals;

package body Cargame.ECS is

   Next_Entity : Entity := Entity'First;

   ------------------
   --  New_Entity  --
   ------------------

   function New_Entity return Entity is
      pragma Assert (Manager.Next_Entity /= Entity'Last,
                     "Exceeded allowable number of entities.");
      Ret : constant Entity := Next_Entity;
   begin
      Next_Entity := Entity'Succ (@);
      return Ret;
   end New_Entity;

   -----------------
   --  Component  --
   -----------------

   generic
      type Data_Type is (<>);
   package body Component is

      function Query return Entity_Set is
         Num_Entities : Natural := 0;
      begin
         for E in Entity'Range loop
            if Is_Present (E) then
               Num_Entities := @ + 1;
            end if;
         end loop;

         declare
            Idx : Natural := 0;
            Ret : Entity_Set (Num_Entities);
         begin
            for E in Entity'Range loop
               if Is_Present (E) then
                  Ret (Idx) := E;
                  Idx := @ + 1;
               end if;
            end loop;

            return Ret;
         end;
      end Query;

   end Component;

end Cargame.ECS;
