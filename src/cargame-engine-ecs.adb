with Cargame.Globals;  use Cargame.Globals;
with Cargame.Types;    use Cargame.Types;
with Cargame.Util;

with GL;               use GL;
with GL.Types;         use GL.Types;          use GL.Types.Singles;
with GL.Buffers;       use GL.Buffers;
with GL.Objects;
with GL.Objects.Textures;

with Glfw.Windows.Context;


package body Cargame.Engine.ECS is

   function Union (Sets : in Entity_Sets)
      return Entity_Set
      is ([for E in Entity => (for S of Sets => S (E))]);

   ------------------
   --  New_Entity  --
   ------------------

   Next_Entity : Entity := Entity'First;

   function New_Entity
      return Entity
   is
      pragma Assert (Next_Entity /= Entity'Last,
                     "Exceeded allowable number of entities.");
      Ret : constant Entity := Next_Entity;
   begin
      Next_Entity := Entity'Succ (@);
      return Ret;
   end New_Entity;

   -----------------
   --  Component  --
   -----------------

   package body Component is
      protected body Mgr is

         <<Component_Loop>> loop

            accept Lock (Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity)
               when not Locked
            do
               Util.Log ("Component ", Name, " locked by ", Who_Are_You);
               Locked := True;
            end Lock;

            <<Locked_Loop>> loop
               --  Allow access while locked

               select
                  accept Read (D : out Data_T)
                     when Locked
                  do
                     D := Data;
                  end Get;
               or
                  accept Write (D : in Data_T)
                     when Locked
                  do
                     Data := D;
                  end Set;
               or
                  accept Unlock (Who_Are_You : String := GNAT.Source_Info.Enclosing_Entity)
                     when Locked
                  do
                     Util.Log ("Component ", Name, " released by ", Who_Are_You);
                     Locked := False;
                     exit Locked_Loop;
                  end Unlock;
               or
                  terminate;
               end select;
            end loop Locked_Loop;
         end loop Component_Loop;
      end Mgr;
   end Component;

   ---------------
   --  Systems  --
   ---------------

   package body System is
      task body Kernel_Runner is
      begin
         for Sem of After loop
            Sem.Wait;
         end loop;

         Kernel;
         Done.Signal;
      end Kernel_Runner;
   end System;

end Cargame.Engine.ECS;
