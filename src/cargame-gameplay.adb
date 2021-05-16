package body Cargame.Gameplay is
   package body Controls is
      function Key_To_Action (K : in Key) return Any_Control_Action is
      begin
         for A in Any_Control_Action loop
            if K = Control_Mapping (A) then
               return A;
            end if;
         end loop;
         return No_Action;
      end Key_To_Action;
   end Controls;
end Cargame.Gameplay;
