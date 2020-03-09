package body Cargame.Generic_Vector_Math is

   ----------------------------------------------------------------------------
   function Generic_Magnitude (V : in Vec_Type) return Single is
      Sum_Of_Squares : Single := 0.0;
   begin
      for Val of V loop
         Sum_Of_Squares := Sum_Of_Squares + (Val ** 2);
      end loop;
      return Sqrt (Sum_Of_Squares);
   end Generic_Magnitude;

   ----------------------------------------------------------------------------
   function Generic_Normalized (V : in Vec_Type) return Vec_Type is
      Mag : constant Single := Magnitude (V);
   begin
      return (if Mag = 0.0 then Vec_Type'(others => 0.0) else V / Mag);
   end Generic_Normalized;

end Cargame.Generic_Vector_Math;
