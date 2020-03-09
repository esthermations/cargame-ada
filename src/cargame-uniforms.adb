with GL.Objects.Programs; use GL.Objects.Programs;

package body Cargame.Uniforms is

   procedure Set_Vector3_Wrapper (U : in Uniform; Vec : in Vector3) is
      use GL;
   begin
      Set_Single (U, Vec (X), Vec (Y), Vec (Z));
   end Set_Vector3_Wrapper;

end Cargame.Uniforms;
