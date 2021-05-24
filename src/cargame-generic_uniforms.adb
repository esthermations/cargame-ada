package body Cargame.Generic_Uniforms is

   package body Generic_Uniform is
      Location : Uniform := -1;
      Value    : Uniform_Type;

      function Have_Location return Boolean is
         (Location /= -1);

      procedure Find_Location (Shader : in Program) is
      begin
         Location := Shader.Uniform_Location (Name);
      end Find_Location;

      function Get_Value return Uniform_Type is
         (Value);

      function Get_Location return Uniform is (Location);

      procedure Set_Value (Val : in Uniform_Type) is
      begin
         Value := Val;
      end Set_Value;

      procedure Send_To_GL is
      begin
         Set_Procedure (Location, Value);
      end Send_To_GL;

      procedure Set_And_Send (Val : in Uniform_Type) is
      begin
         Set_Value (Val);
         Send_To_GL;
      end Set_And_Send;

   end Generic_Uniform;

end Cargame.Generic_Uniforms;