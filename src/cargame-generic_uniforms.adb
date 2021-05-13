package body Cargame.Generic_Uniforms is

   package body Generic_Uniform
      with Refined_State => (State => (Uniform_Index, Current_Value))
   is
      Uniform_Index : Uniform := -1;
      Current_Value : Uniform_Type;

      function Initialised return Boolean is (Uniform_Index /= -1);

      procedure Initialise (GL_Program : in Program) is
      begin
         if not Initialised then
            pragma Assert (GL_Program.Initialized);
            Uniform_Index := GL_Program.Uniform_Location(Name);
         end if;
         -- Re-initialisation is a no-op.
      end Initialise;

      procedure Initialise (GL_Program : in Program;
                            Value      : in Uniform_Type) is
      begin
         Initialise (GL_Program);
         Set (Value);
      end Initialise;

      function Image return String is (Name);

      function Get return Uniform_Type is (Current_Value);

      procedure Set_Without_Sending (Val : in Uniform_Type) is
      begin
         Current_Value := Val;
      end Set_Without_Sending;

      procedure Send_To_GPU is
      begin
         Set_Procedure (Uniform_Index, Current_Value);
      end Send_To_GPU;

      procedure Set (Val : in Uniform_Type) is
      begin
         Set_Without_Sending (Val);
         Send_To_GPU;
      end Set;

   end Generic_Uniform;

end Cargame.Generic_Uniforms;