with GL;
with GL.Uniforms;
with GL.Objects.Programs;

package Cargame.Generic_Uniforms is

   use GL;
   use GL.Uniforms;
   use GL.Objects.Programs;

   --  This is derived from the Uniform implementation from AdaDoom3. It was
   --  cleverer than mine so I'm using it.
   --
   --  The idea is that we have a local copy of the value that we can modify
   --  willy-nilly via Set_Without_Sending, and call Send_to_GL when ready.
   --
   --  The Set proc just does one after the other, for convenience.

   generic
      Name : String;
      type Uniform_Type is private;
      with procedure Set_Procedure (U   : in Uniform;
                                    Val : in Uniform_Type);
   package Generic_Uniform with Abstract_State => State is

      procedure Initialise (Shader : in Program);
      procedure Initialise (Shader : in Program; Value : in Uniform_Type);

      function  Initialised return Boolean;

      function  Get return Uniform_Type;

      procedure Set_And_Send        (Val : in Uniform_Type);
      procedure Set_Without_Sending (Val : in Uniform_Type);

      procedure Send_To_GL;

      function  Image return String;

   end Generic_Uniform;

end Cargame.Generic_Uniforms;