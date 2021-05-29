with GL;                  use GL;
with GL.Uniforms;
with GL.Objects.Programs; use GL.Objects.Programs;

package Cargame.Renderer.Generic_Uniforms with Elaborate_Body is

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
      with procedure Set_Procedure (U : in Uniform; Val : in Uniform_Type);
   package Generic_Uniform is

      function  Have_Location return Boolean;
      --  Do we have a location for this uniform in the shader? i.e., can we
      --  send values to the shader to update it?

      procedure Find_Location (Shader : in Program)
      --  Query the given shader for this uniform by name and cache the value.
         with Pre  => Shader.Initialized,
              Post => Have_Location;

      function  Get_Location return GL.Uniforms.Uniform
         with Pre => Have_Location;

      function  Get_Value return Uniform_Type;
      procedure Set_Value (Val : in Uniform_Type);

      procedure Set_And_Send (Val : in Uniform_Type)
         with Pre => Have_Location;

      procedure Send_To_GL
      --  Perform the actual GL call to update this value in the shader.
         with Pre => Have_Location;

   end Generic_Uniform;

end Cargame.Renderer.Generic_Uniforms;