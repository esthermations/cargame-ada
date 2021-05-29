with Cargame.Renderer.Generic_Uniforms;

--  GL Uniform declarations for use in the renderer

package Cargame.Renderer.Uniforms is

   package GU renames Cargame.Renderer.Generic_Uniforms;

   pragma Style_Checks (Off);
      package Projection is new GU.Generic_Uniform ("Projection", Matrix4, Set_Single);
      package View       is new GU.Generic_Uniform ("View"      , Matrix4, Set_Single);
      package Model      is new GU.Generic_Uniform ("Model"     , Matrix4, Set_Single);
   pragma Style_Checks (On);

end Cargame.Renderer.Uniforms;
