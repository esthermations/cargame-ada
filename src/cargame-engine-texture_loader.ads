with Ada.Directories;
with GL.Objects.Textures;

package Cargame.Engine.Texture_Loader is
   use GL.Objects.Textures;

   function Load_Texture (Filename : in String)
      return Texture
      with Pre  => Ada.Directories.Exists (Filename),
           Post => Load_Texture'Result.Initialized;

end Cargame.Engine.Texture_Loader;
