with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with GL.Types;
with GL.Pixels;
with GL.Objects.Textures.Targets;
with GL.Images;

with Cargame.Util;

package body Cargame.Engine.Texture_Loader is
   use GL.Types;
   use Interfaces.C.Strings;
   use GL.Objects.Textures.Targets;
   use GL.Objects.Textures.Targets.Texture_2D_Target;

   procedure Load_Texture (Filename : in     String;
                           Tex      : in out Texture)
   is
   begin

      Util.Log ("Loading texture: " & Filename);

      GL.Images.Load_File_To_Texture
         (Path           => Filename,
          Texture        => Tex,
          Texture_Format => GL.Pixels.RGB);

      Util.Log ("Load_From_Data completed.");

      Generate_Mipmap (Texture_2D);

      --  Texture_2D.Bind(0);
   end Load_Texture;

   function Load_Texture (Filename : in String) return Texture is
      Ret : Texture;
   begin
      Load_Texture (Filename, Ret);
      return Ret;
   end Load_Texture;

end Cargame.Engine.Texture_Loader;
