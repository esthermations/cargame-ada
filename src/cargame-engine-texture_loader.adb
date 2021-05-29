with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with GL.Types;
with GL.Pixels;
with GL.Objects.Textures.Targets;

with Cargame.Util;

--  This is just a non-comprehensive wrapper around stb_image.h.

package body Cargame.Engine.Texture_Loader is
   use GL.Types;
   use Interfaces.C.Strings;
   use GL.Objects.Textures.Targets;
   use GL.Objects.Textures.Targets.Texture_2D_Target;

   procedure Load_Texture (Filename : in     String;
                           Tex      : in out Texture);

   --  stbi_uc *stbi_load (char const *filename,
   --                      int *x, int *y, int *channels_in_file,
   --                      int desired_channels);
   function STBI_Load (Filename               : in     chars_ptr;
                       X, Y, Channels_In_File :    out GL.Types.Int;
                       Desired_Channels       : in     GL.Types.Int)
      return chars_ptr
         with Import, Convention => C, External_Name => "stbi_load",
              Post => Interfaces.C.Is_Nul_Terminated
                         (Value (STBI_Load'Result));

   procedure STBI_Image_Free (Data : in chars_ptr)
      with Import, Convention => C, External_Name => "stbi_image_free";

   function STBI_Failure_Reason return Interfaces.C.Strings.chars_ptr
      with Import, Convention => C, External_Name => "stbi_failure_reason";

   procedure Load_Texture (Filename : in     String;
                           Tex      : in out Texture)
   is
      Image_Data : chars_ptr;
      Width, Height, Channels_In_File : Int := -1;
      Desired_Channels : constant Int := 3;

      Filename_C : chars_ptr := New_String (Filename);

      function To_Image_Source is
         new Ada.Unchecked_Conversion (chars_ptr, Image_Source);

   begin

      Util.Log ("Loading texture: " & Filename);

      Tex.Initialize_Id;
      Texture_2D.Bind (Tex);

      Image_Data := STBI_Load (Filename         => Filename_C,
                               X                => Width,
                               Y                => Height,
                               Channels_In_File => Channels_In_File,
                               Desired_Channels => Desired_Channels);

      Free (Filename_C);

      pragma Assert (Width > 0 and Height > 0 and Channels_In_File > 0,
                     "STBI_Load error: " &
                        (if STBI_Failure_Reason /= Null_Ptr
                         then Value (STBI_Failure_Reason)
                         else "UNKNOWN"));

      Texture_2D.Load_From_Data
         (Level           => Mipmap_Level (0),
          Internal_Format => GL.Pixels.RGB,
          Width           => Width,
          Height          => Height,
          Source_Format   => GL.Pixels.RGB,
          Source_Type     => GL.Pixels.Unsigned_Byte,
          Source          => To_Image_Source (Image_Data));

      Util.Log ("Load_From_Data completed.");

      Generate_Mipmap (Texture_2D);

      STBI_Image_Free (Image_Data);

      --  Texture_2D.Bind(0);
   end Load_Texture;

   function Load_Texture (Filename : in String) return Texture is
      Ret : Texture;
   begin
      Load_Texture (Filename, Ret);
      return Ret;
   end Load_Texture;

end Cargame.Engine.Texture_Loader;
