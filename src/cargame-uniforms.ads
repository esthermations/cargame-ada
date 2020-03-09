with GL;
with GL.Uniforms;              use GL.Uniforms;
with GL.Types;                 use GL.Types; use GL.Types.Singles;

with Cargame.Types;            use Cargame.Types;
with Cargame.Generic_Uniforms; use Cargame.Generic_Uniforms;

package Cargame.Uniforms is

   --  Set_Single(Some_Vector3) throws, for some reason. But Set_Single(X,Y,Z)
   --  doesn't. So map the single-vector call to the one that won't crash.
   procedure Set_Vector3_Wrapper (U : in Uniform; Vec : in Vector3);

   package Projection              is new Generic_Uniform ("u_Projection"         , Matrix4, Set_Single);
   package Camera_Transform        is new Generic_Uniform ("u_Camera_Transform"   , Matrix4, Set_Single);
   package Object_Transform        is new Generic_Uniform ("u_Object_Transform"   , Matrix4, Set_Single);
   package CamObj_Transform        is new Generic_Uniform ("u_CamObj_Transform"   , Matrix4, Set_Single);
   package Normal_Transform        is new Generic_Uniform ("u_Normal_Transform"   , Matrix3, Set_Single);
   package Diffuse_Map             is new Generic_Uniform ("u_Diffuse_Map"        , Int    , Set_Int);
   package Specular_Map            is new Generic_Uniform ("u_Specular_Map"       , Int    , Set_Int);
   package Material_Ambient        is new Generic_Uniform ("u_Material_Ambient"   , Vector3, Set_Vector3_Wrapper);
   --  package Material_Diffuse    is new Generic_Uniform ("u_Material_Diffuse"   , Vector3, Set_Vector3_Wrapper);
   --  package Material_Specular   is new Generic_Uniform ("u_Material_Specular"  , Vector3, Set_Vector3_Wrapper);
   package Material_Shininess      is new Generic_Uniform ("u_Material_Shininess" , Single , Set_Single);
   package Light_Position          is new Generic_Uniform ("u_Light_Position"     , Vector3, Set_Vector3_Wrapper);
   package Light_Ambient           is new Generic_Uniform ("u_Light_Ambient"      , Vector3, Set_Vector3_Wrapper);
   package Light_Diffuse           is new Generic_Uniform ("u_Light_Diffuse"      , Vector3, Set_Vector3_Wrapper);
   package Light_Specular          is new Generic_Uniform ("u_Light_Specular"     , Vector3, Set_Vector3_Wrapper);
   package Procedural_Terrain      is new Generic_Uniform ("u_Procedural_Terrain" , Int    , Set_Int); -- Boolean 
   package Drawing_A_Line          is new Generic_Uniform ("u_Drawing_A_Line"     , Int    , Set_Int); -- Boolean

end Cargame.Uniforms;
