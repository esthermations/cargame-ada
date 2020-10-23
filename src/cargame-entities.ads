
package Cargame.Entities is

   type Component_Kind is 
      (
       --  Physics-related

       Position, 
       Velocity, 
       Rotation,
       Rotational_Speed,
       --Hitbox,

       --  Render-related 

       Render_Scale,
       Object_Matrix,
       CamObj_Matrix,
       Normal_Matrix
       --Ambient_Light,
       --Diffuse_Light,
       --Specular_Light,
       --Material_Name,
       --Element_Range,
       --Shininess,
       --Specular_Texture,
       --Diffuse_Texture,
       --Vertex_Array,
       --Vertex_Buffer
       );


    Max_Entities : constant := 1000;

    type Entity_ID is range 1 .. Max_Entities;

    type Enabled_Components is array (Component_Kind) of Boolean with Pack;

    type Entity is tagged limited record
        ID         : Entity_ID;
        Components : Enabled_Components;
    end record with Pack;

    Entities : array (Entity_ID) of Entities with Pack;

    package Components is
    end Components;

end Cargame.Entities;