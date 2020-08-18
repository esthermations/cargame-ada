#version 330

in vec4 t_Original_Vertex;
in vec4 t_Modified_Vertex;
in vec3 t_Modified_Normal;
in vec2 t_Modified_TexCrd;

out vec4 o_Frag_Colour;

uniform vec3  u_Light_Position;
uniform vec3  u_Light_Ambient;
uniform vec3  u_Light_Diffuse;
uniform vec3  u_Light_Specular;

uniform vec3  u_Material_Ambient;
uniform vec3  u_Material_Diffuse;
uniform vec3  u_Material_Specular;
uniform float u_Material_Shininess;

uniform sampler2D u_Diffuse_Map;
uniform sampler2D u_Specular_Map;

uniform float u_Seconds_Since_Epoch;
uniform bool  u_Drawing_A_Line = false;

/// Generate a number that looks random but definitely isn't.
float Fake_Random(in vec2 Seed) {
    return 0.5 
        + (0.5 * fract(sin(Seed.x * 12.9898 + Seed.y * 78.233) * 43758.5453));
}

/// Is Val between Low and High?
bool Is_Between(float Val, float Low, float High) {
    return (Low <= Val && Val <= High);    
}

/// Render a Phong point light at the given position.
vec4 Phong_Point_Light(in vec4 Position, in vec3 Normal) {
    vec3 S = normalize(u_Light_Position - Position.xyz);
    vec3 R = reflect(-(S), Normal);

    // Set Ambient
    vec3 Ambient = u_Light_Ambient * u_Material_Ambient;

    // Set Diffuse
    float S_Dot_Normal = max(dot(S, Normal), 0);
    vec3 Diffuse = u_Light_Diffuse
                 * vec3(texture(u_Diffuse_Map, t_Modified_TexCrd))
                 * S_Dot_Normal;

    // Set Specular
    vec3 Specular;

    if (S_Dot_Normal > 0) {
        vec3 V = normalize(-(Position.xyz));
        Specular = u_Light_Specular
                 * vec3(texture(u_Specular_Map, t_Modified_TexCrd))
                 * pow(max(dot(R, V), 0.0), u_Material_Shininess);
    } else {
        Specular = vec3(0.0);
    }

    return vec4(Ambient + Diffuse + Specular, 1.0);
}

void main(void) {
    // gl_FragColor = normalize(vec4(0.8) + t_Original_Vertex);
    vec3 Modified_Normal = normalize(t_Modified_Normal);

    if (u_Drawing_A_Line) {
        o_Frag_Colour = vec4 (1.0, 0.0, 1.0, 1.0); // Purple
    } else {
        // Non-procedural stuff, so do light+textures, etc.
        o_Frag_Colour = Phong_Point_Light(t_Modified_Vertex, Modified_Normal);
    }
}
