#version 330

layout (location = 0) in vec3 a_Vertex;
layout (location = 1) in vec3 a_Normal;
layout (location = 2) in vec2 a_TexCrd;

out vec4 t_Original_Vertex;
out vec3 t_Original_Normal;

out vec4 t_Modified_Vertex;
out vec3 t_Modified_Normal;
out vec2 t_Modified_TexCrd;

uniform mat4 u_Projection; // Look_At or Orthogonal
uniform mat4 u_CamObj_Transform;
uniform mat3 u_Normal_Transform;

void main(void) {
    t_Original_Vertex = vec4(a_Vertex, 1.0);
    t_Original_Normal = a_Normal;

    t_Modified_Vertex = u_CamObj_Transform * t_Original_Vertex;
    t_Modified_Normal = u_Normal_Transform * t_Original_Normal;
    t_Modified_TexCrd = vec2(a_TexCrd.x, -(a_TexCrd.y));

    gl_Position = u_Projection * t_Modified_Vertex;
}
