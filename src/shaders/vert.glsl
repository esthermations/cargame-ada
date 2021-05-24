#version 450

uniform mat4 Model;
uniform mat4 View;
uniform mat4 Projection;

layout (location = 0) in  vec3 in_Position;
layout (location = 1) in  vec3 in_Normal;

layout (location = 0) out vec3 out_Colour;

void main() {
    out_Colour = abs(in_Normal);
    gl_Position = Projection * View * Model * vec4(in_Position, 1.0);
}
