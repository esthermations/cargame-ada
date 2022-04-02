#version 330

uniform mat4 Model;
uniform mat4 View;
uniform mat4 Projection;

layout (location = 0) in  vec3 in_Position;
layout (location = 1) in  vec3 in_Normal;

out vec4 Colour;

void main() {
    Colour = vec4(abs(in_Normal), 0.5);
    gl_Position = Projection * View * Model * vec4(in_Position, 1.0);
}
