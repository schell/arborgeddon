attribute vec3 position;
attribute vec2 uv;

varying vec2 vTex;
varying vec4 vColor;

uniform vec4 color;
uniform mat4 projection;
uniform mat4 modelview;

void main () {
    vTex = uv;
    vColor = color;
    gl_Position = projection * modelview * vec4(position, 1);
}
