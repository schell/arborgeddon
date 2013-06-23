attribute vec3 position;
attribute vec4 color;

varying vec4 vColor;

uniform mat4 modelview;
uniform mat4 projection;

void main () {
    vColor = color;
    gl_Position = projection * modelview * vec4(position, 1);
}
