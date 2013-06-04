attribute vec3 position;
attribute vec4 color;

varying vec4 vColor;

void main () {
    gl_Position = vec4(position, 1);
    vColor = color;
}
