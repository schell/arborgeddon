attribute vec3 position;
attribute vec4 color;

varying vec4 vColor;

uniform mat4 modelviewMat;
uniform mat4 projectionMat;

void main () {
    vColor = color;
    gl_Position = projectionMat * modelviewMat * vec4(position, 1);
}
