varying vec2 vTex;

uniform sampler2D sampler;

void main() {
    gl_FragColor = texture2D(sampler, vec2(vTex.s, vTex.t));
}
