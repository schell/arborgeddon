varying vec2 vTex;
varying vec4 vColor;

uniform sampler2D sampler;

void main() {
    vec4 c = texture2D(sampler, vec2(vTex.s,vTex.t));
    float percent = (c.r + c.g + c.b + c.a)/4.0;
    gl_FragColor = vec4(vColor.r-percent,vColor.g-percent,vColor.b-percent,vColor.a);
}
