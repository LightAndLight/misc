struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
};

const CORNERS = array(
    // Top left
    vec2(1.0, 1.0),
    vec2(-1.0, 1.0),
    vec2(-1.0, -1.0),
    // Bottom left
    vec2(1.0, 1.0),
    vec2(-1.0, -1.0),
    vec2(1.0, -1.0),
);

@vertex
fn vs_main(
    @builtin(vertex_index) in_vertex_index: u32,
) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = vec4(CORNERS[in_vertex_index], 0.0, 1.0);
    return out;
}

@group(0) @binding(0)
var t: texture_2d<f32>;

@group(0) @binding(1)
var s: sampler;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // var value = textureSample(t, s, in.clip_position.xy / in.clip_position.w);
    //return value.r * vec4(0, 1, 0, 1);

    // Doesn't work because clip_position is in viewport coordinates here. https://gpuweb.github.io/gpuweb/#coordinate-systems
    return vec4(in.clip_position.xy / in.clip_position.w, 0.0, 1.0);
}
