/*
 * Script for decomple last light load screen like l08_swamp
 */

// HEADER INFO
reader.ReadU8('mp_mode');
reader.ReadString('mp_name');
reader.ReadString('name');
reader.ReadHintStr('caption', 'chose');
reader.ReadU8('max_players');
reader.ReadU8('opt_players');
reader.ReadBool('press_any_key')

// MUSIC DATA
reader.ReadU32('fade_time')
reader.ReadHintStr('music', 'sound')
reader.ReadU32('music_falloff')
reader.ReadFP32('music_volume')

// NARRATIVE DATA
reader.ReadHintStr('voice', 'sound')
reader.ReadU32('voice_delay')
reader.ReadU32('voice_falloff')
reader.ReadFP32('voice_volume')

// TEXT DATA
reader.ReadHintStr('text', 'chose')
reader.ReadHintStr('title', 'chose')
reader.ReadU32('text_delay')
reader.ReadVec4('text_color', 'color, vec4')
reader.ReadFP32('text_indient')

// TEXTURE DATA
reader.ReadHintStr('progress_map', 'texture, str_shared')
reader.ReadU32('progress_duration')

// LABELS DATA
var labels = reader.ReadArray('labels')
for (var i = 0; i < labels.More(); i++) {
    var rect = labels.ReadSection(RecStr("_rec", i, 4), false)
    rect.ReadFP32('font_size')
    rect.ReadU32('line_spacing')
    rect.ReadVec4('color', 'color, vec4')
    rect.ReadHintStr('text', 'chose')
    rect.ReadVec4('rect')
    rect.ReadBool('fade_in')
}

// TEXT DATA
reader.ReadStrArray16('textures')
reader.ReadHintStr('picture', 'texture, str_shared')