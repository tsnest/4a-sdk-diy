// Auto-generated file

reader.ReadU16("version")
var section1 = reader.ReadSection("__edit")
section1.ReadString("caption")
reader.ReadBool("editable")
reader.ReadBool("visible_for_ai")
reader.ReadBool("block_ai_los")
reader.ReadBool("accept_fast_explosion")
reader.ReadBool("collideable")
reader.ReadFP32("usage_distance")
reader.ReadString("collision_sound")
reader.ReadHintStr("collision_track", "choose")
reader.ReadU32("collision_interval")
reader.ReadFP32("collision_move")
reader.ReadFP32("attach_threshold")
reader.ReadFP32("attach_armor")