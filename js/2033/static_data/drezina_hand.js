// Auto-generated file

reader.ReadU16("version")
var section1 = reader.ReadSection("__edit")
section1.ReadString("caption")
reader.ReadBool("editable")
reader.ReadBool("visible_for_ai")
reader.ReadBool("block_ai_los")
reader.ReadBool("collideable")
reader.ReadString("collision_sound")
reader.ReadHintStr("collision_track", "camera_track, str_shared")
reader.ReadU32("collision_interval")
reader.ReadFP32("collision_move")
reader.ReadBool("slope_enabled")
reader.ReadBool("pump_mouse")
reader.ReadBool("pump_keyboard")
reader.ReadFP32("click_power_max")
reader.ReadBool("left_right_pump")
