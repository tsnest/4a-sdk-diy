var i = 0, array = reader.ReadArray("suit_type")
while(array.More())
{
	var rec = array.ReadSection(RecStr("rec_", i++, 4), false)
	rec.ReadString("name")
	rec.ReadU32("id")
	rec.ReadHintStr("third_model", "choose")
	rec.ReadHintStr("survival_third_model", "choose")
	rec.ReadString("timer_locator")
	rec.ReadFP32("base_speed")
	rec.ReadFP32("base_jump")
	rec.ReadFP32("luminocity")
	rec.ReadU32("sound_type_walk")
	rec.ReadU32("sound_type_run")
	rec.ReadU32("sound_type_crouch")
	rec.ReadU32("sound_type_sprint")
	rec.ReadU32("sound_type_jump")
	rec.ReadU32("sound_type_landing")
	rec.ReadU32("survival_sound_type_walk")
	rec.ReadU32("survival_sound_type_run")
	rec.ReadU32("survival_sound_type_crouch")
	rec.ReadU32("survival_sound_type_sprint")
	rec.ReadU32("survival_sound_type_jump")
	rec.ReadU32("survival_sound_type_landing")	
}