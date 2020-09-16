var arr = reader.ReadArray("ucovers")
for(var i = 0; i < arr.count; i++)
{
	var e = arr.ReadSection(RecStr("rec_", i, 4), false)
	e.ReadVec3("position")
	e.ReadS32("ground_cell")
	e.ReadS32("conn_reg")
	e.ReadU32("dist_in_air")
	e.ReadU16("cover_id")
	e.ReadU8("group0")
	e.ReadU8("group1")
	e.ReadU16("pe_index")
	e.ReadU16("direction")
	e.ReadU8("cover_type_id")
	e.ReadHint("allowed_actions", "flags8")
	e.ReadU8("allowed_actions")
	e.ReadU8("radius", "fp32_q8")
}