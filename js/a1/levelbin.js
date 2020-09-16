// interface
this.ReadStartup = ReadStartup;
this.ReadEntities = ReadEntities;

// implementation
var typed_strings = module("a1\\typed_strings");
//module("visualscript");

function ReadVssVer6(e)
{
	// stub!
	e.TryReadArray("vss_ver_7") || e.ReadArray("vss_ver_6")
}

function ReadTime(e, name)
{
	e.ReadHint(name, "time")
	e.ReadU32(name)
}

function ReadInterest(e)
{
	var i = e.ReadSection("interest")
	
	i.ReadU16("min_importance")
	i.ReadU16("max_importance")
	i.ReadU8("interest_type")
	i.ReadU16("duration")
	i.ReadFP32("speed")
	i.ReadFP32("distance")
}

function ReadVsRef(e, name)
{
	e.ReadHint(name, "vs_ref, str_shared");
	e.ReadString(name);
	e.ReadBool(name + "_dyn_state_exist");
}

function ReadCommonsVs(e)
{
	var k = 0, arr = e.ReadArray("commons_vs");
	while(arr.More())
	{
		var vs = arr.ReadSection(RecStr("rec_", k++, 4), false);
		// TODO
		/*
		vs.ReadString("vs_name");
		vs.ReadBool("vs_debug");
		ReadVsRef(vs, "vs_ref");
		*/
	}
}

function ReadRemovedVs(e)
{
	var k = 0, arr = e.ReadArray("removed_vs");
	while(arr.More())
	{
		var vs = arr.ReadSection(RecStr("rec_", k++, 4), false);
		// TODO
	}
}

function ReadPhysicsShell(e)
{
	var p = e.ReadSection("physics_shell");
	// TODO
	/*
	p.ReadBool("breaking_blocked");
	var elements = p.ReadArray("elements");
	for(var i = 0; elements.More(); i++)
	{
		var l = elements.ReadSection(RecStr("rec_", i, 4), false);
		l.ReadU16("root_bid");
		l.ReadFP32("accumulated_impulse");
		l.ReadMatrix43("xform"); // if (version < 28) ReadMatrix44
		l.ReadVec3("velocity");
		l.ReadBool("nx_awake");
		var shapes = l.ReadArray("shapes");
		for(var j = 0; shapes.More(); j++)
		{
			var s = shapes.ReadSection(RecStr("rec_", j, 4), false);
			s.ReadU16("bid");
		}
	}
	*/
}

function ReadUObject(e)
{
	e.ReadName("name");
	e.ReadBool("force_hud")
	e.ReadBool("ghost2")
	e.ReadU8("oflags", "bool8")
	e.ReadU8("sflags", "bool8")
	e.ReadFP32("cull_distance")
	e.ReadMatrix43("", "pose, matrix");
	e.ReadHintStr("visual", "choose")
	e.ReadU16("dao_val");
	e.ReadVec4("render_aux_val", "color, vec4f")
	//visualscript.ReadVssVer6(e)
	ReadVssVer6(e)
	e.ReadBool("vs_active")
	e.ReadU16("spatial_sector")
}

function ReadUObject_Static(e)
{
	ReadUObject(e)
		
	e.ReadU8("flags", "bool8")
	e.ReadU8("collision_group")
}

function ReadUObject_StaticBreakable(e)
{
	e.ReadFP32("health");
	
	ReadUObject_Static(e);
	
	e.ReadHint("die_model", "ref_model");
	e.ReadString("die_model");
	e.ReadHint("die_sound", "choose");
	e.ReadString("die_sound");
	e.ReadU8("die_sound_type");
	e.ReadHint("die_particles", "particles, str_shared");
	e.ReadString("die_particles");
	e.ReadBool("die_particles_ignore_rotation");
	e.ReadBool("block_ai_los");
	ReadCommonsVs(e);
}

function ReadUObject_Effect(e)
{
	ReadUObject(e);
	
	ReadCommonsVs(e)
	ReadRemovedVs(e)
	
	e.ReadHintStr("startup_animation", "animation_str")
	e.ReadHintStr("bone_part", "part_str")
	e.ReadU16("start_frame")
	e.ReadFP32("speed")
	e.ReadU8("startup_animation_flags", "bool8")
	e.ReadU8("force_looped")
	e.ReadHintStr("sound", "choose")
	e.ReadU8("sound_volume", "fp32_q8")
	e.ReadU8("sound_filter")
	e.ReadHintStr("particles", "choose")
	e.ReadU8("particle_flags", "bool8")
	ReadInterest(e)
	e.ReadStrArray32("labels")
}

function ReadUObject_Effect_MLeaf(e)
{
	ReadUObject_Effect(e)
	
	e.ReadU32("particles_color", "color, u32")
}

function ReadCEntity(e)
{
	e.ReadFP32("health")
	e.ReadU32("dying_mask")
	
	ReadUObject_Effect(e)
	
	e.ReadU8("add_physics_flags", "bool8")
	e.ReadU8("friend_type")
	e.ReadU8("reaction_type")
	e.ReadHintStr("fixed_bones", "choose_array, str_shared")
	e.ReadU8("physics_flags0", "bool8")
	e.ReadU8("physics_flags1", "bool8")
	e.ReadFP32("break_impulse_threshold")
	//? e.ReadBool("no_collision")
	e.ReadU8("collisions_group")
	e.ReadU8("scene_type")
	e.ReadHintStr("break_particles_break", "choose")
	e.ReadHintStr("break_particles_death", "choose")
	e.ReadHintStr("break_sound_death", "choose")
	e.ReadHint("type_mask", "flags64")
	e.ReadU64("type_mask")
	e.ReadU32("ph_shell_model_src")
	e.ReadU32("ph_shell_skltn_src")
	e.ReadU32("ph_shell_skltn_bcount")
	var writed = e.ReadBool("ph_shell_writed")
	if(writed)
		ReadPhysicsShell(e);
	var attach = e.ReadBool("attach_with_joint")
	if(attach)
		; // ReadJoint?
	e.ReadFP32("footprint_size")
	e.ReadFP32("footprint_power")
}

function ReadHangingLamp(e)
{
	ReadCEntity(e);
	
	e.ReadBool("initial_state")
	e.ReadU8("die_sound_type")
	e.ReadHintStr("die_sound", "choose")
	e.ReadHintStr("die_particle", "choose")
	e.ReadHintStr("light_main_bone", "attp_str")
	e.ReadHintStr("dark_bone", "attp_str")
	e.ReadHintStr("broken_bone", "attp_str")
	
	// function load_light_data
	var l = e.ReadSection("main_light")
	l.ReadU8("type")
	l.ReadVec4("color", "color, vec4f")
	l.ReadFP32("brightness")
	l.ReadFP32("range_far")
	l.ReadVec3("data1")
	l.ReadVec2("data2")
	l.ReadFP32("ibl_gen_radius")
	l.ReadFP32("range_near")
	l.ReadFP32("source_size")
	l.ReadFP32("cone", "angle, fp32")
	l.ReadFP32("quality")
	l.ReadU32("min_ql")
	l.ReadU32("min_ql_shad")
	l.ReadVec3("position")
	l.ReadVec3("direction")
	l.ReadVec3("right")
	l.ReadHintStr("color_ca", "choose")
	l.ReadHintStr("texture", "choose")
	l.ReadHint("faces", "flags8")
	l.ReadU8("flags")
	l.ReadU8("light_flags1", "bool8")
	l.ReadU8("light_flags2", "bool8")

	e.ReadBool("color_to_aux")
	e.ReadBool("sync_color_to_aux")
	e.ReadBool("is_point_ambient")
	e.ReadHintStr("ambient_bone", "attp_str")
	e.ReadFP32("ambient_power")
	e.ReadFP32("ambient_radius")
	e.ReadHintStr("ambient_texture", "choose")
	e.ReadBool("backlight")
	e.ReadU16("backlight_ref", "entity_link, uobject_link")
	e.ReadFP32("backlight_dist")
	e.ReadBool("backlight_dynamic")
	e.ReadBool("backlight_ignore_parents")
	e.ReadFP32("backlight_force_offset")
	e.ReadHintStr("backlight_ray", "choose")
	e.ReadHintStr("backlight_ray_particles", "choose")
	e.ReadBool("backlight_trace_npc_only")
	e.ReadU16("master", "entity_link, uobject_link")
	e.ReadHintStr("flares", "choose")
	e.ReadHintStr("flares_bone", "choose")
	e.ReadU8("flares_axis")
}

function ReadUObject_AIPoint(e)
{
	ReadUObject(e)
	
	for(var i = 0 ; i < 4; i++)
	{
		var l = e.ReadSection("link_"+i)
		l.ReadU16("object", "entity_link, uobject_link")
		l.ReadFP32("weight")
	}
	
	e.ReadU8("ai_map", "bool8")
	e.ReadHintStr("cover_group", "choose")
}

function ReadPatrolPoint(e)
{
	ReadUObject_AIPoint(e)
	
	e.ReadU32("min_wait_time")
	e.ReadU32("max_wait_time")
	
	// patrol state
	e.ReadString("body_state")
	e.ReadString("anim_state")
	e.ReadString("movement_type")
	e.ReadString("weapon_state")
	e.ReadString("action");
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadHint("flags", "flags32")
	e.ReadU32("flags")
	e.ReadFP32("anim_state_approach_speed")
	e.ReadFP32("approaching_accel")
}

function ReadUObject_Restrictor(e)
{
	ReadUObject(e)
	
	e.ReadString("label")
	
	var k = 0, shapes = e.ReadArray("shapes")
	while(shapes.More())
	{
		var shape = shapes.ReadSection(RecStr("shape_", k++, 2), false)
		var type = shape.ReadU32("type")
		
		switch(type)
		{
			case 0:
				var s = shape.ReadSection("sphere")
				s.ReadVec3("center")
				s.ReadFP32("radius")
			break
			
			case 1:
				var b = shape.ReadSection("box")
				b.ReadMatrix43("", "pose, matrix")
				b.ReadVec3("h_size")
			break
			
			default:
				print("unknown shape type ", type)
		}
	}
	
	e.ReadU8("collisions_group")
	e.ReadU8("obstacle_collision_group")
	e.ReadU8("flags0", "bool8")
	e.ReadU8("scene_type")
	e.ReadHintStr("step_gmtrl", "choose")
	e.ReadU8("dynamic_mode")
}

function ReadUObject_Zone(e)
{
	ReadUObject_Restrictor(e)
	
	e.ReadHint("type_mask", "flags64")
	e.ReadU64("typee_mask")
	e.ReadBool("type_filter_on")
}

function ReadWaterZone(e)
{
	ReadUObject_Zone(e);

	e.ReadHint("texture", "texture, str_shared");
	e.ReadString("texture");
	e.ReadFP32("angle", "angle, fp32");
	e.ReadVec4("color", "color, vec4f");
	e.ReadFP32("opacity_near");
	e.ReadFP32("opacity_far");
	e.ReadFP32("cubemap_opacity");
	e.ReadFP32("transparency_depth");
	e.ReadFP32("fresnel");
	e.ReadFP32("density");
	e.ReadFP32("tile_x");
	e.ReadFP32("tile_y");
	e.ReadFP32("speed");
	e.ReadFP32("bump_intensity");
	e.ReadFP32("distort_intensity");
	e.ReadFP32("distort_distant_fade");
	e.ReadFP32("farplane");
	e.ReadFP32("rdist_lim");
	e.ReadBool("reflectplayer");
	e.ReadBool("sunshadow");
	e.ReadBool("envlit");
	e.ReadHint("material", "material, str_shared");
	e.ReadString("material");
}

function ReadUObject_Proxy(e)
{
	ReadUObject(e)
	
	var k = 0, entities = e.ReadArray("entities")
	while(entities.More())
	{
		var e = entities.ReadSection(RecStr("rec_", k++, 4), false)
		e.ReadU16("entity")
	}
}

function ReadUObject_Explosion(e)
{
	ReadUObject(e);
	e.ReadFP32("blast_radius_min");
	e.ReadFP32("blast_radius_max");
	e.ReadFP32("blast_hit_min");
	e.ReadFP32("blast_hit_max");
	e.ReadFP32("blast_up_throw_factor");
	e.ReadFP32("blast_hit_impulse_factor");
	e.ReadU8("blast_hit_type");
	e.ReadU8("blast_raycast_type");
	e.ReadU32("delay");
	e.ReadU32("flame_time");
	e.ReadU32("flame_interval");
	e.ReadU32("frags_num");
	e.ReadFP32("frags_radius");
	e.ReadFP32("frag_hit");
	e.ReadFP32("frag_hit_impulse");
	e.ReadFP32("frag_speed");
	
	var s = e.ReadSection("frag_ammo");
	s.ReadFP32("k_power");
	s.ReadFP32("k_impulse");
	s.ReadFP32("k_power_falloff");
	s.ReadFP32("k_impulse_falloff");
	s.ReadFP32("k_speed");
	s.ReadFP32("k_head_coef");
	s.ReadFP32("k_fire_distance_min");
	s.ReadFP32("k_fire_distance_max");
	s.ReadFP32("k_file_distance");
	s.ReadFP32("k_pierce");
	s.ReadFP32("k_tracer_scale_xy");
	s.ReadFP32("k_tracer_scale_z");
	s.ReadFP32("k_tracer_min_dist");
	s.ReadString("k_material");
	s.ReadBool("k_shoot_through");
	s.ReadFP32("blast_radius_min");
	s.ReadFP32("blast_radius_max");
	s.ReadFP32("blast_hit_min");
	s.ReadFP32("blast_hit_max");
	s.ReadFP32("blast_up_throw_factor");
	s.ReadFP32("blast_hit_impulse_factor");
	s.ReadU8("blast_hit_type");
	s.ReadU8("blast_raycast_type");
	s.ReadHint("tracer_mesh", "ref_model");
	s.ReadString("tracer_mesh");
	s.ReadHint("tracer_mesh_hud", "ref_model");
	s.ReadString("tracer_mesh_hud");
	s.ReadHint("bullet_particles", "particles, str_shared");
	s.ReadString("bullet_particles");
	s.ReadHint("impact_particles", "particles, str_shared");
	s.ReadString("impact_particles");
	s.ReadHint("trail_mesh", "ref_model");
	s.ReadString("trail_mesh");
	s.ReadU32("trail_time");
	s.ReadVec2("trail_beg_scale_xy");
	s.ReadVec2("trail_end_scale_xy");
	s.ReadFP32("wm_size");
	s.ReadFP32("k_monster_coef");
	s.ReadString("ammo_material");
	s.ReadU8("flags0", "bool8");
	s.ReadFP32("k_disp");
	s.ReadS32("buck_shot");
	s.ReadU32("type");
	
	e.ReadU8("hit_frag");
	e.ReadVec4("light_color", "color, vec4f");
	e.ReadFP32("light_range");
	e.ReadU32("light_time_max");
	e.ReadU32("explode_duration_max");
	e.ReadString("label");
}

function ReadForceField(e)
{
	ReadUObject(e);
	
	var volumes = e.ReadArray("volumes");
	for(var i = 0; volumes.More(); i++)
	{
		var v = volumes.ReadSection(RecStr("volume_", i, 2), false);
		var type = v.ReadU32("type");
		
		switch(type)
		{
			case 0:
				var s = v.ReadSection("sphere");
				s.ReadVec3("center");
				s.ReadFP32("radius");
			break;
			case 1:
				var b = v.ReadSection("box");
				b.ReadMatrix43("", "pose, matrix");
				b.ReadVec3("h_size");
			break;
			case 2:
				var c = v.ReadSection("capsule");
				c.ReadVec3("center");
				c.ReadFP32("radius");
				c.ReadFP32("height");
			break;
			default:
				print("unknown volume type " + type);
		}
	}
	
	e.ReadBool("active");
	e.ReadU8("type");
	for(var j = 0; j < 9; j++)
		e.ReadFP32("param_"+j);
	e.ReadU8("flags0", "bool8");
	e.ReadVec3("rotation_power");
	e.ReadU8("anim_type");
}

function ReadLadder(e)
{
	ReadUObject_Restrictor(e);
	
	e.ReadFP32("threshold_offset");
	e.ReadU8("detach_side");
	e.ReadFP32("detach_distance");
	e.ReadFP32("detach_speed");
	e.ReadBool("enabled");
	e.ReadBool("custom_movement");
	e.ReadBool("custom_civil_mode");
	e.ReadFP32("up_angle", "angle, fp32");
	e.ReadFP32("down_angle", "angle, fp32");
}

function ReadBreakableIce(e)
{
	ReadUObject_Static(e);
	
	e.ReadVec4("sphere");
	e.ReadMatrix43("crack_particle_offset", "pose, matrix_43T");
	e.ReadMatrix43("push_particle_offset", "pose, matrix_43T");
	e.ReadBool("cracked");
}

function ReadVRCube(e)
{
	function ReadAuxParams(e, n)
	{
		var r = e.ReadSection(n)
		
		r.ReadVec4("color", "color, vec4f")
		r.ReadVec3i("blend_time")
		r.ReadU8("flags", "bool8")
	}
	
	ReadUObject_Effect(e)
	
	e.ReadVec3("displacement_scale")
	e.ReadFP32("displacement_speed")
	e.ReadU8("flags", "bool8")
	
	ReadAuxParams(e, "highlight_default_section")
	ReadAuxParams(e, "highlight_by_name_section")
	ReadAuxParams(e, "highlight_player_section")
	ReadAuxParams(e, "highlight_by_group_section")
	
	e.ReadU16("target", "entity_link, uobject_link")
	e.ReadString("group_name")
	e.ReadFP32("distance_to_target")
	e.ReadFP32("distance_to_player")
	e.ReadFP32("distance_to_group_member")
}

var entity_readers = {
	// basic
	"STATICPROP" 		: ReadUObject_Static,
 	//"STATICPROP_BREAKABLE" : ReadUObject_StaticBreakable,
	"EFFECT" 				: ReadUObject_Effect,
	"EFFECTM"				: ReadUObject_Effect_MLeaf,
	"O_ENTITY" 			: ReadCEntity,
	"o_hlamp" 			: ReadHangingLamp,
	"O_AIPOINT" 		: ReadUObject_AIPoint,
	"PATROL_POINT" 	: ReadPatrolPoint,
	"VISUALSCRIPT"	: ReadUObject,
	"O_BASEZONE"		: ReadUObject_Zone,
	//"O_WATERZONE"		: ReadWaterZone,
	"PROXY"					: ReadUObject_Proxy,
	//"SOFT_ENTITY"		: ReadSoftEntity,
	//"O_INTEREST"		: function(e)
	//{
	//	ReadUObject(e);
	//	ReadInterest(e);
	//},
	//"O_EXPLOSION"		: ReadUObject_Explosion,
	//"FORCE_FIELD"		: ReadForceField,
	//"LADDER"				: ReadLadder,
	//"BREAKABLE_ICE"	: ReadBreakableIce,
	"VR_CUBE"				: ReadVRCube
};

function ReadStartup(s)
{
	function ReadPresetBlend(e)
	{
		e.ReadString("name")
		e.ReadBool("game_time")
		e.ReadBool("removing")
		e.ReadS32("start")
		e.ReadS32("finish")	
	}
	
	s.ReadU32("game_time")
	s.ReadFP32("dc_prev_speed")
	s.ReadFP32("dc_target_speed")
	s.ReadFP32("dc_curr_speed")
	s.ReadS32("dc_time_0")
	s.ReadS32("dc_time_1")
	s.ReadString("current")
	ReadPresetBlend(s)
	var modifiers = s.ReadArray("modifiers")
	for(var i = 0; modifiers.More(); i++)
		ReadPresetBlend(modifiers.ReadSection(RecStr("rec_", i, 4), false))
	s.ReadHintStr("weather_preset", "choose")
	ReadTime(s, "dc_start")
	ReadTime(s, "dc_duration")
	s.ReadHintStr("foliage_set", "choose")
	s.ReadFP32("foliage_fuzziness")
	s.ReadVec3("map_positional_min")
	s.ReadVec3("map_positional_max")
	s.ReadBool("map_positional_preview_aabb")
	s.ReadHintStr("map_menu_name", "choose")
	s.ReadU8("map_rotation")
	s.ReadHintStr("subst_tablet_model", "choose")
	s.ReadFP32("dao_base")
	s.ReadHintStr("next_level", "choose")
	s.ReadHintStr("back_music", "choose")
	var wil = s.ReadArray("weapon_items_link")
	for(var j = 0; wil.More(); j++)
		wil
			.ReadSection(RecStr("rec_", j, 4), false)
				.ReadU16("items", "entity_link, uobject_link")
}

function ReadEntities(entities)
{
	var already_listed = new Object
	while(entities.More())
	{
		var e = entities.ReadSection()
		
		// common params
		//var classcrc = e.ReadU32("class")
		//var static_data_keycrc = e.ReadU32("static_data_key")
		var _class = e.ReadStringCrc("class", typed_strings.get_class)
		var _static_data_key = e.ReadStringCrc("static_data_key", typed_strings.get_static_data_key)
		e.ReadHintStr("att_bone_id", "choose")
		e.ReadU16("id")
		e.ReadU16("parent_id")
		e.ReadMatrix43("att_offset", "pose, matrix_43T")
		e.ReadBool("att_root")
		
		//var _class = typed_strings.get_class(classcrc);
		//var _static_data_key = typed_strings.get_static_data_key(static_data_keycrc);
		
		//print(_class, " ", _static_data_key);
		
		if(entity_readers[_class])
		{
			entity_readers[_class](e)
	
			if(e.More())
				print(_class, " data left")
		}
		else
		{
			if(already_listed[_class] != true)
			{
				print("not implemented class ", _class)
				already_listed[_class] = true
			}
		}
	}
}

