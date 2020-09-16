// interface
this.ReadStartup = ReadStartup;
this.ReadEntities = ReadEntities;

// implementation
var typed_strings = module("exodus\\typed_strings");
//module("visualscript");

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
	i.ReadFP32("max_angle_x", "angle, fp32")
	i.ReadFP32("max_angle_y", "angle, fp32")
	i.ReadFP32("angle_coef")
}

function ReadVsRef(e, name)
{
	e.ReadHintStr(name, "choose")
	var exist = e.ReadBool(name + "_dyn_state_exist")
	
	if(exist)
	{
		print("!!!! VS REF dyn_state_exist not implemented")
	}
}

function ReadCommonsVs(e, name)
{
	var k = 0, arr = e.ReadArray(name)
	while(arr.More())
	{
		var vs = arr.ReadSection(RecStr("rec_", k++, 4), false)

		vs.ReadString("vs_name")
		vs.ReadBool("vs_debug")
		vs.ReadBool("vs_active")
		vs.ReadBool("disable_qsave")
		vs.ReadBool("save_on_nextlevel")
		ReadVsRef(vs, "vs_ref")
	}
}

function ReadPhysicsShell(e)
{
	var p = e.ReadSection("physics_shell")
	
	var elements = p.ReadArray("elements")
	for(var i = 0; elements.More(); i++)
	{
		var l = elements.ReadSection(RecStr("rec_", i, 4), false)
		l.ReadU16("root_bid")
		l.ReadFP32("accumulated_impulse")
		l.ReadMatrix43("xform", "pose, matrix_43T")
		l.ReadVec3("velocity")
		l.ReadBool("nx_awake")
		var shapes = l.ReadArray("shapes")
		for(var j = 0; shapes.More(); j++)
		{
			var s = shapes.ReadSection(RecStr("rec_", j, 4), false)
			s.ReadU16("bid")
		}
	}
}

function ReadUObject(e)
{
	e.ReadName("name")
	e.ReadU8("oflags", "bool8")
	e.ReadU8("sflags", "bool8")
	e.ReadFP32("cull_distance")
	e.ReadMatrix43("", "pose, matrix")
	e.ReadHintStr("visual", "ref_model")
	e.ReadU16("dao_val")
	e.ReadVec4("render_aux_val", "color, vec4f")
	
	//visualscript.ReadVssVer6(e)
	e.TryReadArray("vss_ver_7") || e.ReadArray("vss_ver_6") // TODO
	
	e.ReadBool("vs_active")
	e.ReadU16("spatial_sector")
	e.ReadU8("qsave_chunk")
}

function ReadUObject_Static(e)
{
	ReadUObject(e)
		
	e.ReadU8("flags", "bool8")
	e.ReadU8("collision_group")
	
	ReadInterest(e)
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
	ReadCommonsVs(e, "commons_vs")
}

function ReadHelperText(e)
{
	ReadUObject(e)
	
	e.ReadString("text")
	e.ReadHintStr("text_key", "choose")
	e.ReadFP32("size")
	e.ReadVec4("color", "color, vec4f")
	e.ReadHintStr("font", "choose")
	e.ReadU8("flags0", "bool8")
	e.ReadFP32("width")
	e.ReadFP32("height")
	e.ReadU8("h_alignment")
	e.ReadFP32("display_dist")
}

function ReadUObject_Effect(e)
{
	ReadUObject(e)
	
	ReadCommonsVs(e, "commons_vs")
	ReadCommonsVs(e, "removed_vs")
	
	e.ReadHintStr("startup_animation", "animation_str")
	e.ReadHintStr("bone_part", "part_str")
	e.ReadU16("start_frame")
	e.ReadFP32("speed")
	e.ReadU8("startup_animation_flags", "bool8")
	e.ReadU8("force_looped")
	e.ReadHintStr("sound", "sound")
	e.ReadU8("sound_volume", "fp32_q8")
	e.ReadU8("sound_filter")
	e.ReadU8("particle_flags", "bool8")
	e.ReadHintStr("particles", "choose")
	ReadInterest(e)
	e.ReadStrArray32("labels")
}

function ReadUObject_Effect_MLeaf(e)
{
	ReadUObject_Effect(e)
	
	// in Arktika.1 type was color, u32
	e.ReadVec4("particles_color", "color, vec4f")
}

function ReadCEntity(e)
{
	// class uobject_hit<>
	e.ReadFP32("health")
	
	// unknown class
	e.ReadU32("dying_mask")
	e.ReadU8("physics_flags", "bool8")
	e.ReadU8("physics_flags1", "bool8")
	e.ReadU8("physics_flags2", "bool8")
	
	ReadUObject_Effect(e);
	
	// class centity
	e.ReadU8("friend_type")
	e.ReadU8("reaction_type")
	e.ReadHintStr("fixed_bones", "choose_array, str_shared")
	e.ReadFP32("break_impulse_threshold")
	e.ReadU8("collisions_group")
	e.ReadU8("scene_type")
	e.ReadHintStr("break_particles_break", "choose")
	e.ReadHintStr("break_particles_death", "choose")
	e.ReadHintStr("break_sound_death", "choose")
	e.ReadU8("break_sound_death_ai_type")
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
	{
		var j = e.ReadSection("joint_section")
		j.ReadBool("enabled")
		j.ReadU16("entity_src", "entity_link, uobject_link")
		j.ReadHintStr("bone_src", "attp_str")
		j.ReadU16("entity_dst", "entity_link, uobject_link")
		j.ReadHintStr("bone_dst", "attp_str")
		j.ReadVec3("pos")
		j.ReadVec3("rot", "ang3f")
		// TODO uncomplete
	}
	e.ReadFP32("footprint_size")
	e.ReadFP32("footprint_power")
}

function ReadLamp(e)
{
	e.ReadBool("initial_state")
	e.ReadU8("die_sound_type")
	e.ReadHintStr("die_sound", "choose")
	e.ReadHintStr("die_particle", "choose")
	e.ReadHintStr("light_main_bone", "attp_str")
	e.ReadHintStr("dark_bone", "attp_str")
	e.ReadHintStr("broken_bone", "attp_str")
	
	var l = e.ReadSection("main_light")
	l.ReadU8("type")
	l.ReadVec4("color", "color, vec4f")
	l.ReadFP32("brightness")
	l.ReadFP32("range_far")
	l.ReadFP32("lod_scale")
	l.ReadVec3("data1")
	l.ReadVec2("data2")
	l.ReadFP32("ibl_gen_radius")
	l.ReadFP32("range_near")
	l.ReadFP32("source_size")
	l.ReadFP32("cone", "angle, fp32")
	l.ReadFP32("quality")
	l.ReadVec3("position")
	l.ReadVec3("direction")
	l.ReadVec3("right")
	l.ReadHintStr("color_ca", "choose")
	l.ReadHintStr("texture", "choose")
	l.ReadHint("faces", "flags8")
	l.ReadU8("faces")
	l.ReadU8("light_flags1", "bool8")
	l.ReadU8("light_flags2", "bool8")
	
	e.ReadBool("color_to_aux")
	e.ReadBool("sync_color_to_aux")
	e.ReadU8("secondary_type")
	e.ReadHintStr("secondary_bone", "attp_str")
	e.ReadFP32("secondary_power")
	e.ReadFP32("secondary_radius")
	e.ReadBool("secondary_mul_by_ao")
	e.ReadBool("backlight")
	e.ReadU16("backlight_ref", "entity_link, uobject_link")
	e.ReadFP32("backlight_dist")
	e.ReadBool("backlight_dynamic")
	e.ReadBool("backlight_ignore_parents")
	e.ReadBool("backlight_brightness_compensation")
	e.ReadFP32("backlight_force_offset")
	e.ReadHintStr("backlight_ray", "choose")
	e.ReadHintStr("backlight_ray_particles", "choose")
	e.ReadBool("backlight_trace_npc_only")
	e.ReadU16("master", "entity_link, uobject_link")

	var f = e.ReadSection("flares_data")
	f.ReadHintStr("name", "choose")
	f.ReadHintStr("bone", "choose")
	f.ReadU8("axis")
	f.ReadVec4("cmul", "color, vec4f")
}

function ReadHangingLamp(e)
{
	ReadCEntity(e)
	ReadLamp(e)
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
	e.ReadHint("cover_group", "choose")
	e.ReadString("cover_group")
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
	e.ReadString("action")
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
			break;
			
			case 1:
				var b = shape.ReadSection("box")
				b.ReadMatrix43("", "pose, matrix")
				b.ReadVec3("h_size")
			break;
			
			default:
				print("unknown shape type ", type)
		}
	}
	
	e.ReadU8("collisions_group")
	e.ReadU8("obstacle_collision_group")
	e.ReadU8("flags0", "bool8")
	e.ReadU8("block_ai_vision")
	e.ReadU8("scene_type")
	e.ReadHint("step_gmtrl", "choose")
	e.ReadString("step_gmtrl")
	e.ReadU8("dynamic_mode")
}

function ReadUObject_Zone(e)
{
	ReadUObject_Restrictor(e)
	e.ReadHint("type_mask", "flags64")
	e.ReadU64("type_mask")
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
	
	// class uobject_proxy
	e.ReadU16("slice_count")
	var k = 0, entities = e.ReadArray("entities")
	while(entities.More())
	{
		var e = entities.ReadSection(RecStr("rec_", k++, 4), false)
		e.ReadU16("entity")
	}
}

function ReadInventoryItemObject(e)
{
	// class inventory_item
	e.ReadU8("flags0", "bool8")
	e.ReadU16("trade_weight")
	e.ReadU8("ui_force_slot_id")

	ReadCEntity(e)
}

function ReadUpgradeItem(e)
{
	ReadInventoryItemObject(e)
	
  // class chud_item_container (probably)
	e.ReadBool("anim_simplification")
	
	// class upgrade_item
	e.ReadHintStr("upgrade_id", "choose")
	e.ReadBool("free_on_level")
}

function ReadUpgradeItemMagazine(e)
{
	e.ReadBool("loot_ammo_limited_by_mag_size")
	e.ReadU16("loot_ammo_count")
	
	ReadUpgradeItem(e)
}

function ReadUpgradeItemLamp(e)
{
	ReadUpgradeItem(e)
	ReadLamp(e)
}

function ReadNpcBase(e)
{
	e.ReadBool("fully_dead")
	e.ReadU8("dying_from")
	
	ReadCEntity(e)
	
	e.ReadU8("base_npc_flags", "bool8")
	e.ReadU8("base_npc_flags1", "bool8")
	e.ReadFP32("luminocity")
	e.ReadU32("min_corpse_lum_update_interval")
	e.ReadU32("max_corpse_lum_update_interval")
	e.ReadFP32("shoot_dispersion_coef")
	e.ReadFP32("hit_power_coef")
	e.ReadFP32("hit_impulse_coef")
	e.ReadFP32("illuminated_react_level")
	e.ReadS32("offmesh_penalty")
	e.ReadU32("offmesh_reuse_delay")
	e.ReadU32("offmesh_inter_reuse_delay")
	e.ReadBool("stick_to_ground")
	e.ReadFP32("stick_to_ground_max_step")
	e.ReadFP32("stick_to_ground_ai_max_max_upward")
	e.ReadFP32("stick_to_ground_ai_max_max_downward")
	
	// virtual function load_dynamic_dhps
	{
		e.ReadU32("damage_handle_preset")
	}
		
	ReadVsRef(e, "movement_hit_vs_ref")
	
	// base_brain_unit
	e.ReadU8("anim_state")
	e.ReadU8("movement_type")
	e.ReadU8("min_movement_type")
	e.ReadU8("max_movement_type")
	e.ReadU8("body_state")
	e.ReadFP32("step_acceleration")
	e.ReadHint("group", "stringcrc")
	e.ReadU32("group")
	e.ReadU8("flags0", "bool8")
	
	// base_visual_memory_manager
	e.ReadBool("vision_enabled");
	e.ReadFP32("free_vision_range");
	e.ReadFP32("free_vision_bwd_range");
	e.ReadFP32("free_vision_fov", "angle, fp32");
	e.ReadU32("free_vision_min_update_time");
	e.ReadFP32("free_vision_luminocity_fac");
	e.ReadFP32("free_vision_luminocity_factor");
	e.ReadFP32("free_vision_velocity_fac");
	e.ReadFP32("free_vision_decrease_value");
	e.ReadFP32("free_vision_min_lum_threshold");
	e.ReadFP32("free_vision_min_threshold");
	e.ReadFP32("free_vision_threat_threshold");
	e.ReadFP32("free_vision_light_alert_threshold");
	e.ReadFP32("free_vision_vis_threshold");
	e.ReadFP32("free_vision_lum_min_distance");
	e.ReadFP32("free_vision_force_danger_range")
	e.ReadFP32("free_vision_surv_thread_threshold")
	e.ReadFP32("free_vision_surv_light_alert_threshold")
	e.ReadFP32("free_vision_surv_vis_threshold")
	e.ReadFP32("free_vision_surv_lum_min_distance")
	e.ReadFP32("free_vision_peripheral_dist")
	e.ReadFP32("free_vision_peripheral_angle_x", "angle, fp32")
	e.ReadFP32("free_vision_peripheral_angle_y", "angle, fp32")
	e.ReadFP32("free_vision_peripheral_lum_min_distance")
	e.ReadU32("free_vision_transition_time")
	e.ReadFP32("alert_vision_range");
	e.ReadFP32("alert_vision_bwd_range");
	e.ReadFP32("alert_vision_fov", "angle, fp32");
	e.ReadU32("alert_vision_min_update_time");
	e.ReadFP32("alert_vision_luminocity_fac");
	e.ReadFP32("alert_vision_luminocity_factor");
	e.ReadFP32("alert_vision_velocity_fac");
	e.ReadFP32("alert_vision_decrease_value");
	e.ReadFP32("alert_vision_min_lum_threshold");
	e.ReadFP32("alert_vision_min_threshold");
	e.ReadFP32("alert_vision_threat_threshold");
	e.ReadFP32("alert_vision_light_alert_threshold");
	e.ReadFP32("alert_vision_vis_threshold");
	e.ReadFP32("alert_vision_lum_min_distance");
	e.ReadFP32("alert_vision_force_danger_range")
	e.ReadFP32("alert_vision_surv_thread_threshold")
	e.ReadFP32("alert_vision_surv_light_alert_threshold")
	e.ReadFP32("alert_vision_surv_vis_threshold")
	e.ReadFP32("alert_vision_surv_lum_min_distance")
	e.ReadFP32("alert_vision_peripheral_dist")
	e.ReadFP32("alert_vision_peripheral_angle_x", "angle, fp32")
	e.ReadFP32("alert_vision_peripheral_angle_y", "angle, fp32")
	e.ReadFP32("alert_vision_peripheral_lum_min_distance")
	e.ReadU32("alert_vision_transition_time")
	e.ReadFP32("danger_vision_range");
	e.ReadFP32("danger_vision_bwd_range");
	e.ReadFP32("danger_vision_fov", "angle, fp32");
	e.ReadU32("danger_vision_min_update_time");
	e.ReadFP32("danger_vision_luminocity_fac");
	e.ReadFP32("danger_vision_luminocity_factor");
	e.ReadFP32("danger_vision_velocity_fac");
	e.ReadFP32("danger_vision_decrease_value");
	e.ReadFP32("danger_vision_min_lum_threshold");
	e.ReadFP32("danger_vision_min_threshold");
	e.ReadFP32("danger_vision_threat_threshold");
	e.ReadFP32("danger_vision_light_alert_threshold");
	e.ReadFP32("danger_vision_vis_threshold");
	e.ReadFP32("danger_vision_lum_min_distance");
	e.ReadFP32("danger_vision_force_danger_range")
	e.ReadFP32("danger_vision_surv_thread_threshold")
	e.ReadFP32("danger_vision_surv_light_alert_threshold")
	e.ReadFP32("danger_vision_surv_vis_threshold")
	e.ReadFP32("danger_vision_surv_lum_min_distance")
	e.ReadFP32("danger_vision_peripheral_dist")
	e.ReadFP32("danger_vision_peripheral_angle_x", "angle, fp32")
	e.ReadFP32("danger_vision_peripheral_angle_y", "angle, fp32")
	e.ReadFP32("danger_vision_peripheral_lum_min_distance")
	e.ReadU32("danger_vision_transition_time")
	
	// base_sound_memory_manager
	e.ReadBool("sound_enabled")
	e.ReadFP32("free_disturb_threshold")
	e.ReadFP32("free_light_alert_threshold")
	e.ReadFP32("free_alert_threshold")
	e.ReadFP32("free_danger_threshold")
	e.ReadFP32("alert_disturb_threshold")
	e.ReadFP32("alert_light_alert_threshold")
	e.ReadFP32("alert_alert_threshold")
	e.ReadFP32("alert_danger_threshold")
	e.ReadFP32("danger_disturb_threshold")
	e.ReadFP32("danger_light_alert_threshold")
	e.ReadFP32("danger_alert_threshold")
	e.ReadFP32("danger_danger_threshold")
	
	// base_threat_memory_manager
	e.ReadU32("la_free_interia_time")
	e.ReadU32("a_free_inertia_time")
	e.ReadU32("ua_free_inertia_time")
	e.ReadU32("d_la_sounds_all_time")
	e.ReadU32("d_la_sounds_with_gap")
	e.ReadU32("d_la_sounds_gap")
	e.ReadU32("la_a_vision_all_time")
	e.ReadU32("la_a_vision_with_gap")
	e.ReadU32("la_a_vision_gap")
	e.ReadU32("la_a_sounds_all_time")
	e.ReadU32("la_a_sounds_with_gap")
	e.ReadU32("la_a_sounds_gap")
	e.ReadU32("a_ua_sounds_all_time")
	e.ReadU32("a_ua_sounds_with_gap")
	e.ReadU32("a_ua_sounds_gap")
	e.ReadU32("a_ua_vision_all_time")
	e.ReadU32("a_ua_vision_with_gap")
	e.ReadU32("a_ua_vision_gap")
	e.ReadU32("danger_ua_inertia_time")
	e.ReadBool("ua_free_corpse_block")
	
	// base_sound_player
	e.ReadHintStr("sound_scheme", "choose")
	
	// again base_brain_unit
	e.ReadFP32("max_heading", "angle, fp32")
	e.ReadFP32("max_pitch", "angle, fp32")
	
	// ???
	e.ReadU8("species_flags", "bool8")
	e.ReadU32("species_type")
	e.ReadFP32("npc_threat_level")
	e.ReadFP32("npc_courage_threshold")
	e.ReadFP32("npc_fear_threshold")
	e.ReadFP32("species_attack_confusion_dist")
	e.ReadFP32("species_fallback_cover_dist")
	e.ReadU32("species_fallback_cover_fear")
	e.ReadU32("species_fallback_cover")
	e.ReadFP32("species_threat_can_stand_dist")
	e.ReadU32("species_min_fallback_time")
	e.ReadHint("species_enemy_blacklist", "identifier_array") // ??? probably str_array32
	e.ReadU32("species_enemy_blacklist") // ??? probably str_array32
	e.ReadHint("species_attack_blacklist", "identifier_array") // ??? probably str_array32
	e.ReadU32("species_attack_blacklist") // ??? probably str_array32
	var s = e.ReadSection("species_behavior_task") // empty section
	
	// ?????
	e.ReadU8("smell_flags", "bool8")
	e.ReadFP32("smell_radius")
	e.ReadU32("smell_interval")
	e.ReadU8("smell_threat_type")
}

function ReadSimpleNpc(e)
{
	ReadNpcBase(e)
	
	e.ReadBool("ph_dying")
	
	// ?????
	var wed = e.ReadSection("waves_emitter_data")
	wed.ReadU32("respawn_time")
	wed.ReadFP32("start_len")
	wed.ReadFP32("start_height")
	wed.ReadVec3("start_offset")
	wed.ReadBool("waves_active")
}

function ReadPlayer(e)
{
	ReadNpcBase(e)
	
	e.ReadHint("human_flags", "flags16")
	e.ReadU16("human_flags")
}

var entity_readers = {
	// basic
	"STATICPROP" 		: ReadUObject_Static,
// 	"STATICPROP_BREAKABLE" : ReadUObject_StaticBreakable,
	"EFFECT" 				: ReadUObject_Effect,
	"EFFECTM"				: ReadUObject_Effect_MLeaf,
	"O_ENTITY" 			: ReadCEntity,
	"SCRIPTED_ENTITY" : ReadCEntity,
	"o_hlamp" 			: ReadHangingLamp,
	"O_AIPOINT" 		: ReadUObject_AIPoint,
	"PATROL_POINT" 	: ReadPatrolPoint,
	"VISUALSCRIPT"	: ReadUObject,
	"O_BASEZONE"		: ReadUObject_Zone,
//	"O_WATERZONE"		: ReadWaterZone,
	"PROXY"					: ReadUObject_Proxy,
//	"SOFT_ENTITY"		: ReadSoftEntity,
	"O_HELPERTEXT"	: ReadHelperText,

	// flora & fauna
	"WOMAN"					: ReadSimpleNpc,

	// upgrades
	"WEAPON_ITEM_LAMP"				: ReadUpgradeItemLamp,
	"WEAPON_ITEM_LAMP_BACKLIGHT" : ReadUpgradeItemLamp,
	"DEVICE_UPGRADE_LAMP"			: ReadUpgradeItemLamp,
	"WICK_VISUAL"							: ReadUpgradeItem,
	"LIGHTER_VISUAL"					: ReadUpgradeItem,
	"COSTUME_UPGRADE"					: ReadUpgradeItem,
	"COMPASS"									: ReadUpgradeItem,
	"DEVICE_UPGRADE"					: ReadUpgradeItem,
	"VISOR"										: ReadUpgradeItem,
	"PLAYER_TIMER_UPGRADE" 		: ReadUpgradeItem,
	"MAP_PAD_UPGRADE"					: ReadUpgradeItem,
	"METAL_DETECTOR_UPGRADE" 	: ReadUpgradeItem,
	"MOTION_SENSOR_UPGRADE" 	: ReadUpgradeItem,
	"PULSOMETER_UPGRADE" 			: ReadUpgradeItem,
	"WEAPON_ITEM"							: ReadUpgradeItem,
	"WEAPON_ITEM_AMMO" 				: ReadUpgradeItem,
	"WEAPON_ITEM_SILENCER" 		: ReadUpgradeItem,	
	"WEAPON_ITEM_OPTIC" 			: ReadUpgradeItem,
	"WEAPON_ITEM_LASER" 			: ReadUpgradeItem,
	"WEAPON_ITEM_VR" 					: ReadUpgradeItem,
	"WEAPON_ITEM_PRESET" 			: ReadUpgradeItem,
	"WEAPON_ITEM_MAGAZINE" 		: function(e)
	{
		ReadUpgradeItemMagazine(e)
		e.ReadBool("ignore_difficulty")
	},
	"WEAPON_ITEM_SPEEDLOADER" : ReadUpgradeItemMagazine,
	"WEAPON_ITEM_VR_ATTACH" 	: function(e)
	{
		ReadUpgradeItem(e)
		e.ReadHint("preview_model", "choose")
		e.ReadString("preview_model")
	}
};

function ReadStartup(s, entity_ver)
{
	function ReadPresetBlend(e)
	{
		e.ReadString("name")
		e.ReadBool("game_time")
		e.ReadBool("removing")
		e.ReadS32("start")
		e.ReadS32("finish")	
	}
	
	function ReadPresetTablet(e)
	{
		e.ReadVec3("map_positional_min")
		e.ReadVec3("map_positional_max")
		e.ReadBool("map_positional_preview_aabb")
		e.ReadHintStr("map_menu_name", "choose")
		e.ReadU8("map_rotation")
		e.ReadString("subst_tablet_model")
	}

	s.ReadU32("game_time")
	s.ReadFP32("dc_prev_speed")
	s.ReadFP32("dc_target_speed")
	s.ReadFP32("dc_curr_speed")
	s.ReadS32("dc_time_0")
	s.ReadS32("dc_time_1")
	s.ReadString("main_preset_name")
	s.ReadString("current")
	ReadPresetBlend(s)
	s.ReadU8("source_type")
	
	var modifiers = s.ReadArray("modifiers")
	for(var i = 0; modifiers.More(); i++)
	{
		var m = modifiers.ReadSection(RecStr("rec_", i, 4), false);
		ReadPresetBlend(m);
	}
	
	s.ReadU64("_total_time")
	s.ReadHintStr("weather_preset", "choose")
	ReadTime(s, "dc_start")
	ReadTime(s, "dc_duration")
	s.ReadHintStr("foliage_set", "choose")
	s.ReadFP32("foliage_fuzziness")
	
	if(entity_ver >= 53) // DLC Sam Story
	{
		s.ReadHint("raytrace_options", "flags32")
		s.ReadU32("raytrace_options")
		s.ReadU8("tablet_presets_count")
		
		var presets = s.ReadArray("tablet_presets_vector")
		for(var i = 0; presets.More(); i++)
			ReadPresetTablet(presets.ReadSection(RecStr("rec_", i, 4), false))
	}
	else
	{
		ReadPresetTablet(s)
	}
	
	s.ReadString("next_level")
	s.ReadString("back_music")
	s.ReadU16("migration_rules")
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
			entity_readers[_class](e);
	
			if(e.More())
				print(_class, " data left");
		}
		else
		{
			if(already_listed[_class] != true)
			{
				print("not implemented class ", _class);
				already_listed[_class] = true;
			}
		}
	}
}

