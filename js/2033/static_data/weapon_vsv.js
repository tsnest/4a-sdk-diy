// Auto-generated file

reader.ReadU16("version")
reader.ReadU32("slot")
reader.ReadFP32("control_inertion_factor")
reader.ReadHint("flags", "flags8")
reader.ReadU8("flags")
reader.ReadU8("slot_max_num")
reader.ReadU32("animation_slot")
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
reader.ReadString("hr_class")
reader.ReadString("using_action")
reader.ReadString("using_action_replace")
reader.ReadFP32("take_impulse")
reader.ReadHintStr("take_sound", "sound")
var section2 = reader.ReadSection("icon")
section2.ReadString("texture")
section2.ReadVec4("tex_rect")
section2.ReadString("technics")
section2.ReadString("shader")
section2.ReadVec4("wnd_rect")
section2.ReadVec4i("color")
section2.ReadU32("scale")
section2.ReadU8("h_alignment")
section2.ReadU8("v_alignment")
section2.ReadVec2("icon_offset")
section2.ReadFP32("rotate_speed")
section2.ReadVec2("font_pos")
reader.ReadFP32("dispersion_base")
reader.ReadFP32("dispersion_aim")
reader.ReadFP32("dispersion_move_factor")
reader.ReadFP32("dispersion_inc")
reader.ReadFP32("dispersion_min")
reader.ReadFP32("dispersion_max")
reader.ReadFP32("dispersion_dec")
var section3 = reader.ReadSection("axis_disp")
section3.ReadFP32("left_axis")
section3.ReadFP32("up_axis")
section3.ReadFP32("right_axis")
section3.ReadFP32("down_axis")
reader.ReadFP32("hit_power")
reader.ReadFP32("hit_impulse")
reader.ReadFP32("fire_distance")
reader.ReadFP32("bullet_speed")
reader.ReadFP32("min_radius")
reader.ReadFP32("max_radius")
reader.ReadString("fire_bone")
reader.ReadString("shell_bone")
reader.ReadVec4("light_color", "color, vec4f")
reader.ReadFP32("light_range")
reader.ReadFP32("light_var_color")
reader.ReadFP32("light_var_range")
reader.ReadFP32("light_time")
reader.ReadString("light_coloranim")
reader.ReadVec3("light_point")
reader.ReadHintStr("flame_particles", "particles, str_shared")
reader.ReadHintStr("smoke_particles", "particles, str_shared")
reader.ReadHintStr("shot_particles", "particles, str_shared")
reader.ReadHintStr("shell_particles", "particles, str_shared")
reader.ReadFP32("shell_occlusion_distance")
reader.ReadBool("shell_drop")
reader.ReadFP32("rpm")
reader.ReadHintStr("hud_visual", "ref_model")
reader.ReadString("anim_time")
reader.ReadBool("ad_scope")
reader.ReadBool("ad_silencer")
reader.ReadBool("ad_barrel")
reader.ReadBool("ad_barrel_2")
reader.ReadBool("ad_butt")
reader.ReadBool("ad_pivot")
reader.ReadBool("ad_bayonet")
reader.ReadBool("ad_laser")
reader.ReadBool("ad_laser_2")
reader.ReadBool("ad_magazine")
reader.ReadBool("ad_magazine_2")
reader.ReadString("ad_scope_bone")
reader.ReadString("ad_silencer_bone")
reader.ReadString("ad_barrel_bone")
reader.ReadString("ad_barrel_2_bone")
reader.ReadString("ad_butt_bone")
reader.ReadString("ad_pivot_bone")
reader.ReadString("ad_bayonet_bone")
reader.ReadString("ad_laser_bone")
reader.ReadString("ad_laser_2_bone")
reader.ReadString("ad_magazine_bone")
reader.ReadString("ad_magazine_2_bone")
var section4 = reader.ReadSection("ad_scope_params")
section4.ReadFP32("holder_range_modifier")
section4.ReadFP32("holder_fov_modifier")
section4.ReadFP32("scope_zoom_factor")
section4.ReadHintStr("scope_texture", "texture, str_shared")
reader.ReadString("active_holder_attp")
reader.ReadString("holder_attp")
reader.ReadString("anim_aim_in")
reader.ReadString("anim_aim_out")
reader.ReadU32("snd_ignore_ai_type")
reader.ReadU32("snd_fly_by_camera_type")
reader.ReadU32("deferred_action_interval")
reader.ReadU32("weapon_replace_interval")
reader.ReadString("anim_idle")
reader.ReadString("anim_idle_run")
reader.ReadString("anim_idle_walk")
reader.ReadString("anim_idle_sprint")
reader.ReadString("anim_idle_aim")
reader.ReadString("anim_idle_jump")
reader.ReadString("anim_draw")
reader.ReadString("anim_holster")
reader.ReadString("anim_idle_friend")
reader.ReadString("anim_idle_friend_run")
reader.ReadU32("delay_friend_on")
reader.ReadU32("delay_friend_off")
reader.ReadU16("item_priority")
reader.ReadString("anim_idle_long_prefix")
reader.ReadFP32("time_before_long_idle")
reader.ReadU32("cost")
var section5 = reader.ReadSection("icon")
section5.ReadString("texture")
section5.ReadVec4("tex_rect")
section5.ReadString("technics")
section5.ReadString("shader")
section5.ReadVec4("wnd_rect")
section5.ReadVec4i("color")
section5.ReadU32("scale")
section5.ReadU8("h_alignment")
section5.ReadU8("v_alignment")
section5.ReadVec2("icon_offset")
section5.ReadFP32("rotate_speed")
section5.ReadVec2("font_pos")
var section6 = reader.ReadSection("bw_icon")
section6.ReadString("texture")
section6.ReadVec4("tex_rect")
section6.ReadString("technics")
section6.ReadString("shader")
section6.ReadVec4("wnd_rect")
section6.ReadVec4i("color")
section6.ReadU32("scale")
section6.ReadU8("h_alignment")
section6.ReadU8("v_alignment")
section6.ReadVec2("icon_offset")
section6.ReadFP32("rotate_speed")
reader.ReadBool("auto_reload")
reader.ReadBool("auto_spawn_ammo")
reader.ReadU32("min_ammo_show")
reader.ReadString("ammo_class")
var section7 = reader.ReadSection("ammo_class")
section7.ReadFP32("k_dist")
section7.ReadFP32("k_disp")
section7.ReadFP32("k_hit")
section7.ReadFP32("k_impulse")
section7.ReadFP32("k_pierce")
section7.ReadFP32("k_monster_coef")
section7.ReadBool("tracer")
section7.ReadS32("buck_shot")
section7.ReadFP32("wm_size")
section7.ReadString("bullet_particles")
section7.ReadBool("can_be_unlimited")
section7.ReadBool("explosive")
section7.ReadBool("unlimited")
section7.ReadBool("valuable")
section7.ReadU32("type")
reader.ReadU32("snd_shoot_ai_type")
reader.ReadU32("snd_reload_ai_type")
reader.ReadHintStr("snd_shoot_prefix", "sound")
reader.ReadHintStr("snd_show_prefix", "sound")
reader.ReadHintStr("snd_hide_prefix", "sound")
reader.ReadHintStr("snd_empty_prefix", "sound")
reader.ReadHintStr("snd_reload_prefix", "sound")
reader.ReadHintStr("snd_unload_prefix", "sound")
reader.ReadHintStr("snd_unload_money_prefix", "sound")
reader.ReadHintStr("snd_fly_by_prefix", "sound")
reader.ReadString("anim_reload")
reader.ReadString("anim_shoot")
reader.ReadString("anim_shoot_aim")
reader.ReadU32("camera_track_attack_num")
reader.ReadString("camera_track_attack")
reader.ReadU32("camera_track_aim_attack_num")
reader.ReadString("camera_track_aim_attack")
reader.ReadU32("ammo_mag_size")
reader.ReadString("gold_ammo_class")
var section8 = reader.ReadSection("gold_ammo_class")
section8.ReadFP32("k_dist")
section8.ReadFP32("k_disp")
section8.ReadFP32("k_hit")
section8.ReadFP32("k_impulse")
section8.ReadFP32("k_pierce")
section8.ReadFP32("k_monster_coef")
section8.ReadBool("tracer")
section8.ReadS32("buck_shot")
section8.ReadFP32("wm_size")
section8.ReadString("bullet_particles")
section8.ReadBool("can_be_unlimited")
section8.ReadBool("explosive")
section8.ReadBool("unlimited")
section8.ReadBool("valuable")
section8.ReadU32("type")
reader.ReadHintStr("golden_flame_particles", "particles, str_shared")
reader.ReadHintStr("golden_smoke_particles", "particles, str_shared")
reader.ReadHintStr("snd_golden_shoot_prefix", "sound")
reader.ReadString("start_mech_reload")
reader.ReadString("start_oboyma_reload")
reader.ReadString("shells_reload")
reader.ReadMatrix44("hud")
reader.ReadBool("allow_inertion")
reader.ReadFP32("tend_to_speed")
reader.ReadFP32("fov_coef_tend")
reader.ReadBool("ad_scope")
reader.ReadBool("ad_silencer")
reader.ReadBool("ad_barrel")
reader.ReadBool("ad_barrel_2")
reader.ReadBool("ad_butt")
reader.ReadBool("ad_pivot")
reader.ReadBool("ad_bayonet")
reader.ReadBool("ad_laser")
reader.ReadBool("ad_laser_2")
reader.ReadBool("ad_magazine")
reader.ReadBool("ad_magazine_2")
reader.ReadHintStr("fire_bone", "locator_id")
reader.ReadHintStr("shell_bone", "locator_id")
reader.ReadHintStr("ad_scope_bone", "locator_id")
reader.ReadHintStr("ad_silencer_bone", "locator_id")
reader.ReadHintStr("ad_barrel_bone", "locator_id")
reader.ReadHintStr("ad_barrel_2_bone", "locator_id")
reader.ReadHintStr("ad_butt_bone", "locator_id")
reader.ReadHintStr("ad_pivot_bone", "locator_id")
reader.ReadHintStr("ad_bayonet_bone", "locator_id")
reader.ReadHintStr("ad_laser_bone", "locator_id")
reader.ReadHintStr("ad_laser_2_bone", "locator_id")
reader.ReadHintStr("ad_magazine_bone", "locator_id")
reader.ReadHintStr("ad_magazine_2_bone", "locator_id")
reader.ReadHintStr("aim_in_camera_track", "camera_track, str_shared")
reader.ReadHintStr("aim_idle_camera_track", "camera_track, str_shared")
reader.ReadHintStr("aim_out_camera_track", "camera_track, str_shared")
reader.ReadHintStr("reload_camera_track", "camera_track, str_shared")

reader.ReadBool("laser_available")
reader.ReadBool("spot")
reader.ReadBool("spot_far")
reader.ReadFP32("laser_light_radius")
reader.ReadFP32("laser_light_brightness")
reader.ReadFP32("laser_light_angle")
reader.ReadU8("laser_light_scurve")
reader.ReadVec4("laser_light_color", "color, vec4f")
reader.ReadBool("laser_light_shadow")
reader.ReadVec3("laser_light_offset")
reader.ReadVec3("laser_light_aim_offset")
reader.ReadString("proj_map")
reader.ReadFP32("laser_light_far_radius")
reader.ReadFP32("laser_light_far_brightness")
reader.ReadFP32("laser_light_far_angle")
reader.ReadVec2("laser_light_far_size")

reader.ReadHintStr("golden_bullet_tex", "texture, str_shared")
reader.ReadHintStr("golden_bullet_bump_tex", "texture, str_shared")
reader.ReadHintStr("normal_bullet_tex", "texture, str_shared")
reader.ReadHintStr("normal_bullet_bump_tex", "texture, str_shared")
reader.ReadHintStr("bone_part_clip", "part_id")
reader.ReadString("bone_patron_prefix")
reader.ReadString("anim_oboyma_idle_prefix")
