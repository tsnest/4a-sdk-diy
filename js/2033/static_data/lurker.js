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
reader.ReadVec3("ph_box0_center")
reader.ReadVec3("ph_box0_size")
reader.ReadVec3("ph_box1_center")
reader.ReadVec3("ph_box1_size")
reader.ReadVec3("ph_box2_center")
reader.ReadVec3("ph_box2_size")
reader.ReadFP32("ph_crash_vel_min")
reader.ReadFP32("ph_crash_vel_max")
reader.ReadFP32("ph_mass")
reader.ReadFP32("air_control_param")
reader.ReadFP32("ph_skeleton_airr_ang_factor")
reader.ReadFP32("ph_skeleton_airr_lin_factor")
reader.ReadFP32("ph_skeleton_hinger_factor1")
reader.ReadS32("ph_skeleton_ddelay")
reader.ReadFP32("ph_skel_fatal_impulse_factor")
reader.ReadFP32("ph_skel_shot_up_factor")
reader.ReadFP32("ph_after_death_velocity_factor")
reader.ReadFP32("jump_speed")
reader.ReadFP32("crit_damage_falloff")
var section2 = reader.ReadArray("melee_attacks")
for(var i = 0; section2.More(); i++)
{
	var section3 = section2.ReadSection(RecStr("rec_", i, 4), false)
	section3.ReadString("key")
	section3.ReadString("caption")
	section3.ReadU32("is_moving")
	section3.ReadU32("is_turned")
	section3.ReadU32("player_only")
	section3.ReadFP32("min_speed")
	section3.ReadFP32("min_dist")
	section3.ReadFP32("initial_dist")
	section3.ReadFP32("melee_dist")
	section3.ReadVec3("move_check")
	section3.ReadFP32("min_dot")
	section3.ReadFP32("pick_pitch")
	section3.ReadFP32("pick_yaw")
	section3.ReadString("action")
	section3.ReadVec3("min")
	section3.ReadVec3("max")
	section3.ReadFP32("probability")
	section3.ReadBool("crit")
}
reader.ReadFP32("optimal_melee_dist")
reader.ReadFP32("player_melee_dist_plus")
reader.ReadFP32("coll_attack_power_lb")
reader.ReadFP32("coll_attack_power_ub")
reader.ReadFP32("coll_attack_impulse_lb")
reader.ReadFP32("coll_attack_impulse_ub")
reader.ReadHintStr("bone_head", "bone_str")
reader.ReadHintStr("bone_explosion_hit", "bone_str")
reader.ReadHintStr("bone_capsule_hit", "bone_str")
reader.ReadFP32("camera_height_factor")
var section5 = reader.ReadSection("actor_firsteye_cam")
section5.ReadVec2("lim_yaw")
section5.ReadVec2("lim_pitch")
section5.ReadVec2("lim_roll")
section5.ReadBool("first_eye")
section5.ReadFP32("limit_factor")
var section6 = reader.ReadSection("actor_ladder_cam")
section6.ReadVec2("lim_yaw")
section6.ReadVec2("lim_pitch")
section6.ReadVec2("lim_roll")
section6.ReadBool("first_eye")
section6.ReadFP32("limit_factor")
var section7 = reader.ReadSection("actor_look_cam")
section7.ReadVec2("lim_yaw")
section7.ReadVec2("lim_pitch")
section7.ReadVec2("lim_roll")
section7.ReadBool("first_eye")
section7.ReadFP32("limit_factor")
section7.ReadVec2("lim_zoom")
section7.ReadFP32("zoom_speed")
var section8 = reader.ReadSection("actor_free_cam")
section8.ReadVec2("lim_yaw")
section8.ReadVec2("lim_pitch")
section8.ReadVec2("lim_roll")
section8.ReadBool("first_eye")
section8.ReadFP32("limit_factor")
section8.ReadVec2("lim_zoom")
section8.ReadFP32("zoom_speed")
var section9 = reader.ReadSection("actor_station_cam")
section9.ReadVec2("lim_yaw")
section9.ReadVec2("lim_pitch")
section9.ReadVec2("lim_roll")
section9.ReadBool("first_eye")
section9.ReadFP32("limit_factor")
section9.ReadFP32("limit_pitch_factor")
section9.ReadFP32("limit_pitch_min_ratio")
var section10 = reader.ReadSection("actor_locked_cam")
section10.ReadVec2("lim_yaw")
section10.ReadVec2("lim_pitch")
section10.ReadVec2("lim_roll")
section10.ReadBool("first_eye")
section10.ReadFP32("limit_factor")
reader.ReadFP32("min_pforce_coef")
reader.ReadFP32("max_pforce_coef")
reader.ReadHintStr("death_init_anim", "animation_str")
reader.ReadHintStr("death_face_anim0", "animation_str")
reader.ReadHintStr("death_face_anim1", "animation_str")
reader.ReadHintStr("death_face_anim2", "animation_str")
reader.ReadHintStr("death_face_anim3", "animation_str")
reader.ReadU32("footsteps_min_interval")
reader.ReadString("event_step_left")
reader.ReadString("event_step_right")
reader.ReadString("event_step_left_hand")
reader.ReadString("event_step_right_hand")
reader.ReadString("event_jump_left")
reader.ReadString("event_jump_right")
reader.ReadString("bone_foot_left")
reader.ReadString("bone_foot_right")
reader.ReadString("bone_hand_left")
reader.ReadString("bone_hand_right")
reader.ReadU32("sound_type_unknown")
reader.ReadU32("sound_type_walk")
reader.ReadU32("sound_type_run")
reader.ReadU32("sound_type_sprint")
reader.ReadU32("sound_type_crouch")
reader.ReadU32("sound_type_jump")
reader.ReadFP32("walk_slowly_speed")
reader.ReadFP32("walk_speed")
reader.ReadFP32("run_speed")
reader.ReadFP32("sprint_speed")
reader.ReadFP32("strafe_coef")
reader.ReadFP32("backward_coef")
reader.ReadFP32("nostrafe_speed")
reader.ReadFP32("free_accel")
reader.ReadFP32("alert_accel")
reader.ReadFP32("danger_accel")
reader.ReadFP32("sprint_accel")
reader.ReadU32("move_shape_type")
reader.ReadU32("pathfind_shape_type")
reader.ReadFP32("pathfind_avoid_radius0")
reader.ReadFP32("pathfind_avoid_cost0")
reader.ReadFP32("pathfind_avoid_radius1")
reader.ReadFP32("pathfind_avoid_cost1")
reader.ReadFP32("pathfind_avoid_radius2")
reader.ReadFP32("pathfind_avoid_cost2")
reader.ReadFP32("pathfind_avoid_radius3")
reader.ReadFP32("pathfind_avoid_cost3")
reader.ReadFP32("move_avoid_radius0")
reader.ReadFP32("move_avoid_cost0")
reader.ReadFP32("move_avoid_radius1")
reader.ReadFP32("move_avoid_cost1")
reader.ReadFP32("move_avoid_radius2")
reader.ReadFP32("move_avoid_cost2")
reader.ReadFP32("move_avoid_radius3")
reader.ReadFP32("move_avoid_cost3")
reader.ReadFP32("hit_impulse_upper_bound")
reader.ReadFP32("width_coef")
reader.ReadFP32("height_coef")
reader.ReadFP32("strict_width_coef")
reader.ReadFP32("strict_height_coef")
reader.ReadU32("danger_delay")
reader.ReadU32("uber_delay")
reader.ReadU32("alert_delay")
reader.ReadBool("full_scripted_motion_play")
reader.ReadBool("use_physics_in_movement")
reader.ReadBool("use_IKs")
reader.ReadBool("can_avoid")
reader.ReadBool("can_push")
reader.ReadFP32("scripted_push_distance")
reader.ReadFP32("scripted_push_smooth")
reader.ReadFP32("foot_step_volume")
reader.ReadBool("group_behavior")
reader.ReadBool("dbg_pacifist")
reader.ReadBool("dbg_show_role")
reader.ReadBool("dbg_show_orbit")
reader.ReadFP32("orbit_radius")
reader.ReadFP32("orbit_epsilon")
reader.ReadFP32("approach_delta")
reader.ReadFP32("revolve_delta_min")
reader.ReadFP32("revolve_delta_max")
reader.ReadFP32("snarl_probability")
reader.ReadFP32("evil_time_min")
reader.ReadFP32("evil_time_max")
reader.ReadFP32("evil_time_back_min")
reader.ReadFP32("evil_time_back_max")
reader.ReadU32("group_model_type")
reader.ReadStrArray32("vulnerable_motions", "str_array")
reader.ReadStrArray32("vulnerable_bones", "str_array")
reader.ReadFP32("vulnerability_factor")
reader.ReadU32("lookout_timeout")
reader.ReadFP32("change_lair_dist")
reader.ReadFP32("change_lair_chance")
reader.ReadFP32("lookback_retreat_chance")
reader.ReadFP32("second_attack_chance")
reader.ReadFP32("nolair_attack_chance")
