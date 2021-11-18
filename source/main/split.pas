program split;
uses windows, classes, sysutils, uCrc, Variants;

var
  knownscripts : array of String = [
    // configs (45)
    'gamedata',
    'language_type',
    'game_materials',
    'mainmenu',
    'subtitle_manager',
    'cover_manager',
    'render_subst_all',
    'render_tint_colors',
    'sound_type',
    'hit_type',
    'inner_shape_type',
    'shape_type',
    'behaviour_type',
    'hud_manager',
    'bullet_manager',
    'speech_manager',
    'suit_type',
    'joystick_delay',
    'trigger_main_menu_sequences',
    'environments',
    'color_anim_lib',
    'render_static_textures',
    'flares',
    'menus',
    'ehit_type',
    'interest_type',
    'net_props',
    'player_props',
    'damage_handle_presets',
    'ammo_type',
    'zone_type',
    'hud_particles_type',
    'achievements_icons',
    'class_icons',
    'weapon_icons',
    'weapon_attaches',
    'maploader_datas',
    'trade_presets',
    'dlc_tokens',
    'diary',
    'loader_data',
    'activities_storage',
    'speech_config_data',
    'mp_class_type_sgl',
    '_g.entities.ui_weapons_data',
    
    // visual scripts (9)
    'vs\player\achievement',
    'vs\player\death',
    'vs\player\fire_zone',
    'vs\player\gas_zone',
    'vs\player\rad_zone',
    'vs\npc\npc_gas_zone_1_mask',
    'vs\npc\npc_gas_zone_2_mask',
    'vs\npc\npc_outdoor_breath',
    'vs\npc\npc_fonarik',
    
    // levels 2033
    '000',
    'l00_intro',
    'l01_hunter',
    'l02_exhibition',
    'l03_chase',
    'l04_riga',
    'l05_lost_tunnel',
    'l06_bridge',
    'l07_catacombes',
    'l08_market',
    'l09_dead_city_1',
    'l10_dead_city_2',
    'l11_dry',
    'l12_ghosts',
    'l13_anomaly',
    'l14_cursed',
    'l15_armory',
    'l16_frontline',
    'l17_trolley_combat',
    'l18_depot',
    'l19_defence',
    'l20_child',
    'l21_nazi_outpost',
    'l22_black',
    'l23_polis',
    'l24_alley',
    'l25_library',
    'l26_depository',
    'l27_archives',
    'l28_driving',
    'l30_darkstar',
    'l31_catacombes',
    'l32_cave',
    'l33_d6',
    'l34_biomass',
    'l35_division',
    'l36_ostankino',
    'l38_tower',
    'l39_ethereal',
    
    // levels Last Light
    //'000',
    'benchmark',
    'dlc_dev_armory',
    'dlc_faction_ganza',
    'dlc_faction_nazi',
    'dlc_faction_red',
    'dlc_pyro',
    'dlc_story_anna',
    'dlc_story_khan',
    'dlc_story_pavel',
    'dlc_tower1',
    'l00_intro1',
    'l00_intro2',
    'l01_jail',
    'l02_escape',
    'l03_camp_1',
    'l03_camp_2',
    'l04_catacombes',
    'l04_plane',
    'l05_theatre',
    'l06_revolution',
    'l06_revolution_intro',
    'l07_tunnel',
    'l07_tunnel_2',
    'l08_water_tunnel',
    'l09_venice',
    'l10_swamp',
    'l10_swamp_02',
    'l11_undercity',
    'l12_circus',
    'l12_circus_2',
    'l13_train_1',
    'l13_train_2',
    'l14_bridge',
    'l14_bridge_2',
    'l15_train_depot',
    'l16_dead_city',
    'l17_red_square',
    'l18_garden',
    'l19_polis',
    'l20_d6_defense',
    
    // levels Redux
    '2033\000', 
    '2033\l00_intro', 
    '2033\l01_hunter', 
    '2033\l02_exhibition', 
    '2033\l03_chase', 
    '2033\l04_riga', 
    '2033\l05_lost_tunnel', 
    '2033\l08_market', 
    '2033\l09_dead_city_1', 
    '2033\l11_dry', 
    '2033\l12_ghosts', 
    '2033\l14_cursed', 
    '2033\l15_armory', 
    '2033\l16_frontline', 
    '2033\l17_trolley_combat', 
    '2033\l18_depot', 
    '2033\l19_defence', 
    '2033\l21_nazi_outpost', 
    '2033\l22_black', 
    '2033\l23_polis', 
    '2033\l24_alley', 
    '2033\l26_depository', 
    '2033\l27_archives', 
    '2033\l28_driving', 
    '2033\l30_darkstar', 
    '2033\l32_cave', 
    '2033\l33_d6', 
    '2033\l36_ostankino', 
    '2034\000', 
    '2034\dlc_dev_armory', 
    '2034\dlc_faction_ganza', 
    '2034\dlc_faction_nazi', 
    '2034\dlc_faction_red', 
    '2034\dlc_pyro', 
    '2034\dlc_story_anna', 
    '2034\dlc_story_khan', 
    '2034\dlc_story_pavel', 
    '2034\dlc_tower1', 
    '2034\l00_intro_1', 
    '2034\l00_intro_2', 
    '2034\l01_jail', 
    '2034\l02_escape', 
    '2034\l03_camp_1', 
    '2034\l03_camp_2', 
    '2034\l04_catacombs', 
    '2034\l04_plane', 
    '2034\l05_theatre', 
    '2034\l06_revolution_intro', 
    '2034\l06_revolution_main', 
    '2034\l07_tunnel', 
    '2034\l07_tunnel_2', 
    '2034\l08_water_tunnel', 
    '2034\l09_venice', 
    '2034\l10_swamp', 
    '2034\l10_swamp_02', 
    '2034\l11_undercity', 
    '2034\l12_circus', 
    '2034\l12_circus_2', 
    '2034\l13_train_1', 
    '2034\l13_train_2', 
    '2034\l14_bridge', 
    '2034\l14_bridge_2', 
    '2034\l15_train_depot', 
    '2034\l16_dead_city', 
    '2034\l17_red_square', 
    '2034\l18_garden', 
    '2034\l19_polis', 
    '2034\l20_d6_defense',
    'benchmarks\benchmark',
    'benchmarks\benchmark33',
    
    // static data 2033 (101)
    'static_data\ameba__g.config.entity.ameba',
    'static_data\ammo__g.config.entity.ammo_044_fmj',
    'static_data\ammo__g.config.entity.ammo_12x70mm',
    'static_data\ammo__g.config.entity.ammo_15mm',
    'static_data\ammo__g.config.entity.ammo_545x39_fmj',
    'static_data\ammo__g.config.entity.ammo_money',
    'static_data\anim_object__g.config.entity.hands_for_drezina',
    'static_data\anomaly__g.config.entity.simple_anomaly',
    'static_data\biomass_large__g.config.entity.biomass_large',
    'static_data\charger__g.config.entity.charger',
    'static_data\dark__g.config.entity.dark',
    'static_data\drezina_hand__g.config.entity.drezina_hand',
    'static_data\drezina_moto__g.config.entity.drezina_moto',
    'static_data\effect__g.config.entity.effect',
    'static_data\filter__g.config.entity.filter',
    'static_data\flower__g.config.entity.flower',
    'static_data\gasmask__g.config.entity.gasmask',
    'static_data\hands_for_drezina__g.config.entity.hands_for_drezina',
    'static_data\harpy__g.config.entity.harpy',
    'static_data\helsing_arrow__g.config.entity.wpn_arrow',
    'static_data\kid__g.config.entity.kid',
    'static_data\kulemet__g.config.entity.flame_thrower',
    'static_data\kulemet__g.config.entity.kulemet',
    'static_data\kulemet__g.config.entity.kulemet_bashnya',
    'static_data\ladder__g.config.entity.ladder',
    'static_data\lian__g.config.entity.lian',
    'static_data\librarian__g.config.entity.librarian',
    'static_data\lurker__g.config.entity.lurker',
    'static_data\medkit__g.config.entity.medkit',
    'static_data\nightvision__g.config.entity.nightvision',
    'static_data\nosalis__g.config.entity.nosalis',
    'static_data\nosalis__g.config.entity.volosaty',
    'static_data\nosalis_female__g.config.entity.nosalis_female',
    'static_data\npc_fx__g.config.entity.npc_enemy_fx',
    'static_data\npc_fx__g.config.entity.npc_friend_fx',
    'static_data\o_aipoint__g.config.entity.ai_point',
    'static_data\o_basezone__g.config.entity.restrictor',
    'static_data\o_entity__g.config.entity.entity',
    'static_data\o_entity__g.config.entity.station',
    'static_data\o_explosion__g.config.entity.explosion',
    'static_data\o_hlamp__g.config.entity.hanging_lamp',
    'static_data\o_waterzone__g.config.entity.waterzone',
    'static_data\patrol_point__g.config.entity.patrol_point',
    'static_data\player__g.config.entity.player',
    'static_data\player_map__g.config.entity.player_map',
    'static_data\players_hands__g.config.entity.players_hands',
    'static_data\players_knife__g.config.entity.players_knife',
    'static_data\proxy__g.config.entity.proxy',
    'static_data\rat__g.config.entity.kid',
    'static_data\rat__g.config.entity.rat',
    'static_data\simple_monster__g.config.entity.simple_anomaly',
    'static_data\simple_npc__g.config.entity.monster',
    'static_data\soft_entity__g.config.entity.soft_entity',
    'static_data\soft_entity_inst__g.config.entity.soft_entity_inst',
    'static_data\staticprop__g.config.entity.static',
    'static_data\station_stand__g.config.entity.station_stand',
    'static_data\torch__g.config.entity.torchlight',
    'static_data\visualscript__g.config.entity.vs',
    'static_data\watchman__g.config.entity.watchman',
    'static_data\weapon_2012__g.config.entity.wpn_2012',
    'static_data\weapon_2012__g.config.entity.wpn_2012_scope',
    'static_data\weapon_abzac__g.config.entity.wpn_abzac',
    'static_data\weapon_ak74_test__g.config.entity.wpn_ak74_optics_test',
    'static_data\weapon_ak74_test__g.config.entity.wpn_ak74_relsa',
    'static_data\weapon_ak_74__g.config.entity.wpn_ak_74',
    'static_data\weapon_ak_74__g.config.entity.wpn_ak_74_optics',
    'static_data\weapon_dagger__g.config.entity.wpn_dagger',
    'static_data\weapon_duplet__g.config.entity.wpn_duplet_fx',
    'static_data\weapon_dynamite__g.config.entity.wpn_dynamite',
    'static_data\weapon_flash_grenade__g.config.entity.wpn_flash_grenage',
    'static_data\weapon_hellbreath__g.config.entity.wpn_hellbreath',
    'static_data\weapon_helsing__g.config.entity.wpn_helsing',
    'static_data\weapon_helsing__g.config.entity.wpn_optics_helsing',
    'static_data\weapon_macheta__g.config.entity.wpn_macheta',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_barrel',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_barrel_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_optic',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_optic_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_optic_silencer',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_silencer',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_rifle_silencer_laser',
    'static_data\weapon_revolver__g.config.entity.wpn_revolver_silencer',
    'static_data\weapon_sticky_dynamite__g.config.entity.wpn_sticky_dynamite',
    'static_data\weapon_tihar__g.config.entity.wpn_optics_tihar',
    'static_data\weapon_tihar__g.config.entity.wpn_tihar',
    'static_data\weapon_ubludok__g.config.entity.wpn_ak74',
    'static_data\weapon_ubludok__g.config.entity.wpn_ak74_fixed',
    'static_data\weapon_ubludok__g.config.entity.wpn_ak74_fx',
    'static_data\weapon_ubludok__g.config.entity.wpn_ubludok',
    'static_data\weapon_ubludok__g.config.entity.wpn_ubludok_fx',
    'static_data\weapon_ubludok__g.config.entity.wpn_ubludok_silencer',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_bayonet_uboynicheg_fx',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_shotgun_trap',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg',
    'static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx',
    'static_data\weapon_vsv__g.config.entity.wpn_vsv',
    'static_data\weapon_vsv__g.config.entity.wpn_vsv_scope',
    'static_data\woman__g.config.entity.woman',
    
    // skeleton sequence value 2033 (22)
    'dynamic\hud\weapon\2012\2012_skeleton_sequence_value',
    'dynamic\hud\weapon\ak\ak_skeleton_sequence_value',
    'dynamic\hud\weapon\bomb\predohranitel\bomb_adhesive_skeleton_sequence_value',
    'dynamic\hud\weapon\bomb\predohranitel\bomb_flare_skeleton_sequence_value',
    'dynamic\hud\weapon\bomb\predohranitel\bomb_predohranitel_skeleton_sequence_value',
    'dynamic\hud\weapon\duplet\duplet_skeleton_sequence_value',
    'dynamic\hud\weapon\dynamo_hand\dynamo_hand_skeleton_sequence_value',
    'dynamic\hud\weapon\gasmask\gasmask_skeleton_sequence_value',
    'dynamic\hud\weapon\helsing\helsing_skeleton_sequence_value',
    'dynamic\hud\weapon\knives\knives_skeleton_sequence_value',
    'dynamic\hud\weapon\macheta\macheta_skeleton_sequence_value',
    'dynamic\hud\weapon\map\map_skeleton_sequence_value',
    'dynamic\hud\weapon\med_kit\med_kit_skeleton_sequence_value',
    'dynamic\hud\weapon\nv\nv_skeleton_sequence_value',
    'dynamic\hud\weapon\ognemet\ognemet_skeleton_sequence_value',
    'dynamic\hud\weapon\railgun_hand\railgun_hand_skeleton_sequence_value',
    'dynamic\hud\weapon\revolver\a_revolver_skeleton_sequence_value',
    'dynamic\hud\weapon\shot\shot_skeleton_sequence_value',
    'dynamic\hud\weapon\tihar\tihar_skeleton_sequence_value',
    'dynamic\hud\weapon\ubludok\ubludok_gold_skeleton_sequence_value',
    'dynamic\hud\weapon\uboynicheg\uboynicheg_skeleton_sequence_value',
    'dynamic\hud\weapon\vsv\vsv_skeleton_sequence_value',
    
    // configs Arktika.1
    'ecostume_type',
    'upgradables_registry',
    'upgradables_registry_ovr',
    
    // visual scripts Arktika.1
    'vs\player\death',
    'vs\player\metro3\bracer',
    'vs\player\map_task_2033',
    'vs\player\karma',
    'vs\player\bullets_dirt',
    'vs\player\end_level',
    'vs\player\crouch_control_dlc1',
    'vs\player\safe',
    'vs\player\metro3\general_rules',
    'vs\player\civil_station',
    'vs\player\metro3\binocular',
    'vs\player\weapon',
    'vs\player\jumpover',
    'vs\npc\m3\role_machine_gunner_oculus',
    
    // levels Arktika.1
    'oculus\pc_01_citadel',
    'oculus\pc_02_subway_phase_1',
    'oculus\pc_03_airport_phase_1',
    'oculus\pc_04_base_phase_1',
    'oculus\pc_04_base_phase_2',
    'oculus\pc_05_mall_phase_1',
    'oculus\pc_05_mall_phase_2',
    'oculus\pc_06_railstation_phase_1',
    'oculus\pc_06_railstation_phase_2',
    'oculus\pc_07_baggages_phase_1',
    'oculus\pc_07_baggages_phase_2',
    
    // static data Arktika.1 (104)
    'static_data\4f830e36_4f830e36_4e4e2e53', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_barrel
    'static_data\0f10b43b_0f10b43b_dd755ca5', // static_data\o_entity__g.config.entity.entity
    'static_data\0cbbe8a6_0cbbe8a6_7b9fa65a', // static_data\o_hlamp__g.config.entity.hanging_lamp
    'static_data\46985674_46985674_653d893f', // static_data\effect__g.config.entity.effect
    'static_data\05275e6e_05275e6e_4f2ab9f8', // static_data\o_basezone__g.config.entity.restrictor
    'static_data\b42ea669_b42ea669_6dfe6770', // static_data\patrol_point__g.config.entity.patrol_point
    'static_data\2301c4ef_2301c4ef_539d30e8', // static_data\staticprop__g.config.entity.static
    'static_data\4f830e36_4f830e36_ae5e1359', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_rotor_em
    'static_data\4f830e36_4f830e36_5b68eda5', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_barrel_damage
    'static_data\4f830e36_4f830e36_cdc7c4fb', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_rotor_plasma
    'static_data\4f830e36_4f830e36_463aefa7', // static_data\weapon_item_vr_attach__g.config.entity.vr_attach_base
    'static_data\4f830e36_4f830e36_df49be8c', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_ammo_laser_ricochet
    'static_data\4f830e36_4f830e36_3bc02e0c', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_barrel_accuracy
    'static_data\945fd87c_945fd87c_d671f740', // static_data\weapon_hellbreath__g.config.entity.wpn_lpt
    'static_data\4f830e36_4f830e36_69f565e3', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_magazine
    'static_data\4f830e36_4f830e36_779dfdd6', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_battery
    'static_data\6acd63fa_6acd63fa_ef04f459', // static_data\vr_entity__g.config.entity.vr_entity
    'static_data\4f830e36_4f830e36_c57d11ed', // static_data\weapon_item_vr_attach__g.config.entity.vr_module_cylinder
    'static_data\f7fc7e58_f7fc7e58_5da11d93', // static_data\vr_weapon_sf4__g.config.entity.wpn_sf4ag_gun
    'static_data\4df96cbf_4df96cbf_e71c623d', // static_data\virtual_monitor__g.config.entity.virtual_monitor
    'static_data\46985674_46985674_c418c458', // static_data\effect__g.config.entity.dummy
    'static_data\84260b0e_84260b0e_f89effd9', // static_data\proxy__g.config.entity.proxy
    'static_data\391ab8a3_391ab8a3_1abf67e8', // static_data\turret__g.config.entity.turret
    'static_data\4015029a_4015029a_80e3ed61', // static_data\force_field__g.config.entity.force_field
    'static_data\54571bfa_54571bfa_fd805e46', // static_data\visualscript__g.config.entity.vs
    'static_data\68e1bde3_68e1bde3_4b4462a8', // static_data\player__g.config.entity.player
    'static_data\d91a939e_d91a939e_a366b3de', // static_data\ammo__g.config.entity.ammo_15mm_sabot
    'static_data\d91a939e_d91a939e_172a1e1e', // static_data\ammo__g.config.entity.ammo_12x70mm
    'static_data\5aeb1d81_5aeb1d81_cd2679e2', // static_data\players_hands__g.config.entity.players_hands
    'static_data\a7f76a1e_a7f76a1e_d8e29a66', // static_data\o_helpertext__g.config.entity.text_helper
    'static_data\89207469_89207469_81c7a738', // static_data\mech_entity__g.config.entity.mech_entity
    'static_data\e74bf56a_e74bf56a_e4d85d1f', // static_data\weapon_padonag__g.config.entity.wpn_stechkin
    'static_data\b616569a_b616569a_11a4fe47', // static_data\weapon_dynamite__g.config.entity.vr_decoy_npc
    'static_data\a2812f11_a2812f11_f5c81f23', // static_data\weapon_duplet__g.config.entity.wpn_e_mag_oculus
    'static_data\9151aa6f_9151aa6f_f7eb5312', // static_data\npc_fx__g.config.entity.npc_friend_fx
    'static_data\c6bc8b79_c6bc8b79_7867f577', // static_data\anomaly__g.config.entity.simple_anomaly
    'static_data\ea2ae739_ea2ae739_5eeedf12', // static_data\vr_weapon_modular__g.config.entity.wpn_modular_gun
    'static_data\021b4a02_021b4a02_cfcdcfd6', // static_data\o_helpertext_vr_info__g.config.entity.text_helper_vr_info
    'static_data\945fd87c_945fd87c_40384ab8', // static_data\weapon_hellbreath__g.config.entity.wpn_rsnake
    'static_data\9feecd57_9feecd57_fde89fd0', // static_data\torchlight_upgradable__g.config.entity.torchlight_upgradable
    'static_data\9151aa6f_9151aa6f_fe5690a7', // static_data\npc_fx__g.config.entity.npc_enemy_fx
    'static_data\945fd87c_945fd87c_c56c7186', // static_data\weapon_hellbreath__g.config.entity.wpn_rsnake_drone
    'static_data\77af1ab6_77af1ab6_df684305', // static_data\o_scaling_entity__g.config.entity.scaling_entity
    'static_data\e74bf56a_e74bf56a_67bad31e', // static_data\weapon_padonag__g.config.entity.wpn_spray_6
    'static_data\03016a46_03016a46_0be6b917', // static_data\soft_entity__g.config.entity.soft_entity
    'static_data\e74bf56a_e74bf56a_17d02791', // static_data\weapon_padonag__g.config.entity.wpn_spray_3
    'static_data\d91a939e_d91a939e_bc62ab9b', // static_data\ammo__g.config.entity.ammo_545x39_fmj
    'static_data\e74bf56a_e74bf56a_60d71707', // static_data\weapon_padonag__g.config.entity.wpn_spray_2
    'static_data\e74bf56a_e74bf56a_feb382a4', // static_data\weapon_padonag__g.config.entity.wpn_spray_5
    'static_data\8d41ffd3_8d41ffd3_5702e2dc', // static_data\effectm__g.config.entity.effectm
    'static_data\b0584545_b0584545_ce66eb07', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_ult
    'static_data\b0584545_b0584545_1026b2e2', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_med
    'static_data\b0584545_b0584545_6fb57181', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_low
    'static_data\b0584545_b0584545_ca96f994', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl_alt_high
    'static_data\15583d02_15583d02_c2617beb', // static_data\helsing_arrow__g.config.entity.ammo_sf4ag_arrow_pin
    'static_data\0dd68074_0dd68074_881f17d7', // static_data\vr_weapon__g.config.entity.vr_weapon
    'static_data\3a55b380_3a55b380_a67be6c8', // static_data\flexible_entity__g.config.entity.flexible_entity
    'static_data\b574ea5d_b574ea5d_7f21d1cc', // static_data\o_explosion__g.config.entity.explosion
    'static_data\6b10940e_6b10940e_84524940', // static_data\virtual_camera__g.config.entity.virtual_camera
    'static_data\b574ea5d_b574ea5d_f48bb5e6', // static_data\o_explosion__g.config.entity.explosion_oculus
    'static_data\b574ea5d_b574ea5d_f41c6835', // static_data\o_explosion__g.config.entity.explosion_oculus_drone
    'static_data\d91a939e_d91a939e_c8aefcf3', // static_data\ammo__g.config.entity.ammo_15mm
    'static_data\a2812f11_a2812f11_da11c7f9', // static_data\weapon_duplet__g.config.entity.wpn_sf4ag
    'static_data\a2812f11_a2812f11_02fbdf25', // static_data\weapon_duplet__g.config.entity.wpn_punisher_oculus
    'static_data\eea4510f_eea4510f_3fc6bf78', // static_data\effect_pausable__g.config.entity.effect_pausable
    'static_data\daef9a87_daef9a87_46c1cfcf', // static_data\scripted_entity__g.config.entity.scripted_entity
    'static_data\e66cf1c4_e66cf1c4_6dd970ff', // static_data\grab_zone__g.config.entity.grab_zone
    'static_data\15583d02_15583d02_2ec16f2b', // static_data\helsing_arrow__g.config.entity.ammo_sf4ag_arrow
    'static_data\01437105_01437105_1ac1430e', // static_data\vr_cube__g.config.entity.vr_cube
    'static_data\71871a27_71871a27_b786199b', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74_ng
    'static_data\9a1939a3_9a1939a3_85a4d288', // static_data\virtual_hand__g.config.entity.virtual_hand
    'static_data\8f22a0ed_8f22a0ed_f39a543a', // static_data\woman__g.config.entity.woman
    'static_data\1c0e989a_1c0e989a_5ffb5fa5', // static_data\magnetic_holster__g.config.entity.magnetic_holster
    'static_data\57b8e36c_57b8e36c_39d5170f', // static_data\vr_weapon_carver__g.config.entity.wpn_baranka
    'static_data\b0584545_b0584545_5985dbc3', // static_data\weapon_flame_dynamite__g.config.entity.plasma_ball_expl
    'static_data\e74bf56a_e74bf56a_90908d34', // static_data\weapon_padonag__g.config.entity.wpn_revolver_vr
    'static_data\1eb40ac1_1eb40ac1_b5bf7485', // static_data\teleport__g.config.entity.teleport
    'static_data\13e11d26_13e11d26_9c9b4010', // static_data\weapon_saiga__g.config.entity.wpn_saiga
    'static_data\91551e3d_91551e3d_650ed131', // static_data\weapon_ventil__g.config.entity.wpn_ventil
    'static_data\945fd87c_945fd87c_da3a8ba7', // static_data\weapon_hellbreath__g.config.entity.wpn_fudo_hell
    'static_data\e8ca57b3_e8ca57b3_229f6c22', // static_data\o_waterzone__g.config.entity.waterzone
    'static_data\71871a27_71871a27_685a059b', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74
    'static_data\e74bf56a_e74bf56a_217b03ab', // static_data\weapon_padonag__g.config.entity.wpn_spray
    'static_data\71871a27_71871a27_702644db', // static_data\weapon_ak_74__g.config.entity.wpn_ak74_oculus_griz
    'static_data\90c81990_90c81990_dabcb7af', // static_data\humanimal__g.config.entity.humanimal
    'static_data\b616569a_b616569a_abf295fc', // static_data\weapon_dynamite__g.config.entity.wpn_humanimal_stuff
    'static_data\151dc865_151dc865_65d5e65c', // static_data\rat__g.config.entity.rat
    'static_data\a6d94122_a6d94122_e312ab02', // static_data\big_mother__g.config.entity.ruckus
    'static_data\b0584545_b0584545_a9e80d6b', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_grenage
    'static_data\aa177401_aa177401_53549294', // static_data\staticprop_breakable__g.config.entity.static_breakable
    'static_data\27dcdaf2_27dcdaf2_862a2a69', // static_data\o_aipoint__g.config.entity.ai_point
    'static_data\0648d18a_0648d18a_0a5f7b5a', // static_data\o_helpertext_counter__g.config.entity.text_helper_counter
    'static_data\945fd87c_945fd87c_94e4093b', // static_data\weapon_hellbreath__g.config.entity.wpn_fudo_hell_jeremie
    'static_data\16f5ef2e_16f5ef2e_aef34af2', // static_data\o_anim_entity__g.config.entity.anim_entity
    'static_data\71871a27_71871a27_7b43ba6d', // static_data\weapon_ak_74__g.config.entity.wpn_ak74_oculus_plasma_rifle
    'static_data\7740aebc_7740aebc_f83af38a', // static_data\weapon_flare__g.config.entity.wpn_flare
    'static_data\b5b0650c_b5b0650c_55b0bc3d', // static_data\weapon_gatling__g.config.entity.wpn_gatling_l17_pasha
    'static_data\b574ea5d_b574ea5d_d2cf4d05', // static_data\o_explosion__g.config.entity.explosion_oculus_rocket
    'static_data\90c81990_90c81990_c35b054f', // static_data\humanimal__g.config.entity.humanimal_oculus
    'static_data\9861566b_9861566b_056dc363', // static_data\weapon_decoy__g.config.entity.vr_decoy
    'static_data\e74bf56a_e74bf56a_89b4b232', // static_data\weapon_padonag__g.config.entity.wpn_spray_4
    'static_data\57174204_57174204_69bf2010', // static_data\weapon_ashot__g.config.entity.wpn_ashot_ng
    'static_data\71871a27_71871a27_ad2afe80', // static_data\weapon_ak_74__g.config.entity.wpn_ak74_oculus
    'static_data\9151aa6f_9151aa6f_bcc89637', // static_data\npc_fx__g.config.entity.npc_teleporting_boss
    // my invention, doesn't exist in original game
    'static_data\24c51a9c_24c51a9c_fe860793'  // static_data\vehicle__g.config.entity.vehicle
  ];


function GuessName(hash : Longint) : String;
var
  I : Integer;
begin
  Result := '';
  for I := Low(knownscripts) to High(knownscripts) do
    if GetStringCrc('content\scripts\' + knownscripts[I] + '.bin') = hash then
    begin
      Result := knownscripts[I];
      Break;
    end;
end;

procedure SplitScripts(const filename : String);
var
  hash, len : Longint;
  scripts, one : TFileStream;
  name, path : String;
  buffer : array of byte;
begin
  scripts := TFileStream.Create(filename, fmOpenRead);
  while scripts.Position < scripts.Size do
  begin
    scripts.ReadBuffer(hash, sizeof(hash));
    scripts.ReadBuffer(len, sizeof(len));

    SetLength(buffer, len);
    scripts.Read(buffer[0], len);

    name := GuessName(hash);
    if name = '' then
      name := '#' + IntToHex(hash, 8);
    Write(name, ' length ', len); Writeln;
    
    path := 'scripts\' + name + '.bin';
    ForceDirectories(ExtractFilePath(path));

    one := TFileStream.Create(path, fmCreate);
    one.WriteBuffer(buffer[0], len);
    one.Free;
  end;
  scripts.Free;
end;

procedure MergeDir(f : TFileStream; const dir : String);
var
  script : TFileStream;
  buffer : array of Byte;
  crc, size : Longint;
  sr : TSearchRec;
begin
  if FindFirst(dir+'\*', faDirectory, sr) = 0 then
  begin
    repeat
      if not ((sr.Name = '.') or (sr.Name = '..')) then
        MergeDir(f, dir + '\' + sr.Name)
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  if FindFirst(dir+'\*.bin', faAnyFile xor faDirectory, sr) = 0 then
  begin
    repeat                                
      script := TFileStream.Create(dir+'\'+sr.Name, fmOpenRead);
      SetLength(buffer, script.Size);
      script.ReadBuffer(buffer[0], script.Size);
      script.Free;

      if sr.Name[1] = '#' then
        crc := StrToInt('$' + Copy(sr.Name, 2, 8))
      else
        crc := GetStringCrc('content\'+dir+'\'+sr.Name);
      size := Length(buffer);

      f.WriteBuffer(crc, Sizeof(crc));
      f.WriteBuffer(size, Sizeof(size));
      f.WriteBuffer(buffer[0], size);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure MergeScripts(const filename : String);
var
  scripts : TFileStream;
begin
  scripts := TFileStream.Create(filename, fmCreate);
  MergeDir(scripts, 'scripts');
  scripts.Free;
end;

var
  filename : String;
begin
  filename := 'scripts.bin';
  
  if ParamCount >= 2 then
    filename := ParamStr(2);

  if (ParamCount < 1) or (ParamStr(1) = '-d') then
    SplitScripts(filename)
  else if ParamStr(1) = '-c' then
    MergeScripts(filename);
end.