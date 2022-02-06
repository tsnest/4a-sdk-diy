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
    
    // visual scripts 2033 (8)
    'vs\player\achievement',
    'vs\player\death',
    'vs\player\fire_zone',
    'vs\player\gas_zone',
    'vs\player\rad_zone',
    'vs\npc\npc_gas_zone_1_mask',
    'vs\npc\npc_gas_zone_2_mask',
    'vs\npc\npc_outdoor_breath',
    
    // visual scripts Last Light (278)
    'aura\aura_15_1',
    'aura\aura_15_1_h',
    'aura\aura_15_2',
    'aura\aura_15_2_h',
    'aura\aura_15_3',
    'aura\aura_15_3_h',
    'aura\aura_15_4',
    'aura\aura_15_4_h',
    'aura\aura_17_pusher',
    'aura\aura_17_scout',
    'aura\luminocity_check',
    'dynamic\human\flag_l06',
    'dynamic\human\head_torch_free',
    'dynamic\objects\lamps\electric_lamp_hit_alert',
    'dynamic\objects\lamps\kerosinka_skrip',
    'dynamic\objects\lamps\lamp_attached_turn_off',
    'dynamic\objects\lamps\lamp_kerosinka_off_on',
    'dynamic\objects\lamps\lamp_npc_attached_turn_on_off',
    'fash_shoot_scary_3',
    'human\npc_human_deathbyfire',
    'human\vsref_npc_human_deathbyfire',
    'level_vs_temp\l14_entrance_wire',
    'levels\dlc_bonus\arahi_destroy',
    'levels\dlc_bonus\arahi_destroy_cocon',
    'levels\dlc_bonus\arahi_small_in_cocon_burn',
    'levels\dlc_bonus\arahi_small_orda',
    'levels\dlc_bonus\arahi_small_orda_dummy',
    'levels\dlc_bonus\arahind_fake_hit',
    'levels\dlc_bonus\arahind_footsteps',
    'levels\dlc_bonus\arahind_gasmask_hit',
    'levels\dlc_bonus\arahind_normal_flamethrower',
    'levels\dlc_bonus\arahind_normal_flamethrower_cocon',
    'levels\dlc_bonus\arahind_normal_flamethrower_fake',
    'levels\dlc_bonus\arahind_torch',
    'levels\dlc_khan\cocoon_dum',
    'levels\dlc_khan\cocoon_dum0',
    'levels\dlc_khan\rat_anomaly_lamphit',
    'levels\dlc_khan\rat_hit',
    'levels\dlc_khan\rat_hit_vent',
    'levels\dlc_khan\skelet_interes',
    'levels\dlc_ranger\lamp_electric_turn_on',
    'levels\dlc_ranger\rats_no_destroy',
    'levels\dlc_red\npc_ach_die',
    'levels\dlc_red\npc_house2_die',
    'levels\dlc_red\npc_zone1_die',
    'levels\dlc_red\npc_zone2_die',
    'levels\dlc_red\sniper_part2_die',
    'levels\l00\nos_speed',
    'levels\l04\destroy_roof',
    'levels\l07\cocon_small_final_rusik',
    'levels\l07\l07_fly_sound',
    'levels\l07\l07_ghosts',
    'levels\l07\l07_water_drippling_sound',
    'levels\l07\l07_water_falling_hard_sound',
    'levels\l07\l07_water_small_stream_sound',
    'levels\l07\rat_01',
    'levels\l07\wood_broken_effect',
    'levels\l08\l08_aqua_female_death',
    'levels\l08\l08_aqua_female_sound_scheme',
    'levels\l08\l08_kamish',
    'levels\l08\l08_reed_idle',
    'levels\l14_bridge\l14_1_demon_aqua_hit',
    'levels\l14_bridge\l14_2_ach',
    'levels\l14_bridge\l14_2_darckone_aura',
    'levels\l14_bridge\l14_2_demons_aura',
    'levels\l14_bridge\l14_2_metal_plats_sound_2',
    'levels\l14_bridge\l14_2_watchmans_aura',
    'levels\l14_bridge\l14_2_water_dripling',
    'levels\l14_bridge\swing_from_metal_01',
    'levels\l14_bridge\swing_from_metal_02',
    'levels\l14_bridge\swing_from_metal_02_up',
    'levels\l14_bridge\swing_from_metal_03',
    'levels\l14_bridge\swing_from_metal_03_up',
    'levels\l14_bridge\swing_from_metal_04',
    'levels\l14_bridge\swing_from_metal_04_up',
    'manto_l04',
    'monster\aqua_female_armor',
    'monster\aqua_female_swim_pfx',
    'monster\aqua_male_armor',
    'monster\arahnid_burn_effect',
    'monster\arahnid_death_melee',
    'monster\arahnid_on_shoot_kill',
    'monster\arahnid_poison_hit',
    'monster\arahnid_res_death',
    'monster\l08_aqua_female_swim_pfx',
    'monster\nosalis_male_deathbyfire',
    'monster\vsref_aqua_female_deathbyfire',
    'monster\vsref_aqua_male_deathbyfire',
    'monster\vsref_arahnid_deathbyfire',
    'monster\vsref_nosalis_female_deathbyfire',
    'monster\vsref_nosalis_male_deathbyfire',
    'monster\vsref_watchman_deathbyfire',
    'random_fash_face',
    'temp\alarm_camp_p1',
    'temp\alarm_camp_p2',
    'temp\alarm_camp_p3',
    'temp\alarm_venice',
    'temp\antiwallman',
    'temp\aqua_bulb',
    'temp\cable_idle',
    'temp\cg_door',
    'temp\danger_checker',
    'temp\dlc_arena_teleport',
    'temp\dlc_armory_mantabat_1',
    'temp\dlc_armory_mantabat_2',
    'temp\dlc_npc_part2_die',
    'temp\dlc_tower_nos_in_water',
    'temp\face_idle_bandit',
    'temp\face_idle_inventor',
    'temp\fash_shoot_scary_2',
    'temp\fash_shoot_scary_4',
    'temp\goo_reaction',
    'temp\l04_catacombs_ach',
    'temp\l05_inter',
    'temp\l05_smeh',
    'temp\l06_flag_2',
    'temp\l06_glass\glass_big_parts',
    'temp\l06_glass\glass_big_parts_danger',
    'temp\l06_glass\glass_big_true',
    'temp\l06_glass\glass_strem_sound',
    'temp\l06_intro_fake_chest',
    'temp\l06_intro_fake_listen',
    'temp\l06_intro_fake_pressup',
    'temp\l06_intro_fake_salute',
    'temp\l06_intro_fake_walk',
    'temp\nos_female_effects',
    'temp\nos_jump_effects',
    'temp\npc_arah_fire_control',
    'temp\npc_radio_mls',
    'temp\npc_torchlight_lin',
    'temp\obst_ceiling_hard_script',
    'temp\obst_ceiling_light_script',
    'temp\obst_high_hard_script',
    'temp\obst_high_light_script',
    'temp\obst_low_hard_script',
    'temp\obst_low_light_script',
    'temp\polis_uber_anim_1',
    'temp\polis_uber_anim_zit',
    'temp\random_fash_anim',
    'temp\random_fash_scared_left',
    'temp\random_fash_scared_right',
    'temp\random_lent',
    'temp\sound_manager_1',
    'temp\sound_support',
    'temp\tarakan_hide',
    'temp\temp_dlc_end_level',
    'temp\tower_arm',
    'temp\tower_cheat_dispersion',
    'temp\tower_destroy_weapon',
    'temp\tower_nos_speed_control',
    'temp\tower_npc_logic',
    'temp\wall_dyn_h_city',
    'temp\wall_dyn_l_city',
    'temp\wall_dyn_m_city',
    'temp\wallman',
    'temp\wallman_v2',
    'temp\watchman_aura_lin',
    'temp\watchman_die_karma',
    'temp\watchman_hit_impulse',
    'temp\watchman_regular',
    'temp\yazoo\attach_feedback',
    'temp\yazoo\death_noregdoll_0',
    'temp\yazoo\death_noregdoll_ent',
    'temp\yazoo\l20\dead_friend',
    'temp\yazoo\l20\helm_close',
    'temp\yazoo\l20\interes_onplayer',
    'temp\yazoo\l20\l20_alarm_react',
    'temp\yazoo\l20\l20_alarm_react0',
    'temp\yazoo\l20\npc_cvr_events',
    'temp\yazoo\l20\npc_cvr_hit',
    'temp\yazoo\tolpa_d6_inside',
    'vs\levels\destroy_loot',
    'vs\levels\dlc\bib_melee_attack_1',
    'vs\levels\dlc\fire_ammo_death',
    'vs\levels\dlc\han_nos_snarl',
    'vs\levels\dlc\monster_long_destroy',
    'vs\levels\dlc\npc_blood',
    'vs\levels\gel_loot',
    'vs\levels\gel_loot_use',
    'vs\levels\l00_intro_2\auto_lamps',
    'vs\levels\l00_intro_2\bugs_hide',
    'vs\levels\l00_intro_2\mantabat_monsters',
    'vs\levels\l00_intro_2\mantabat_monsters_lin',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_2',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_3',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_4',
    'vs\levels\l00_intro_2\mantabat_monsters_lin_6',
    'vs\levels\l04_catacombs\l04_catacombs_cocon_mid',
    'vs\levels\l04_catacombs\l04_monst_destr',
    'vs\levels\l04_plane\demon_death',
    'vs\levels\l04_plane\watch_horde_destroy',
    'vs\levels\l10_swamp\aqua_demon_hit',
    'vs\levels\l10_swamp\bug',
    'vs\levels\l10_swamp\demon_att',
    'vs\levels\l10_swamp\manta_restore',
    'vs\levels\l10_swamp\naushnik_hit',
    'vs\levels\l10_swamp\swap_model_to_night',
    'vs\levels\l10_swamp\wall_aqua',
    'vs\levels\l11_undercity\l11_monster_destroy',
    'vs\levels\l13_1\nos_eye',
    'vs\levels\l13_1\water_avoid',
    'vs\levels\l16_dead_city\dark_loot',
    'vs\levels\l16_dead_city\demon_roof_attack_new',
    'vs\levels\l16_dead_city\far_ghost_aura',
    'vs\levels\l16_dead_city\ghost_aura',
    'vs\levels\l16_dead_city\metro_ghost_walk',
    'vs\levels\l16_dead_city\monster_destroy',
    'vs\levels\l16_dead_city\provod_short',
    'vs\levels\l16_dead_city\tryapka_1',
    'vs\levels\l16_dead_city\watch_aura_new',
    'vs\levels\l16_dead_city\watch_aura_new_2',
    'vs\levels\l18_garden\l18_arachnid_hide_water',
    'vs\levels\l18_garden\l18_buggy_static',
    'vs\levels\l18_garden\l18_dark_teleport',
    'vs\levels\l20\helm_lynx_close',
    'vs\levels\mob_loot',
    'vs\levels\mob_loot_ammo',
    'vs\levels\mob_loot_bib',
    'vs\monster\aqua_attack_watch\aqua_attack_watch_0000',
    'vs\monster\l04\rat_hide',
    'vs\monster\l04_catacombs\l04_cata_rats',
    'vs\monster\l10\aqua_big_attack',
    'vs\monster\l10\aqua_female_swim_part',
    'vs\monster\l10\aqua_male_swim_part',
    'vs\monster\l10\aqua_part_dead',
    'vs\monster\l10\demon_attack_no_p_force',
    'vs\monster\l10\demon_death',
    'vs\monster\l10\player_friend',
    'vs\monster\manta\arahi_go',
    'vs\monster\rat_hide',
    'vs\npc\collision',
    'vs\npc\cvr_force_fire',
    'vs\npc\death_noregdoll_test',
    'vs\npc\ik',
    'vs\npc\lian_att',
    'vs\npc\luminocity',
    'vs\npc\npc_fonarik',
    'vs\npc\npc_fonarik_helm',
    'vs\npc\npc_fonarik_lamp',
    'vs\npc\npc_gas_zone_1_mask',
    'vs\npc\npc_gas_zone_mls',
    'vs\npc\npc_melee_death_2',
    'vs\npc\npc_melee_death_2_no_stun',
    'vs\npc\npc_outdoor_breath',
    'vs\player\achievement',
    'vs\player\achievement_dlc',
    'vs\player\achievement_dlc2',
    'vs\player\bullets_dirt',
    'vs\player\civil_station',
    'vs\player\crouch_control',
    'vs\player\crouch_control_dlc1',
    'vs\player\death',
    'vs\player\death_dlc_armory',
    'vs\player\death_dlc_story_ann',
    'vs\player\end_level',
    'vs\player\fast_drop_hint',
    'vs\player\fire_zone',
    'vs\player\gas_zone',
    'vs\player\gas_zone_dlc_story_ann',
    'vs\player\hints',
    'vs\player\karma',
    'vs\player\map_task',
    'vs\player\no_electro_anomaly',
    'vs\player\rad_zone',
    'vs\player\rad_zone_dlc1',
    'vs\player\rain_zone',
    'vs\player\ranger_mode',
    'vs\player\take_weapon_hint',
    'vs\player\water_death',
    'vs\test\demon_npc_hit',
    'vs\test\l17\aura',
    'vs\weapon\fire_imitation_oneshot',
    'vs\weapon\fire_imitation_oneshot_noai',
    'vs\weapon\fire_imitation_oneshot_noai_true',
    'vs\weapon\fire_imitation_oneshot_shorts',
    'vs\weapon\laser_sight_disable_on_npc_death',
    'vslevelsl10_swampbug_2',
    'web_nospider_ach',
    
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
    
    // static data Last Light (239)
    'static_data\2301c4ef_2301c4ef_539d30e8', // static_data\staticprop__g.config.entity.static
    'static_data\7f6adca7_7f6adca7_5af35372', // static_data\weapon_item_noobetube__g.config.entity.weapon_noobtube
    'static_data\0cbbe8a6_0cbbe8a6_7b9fa65a', // static_data\o_hlamp__g.config.entity.hanging_lamp
    'static_data\ebc6700d_ebc6700d_76e6b55a', // static_data\weapon_item__g.config.entity.weapon_magazine
    'static_data\46985674_46985674_653d893f', // static_data\effect__g.config.entity.effect
    'static_data\186d3d2d_186d3d2d_ee3048bf', // static_data\weapon_item_noobegun__g.config.entity.weapon_noobgun
    'static_data\27fdf76a_27fdf76a_8fb8d996', // static_data\weapon_item_laser__g.config.entity.weapon_laser
    'static_data\b42ea669_b42ea669_6dfe6770', // static_data\patrol_point__g.config.entity.patrol_point
    'static_data\d91a939e_d91a939e_5ab4373b', // static_data\ammo__g.config.entity.ammo_12x70mm_fire
    'static_data\5aeb1d81_5aeb1d81_cd2679e2', // static_data\players_hands__g.config.entity.players_hands
    'static_data\d91a939e_d91a939e_81767601', // static_data\ammo__g.config.entity.ammo_044_ap
    'static_data\45215999_45215999_68518ffc', // static_data\player_map__g.config.entity.player_map
    'static_data\0f10b43b_0f10b43b_dd755ca5', // static_data\o_entity__g.config.entity.entity
    'static_data\d91a939e_d91a939e_172a1e1e', // static_data\ammo__g.config.entity.ammo_12x70mm
    'static_data\b713a3c2_b713a3c2_94b67c89', // static_data\medkit__g.config.entity.medkit
    'static_data\b616569a_b616569a_bf923686', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite
    'static_data\4015029a_4015029a_80e3ed61', // static_data\force_field__g.config.entity.force_field
    'static_data\2c1992e1_2c1992e1_4c834911', // static_data\weapon_launcher_time__g.config.entity.wpn_launcher_time_grenage
    'static_data\d91a939e_d91a939e_f2c05bb2', // static_data\ammo__g.config.entity.ammo_044_fmj
    'static_data\1c36400d_1c36400d_2e1a7721', // static_data\weapon_macheta__g.config.entity.wpn_macheta
    'static_data\d91a939e_d91a939e_4c6bd212', // static_data\ammo__g.config.entity.ammo_money
    'static_data\d91a939e_d91a939e_bc62ab9b', // static_data\ammo__g.config.entity.ammo_545x39_fmj
    'static_data\9151aa6f_9151aa6f_f7eb5312', // static_data\npc_fx__g.config.entity.npc_friend_fx
    'static_data\84260b0e_84260b0e_f89effd9', // static_data\proxy__g.config.entity.proxy
    'static_data\d91a939e_d91a939e_9ca45a39', // static_data\ammo__g.config.entity.ammo_762x39_fmj
    'static_data\05275e6e_05275e6e_4f2ab9f8', // static_data\o_basezone__g.config.entity.restrictor
    'static_data\d91a939e_d91a939e_d47ea361', // static_data\ammo__g.config.entity.ammo_dshk
    'static_data\72ad9e96_72ad9e96_a8ee8399', // static_data\charger__g.config.entity.charger
    'static_data\15583d02_15583d02_e8690f4b', // static_data\helsing_arrow__g.config.entity.wpn_arrow
    'static_data\473ce97a_473ce97a_1601ce81', // static_data\player_timer__g.config.entity.player_timer_entity
    'static_data\8f3c989b_8f3c989b_ac9947d0', // static_data\filter__g.config.entity.filter
    'static_data\04adf9c7_04adf9c7_049b81cd', // static_data\torch__g.config.entity.torchlight
    'static_data\b0584545_b0584545_98bdb76a', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flamethrower_grenage
    'static_data\8b576ec2_8b576ec2_a707b243', // static_data\weapon_sticky_dynamite__g.config.entity.wpn_sticky_dynamite
    'static_data\d91a939e_d91a939e_c8aefcf3', // static_data\ammo__g.config.entity.ammo_15mm
    'static_data\b0584545_b0584545_a9e80d6b', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_grenage
    'static_data\7a245948_7a245948_73a03954', // static_data\weapon_claymore__g.config.entity.wpn_claymore
    'static_data\d91a939e_d91a939e_a117d05d', // static_data\ammo__g.config.entity.ammo_762x39_mg
    'static_data\54571bfa_54571bfa_fd805e46', // static_data\visualscript__g.config.entity.vs
    'static_data\1f193ba1_1f193ba1_eb42f4ad', // static_data\weapon_dagger__g.config.entity.wpn_dagger
    'static_data\e8ca57b3_e8ca57b3_229f6c22', // static_data\o_waterzone__g.config.entity.waterzone
    'static_data\e13d62a8_e13d62a8_3b7e7fa7', // static_data\lighter__g.config.entity.lighter
    'static_data\8685b651_8685b651_1148d232', // static_data\players_knife__g.config.entity.players_knife
    'static_data\bd40ada9_bd40ada9_323af09f', // static_data\weapon_abzac__g.config.entity.wpn_abzac
    'static_data\71871a27_71871a27_685a059b', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74
    'static_data\68e1bde3_68e1bde3_4b4462a8', // static_data\player__g.config.entity.player
    'static_data\d91a939e_d91a939e_bdeb3a72', // static_data\ammo__g.config.entity.ammo_flame_fmj
    'static_data\ebc6700d_ebc6700d_0dd9d5eb', // static_data\weapon_item__g.config.entity.weapon_magazine_base_abzac
    'static_data\d91a939e_d91a939e_564ccb2b', // static_data\ammo__g.config.entity.ammo_12x70mm_flamethrower
    'static_data\27fdf76a_27fdf76a_887dff27', // static_data\weapon_item_laser__g.config.entity.weapon_laser_shotgun
    'static_data\ebc6700d_ebc6700d_3f3b57ec', // static_data\weapon_item__g.config.entity.weapon_autofire_abzac
    'static_data\ebc6700d_ebc6700d_5e065b85', // static_data\weapon_item__g.config.entity.weapon_magazine_abzac
    'static_data\ebc6700d_ebc6700d_6d213aca', // static_data\weapon_item__g.config.entity.weapon_compens_abzac
    'static_data\ebc6700d_ebc6700d_01b49735', // static_data\weapon_item__g.config.entity.weapon_silencer
    'static_data\ebc6700d_ebc6700d_4f7286df', // static_data\weapon_item__g.config.entity.weapon_magazine_2
    'static_data\ebc6700d_ebc6700d_37008456', // static_data\weapon_item__g.config.entity.weapon_envg
    'static_data\ebc6700d_ebc6700d_665b2889', // static_data\weapon_item__g.config.entity.weapon_scope
    'static_data\ebc6700d_ebc6700d_cf440776', // static_data\weapon_item__g.config.entity.weapon_optics_mount_ak
    'static_data\ebc6700d_ebc6700d_86a5a38e', // static_data\weapon_item__g.config.entity.weapon_kalimator
    'static_data\8f22a0ed_8f22a0ed_f39a543a', // static_data\woman__g.config.entity.woman
    'static_data\ebc6700d_ebc6700d_82be931d', // static_data\weapon_item__g.config.entity.weapon_silencer_saiga
    'static_data\ebc6700d_ebc6700d_f0a7eb0a', // static_data\weapon_item__g.config.entity.weapon_magazine_saiga
    'static_data\b574ea5d_b574ea5d_7f21d1cc', // static_data\o_explosion__g.config.entity.explosion
    'static_data\ebc6700d_ebc6700d_a3786564', // static_data\weapon_item__g.config.entity.weapon_magazine_base_saiga
    'static_data\2fa04ac0_2fa04ac0_fc6a503c', // static_data\kulemet__g.config.entity.kulemet_light
    'static_data\2fa04ac0_2fa04ac0_a5b4bb3b', // static_data\kulemet__g.config.entity.kulemet_bashnya
    'static_data\ebc6700d_ebc6700d_6b0641df', // static_data\weapon_item__g.config.entity.weapon_flashhider_ventil
    'static_data\ebc6700d_ebc6700d_980014e0', // static_data\weapon_item__g.config.entity.weapon_magazine_ventil
    'static_data\ebc6700d_ebc6700d_b90aa004', // static_data\weapon_item__g.config.entity.weapon_optics_mount_picatinny_ventil
    'static_data\ebc6700d_ebc6700d_32598731', // static_data\weapon_item__g.config.entity.weapon_magazine_base_ventil
    'static_data\ebc6700d_ebc6700d_33a16665', // static_data\weapon_item__g.config.entity.weapon_barrel_revolver
    'static_data\83c583e4_83c583e4_8a41e3f8', // static_data\weapon_revolver__g.config.entity.wpn_revolver
    'static_data\ebc6700d_ebc6700d_6e7242cf', // static_data\weapon_item__g.config.entity.weapon_no_butt_revolver
    'static_data\ebc6700d_ebc6700d_1d8f9dd5', // static_data\weapon_item__g.config.entity.weapon_collimator_small
    'static_data\ebc6700d_ebc6700d_d67646db', // static_data\weapon_item__g.config.entity.weapon_optics_mount_picatinny
    'static_data\ebc6700d_ebc6700d_3dda1af5', // static_data\weapon_item__g.config.entity.weapon_optics_sniper
    'static_data\ebc6700d_ebc6700d_ea72283e', // static_data\weapon_item__g.config.entity.weapon_magazine_preved
    'static_data\b616569a_b616569a_a429ee92', // static_data\weapon_dynamite__g.config.entity.wpn_dynamite_yz
    'static_data\ebc6700d_ebc6700d_402bbbef', // static_data\weapon_item__g.config.entity.weapon_magazine_base_preved
    'static_data\151dc865_151dc865_65d5e65c', // static_data\rat__g.config.entity.rat
    'static_data\ebc6700d_ebc6700d_c6dab65d', // static_data\weapon_item__g.config.entity.weapon_stabilizer_gear_gatling
    'static_data\e32722e3_e32722e3_177cedef', // static_data\weapon_preved__g.config.entity.wpn_preved
    'static_data\9151aa6f_9151aa6f_fe5690a7', // static_data\npc_fx__g.config.entity.npc_enemy_fx
    'static_data\13e11d26_13e11d26_9c9b4010', // static_data\weapon_saiga__g.config.entity.wpn_saiga
    'static_data\27dcdaf2_27dcdaf2_862a2a69', // static_data\o_aipoint__g.config.entity.ai_point
    'static_data\91551e3d_91551e3d_650ed131', // static_data\weapon_ventil__g.config.entity.wpn_ventil
    'static_data\2fa04ac0_2fa04ac0_4f0e9a77', // static_data\kulemet__g.config.entity.kulemet_light_bench
    'static_data\ebc6700d_ebc6700d_19747d01', // static_data\weapon_item__g.config.entity.weapon_flashhider_preved
    'static_data\ebc6700d_ebc6700d_e27788e1', // static_data\weapon_item__g.config.entity.weapon_butt_gatling
    'static_data\b5b0650c_b5b0650c_1ff32d7b', // static_data\weapon_gatling__g.config.entity.wpn_gatling_d6
    'static_data\ebc6700d_ebc6700d_7152687c', // static_data\weapon_item__g.config.entity.weapon_engine_base_frame_gatling
    'static_data\aa177401_aa177401_53549294', // static_data\staticprop_breakable__g.config.entity.static_breakable
    'static_data\ebc6700d_ebc6700d_a7975e59', // static_data\weapon_item__g.config.entity.weapon_stabilizer_gatling
    'static_data\ebc6700d_ebc6700d_8909bcdd', // static_data\weapon_item__g.config.entity.weapon_engine_base_disk_gatling
    'static_data\ebc6700d_ebc6700d_6e52ffbc', // static_data\weapon_item__g.config.entity.weapon_engine_fuel_gatling
    'static_data\ebc6700d_ebc6700d_86d16236', // static_data\weapon_item__g.config.entity.weapon_engine_frame_gatling
    'static_data\ebc6700d_ebc6700d_0e5f2d6c', // static_data\weapon_item__g.config.entity.weapon_engine_gatling
    'static_data\ebc6700d_ebc6700d_da6989ba', // static_data\weapon_item__g.config.entity.weapon_pentabarrel_gatling
    'static_data\ebc6700d_ebc6700d_895731de', // static_data\weapon_item__g.config.entity.weapon_stabilizer_wheel_gatling
    'static_data\ebc6700d_ebc6700d_5e4fdcc3', // static_data\weapon_item__g.config.entity.weapon_engine_base_arm_gatling
    'static_data\ebc6700d_ebc6700d_e1c4be32', // static_data\weapon_item__g.config.entity.weapon_engine_mech_gatling
    'static_data\ebc6700d_ebc6700d_c4ba7684', // static_data\weapon_item__g.config.entity.weapon_engine_cap_gatling
    'static_data\ebc6700d_ebc6700d_d7b55915', // static_data\weapon_item__g.config.entity.weapon_engine_base_gear_gatling
    'static_data\ebc6700d_ebc6700d_293ea716', // static_data\weapon_item__g.config.entity.weapon_tribarrel_gatling
    'static_data\ebc6700d_ebc6700d_f568347e', // static_data\weapon_item__g.config.entity.weapon_engine_base_gatling
    'static_data\ebc6700d_ebc6700d_6388322e', // static_data\weapon_item__g.config.entity.weapon_silencer_revolver
    'static_data\ebc6700d_ebc6700d_6647fa04', // static_data\weapon_item__g.config.entity.weapon_butt_revolver
    'static_data\ebc6700d_ebc6700d_00fa62fb', // static_data\weapon_item__g.config.entity.weapon_pivot_revolver
    'static_data\ebc6700d_ebc6700d_f3da7a78', // static_data\weapon_item__g.config.entity.weapon_optics_tihar
    'static_data\ebc6700d_ebc6700d_53365630', // static_data\weapon_item__g.config.entity.weapon_barrel_padonag
    'static_data\ebc6700d_ebc6700d_66f99440', // static_data\weapon_item__g.config.entity.weapon_tank_base_tihar
    'static_data\ebc6700d_ebc6700d_edbaa894', // static_data\weapon_item__g.config.entity.weapon_barrel_shotgun
    'static_data\ebc6700d_ebc6700d_d2c2de64', // static_data\weapon_item__g.config.entity.weapon_silencer_shotgun_pistol
    'static_data\65655829_65655829_bf264526', // static_data\arahind__g.config.entity.arahind
    'static_data\ebc6700d_ebc6700d_171b470d', // static_data\weapon_item__g.config.entity.weapon_no_butt_ashot
    'static_data\ebc6700d_ebc6700d_963b0e64', // static_data\weapon_item__g.config.entity.weapon_barrel_ashot
    'static_data\ebc6700d_ebc6700d_b41e34eb', // static_data\weapon_item__g.config.entity.weapon_magazine_rpk_base
    'static_data\ebc6700d_ebc6700d_10c2ba20', // static_data\weapon_item__g.config.entity.weapon_pivot_ashot
    'static_data\a6d94122_a6d94122_e42601e5', // static_data\big_mother__g.config.entity.big_mother
    'static_data\ebc6700d_ebc6700d_05c45a8d', // static_data\weapon_item__g.config.entity.weapon_butt_ashot
    'static_data\ebc6700d_ebc6700d_f1565dc7', // static_data\weapon_item__g.config.entity.weapon_butt_ubludok
    'static_data\ebc6700d_ebc6700d_a9d3bb8a', // static_data\weapon_item__g.config.entity.weapon_no_butt_ubludok
    'static_data\ebc6700d_ebc6700d_14b0402b', // static_data\weapon_item__g.config.entity.weapon_radiator_ubludok
    'static_data\a691b02a_a691b02a_3047382b', // static_data\weapon_ubludok__g.config.entity.wpn_ubludok_fx
    'static_data\81146306_81146306_5b577e09', // static_data\nosalis__g.config.entity.nosalis
    'static_data\ebc6700d_ebc6700d_49bf8626', // static_data\weapon_item__g.config.entity.weapon_optics_mount_vsv
    'static_data\a2812f11_a2812f11_d95e7de3', // static_data\weapon_duplet__g.config.entity.wpn_duplet_fx
    'static_data\ebc6700d_ebc6700d_9725d7fa', // static_data\weapon_item__g.config.entity.weapon_silencer_vsv
    'static_data\28005254_28005254_0ba58d1f', // static_data\grizly__g.config.entity.grizly
    'static_data\ebc6700d_ebc6700d_8eff4650', // static_data\weapon_item__g.config.entity.weapon_battery_hellbreath
    'static_data\ebc6700d_ebc6700d_414ac3fd', // static_data\weapon_item__g.config.entity.weapon_optics_mount_hellbreath
    'static_data\ebc6700d_ebc6700d_eeab07e3', // static_data\weapon_item__g.config.entity.weapon_capacitor_hellbreath
    'static_data\0e154f91_0e154f91_a51e31d5', // static_data\watchman__g.config.entity.watchman
    'static_data\ebc6700d_ebc6700d_0d1e3d40', // static_data\weapon_item__g.config.entity.weapon_silencer_aksu
    'static_data\27fdf76a_27fdf76a_ed48bba9', // static_data\weapon_item_laser__g.config.entity.weapon_laser_aksu
    'static_data\ebc6700d_ebc6700d_7f132140', // static_data\weapon_item__g.config.entity.weapon_silencer_padonag
    'static_data\ebc6700d_ebc6700d_0c2cd322', // static_data\weapon_item__g.config.entity.weapon_butt
    'static_data\1029375d_1029375d_8b3205b4', // static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx
    'static_data\6e8a02bb_6e8a02bb_81c8dff5', // static_data\nosalis_female__g.config.entity.nosalis_female
    'static_data\ebc6700d_ebc6700d_fa5b04ae', // static_data\weapon_item__g.config.entity.weapon_pivot
    'static_data\ebc6700d_ebc6700d_d5309510', // static_data\weapon_item__g.config.entity.weapon_magazine_base_padonag
    'static_data\ebc6700d_ebc6700d_b4ca4f1c', // static_data\weapon_item__g.config.entity.weapon_no_butt
    'static_data\ebc6700d_ebc6700d_93b1cc8f', // static_data\weapon_item__g.config.entity.weapon_autofire_2_padonag
    'static_data\ebc6700d_ebc6700d_99c7d339', // static_data\weapon_item__g.config.entity.weapon_autofire_padonag
    'static_data\ebc6700d_ebc6700d_244e2ec1', // static_data\weapon_item__g.config.entity.weapon_magazine_padonag
    'static_data\29b629e8_29b629e8_2151fab9', // static_data\aqua_female__g.config.entity.aqua_female
    'static_data\b616569a_b616569a_cb6ad99b', // static_data\weapon_dynamite__g.config.entity.wpn_aqua_sputum
    'static_data\39b5ac32_39b5ac32_eb164a07', // static_data\aqua_male_small__g.config.entity.aqua_male_small
    'static_data\57174204_57174204_d86d1f32', // static_data\weapon_ashot__g.config.entity.wpn_ashot
    'static_data\4a3d9517_4a3d9517_368561c0', // static_data\harpy__g.config.entity.harpy
    'static_data\41403263_41403263_b51bfd6f', // static_data\weapon_vyhlop__g.config.entity.wpn_vyhlop
    'static_data\8cd5596a_8cd5596a_3e7715a2', // static_data\aqua_male_big__g.config.entity.aqua_male_big
    'static_data\36fc912e_36fc912e_293f58d2', // static_data\weapon_aksu__g.config.entity.wpn_aksu
    'static_data\548be63c_548be63c_4dc174cf', // static_data\weapon_rpk__g.config.entity.wpn_rpk
    'static_data\ebc6700d_ebc6700d_e24cab6f', // static_data\weapon_item__g.config.entity.weapon_magazine_base_vyhlop
    'static_data\e74bf56a_e74bf56a_d567c246', // static_data\weapon_padonag__g.config.entity.wpn_padonag
    'static_data\27fdf76a_27fdf76a_ffaf0f23', // static_data\weapon_item_laser__g.config.entity.weapon_laser_hellbreath
    'static_data\ebc6700d_ebc6700d_47a5a1b1', // static_data\weapon_item__g.config.entity.weapon_optics_hellbreath
    'static_data\1029375d_1029375d_e67a9685', // static_data\weapon_uboynicheg__g.config.entity.wpn_bigun
    'static_data\ebc6700d_ebc6700d_52e17c03', // static_data\weapon_item__g.config.entity.weapon_duplet_no_butt
    'static_data\ebc6700d_ebc6700d_7534b7cc', // static_data\weapon_item__g.config.entity.weapon_bell_handle
    'static_data\ebc6700d_ebc6700d_489c7795', // static_data\weapon_item__g.config.entity.weapon_bell_base
    'static_data\ebc6700d_ebc6700d_a6359d08', // static_data\weapon_item__g.config.entity.weapon_tank_base_helsing
    'static_data\71871a27_71871a27_d126bfcc', // static_data\weapon_ak_74__g.config.entity.wpn_ak_74_l17_green_laser
    'static_data\e74bf56a_e74bf56a_7299fdad', // static_data\weapon_padonag__g.config.entity.wpn_padonag_green_laser
    'static_data\e32722e3_e32722e3_cbdd59e4', // static_data\weapon_preved__g.config.entity.wpn_preved_green_laser
    'static_data\93cfe433_93cfe433_2d2c81d9', // static_data\weapon_2012__g.config.entity.wpn_2012_green_laser
    'static_data\27fdf76a_27fdf76a_27d7d003', // static_data\weapon_item_laser__g.config.entity.weapon_laser_green
    'static_data\93cfe433_93cfe433_ba99c07b', // static_data\weapon_2012__g.config.entity.wpn_2012
    'static_data\91551e3d_91551e3d_b043f608', // static_data\weapon_ventil__g.config.entity.wpn_ventil_dlc_faction_nazi_green_laser
    'static_data\b5b0650c_b5b0650c_879c5220', // static_data\weapon_gatling__g.config.entity.wpn_gatling
    'static_data\1029375d_1029375d_b710633d', // static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx_green_laser
    'static_data\27fdf76a_27fdf76a_fc1cc2c6', // static_data\weapon_item_laser__g.config.entity.weapon_laser_shotgun_green
    'static_data\13e11d26_13e11d26_f6135754', // static_data\weapon_saiga__g.config.entity.wpn_saiga_green_laser
    'static_data\83c583e4_83c583e4_47fcda70', // static_data\weapon_revolver__g.config.entity.wpn_revolver_green_laser
    'static_data\945fd87c_945fd87c_56314427', // static_data\weapon_hellbreath__g.config.entity.wpn_hellbreath_green_laser
    'static_data\27fdf76a_27fdf76a_f25adf4a', // static_data\weapon_item_laser__g.config.entity.weapon_laser_hellbreath_green
    'static_data\945fd87c_945fd87c_9170e997', // static_data\weapon_hellbreath__g.config.entity.wpn_hellbreath
    'static_data\ebc6700d_ebc6700d_06b67403', // static_data\weapon_item__g.config.entity.weapon_optics_vyhlop
    'static_data\8083cc79_8083cc79_b2affb55', // static_data\weapon_helsing__g.config.entity.wpn_helsing
    'static_data\5060e530_5060e530_12dece68', // static_data\weapon_flamethrower__g.config.entity.wpn_flamethrower
    'static_data\36131f51_36131f51_c248d05d', // static_data\weapon_medved__g.config.entity.wpn_medved
    'static_data\b75365b6_b75365b6_38293880', // static_data\weapon_tihar__g.config.entity.wpn_tihar
    'static_data\1ba971fa_1ba971fa_02e3e309', // static_data\weapon_vsv__g.config.entity.wpn_vsv
    'static_data\ebc6700d_ebc6700d_8c5ab1f3', // static_data\weapon_item__g.config.entity.weapon_duplet_silencer
    'static_data\ebc6700d_ebc6700d_59d6656f', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel
    'static_data\ebc6700d_ebc6700d_442ad5c9', // static_data\weapon_item__g.config.entity.weapon_duplet_butt_2
    'static_data\ebc6700d_ebc6700d_2954bd4d', // static_data\weapon_item__g.config.entity.weapon_duplet_butt
    'static_data\ebc6700d_ebc6700d_761c9ee4', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel_big
    'static_data\ebc6700d_ebc6700d_1c011d41', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel_big_hammer
    'static_data\ebc6700d_ebc6700d_c667eeae', // static_data\weapon_item__g.config.entity.weapon_magazine_rpk
    'static_data\27fdf76a_27fdf76a_40d32ae7', // static_data\weapon_item_laser__g.config.entity.weapon_laser_vyhlop
    'static_data\2c0b059f_2c0b059f_0faedad4', // static_data\lurker__g.config.entity.lurker
    'static_data\ebc6700d_ebc6700d_c19fdfe4', // static_data\weapon_item__g.config.entity.weapon_silencer_shotgun
    'static_data\ebc6700d_ebc6700d_69bb4240', // static_data\weapon_item__g.config.entity.weapon_tank_helsing
    'static_data\ebc6700d_ebc6700d_42f15e29', // static_data\weapon_item__g.config.entity.weapon_tank_tihar
    'static_data\7975e29b_7975e29b_33014ca4', // static_data\librarian__g.config.entity.librarian
    'static_data\cfc909c3_cfc909c3_158a14cc', // static_data\gasmask__g.config.entity.gasmask
    'static_data\c7457ba3_c7457ba3_e4e0a4e8', // static_data\ladder__g.config.entity.ladder
    'static_data\d5452dd1_d5452dd1_121fc71c', // static_data\nightvision__g.config.entity.nightvision
    'static_data\0cbbe8a6_0cbbe8a6_c34945d2', // static_data\o_hlamp__g.config.entity.reflected_light
    'static_data\836e7adb_836e7adb_f3a654e2', // static_data\web__g.config.entity.web
    'static_data\57174204_57174204_3f31b037', // static_data\weapon_ashot__g.config.entity.wpn_ashot_dlc_class_pack
    'static_data\572f065c_572f065c_748ad917', // static_data\flower__g.config.entity.flower
    'static_data\f7cb9c05_f7cb9c05_93d0fceb', // static_data\lian__g.config.entity.lian
    'static_data\e74bf56a_e74bf56a_04633c14', // static_data\weapon_padonag__g.config.entity.wpn_padonag_dlc_class_pack
    'static_data\83c583e4_83c583e4_662fbdf0', // static_data\weapon_revolver__g.config.entity.wpn_revolver_dlc_class_pack
    'static_data\1029375d_1029375d_82d61831', // static_data\weapon_uboynicheg__g.config.entity.wpn_uboynicheg_fx_dlc_class_pack
    'static_data\a2812f11_a2812f11_0a71f95c', // static_data\weapon_duplet__g.config.entity.wpn_duplet_fx_dlc_class_pack
    'static_data\ebc6700d_ebc6700d_28add819', // static_data\weapon_item__g.config.entity.weapon_duplet_barrel_big_class_pack
    'static_data\4250e763_4250e763_0824495c', // static_data\darkchild__g.config.entity.darkchild
    'static_data\eeffe3be_eeffe3be_0106f86f', // static_data\o_interest__g.config.entity.interest
    'static_data\b713a3c2_b713a3c2_9884d789', // static_data\medkit__g.config.entity.medkit_dlc_story_ann
    'static_data\27fdf76a_27fdf76a_c27f1273', // static_data\weapon_item_laser__g.config.entity.weapon_laser_dlc_story_ann
    'static_data\e32722e3_e32722e3_9f377752', // static_data\weapon_preved__g.config.entity.wpn_preved_dlc_story_ann_far
    'static_data\cfc909c3_cfc909c3_ae22d6fc', // static_data\gasmask__g.config.entity.gasmask_dlc_story_ann
    'static_data\e32722e3_e32722e3_71f02ab1', // static_data\weapon_preved__g.config.entity.wpn_preved_dlc_story_ann
    'static_data\1f193ba1_1f193ba1_9c74a56e', // static_data\weapon_dagger__g.config.entity.wpn_dagger_dlc_story_ann
    'static_data\1c36400d_1c36400d_c2db3eb8', // static_data\weapon_macheta__g.config.entity.wpn_macheta_dlc_story_ann
    'static_data\68e1bde3_68e1bde3_b21f88bb', // static_data\player__g.config.entity.player_dlc_story_ann
    'static_data\d384caf6_d384caf6_a34ce4cf', // static_data\kid__g.config.entity.kid
    'static_data\6881c5cb_6881c5cb_a8772a30', // static_data\woman_strip__g.config.entity.woman_strip
    'static_data\03016a46_03016a46_0be6b917', // static_data\soft_entity__g.config.entity.soft_entity
    'static_data\b616569a_b616569a_2941c323', // static_data\weapon_dynamite__g.config.entity.wpn_nosalis_sputum
    'static_data\2de9504f_2de9504f_49f230a1', // static_data\dark__g.config.entity.dark
    'static_data\8d41ffd3_8d41ffd3_5702e2dc', // static_data\effectm__g.config.entity.effectm
    'static_data\2fa04ac0_2fa04ac0_f5e357cf', // static_data\kulemet__g.config.entity.kulemet
    'static_data\23b85b2c_23b85b2c_cd459813', // static_data\hands_for_drezina__g.config.entity.hands_for_drezina
    'static_data\93bf64c4_93bf64c4_f7a4042a', // static_data\bush__g.config.entity.bush
    'static_data\9b9f0f1f_9b9f0f1f_8422e434', // static_data\drezina_hand__g.config.entity.drezina_hand
    'static_data\8120f174_8120f174_9e9d1a5f', // static_data\drezina_moto__g.config.entity.drezina_moto
    'static_data\9b1fdd5e_9b1fdd5e_1dc2b93a', // static_data\siege_bomb__g.config.entity.siege_bomb
    'static_data\8bbccf55_8bbccf55_f5752449', // static_data\heap__g.config.entity.heap_2
    'static_data\b0584545_b0584545_3a2bc827', // static_data\weapon_flame_dynamite__g.config.entity.wpn_flame_good
    'static_data\67a85977_67a85977_57fb971c', // static_data\breakable_ice__g.config.entity.breakable_ice
    'static_data\151dc865_151dc865_a34ce4cf', // static_data\rat__g.config.entity.kid
    'static_data\301e54df_301e54df_4e503fce', // static_data\stretchy_man__g.config.entity.stretchy_man
    'static_data\301e54df_301e54df_4578e2f6', // static_data\stretchy_man__g.config.entity.stretchy_handleft
    'static_data\301e54df_301e54df_d3910576', // static_data\stretchy_man__g.config.entity.stretchy_hand
    
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
    
    // visual scripts Arktika.1 (69)
    'human\vsref_npc_human_deathbyfire',
    'mishen_hit',
    'npc_sound_hitmarks_vr',
    'sf4_col_maniken',
    'subscripts\weapon\modular_gun_module_fx',
    'temp\3523',
    'temp\blink',
    'temp\butilka_hit',
    'temp\death_from_laser',
    'temp\death_from_laser_sniper',
    'temp\death_from_laser_vr_sniper',
    'temp\death_from_plasma',
    'temp\dron_target',
    'temp\for_hunter_attach',
    'temp\hit_sf4_col',
    'temp\lifgr_tree',
    'temp\oculus\ach_box_killer',
    'temp\oculus\ach_killer',
    'temp\oculus\achievement_oculus_new',
    'temp\oculus\fire_extinguisher_npc',
    'temp\oculus\fudo_shot',
    'temp\oculus\hit_mark_sfrom_small',
    'temp\oculus\key_combat_2npc',
    'temp\oculus\movement_type_change',
    'temp\oculus\multi_hit_rsnake',
    'temp\oculus\npc_breath',
    'temp\oculus\npc_grenade_targeting',
    'temp\oculus\npc_grenade_targeting_heavy',
    'temp\oculus\npc_grenade_targeting_heavy_vr',
    'temp\oculus\npc_grenade_targeting_sniper',
    'temp\oculus\npc_grenade_targeting_sniper_vr',
    'temp\oculus\npc_grenade_targeting_vr',
    'temp\oculus\npc_lose_of_player',
    'temp\oculus\npc_sound_hitmarks',
    'temp\oculus\npc_stun_grenade',
    'temp\oculus\npc_surrender',
    'temp\oculus\npc_torch_reaction',
    'temp\oculus\oculus_stars',
    'temp\oculus\perk_all_die',
    'temp\oculus\sniper_icon',
    'temp\oculus\sniper_radar',
    'temp\oculus\stun_death_check',
    'temp\oculus\stun_from_freccia',
    'temp\oculus\torch_player_check',
    'temp\oculus\vr_mission_cube_01_generic',
    'temp\oculus\vr_mission_cube_01_generic_no_move',
    'temp\oculus\vr_mission_cube_01_simple',
    'temp\oculus\vr_mission_cube_02',
    'temp\oculus\vr_mission_cube_perf',
    'temp\oculus_challenges',
    'temp\oculus_monster_death',
    'temp\sound_support',
    'temp\throwable_object',
    'temp\trail_for_dron_engine',
    'temp\yazoo\attach_feedback',
    'vs\npc\m3\role_machine_gunner_oculus',
    'vs\player\bullets_dirt',
    'vs\player\civil_station',
    'vs\player\crouch_control_dlc1',
    'vs\player\death',
    'vs\player\end_level',
    'vs\player\jumpover',
    'vs\player\karma',
    'vs\player\map_task_2033',
    'vs\player\metro3\binocular',
    'vs\player\metro3\bracer',
    'vs\player\metro3\general_rules',
    'vs\player\safe',
    'vs\player\weapon',
    
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