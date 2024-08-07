this.get_class = function(crc)
{
	var classname = hash_table_classes[crc];
	if(!classname)
	{		
		print("unknown class found, crc ", crc);
		return "";
	}
	else
		return classname;
}

this.get_static_data_key = function(crc)
{
	var static_data_key = hash_table_static_data_keys[crc];
	if(!static_data_key)
	{
		print("unknown static data key found, crc ", crc);
		return "";
	}
	else
		return static_data_key;
}

var classes = [
	"AMEBA",
	"AMMO",
	"ANIMSCRIPT",
	"ANIM_OBJECT",
	"ANOMALY",
	"AQUA_FEMALE",
	"AQUA_MALE_BIG",
	"AQUA_MALE_SMALL",
	"ARAHIND",
	"ARM_DEVICES",
	"BIG_MOTHER",
	"BINOCULARS",
	"BIOMASS_LARGE",
	"BOAT",
	"BREAKABLE_ADVANCED",
	"BREAKABLE_ICE",
	"BROADENED_LIAN",
	"BUSH",
	"BUSH_ZONE",
	"CAM_TRACK",
	"CANNIBAL",
	"CATFISH",
	"CHARGER",
	"CLAYMORE_ZONE",
	"COMPASS",
	"COSTUME_UPGRADE",
	"DARK",
	"DARKCHILD",
	"DEER",
	"DEVICE",
	"DEVICE_UPGRADE",
	"DOG",
	"DREZINA_HAND",
	"DREZINA_MOTO",
	"DUMMY_NPC",
	"EFFECT",
	"EFFECTM",
	"EFFECT_PAUSABLE",
	"FILTER",
	"FLEXIBLE_ENTITY",
	"FLOWER",
	"FORCE_FIELD",
	"GASMASK",
	"GATEWAY",
	"GRAB_ZONE",
	"GRIZLY",
	"G_LEVEL",
	"G_PERSIS",
	"HANDS_FOR_DREZINA",
	"HARPY",
	"HEAD_SHOT",
	"HEAP",
	"HEAP_ZONE",
	"HELSING_ARROW",
	"HELSING_ARROW_BREAKABLE",
	"HUD_MNGR",
	"HUMAN",
	"HUMANIMAL",
	"KID",
	"KULEMET",
	"LADDER",
	"LIAN",
	"LIBRARIAN",
	"LIGHTER",
	"LURKER",
	"MAGNETIC_HOLSTER",
	"MAP_PAD_UPGRADE",
	"MECH_ENTITY",
	"MEDKIT",
	"MEDKIT_HEAP",
	"MEDKIT_SPECIAL",
	"MONSTER",
	"MOTION_SENSOR_UPGRADE",
	"MOUNT_GUN",
	"NIGHTVISION",
	"NOSACH",
	"NOSALIS",
	"NOSALIS_FEMALE",
	"NPC",
	"NPC_FX",
	"OBJECT",
	"O_AIPOINT",
	"O_ANIM_ENTITY",
	"O_BASEZONE",
	"O_BRKBL",
	"O_COVER_LINK",
	"O_DUMMY",
	"O_ENTITY",
	"O_EXPLOSION",
	"O_HELPERTEXT",
	"O_HELPERTEXT_COUNTER",
	"O_HELPERTEXT_VR_INFO",
	"O_INTEREST",
	"O_PHYSICS",
	"O_SCALING_ENTITY",
	"O_WATERZONE",
	"PARTICLES",
	"PATROL_POINT",
	"PLAYER",
	"PLAYERS_BODY",
	"PLAYERS_HANDS",
	"PLAYERS_KNIFE",
	"PLAYER_MAP",
	"PLAYER_TIMER",
	"PLAYER_TIMER_2033",
	"PLAYER_TIMER_UPGRADE",
	"PROXY",
	"PULSOMETER_UPGRADE",
	"RABBIT",
	"RAGDOLL",
	"RAT",
	"SCRIPTED_ENTITY",
	"SHIELD",
	"SIMPLE",
	"SIMPLE_MONSTER",
	"SIMPLE_NPC",
	"SNAKE",
	"SOFT_ENTITY",
	"SOFT_ENTITY_INST",
	"STATICPROP",
	"STATICPROP_BREAKABLE",
	"STATION_STAND",
	"STRETCHY_HAND",
	"STRETCHY_HANDLEFT",
	"STRETCHY_MAN",
	"TAPE",
	"TELEPORT",
	"TORCH",
	"TORCHLIGHT",
	"TORCHLIGHT_UPGRADABLE",
	"TURRET",
	"VEHICLE",
	"VIRTUAL_CAMERA",
	"VIRTUAL_HAND",
	"VIRTUAL_MONITOR",
	"VISOR",
	"VISUALSCRIPT",
	"VISUALSCRIPT_REF",
	"VR_CUBE",
	"VR_ENTITY",
	"VR_WEAPON",
	"VR_WEAPON_CARVER",
	"VR_WEAPON_MODULAR",
	"VR_WEAPON_SF4",
	"WALLMARK",
	"WATCHMAN",
	"WAVES_EMITTER",
	"WEAPON",
	"WEAPON_2012",
	"WEAPON_ABZAC",
	"WEAPON_AK74",
	"WEAPON_AK74_TEST",
	"WEAPON_AKSU",
	"WEAPON_AK_74",
	"WEAPON_ASHOT",
	"WEAPON_ASHOT_2B",
	"WEAPON_BIGUN",
	"WEAPON_C4_DYNAMITE",
	"WEAPON_CLAYMORE",
	"WEAPON_DAGGER",
	"WEAPON_DECOY",
	"WEAPON_DSHK",
	"WEAPON_DUPLET",
	"WEAPON_DUPLET_2B",
	"WEAPON_DUPLET_3B",
	"WEAPON_DUPLET_4B",
	"WEAPON_DYNAMITE",
	"WEAPON_FAKE",
	"WEAPON_FLAME",
	"WEAPON_FLAMETHROWER",
	"WEAPON_FLAME_DYNAMITE",
	"WEAPON_FLAME_GRENADE",
	"WEAPON_FLARE",
	"WEAPON_GATLING",
	"WEAPON_HELLBREATH",
	"WEAPON_HELLBREATH_SHOCK",
	"WEAPON_HELSING",
	"WEAPON_ITEM",
	"WEAPON_ITEM_AMMO",
	"WEAPON_ITEM_LASER",
	"WEAPON_ITEM_MAGAZINE",
	"WEAPON_ITEM_NOOBEGUN",
	"WEAPON_ITEM_NOOBETUBE",
	"WEAPON_ITEM_OPTIC",
	"WEAPON_ITEM_SILENCER",
	"WEAPON_ITEM_VR",
	"WEAPON_ITEM_VR_ATTACH",
	"WEAPON_KNIVES",
	"WEAPON_LASER",
	"WEAPON_LAUNCHER",
	"WEAPON_LAUNCHER_TIME",
	"WEAPON_MACHETA",
	"WEAPON_MAG",
	"WEAPON_MEDVED",
	"WEAPON_NOOB_LAUNCHER",
	"WEAPON_NOOB_SHOTGUN",
	"WEAPON_PADONAG",
	"WEAPON_PLASMA",
	"WEAPON_PREVED",
	"WEAPON_REVOLVER",
	"WEAPON_RPK",
	"WEAPON_SAIGA",
	"WEAPON_SHOTGUN",
	"WEAPON_STICKY_DYNAMITE",
	"WEAPON_TIHAR",
	"WEAPON_TUMAK",
	"WEAPON_UBLUDOK",
	"WEAPON_UBOYNICHEG",
	"WEAPON_VENTIL",
	"WEAPON_VSV",
	"WEAPON_VYHLOP",
	"WEB",
	"WEB_ZONE",
	"WOMAN",
	"WOMAN_COMBAT",
	"WOMAN_STRIP",
	"headquarter",
	"o_hlamp",
	"s_actor",
	"siege_bomb"
];

var static_data_keys = [
	"_G.config.entity.ai_point",
	"_G.config.entity.ammo_044_ap",
	"_G.config.entity.ammo_044_fmj",
	"_G.config.entity.ammo_12x70mm",
	"_G.config.entity.ammo_12x70mm_flamethrower",
	"_G.config.entity.ammo_12x70mm_macheta_alt",
	"_G.config.entity.ammo_12x70mm_macheta_fast",
	"_G.config.entity.ammo_15mm",
	"_G.config.entity.ammo_15mm_sabot",
	"_G.config.entity.ammo_545x39_fmj",
	"_G.config.entity.ammo_762x39_fmj",
	"_G.config.entity.ammo_762x39_fmj_unlim",
	"_G.config.entity.ammo_762x39_fmj_vr",
	"_G.config.entity.ammo_762x39_mg",
	"_G.config.entity.ammo_baranka_laser",
	"_G.config.entity.ammo_flamethrower_alt",
	"_G.config.entity.ammo_fudo_no_ricochet",
	"_G.config.entity.ammo_fudo_ricochet",
	"_G.config.entity.ammo_handgun_oculus",
	"_G.config.entity.ammo_handgun_oculus_alt",
	"_G.config.entity.ammo_laser_gun",
	"_G.config.entity.ammo_makeshift_oculus",
	"_G.config.entity.ammo_makeshift_oculus_freccia",
	"_G.config.entity.ammo_minotaur_lin",
	"_G.config.entity.ammo_minotaur_lin_alt",
	"_G.config.entity.ammo_modular_emag_em",
	"_G.config.entity.ammo_modular_emag_laser",
	"_G.config.entity.ammo_modular_emag_plasma",
	"_G.config.entity.ammo_modular_mode_automatic",
	"_G.config.entity.ammo_modular_mode_cannon",
	"_G.config.entity.ammo_modular_mode_frequency_high",
	"_G.config.entity.ammo_modular_mode_frequency_low",
	"_G.config.entity.ammo_modular_mode_frequency_med",
	"_G.config.entity.ammo_modular_mode_lighting",
	"_G.config.entity.ammo_modular_mode_railgun",
	"_G.config.entity.ammo_money",
	"_G.config.entity.ammo_plasma_frags",
	"_G.config.entity.ammo_plasma_frags_ovr",
	"_G.config.entity.ammo_plasma_rifle",
	"_G.config.entity.ammo_rifle_oculus",
	"_G.config.entity.ammo_sf4ag_arrow",
	"_G.config.entity.ammo_sf4ag_arrow_pin",
	"_G.config.entity.ammo_shotgun_alt_oculus",
	"_G.config.entity.ammo_shotgun_e_mag_oculus",
	"_G.config.entity.ammo_shotgun_oculus",
	"_G.config.entity.ammo_shotgun_sf4ag_oculus",
	"_G.config.entity.ammo_sniper_oculus",
	"_G.config.entity.ammo_turret_new",
	"_G.config.entity.anim_entity",
	"_G.config.entity.craft_chemical",
	"_G.config.entity.craft_consumable",
	"_G.config.entity.craft_metal",
	"_G.config.entity.dummy",
	"_G.config.entity.effect",
	"_G.config.entity.effect_pausable",
	"_G.config.entity.effectm",
	"_G.config.entity.entity",
	"_G.config.entity.explosion",
	"_G.config.entity.explosion_oculus",
	"_G.config.entity.explosion_oculus_drone",
	"_G.config.entity.explosion_oculus_rocket",
	"_G.config.entity.flexible_entity",
	"_G.config.entity.force_field",
	"_G.config.entity.grab_zone",
	"_G.config.entity.hanging_lamp",
	"_G.config.entity.humanimal",
	"_G.config.entity.humanimal_oculus",
	"_G.config.entity.magnetic_holster",
	"_G.config.entity.mech_entity",
	"_G.config.entity.npc_enemy_fx",
	"_G.config.entity.npc_friend_fx",
	"_G.config.entity.npc_teleporting_boss",
	"_G.config.entity.patrol_point",
	"_G.config.entity.plasma_ball_expl",
	"_G.config.entity.plasma_ball_expl_alt_high",
	"_G.config.entity.plasma_ball_expl_alt_low",
	"_G.config.entity.plasma_ball_expl_alt_med",
	"_G.config.entity.plasma_ball_expl_alt_ult",
	"_G.config.entity.player",
	"_G.config.entity.players_hands",
	"_G.config.entity.proxy",
	"_G.config.entity.rat",
	"_G.config.entity.restrictor",
	"_G.config.entity.ruckus",
	"_G.config.entity.scaling_entity",
	"_G.config.entity.scripted_entity",
	"_G.config.entity.simple_anomaly",
	"_G.config.entity.soft_entity",
	"_G.config.entity.static",
	"_G.config.entity.static_breakable",
	"_G.config.entity.teleport",
	"_G.config.entity.text_helper",
	"_G.config.entity.text_helper_counter",
	"_G.config.entity.text_helper_vr_info",
	"_G.config.entity.torchlight_upgradable",
	"_G.config.entity.turret",
	"_G.config.entity.virtual_camera",
	"_G.config.entity.virtual_hand",
	"_G.config.entity.virtual_monitor",
	"_G.config.entity.visor0",
	"_G.config.entity.visor1",
	"_G.config.entity.visor2",
	"_G.config.entity.visor3",
	"_G.config.entity.vr_attach_base",
	"_G.config.entity.vr_cube",
	"_G.config.entity.vr_decoy",
	"_G.config.entity.vr_decoy_npc",
	"_G.config.entity.vr_entity",
	"_G.config.entity.vr_module_ammo_laser_ricochet",
	"_G.config.entity.vr_module_barrel",
	"_G.config.entity.vr_module_barrel_accuracy",
	"_G.config.entity.vr_module_barrel_damage",
	"_G.config.entity.vr_module_battery",
	"_G.config.entity.vr_module_cylinder",
	"_G.config.entity.vr_module_magazine",
	"_G.config.entity.vr_module_rotor_em",
	"_G.config.entity.vr_module_rotor_plasma",
	"_G.config.entity.vr_weapon",
	"_G.config.entity.vs",
	"_G.config.entity.waterzone",
	"_G.config.entity.woman",
	"_G.config.entity.wpn_ak74_oculus",
	"_G.config.entity.wpn_ak74_oculus_griz",
	"_G.config.entity.wpn_ak74_oculus_plasma_rifle",
	"_G.config.entity.wpn_ak_74",
	"_G.config.entity.wpn_ak_74_ng",
	"_G.config.entity.wpn_arrow",
	"_G.config.entity.wpn_arrow_breakable",
	"_G.config.entity.wpn_ashot_ng",
	"_G.config.entity.wpn_baranka",
	"_G.config.entity.wpn_e_mag_oculus",
	"_G.config.entity.wpn_flame_grenage",
	"_G.config.entity.wpn_flare",
	"_G.config.entity.wpn_fudo_hell",
	"_G.config.entity.wpn_fudo_hell_jeremie",
	"_G.config.entity.wpn_gatling_l17_pasha",
	"_G.config.entity.wpn_humanimal_stuff",
	"_G.config.entity.wpn_lpt",
	"_G.config.entity.wpn_modular_gun",
	"_G.config.entity.wpn_punisher_oculus",
	"_G.config.entity.wpn_revolver_vr",
	"_G.config.entity.wpn_rsnake",
	"_G.config.entity.wpn_rsnake_drone",
	"_G.config.entity.wpn_saiga",
	"_G.config.entity.wpn_sf4ag",
	"_G.config.entity.wpn_sf4ag_gun",
	"_G.config.entity.wpn_spray",
	"_G.config.entity.wpn_spray_2",
	"_G.config.entity.wpn_spray_3",
	"_G.config.entity.wpn_spray_4",
	"_G.config.entity.wpn_spray_5",
	"_G.config.entity.wpn_spray_6",
	"_G.config.entity.wpn_stechkin",
	"_G.config.entity.wpn_ventil",
	// my invention, doesn't exist in original game
	"_G.config.entity.vehicle",
	"_G.config.entity.weapon_magazine"
];

var hash_table_classes = new Object;
for(var i = 0; i < classes.length; i++)
	hash_table_classes[crc32(classes[i])] = classes[i];
	
var hash_table_static_data_keys = new Object;
for(var i = 0; i < static_data_keys.length; i++)
	hash_table_static_data_keys[crc32(static_data_keys[i])] = static_data_keys[i];