program split;
uses windows, classes, sysutils, uCrc;

var
  knownscripts : array[1..255] of String = (
    // configs (44)
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
    // levels (79)
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
    // static data (101)
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
    // skeleton sequence value (22)
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
    'dynamic\hud\weapon\vsv\vsv_skeleton_sequence_value'
  );


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