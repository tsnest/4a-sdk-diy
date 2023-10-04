unit Engine;

interface
uses Konfig;

type
	TEngineVersion = (
		eVer2033,
		eVerLLBeta15102012,
		eVerLLBeta03122012,
		eVerLL,
		eVerRedux,
		eVerArktika1,
		eVerExodus,
		eVerUnknown
	);

var
	version : TEngineVersion = eVerUnknown;	
	ResourcesPath : String = 'content';
	LevelPath : String;

procedure InitializeEngine;
procedure FinalizeEngine;

function GetSoundParams : TTextKonfig;

implementation
uses sysutils, framework, KonfigLibrary, texturePrefs, Texture, uWeather;

var
	sound_params : TTextKonfig;

function GuessEngineVer : TEngineVersion;
var
	has_scriptsbin : Boolean;
	has_configbin : Boolean;
	has_texturesbin : Boolean;
	has_textures_handles_storage : Boolean;
	has_vr_attach_base : Boolean;
begin
	has_scriptsbin := False;
	has_configbin := False;
	has_texturesbin := False;
	has_textures_handles_storage := False;
	has_vr_attach_base := False;
	
	if FileExists(ResourcesPath + '\scripts.bin') then
	begin
		has_scriptsbin := True;
	end else
	if FileExists(ResourcesPath + '\config.bin') then
	begin
		has_configbin := True;
	end;

	if FileExists(ResourcesPath + '\textures\textures.bin') then
	begin
		has_texturesbin := True;
	end;
	
	if FileExists(ResourcesPath + '\textures_handles_storage.bin') then
	begin
		has_textures_handles_storage := True;
	end;
	
	// не самый лучший спобов отличать арктику.1
	// т.к. этот файл распаковывать необязательно и редактору он нафиг не нужен
	// но я ничего лучше не вижу
	if FileExists(ResourcesPath + '\weaponry\attaches\vr_attach_base.bin') then
	begin
		has_vr_attach_base := True;
	end;
	
	if has_scriptsbin then
	begin
		GuessEngineVer := eVer2033;
	end else
	begin
		if has_texturesbin then
		begin
			GuessEngineVer := eVerLL;
		end else
		begin
			if has_textures_handles_storage then
			begin
				GuessEngineVer := eVerExodus;
			end else
			begin
				if has_vr_attach_base then
					GuessEngineVer := eVerArktika1
				else
					GuessEngineVer := eVerRedux;
			end;
		end;
	end;	
end;

procedure InitializeEngine;
var
	tex_prefs_version : TTBVersion;
begin
	if version = eVerUnknown then
		version := GuessEngineVer;
	
	Write('Engine version is ');
	case version of
		eVer2033:           Write('Metro 2033');
		eVerLL:             Write('Metro Last Light');
		eVerLLBeta15102012: Write('Build 2662');		
		eVerLLBeta03122012: Write('Build 2711');
		eVerRedux:          Write('Metro Redux');
		eVerArktika1:       Write('Arktika.1');
		eVerExodus:         Write('Metro Exodus');
		eVerUnknown:        Write('Unknown');
	end;
	WriteLn;
	
	if version = eVer2033 then
	begin
		if FileExists(ResourcesPath + '\scripts.bin') then
			KonfigLibrary.Load(ResourcesPath + '\scripts.bin')
		else
			WriteLn('content\scripts.bin not found');
	end else
	begin
		if FileExists(ResourcesPath + '\config.bin') then
			KonfigLibrary.Load(ResourcesPath + '\config.bin')
		else
			WriteLn('content\config.bin not found');
	end;

	if FileExists(ResourcesPath + '\textures\textures.bin') then
	begin
		if version in [eVerLLBeta15102012, eVerLLBeta03122012] then
			tex_prefs_version := tbVerLLBeta15102012
		else
			tex_prefs_version := tbVerUnknown;
			
		try
			texture_params := TTexturesBin.CreateAndLoad(ResourcesPath + '\textures\textures.bin', tex_prefs_version);
		except on E: Exception do
			WriteLn('textures.bin loading failed'#10 + E.ClassName + ': ' + E.Message);
		end;
	end;
	
	if FileExists(ResourcesPath + '\scripts\texture_aliases.bin') then
	begin
		try
			texture_aliases := TTextureAliases.CreateAndLoad(ResourcesPath + '\scripts\texture_aliases.bin');
		except on E: Exception do
			WriteLn('texture_aliases.bin loading failed'#10 + E.ClassName + ': ' + E.Message);
		end;
	end;
	
	if FileExists(ResourcesPath + '\textures_handles_storage.bin') then
	begin
		texture_params2 := TTexturesBin2.Create;
		texture_params2.Load(ResourcesPath + '\textures_handles_storage.bin');
	end;
	
	uWeather.Initialize;
end;

procedure FinalizeEngine;
begin
	uWeather.Finalize;

	FreeAndNil(texture_params);
	FreeAndNil(texture_params2);
	FreeAndNil(texture_aliases);
	
	FreeAndNil(sound_params);
end;

function GetSoundParams : TTextKonfig;
var
	K : TKonfig;
	js : TFramework;
begin
	if sound_params = nil then
	begin
		if FileExists(ResourcesPath + '\sounds\sounds.bin') then
		begin
			js := TFramework.Create;
			
			K := TKonfig.Create;
			K.Load(ResourcesPath + '\sounds\sounds.bin');
			
			case version of
				eVer2033:           sound_params := js.DecompileKonfig(K, 'js\2033\sounds.js');
				eVerLLBeta15102012: sound_params := js.DecompileKonfig(K, 'js\ll_beta_15_10_2012\sounds.js');
				eVerLLBeta03122012,
				eVerLL:             sound_params := js.DecompileKonfig(K, 'js\ll\sounds.js');
				eVerRedux:          sound_params := js.DecompileKonfig(K, 'js\redux\sounds.js');
			end;
			
			K.Free;
			js.Free;
		end;
	end;
	
	Result := sound_params;
end;

end.
