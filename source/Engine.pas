unit Engine;

interface
uses Konfig;

type
	TEngineVersion = (
		eVer2033,
		eVerLL,
		eVerRedux,
		eVerArktika1,
		eVerExodus
	);

var
	version : TEngineVersion;	
	ResourcesPath : String = 'content';
	LevelPath : String;
	
	sound_params : TTextKonfig;

procedure InitializeEngine;
procedure FinalizeEngine;

implementation
uses sysutils, framework, KonfigLibrary, texturePrefs, Texture, uWeather;

procedure InitializeEngine;
var
	has_scriptsbin : Boolean;
	has_configbin : Boolean;
	has_texturesbin : Boolean;
	has_textures_handles_storage : Boolean;
	
	K : TKonfig;
begin
	has_scriptsbin := False;
	has_configbin := False;
	has_texturesbin := False;
	has_textures_handles_storage := False;
	
	if FileExists(ResourcesPath + '\scripts.bin') then
	begin
		KonfigLibrary.Load(ResourcesPath + '\scripts.bin');
		has_scriptsbin := True;
	end else
	if FileExists(ResourcesPath + '\config.bin') then
	begin
		KonfigLibrary.Load(ResourcesPath + '\config.bin');
		has_configbin := True;
	end else
		WriteLn('KonfigLibrary not found');

	if FileExists(ResourcesPath + '\textures\textures.bin') then
	begin
		texture_params := TTexturesBin.Create;
		texture_params.Load(ResourcesPath + '\textures\textures.bin');
		has_texturesbin := True;
	end;
	
	if FileExists(ResourcesPath + '\scripts\texture_aliases.bin') then
	begin
		texture_aliases := TTextureAliases.Create;
		texture_aliases.Load(ResourcesPath + '\scripts\texture_aliases.bin');
	end;
	
	if FileExists(ResourcesPath + '\textures_handles_storage.bin') then
	begin
		texture_params2 := TTexturesBin2.Create;
		texture_params2.Load(ResourcesPath + '\textures_handles_storage.bin');
		has_textures_handles_storage := True;
	end;
	
	if has_scriptsbin then
	begin
		version := eVer2033;
	end else
	begin
		if has_texturesbin then
		begin
			version := eVerLL;
		end else
		begin
			if has_textures_handles_storage then
			begin
				version := eVerExodus;
			end else
			begin
				version := eVerRedux; // как отличить арктику.1 ???
			end;
		end;
	end;
	
	uWeather.Initialize;
	
	if FileExists(ResourcesPath + '\sounds\sounds.bin') then
	begin
		framework.Initialize;
		K := TKonfig.Create;
		K.Load(ResourcesPath + '\sounds\sounds.bin');
		
		case version of
			eVer2033:   sound_params := framework.DecompileKonfig(K, 'js\2033\sounds.js');
			eVerLL:     sound_params := framework.DecompileKonfig(K, 'js\ll\sounds.js');
			eVerRedux:  sound_params := framework.DecompileKonfig(K, 'js\redux\sounds.js');
		end;
		
		K.Free;
		framework.Finalize;
	end;
	
	Write('Engine version is ');
	case version of
		eVer2033: 		Write('Metro 2033');
		eVerLL: 			Write('Metro Last Light');
		eVerRedux: 		Write('Metro Redux');
		eVerArktika1: Write('Arktika.1');
		eVerExodus: 	Write('Metro Exodus');
	end;
	WriteLn;
end;

procedure FinalizeEngine;
begin
	uWeather.Finalize;

	FreeAndNil(texture_params);
	FreeAndNil(texture_params2);
	FreeAndNil(texture_aliases);
	
	FreeAndNil(sound_params);
end;

end.