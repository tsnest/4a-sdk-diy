unit framework;

interface
uses Konfig, Variants;

procedure Initialize;
procedure Finalize;

function DecompileKonfig(K : TKonfig; script_name : String) : TTextKonfig;
procedure DefineGlobal(const name : String; v : Variant);

implementation
uses uCrc, Konfig_reader, mujs, chunkedFile, sysutils { for FileExists };

var
	g_state : js_State;

procedure Script_report(J : js_State; message : PAnsiChar); cdecl;
begin
	WriteLn('[JS] ', message);
end;

procedure Script_crc32(J : js_State); cdecl;
var
	crc : Longint;
	str : String;
begin
	if js_gettop(J) >= 2 then
	begin
		str := js_trystring(J, 1, '');
		crc := GetStringCrc(Str);
		js_pushnumber(J, Longword(crc));
	end else
		js_pushnumber(J, 0);
end;

procedure Script_print(J : js_State); cdecl;
var
	I : Longint;
begin
	for I := 1 to js_gettop(J)-1 do
		Write(js_trystring(J, I, ''));
	WriteLn;
	js_pushundefined(J);
end;

procedure Script_module(J : js_State); cdecl;
var
	filename : String;
	module : PAnsiChar;
	error : Boolean;
	
	r : TMemoryReader;
	source : String;
begin
	module := js_trystring(J, 1, nil);
	if module <> nil then
	begin
		filename := 'js\' + module + '.js';
		if not FileExists(filename) then
			filename := ExtractFilePath(ParamStr(0)) + filename;
		
		r := TMemoryReader.CreateFromFile(filename);
		SetLength(source, r.size);
		r.Read(source[1], r.size);
		r.Free;
		
		source := '((function() {' + source + '}).apply(this));'#10;
		error := False;
		
		if js_ploadstring(J, PAnsiChar(filename), PAnsiChar(source)) = 0 then
		begin
			js_newobject(J); // our module
			
			js_copy(J, -2); // copy function (from js_ploadstring)
			js_copy(J, -2); // copy this (from js_newobject)
			
			if js_pcall(J, 0) = 0 then
				js_pop(J, 1) // remove result
			else
				error := True;
		end else
			error := True;
			
		if error then
		begin
			js_report(J, PAnsiChar('module: error loading ''' + module + ''''));
			js_report(J, js_tostring(J, -1));
			
			js_pushnull(J);
		end 
		else 
			js_copy(J, -1); // copy 'our module'
	end else
	begin
		js_report(J, 'module: invalid call');
		js_pushnull(J);
	end;
end;

procedure Initialize;
var
	J : js_State;
	_common_js_name : String;
begin
	J := js_newstate(nil, nil, JS_STRICT);
	js_setreport(J, Script_report);

	js_pushglobal(J);

	js_pushglobal(J);
	js_defproperty(J, -2, '_G', JS_READONLY);

	js_newcfunction(J, Script_crc32, 'crc32', 1);
	js_setproperty(J, -2, 'crc32');

	js_newcfunction(J, Script_print, 'print', 0);
	js_setproperty(J, -2, 'print');
	
	js_newcfunction(J, Script_module, 'module', 0);
	js_setproperty(J, -2, 'module');
	
	js_pop(J, 1);

	if FileExists('js\_common_.js') then
		js_dofile(J, 'js\_common_.js')
	else
	begin
		_common_js_name := ExtractFilePath(ParamStr(0)) + 'js\_common_.js';
		if FileExists(_common_js_name) then
			js_dofile(J, PAnsiChar(_common_js_name));
	end;
	
	Konfig_reader.Script_Init(J);

	g_state := J;
end;

procedure Finalize;
var
	J : js_State;
begin
	J := g_state;

	Konfig_reader.Script_Finish(J);
	js_freestate(J);
end;

function DecompileKonfig(K : TKonfig; script_name : String) : TTextKonfig;
var
	J : js_State;
	tk : TTextKonfig;
	R : TKonfigReader;
begin
	J := g_state;

	tk := TTextKonfig.Create;
	R := TKonfigReader.Create(K, tk.root);
	Konfig_reader.Script_Push(J, R);
	js_setglobal(J, 'reader');

	if not FileExists(script_name) then
		script_name := ExtractFilePath(ParamStr(0)) + script_name;
		
	js_dofile(J, PAnsiChar(script_name));

	js_pushundefined(J);
	js_setglobal(J, 'reader');

	// R freed by mujs

	Result := tk;
end;

procedure DefineGlobal(const name : String; v : Variant);
var
	J : js_State;
begin
	J := g_state;
	
	case varType(v) of
		varEmpty: js_pushundefined(J);
		varNull: js_pushnull(J);
		varSingle: js_pushnumber(J, Single(v));
		varDouble: js_pushnumber(J, Double(v));
		varShortInt: js_pushnumber(J, ShortInt(v));
		varSmallInt: js_pushnumber(J, SmallInt(v));
		varInteger: js_pushnumber(J, Integer(v));
		varInt64: js_pushnumber(J, Int64(v));
		varByte: js_pushnumber(J, Byte(v));
		varWord: js_pushnumber(J, Word(v));
		varLongWord: js_pushnumber(J, LongWord(v));
		varQWord: js_pushnumber(J, QWord(v));
		varBoolean: js_pushboolean(J, Integer(Boolean(v)));
		varString: js_pushstring(J, PAnsiChar(String(v)));
		else 
			WriteLn('framework.DefineGlobal: unsupported variant type');
			Exit;
	end;
	
	js_defglobal(J, PAnsiChar(name), JS_READONLY);
end;

end.