unit uTabWeather;

interface
uses Iup;

function CreateTab : Ihandle;
procedure ResetWeather;
procedure FillList;

implementation
uses classes, sysutils, Konfig, uScene, uWeather;

procedure Redisplay;
var
	gl : Ihandle;
begin
	gl := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'GL_CANVAS');
	IupRedraw(gl, 0);
end;

procedure ResetWeather;
var	
	startup : TSection;
begin
	if Scene.konf <> nil then
	begin
		startup := Scene.konf.root.GetSect('startup', False);
		if startup <> nil then
			uWeather.SetWeather(startup.GetStrDef('desc_0', ''));
	end;
	
	Redisplay;
end;

procedure FillList;
var
	list : Ihandle;
	weathers : TStringList;
	I : Longint;
begin
	list := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'LIST_WEATHERS');

	IupSetAttribute(list, 'REMOVEITEM', 'ALL');

	weathers := GetWeathersList;
	for I := 0 to weathers.Count - 1 do
		IupSetAttribute(list, 'APPENDITEM', PAnsiChar(weathers[I]));			
	weathers.Free;
end;

function list_weathers_cb(ih : Ihandle; txt : PAnsiChar; item, state : Longint) : Longint; cdecl;
begin
	SetWeather(txt);
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_apply_cb(ih : Ihandle) : Longint; cdecl;
var	
	startup : TSection;
	desc_0, desc_1 : TStringValue;
	list : Ihandle;
begin
	list := IupGetDialogChild(ih, 'LIST_WEATHERS');

	if Scene.konf <> nil then
	begin
		try
			startup := Scene.konf.root.GetSect('startup');
			desc_0 := startup.GetParam('desc_0', 'stringz') as TStringValue;
			desc_1 := startup.GetParam('desc_1', 'stringz') as TStringValue;
			
			desc_0.str := IupGetAttribute(list, 'VALUESTRING');
			desc_1.str := IupGetAttribute(list, 'VALUESTRING');
		except
			on E: Exception do
				IupMessageError(IupGetHandle('MAINDIALOG'), PAnsiChar(E.ClassName + ': ' + E.Message));
		end;
	end;
	Result := IUP_DEFAULT;
end;

function btn_reset_cb(ih : Ihandle) : Longint; cdecl;
begin
	ResetWeather;
	Result := IUP_DEFAULT;
end;

function CreateTab : Ihandle;
var
	list_weathers : Ihandle;
	btn_apply : Ihandle;
	btn_reset : Ihandle;
begin
	// Weather tab
	list_weathers := IupList(nil);
	IupSetAttribute(list_weathers, 'NAME', 'LIST_WEATHERS');
	IupSetAttribute(list_weathers, 'EXPAND', 'YES');
	IupSetAttribute(list_weathers, 'VISIBLELINES', '15');
	IupSetCallback(list_weathers, 'ACTION', @list_weathers_cb);
	
	btn_apply := iup.Button('Apply!', @btn_apply_cb);
	btn_Reset := iup.Button('Reset', @btn_reset_cb);
	
	Result := IupVBox(IupHBox(btn_apply, btn_reset, nil), list_weathers, nil)
end;

end.