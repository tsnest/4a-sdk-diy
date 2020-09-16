unit properties;

interface
uses Iup, Konfig;

function EditBool8(v : TIntegerValue; const names : String) : Boolean;
function EditEnum(v : TIntegerValue; const names : String; caption : String = '') : Boolean;
function EditChoose(v : TStringValue; allow_none : Boolean; const names : String; caption : String = '') : Boolean;

procedure SetupProperties(tree : Ihandle; data : TSection);

implementation
uses 
	sysutils, classes,
	uScene, uEntity, vmath, common, 
	matrix_editor,
	uChoose, uChooseTexture, uChooseColoranim;

type
	TPropsValueChangedCb = procedure(val : TSimpleValue); cdecl;
	TPropsEditCb = function(tree : Ihandle; sect : TSection; val : TSimpleValue) : Longint; cdecl; // ret = 0 - cancel, 1 - default editor, 2 - apply

procedure UpdateCaption(tree : Ihandle; nid : Longint; v : TSimpleValue);
var
	e : TEntity;
	eid : Word;
	
	I : Integer;
	vec : TFloatArrayValue;
		
	title : String;
begin
	title := v.name + ' : ' + v.vtype;

	if v.vtype = 'entity_link, uobject_link' then
	begin
		eid := (v as TIntegerValue).num;
		if eid <> 65535 then
		begin
			e := Scene.EntityById(eid);
			if e <> nil then
				title := title + ' = ' + e.Name
			else
				title := title + ' = <invalid>';
		end else
			title := title + ' = <none>';
	end else
	if v is TStringValue then
		title := title + ' = ' + (v as TStringValue).str
	else
	if v is TIntegerValue then
		title := title + ' = ' + IntToStr((v as TIntegerValue).num)
	else
	if v is TSingleValue then
		title := title + ' = ' + FloatToStr((v as TSingleValue).num)
	else
	if v is TBoolValue then
	begin
		if (v as TBoolValue).bool then
			title := title + ' = True'
		else
			title := title + ' = False';
	end else
	if v is TFloatArrayValue then
	begin
		if (v.vtype = 'vec2f') or (v.vtype = 'vec3f') or (v.vtype = 'vec4f') or (v.vtype = 'color, vec4f') then
		begin
			vec := v as TFloatArrayValue;
			title := title + '= [';
			for I := 0 to Length(vec.data) - 1 do
			begin
				if I > 0 then title := title + ', ';
				title := title + FloatToStr(vec.data[I])
			end;
			title := title + ']';
		end;
	end;
			
	IupSetStrAttribute(tree, PAnsiChar('TITLE'+IntToStr(nid)), PAnsiChar(title));
end;

function EditBool8(v : TIntegerValue; const names : String) : Boolean;
var
	sl : TStringList;
	format : String;
	bools : array[0..7] of Longint;
	args : array[0..7] of PLongint;
	I : Longint;
begin
	sl := TStringList.Create;
	sl.CommaText := names;

	format := '';
	for I := 0 to sl.Count-1 do
	begin
		format := format + sl[I] + ': %b'#10;
		
		if (v.num and (1 shl I)) <> 0 then
			bools[I] := 1
		else
			bools[I] := 0;
			
		args[I] := @bools[I];
	end;

	if IupGetParamv(PAnsiChar(v.name), nil, nil, PAnsiChar(format),
		 sl.Count, 0, @args[0]) <> 0 then
	begin
		v.num := 0;
		for I := 0 to sl.Count do
			v.num := v.num or ((bools[I] and 1) shl I);

		EditBool8 := True;
	end else
		EditBool8 := False;

	sl.Free;
end;

function EditEnum(v : TIntegerValue; const names : String; caption : String) : Boolean;
var
	sl : TStringList;
	ret : Longint;
	I : Longint;
begin
	sl := TStringList.Create;
	sl.CommaText := names;

	if caption = '' then
		caption := v.name;

	ret := iup.ListDialog(caption, sl, v.num+1, 15, 15);
	if ret <> -1 then
	begin
		v.num := ret;
		EditEnum := True;
	end else
		EditEnum := False;

	sl.Free;
end;

function EditChoose(v : TStringValue; allow_none : Boolean; const names : String; caption : String) : Boolean;
var
	sl : TStringList;
	pointers : array of PAnsiChar;
	ret : Longint;
	I : Longint;
	op : Longint;
begin
	sl := TStringList.Create;
	sl.CommaText := names;

	op := 1;

	if allow_none then
		sl.Add('<none>');
		
	for I := 0 to sl.Count-1 do
	begin
		if sl[I] = v.str then
			op := I+1;
	end;

	if caption = '' then
		caption := v.name;

	ret := iup.ListDialog(caption, sl, op, 15, 15);
	if ret <> -1 then
	begin
		if allow_none and (ret = sl.Count) then
			v.str := ''
		else
			v.str := sl[ret];
			
		EditChoose := True;
	end else
		EditChoose := False;

	sl.Free;
end;

function EditStrArray(sect : TSection; hint : TSimpleValue; size_type : String) : Boolean;
var
	I, index : Longint;
	src : String;
	maxlen : Longint;
	buffer : array of AnsiChar;
	sl : TStringList;
begin
	Result := False;

	index := sect.items.IndexOf(hint);
	if index < 0 then
		Exit;
	
	I := index+1;
	src := '';
	while (I < sect.items.Count) and (TSimpleValue(sect.items[I]).name = hint.name) do
	begin
		if TSimpleValue(sect.items[I]) is TStringValue then
		begin
			if src <> '' then
				src := src + #10;
				
			src := src + TStringValue(sect.items[I]).str;
		end;
		
		Inc(I);
	end;
	
	maxlen := Length(src)+4096;
	SetLength(buffer, maxlen);
	
	if Length(src) > 0 then
		Move(src[1], buffer[0], Length(src)+1)
	else
		buffer[0] := #0;
	
	if IupGetText(PAnsiChar(hint.name), PAnsiChar(buffer), maxlen) <> 0 then
	begin
		I := index+1;
		while (I < sect.items.Count) and (TSimpleValue(sect.items[I]).name = hint.name) do
		begin
			TSimpleValue(sect.items[I]).Free;
			sect.items.Delete(I);
		end;
		
		sl := TStringList.Create;
		sl.Delimiter := #10;
		sl.StrictDelimiter := True;
		
		sl.DelimitedText := PAnsiChar(buffer);
		
		sect.items.Insert(index+1, TIntegerValue.Create(hint.name, size_type, sl.Count));
		for I := 0 to sl.Count-1 do
			sect.items.Insert(index+2+I, TStringValue.Create(hint.name, sl[I]));
			
		sl.Free;
		
		Result := True;
	end;
end;

function tree_props_button_cb(ih : Ihandle; button, pressed, x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	id : Longint;
	format : String;

	ivalue : Longint;
	fvalue : Single;
	svalue : array[0..4095] of Char;

	v : TSimpleValue;

	entity : TEntity;

	i : TIntegerValue;
	f : TSingleValue;
	s : TStringValue;
	b : TBoolValue;

	c : TFloatArrayValue; // color
	cvalue : TVec4;

	vec2 : TVec2;
	vec3 : TVec3;
	vec4 : TVec4;
	matrix : TMatrix;

	ret : Longint;
	
	parent : Longint;
	sect : TSection;
	
	changed_cb : TPropsValuechangedCb;
	edit_cb : TPropsEditCb;
begin
	id := IupConvertXYToPos(ih, x, y);
	changed_cb := TPropsValuechangedCb(IupGetAttribute(ih, 'PROPS_VALUECHANGED_CB'));
	edit_cb := TPropsEditCb(IupGetAttribute(ih, 'PROPS_EDIT_CB'));

	if (id <> -1) and iup_isdouble(status) then
	begin
		parent := IupGetInt(ih, PAnsiChar('PARENT'+IntToStr(id)));
		sect := TSection(IupGetAttribute(ih, PAnsiChar('USERDATA'+IntToStr(parent))));
				
		v := TSimpleValue(IupGetAttribute(ih, PAnsiChar('USERDATA'+IntToStr(id))));
		if v <> nil then
		begin
			if Assigned(edit_cb) then
				ret := edit_cb(ih, sect, v)
			else
				ret := 1;
						
			if ret = 1 then // default editor
			begin
				if (v.vtype = 'ref_model') or ((v.vtype = 'choose') and (v.name = 'visual')) then
				begin		
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					if ChooseModel(s.str) then
					begin
						if Assigned(changed_cb) then
							changed_cb(s);
					end;					
				end else
				if v.vtype = 'texture, str_shared' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					if ChooseTexture(s.str) then
					begin
						if Assigned(changed_cb) then
							changed_cb(s);
					end;
				end;
				if v.vtype = 'particles, str_shared' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					if ChooseParticles(s.str) then
					begin
						if Assigned(changed_cb) then
							changed_cb(s);
					end;
				end;
				if v.vtype = 'sound' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					if ChooseSound(s.str) then
					begin
						if Assigned(changed_cb) then
							changed_cb(s);
					end;
				end;
				if v.vtype = 'ref_coloranim' then
				begin
					s := sect.GetParam(v.name, 'stringz') as TStringValue;
					if ChooseColoranim(s.str) then
					begin
						if Assigned(changed_cb) then
							changed_cb(s);
					end;
				end;
				if v.vtype = 'entity_link, uobject_link' then
				begin
					i := v as TIntegerValue;
	
					if ChooseEntity(entity) then
					begin
						if entity <> nil then
						begin
							i.num := entity.ID;
						end else
						begin
							i.num := 65535;
						end;
						
						UpdateCaption(ih, id, v);
						
						if Assigned(changed_cb) then
							changed_cb(i);
					end;
				end else
				if v.vtype = 'str_array16' then
				begin
					if EditStrArray(sect, v, 'u16') then
						SetupProperties(ih, TSection(IupGetAttribute(ih, 'USERDATA0')));
				end else
				if (v.vtype = 'str_array32') or (v.vtype = 'str_array') then
				begin
					if EditStrArray(sect, v, 'u32') then
						SetupProperties(ih, TSection(IupGetAttribute(ih, 'USERDATA0')));
				end else
				if v is TIntegerValue then
				begin
					i := v as TIntegerValue;
					ivalue := i.num;
	
					format := '%i';
					if i.vtype = 'u8' then
						format := '%i[0,255]';
					if i.vtype = 's8' then
						format := '%i[-128,127]';
					if i.vtype = 'u16' then
						format := '%i[0,65535]';
					if i.vtype = 's16' then
						format := '%i[-32768,32767]';
	
					format := 'Value: ' + format + #10;
					if IupGetParam(PAnsiChar(v.name), nil, nil, PAnsiChar(format), @ivalue) = 1 then
					begin
						i.num := ivalue;
						
						UpdateCaption(ih, id, v);
						
						if Assigned(changed_cb) then
							changed_cb(i);
					end;
				end else
				if v is TSingleValue then
				begin
					f := v as TSingleValue;
					fvalue := f.num;
					if IupGetParam(PAnsiChar(v.name), nil, nil, 'Value: %r'#10, @fvalue) = 1 then
					begin
						f.num := fvalue;
						
						UpdateCaption(ih, id, v);
						
						if Assigned(changed_cb) then
							changed_cb(f);
					end;
				end else
				if v is TStringValue then
				begin
					s := v as TStringValue;
					Move(s.str[1], svalue[0], Length(s.str));
					svalue[Length(s.str)] := #0;
					if IupGetParam(PAnsiChar(v.name), nil, nil, 'Value: %s'#10, @svalue) = 1 then
					begin
						s.str := PAnsiChar(@svalue);
						
						UpdateCaption(ih, id, v);
					
						if Assigned(changed_cb) then
							changed_cb(s);
					end;
				end else
				if v is TBoolValue then
				begin
					b := v as TBoolValue;
					b.bool := not b.bool;
	
					UpdateCaption(ih, id, v);
					
					if Assigned(changed_cb) then
						changed_cb(b);
				end else
				if v is TFloatArrayValue then
				begin
					c := v as TFloatArrayValue;
	
					ret := 0;
	
					if c.vtype = 'pose, matrix' then
					begin
						if Length(c.data) = 16 then	c.GetMatrix44(matrix)
						else c.GetMatrix43(matrix);
						
						if EditMatrix(matrix, True) then
						begin
							if Length(c.data) = 16 then	c.SetMatrix44(matrix)
							else c.SetMatrix43(matrix);
							ret := 1;
						end;
					end;
					if c.vtype = 'pose, matrix_43T' then
					begin
						c.GetMatrix43T(matrix);
						
						if EditMatrix(matrix, True) then
						begin
							c.SetMatrix43T(matrix);
							ret := 1;
						end;
					end;
					if c.vtype = 'color, vec4f' then
					begin
						Move(c.data[0], cvalue, Sizeof(cvalue));
						if SelectColor(cvalue, True) then
						begin
							Move(cvalue, c.data[0], Sizeof(cvalue));
							ret := 1;
						end;
					end;
					if c.vtype = 'vec4f' then
					begin
						Move(c.data[0], vec4, Sizeof(vec4));
						ret := IupGetParam(PAnsiChar(v.name), nil, nil, 'X: %r'#10'Y: %r'#10'Z: %r'#10'W: %r'#10,
							@vec4.x, @vec4.y, @vec4.z, @vec4.w);
						if ret = 1 then
							Move(vec4, c.data[0], Sizeof(vec4));
					end;
					if c.vtype = 'vec3f' then
					begin
						Move(c.data[0], vec3, Sizeof(vec3));
						ret := IupGetParam(PAnsiChar(v.name), nil, nil, 'X: %r'#10'Y: %r'#10'Z: %r'#10,
							@vec3.x, @vec3.y, @vec3.z);
						if ret = 1 then
							Move(vec3, c.data[0], Sizeof(vec3));
					end;
					if c.vtype = 'vec2f' then
					begin
						Move(c.data[0], vec2, Sizeof(vec2));
						ret := IupGetParam(PAnsiChar(v.name), nil, nil, 'X: %r'#10'Y: %r'#10,
							@vec2.x, @vec2.y);
						if ret = 1 then
							Move(vec2, c.data[0], Sizeof(vec2));
					end;
					
					if ret = 1 then
					begin
						UpdateCaption(ih, id, v);
						 
						if Assigned(changed_cb) then
							changed_cb(c);
					end;
				end;
			end else
			if ret = 2 then // user editor
			begin
				UpdateCaption(ih, id, v);
				
				if Assigned(changed_cb) then
					changed_cb(v);
			end;
			
		end;
	end;

	Result := IUP_DEFAULT;
end;

procedure AddSection(tree : Ihandle; ref : Longint; sect : TSection);
var
	I : Integer;
	v : TSimpleValue;

	nid : String;
begin
	IupSetAttribute(tree, PAnsiChar('ADDBRANCH' + IntToStr(ref)), PAnsiChar(sect.name));
	nid := IupGetAttribute(tree, 'LASTADDNODE');
	IupSetAttribute(tree, PAnsiChar('USERDATA' + nid), Pointer(sect));
	
	ref := StrToInt(nid);
	
	for I := sect.items.Count - 1 downto 0 do
	begin
		v := TSimpleValue(sect.items[I]);

		if v is TSection then
		begin
			AddSection(tree, ref, v as TSection);
		end else
		begin
			IupSetStrAttribute(tree, PAnsiChar('ADDLEAF' + IntToStr(ref)), PAnsiChar(v.name));
			nid := IupGetAttribute(tree, 'LASTADDNODE');
			IupSetAttribute(tree, PAnsiChar('USERDATA' + nid), Pointer(v));
			
			UpdateCaption(tree, StrToInt(nid), v);
		end;
	end;
end;

procedure SetupProperties(tree : Ihandle; data : TSection);
begin
	IupSetCallback(tree, 'BUTTON_CB', @tree_props_button_cb);
	IupSetAttribute(tree, 'DELNODE', 'ALL');
	AddSection(tree, -1, data);	
end;

end.