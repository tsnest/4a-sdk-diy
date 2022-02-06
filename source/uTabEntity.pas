unit uTabEntity;

interface
uses vmath, Iup;

var UpdateSelection : procedure;

function CreateTab : Ihandle;
procedure UpdateTab;

procedure CreateEntity(const hit_pos, hit_nrm : TVec3);

procedure DeselectAll;

procedure DeleteSelection;
procedure CutSelection;
procedure CopySelection;
procedure PasteSelection;

implementation
uses 
	classes, sysutils, 
	fouramdl, skeleton, Konfig, 
	script_editor, properties, 
	uChoose, uEditorUtils, uEntity, uScene, uTemplates, uLevelUndo, uLEOptions;

var select_created : Boolean;

function tg_select_new_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	select_created := state = 1;
	Result := IUP_DEFAULT;
end;

function btn_add_template_cb(ih : Ihandle) : Longint; cdecl;
var
	I : Longint;
	t : Ihandle;

	format : String;
	
	name : array[0..255] of Char;
	pivot : Longint;
	
	selected : TList;
	
	pref : String;
	full_path : Longint;
begin
	t := IupGetDialogChild(ih, 'TREE_TEMPLATES');
	
	// get prefix
	I := IupGetInt(t, 'VALUE');
	if I >= 0 then
	begin
		if IupGetAttribute(t, PAnsiChar('KIND'+IntToStr(I))) = 'BRANCH' then
			pref := IupGetAttribute(t, PAnsiChar('TITLE'+IntToStr(I))) + '\';
	
		while IupGetAttribute(t, PAnsiChar('PARENT' + IntToStr(I))) <> nil do
		begin
			I := IupGetInt(t, PAnsiChar('PARENT' + IntToStr(I)));
			
			if IupGetAttribute(t, PAnsiChar('KIND'+IntToStr(I))) = 'BRANCH' then
				pref := IupGetAttribute(t, PAnsiChar('TITLE'+IntToStr(I))) + '\' + pref;
		end;
	end else
		pref := '';
	
	selected := Scene.GetSelectedList;

	if selected.Count > 0 then
	begin
		StrPCopy(@name, TEntity(selected[0]).Name);
		pivot := 0;
		
		full_path := 0;
		
		format := 'Name: %s'#10;
		format := format + 'Full path: %b'#10;
		if selected.Count > 1 then
		begin
			format := format + 'Pivot: %l|<center>|';
			for I := 0 to selected.Count-1 do
				format := format + TEntity(selected[I]).Name + '|';
			format := format + #10;
		end;
		
		if IupGetParam('Add template', nil, nil, PAnsiChar(format), @name, @full_path, @pivot) = 1 then
		begin
			if full_path <> 0 then
				pref := '';
			
			if name[0] <> #0 then
			begin
				if pivot = 0 then
					NewTemplate(pref + name, selected, nil)
				else
					NewTemplate(pref + name, selected, TEntity(selected[pivot-1]));
	
				UpdateTemplates(t);
			end;
		end;
	end else
		IupMessage('Message', 'Nothing selected!');
	
	selected.Free;

	Result := IUP_DEFAULT;
end;

function btn_remove_template_cb(ih : Ihandle) : Longint; cdecl;
var
	t : Ihandle;
	id : Longint;
	v : TSection;
	
	msg : String;
begin
	t := IupGetDialogChild(ih, 'TREE_TEMPLATES');
	id := IupGetInt(t, 'VALUE');
	if id >= 0 then
	begin
		msg := 'Are you sure to remove ''' + IupGetAttribute(t, PAnsiChar('TITLE'+IntToStr(id))) + '''?';
	
		if IupMessageAlarm(MainDialog, 'Remove template', PAnsiChar(msg), 'YESNO') = 1 then
		begin
			v := TSection(IupGetAttribute(t, PAnsiChar('USERDATA'+IntToStr(id))));
			DeleteTemplate(v);
			
			//UpdateTemplates(t);
			IupSetAttribute(t, PAnsiChar('DELNODE'+IntToStr(id)), 'SELECTED');
		end;
	end;

	Result := IUP_DEFAULT;
end;

function btn_rename_template_cb(ih : Ihandle) : Longint; cdecl;
var
	t : Ihandle;
	id : Longint;
	v : TSection;
	
	name : array[0..255] of Char;
begin
	t := IupGetDialogChild(ih, 'TREE_TEMPLATES');
	id := IupGetInt(t, 'VALUE');
	if id >= 0 then
	begin
		v := TSection(IupGetAttribute(t, PAnsiChar('USERDATA'+IntToStr(id))));
		
		StrPCopy(@name, v.name);
		if IupGetParam('Rename template', nil, nil, 'Name: %s'#10, @name) = 1 then
		begin
			v.name := StrPas(@name[0]);
			
			//UpdateTemplates(t);
			IupSetAttribute(t, PAnsiChar('TITLE'+IntToStr(id)), @name);
		end;
	end;

	Result := IUP_DEFAULT;
end;

function btn_rename_entity_cb(ih : Ihandle) : Longint; cdecl;
var
	name : array[0..255] of Char;
	selected : TEntityArray;
begin
	selected := Scene.GetSelected;

	if Length(selected) = 1 then
	begin
		StrPCopy(@name[0], selected[0].Name);
		if IupGetParam('Rename entity', nil, nil, 'Name: %s'#10, @name) = 1 then
		begin
			UndoSave('Rename entity (' + selected[0].Name + ' -> ' + PAnsiChar(@name) + ')', selected);
			selected[0].Name := PAnsiChar(@name);
			UpdateSelection;
		end;
	end else
		IupMessage('Message', 'Select just one object');

	Result := IUP_DEFAULT;
end;

function btn_delete_entity_cb(ih : Ihandle) : Longint; cdecl;
begin
	UndoSave('Delete entities');
	DeleteSelection;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_script_cb(ih : Ihandle) : Longint; cdecl;
var
	s : TSection;
	s_copy : TSection;
	selected : TEntityArray;
begin
	selected := Scene.GetSelected;

	if Length(selected) = 1 then
	begin
		s := selected[0].data.GetSect('vss_ver_6', False);
		if s = nil then
			s := selected[0].data.GetSect('vss_ver_7', False);
		
		if s <> nil then
		begin
			s_copy := s.Copy as TSection;	
			if EditScript(s_copy) then
			begin
				UndoSave('Script edit', selected);
				selected[0].data.Replace(s, s_copy);
				UpdateSelection;
			end else
				s_copy.Free;
		end else
			WriteLn('Warning! Invalid entity, doesn''t contain vss_ver_6 or vss_ver_7 section');
	end else
		IupMessage('Message', 'Select just one object');

	Result := IUP_DEFAULT;
end;

procedure property_before_change_cb(prop : TSimpleValue) cdecl;
var
	sel : TEntityArray;
begin
	sel := Scene.GetSelected;
	UndoSave('Property changed: ''' + prop.name + ' : ' + prop.vtype + '''', sel);
end;

procedure property_after_change_cb(prop : TSimpleValue) cdecl;
var
	mat : TMatrix;
	selected : TEntityArray;
begin
	selected := Scene.GetSelected;
	
	if (prop.name = '') and (prop.vtype = 'pose, matrix') then
	begin		
		if prop = selected[0].param_matrix then
		begin
			with prop as TFloatArrayValue do
			begin
				if Length(data) = 16 then GetMatrix44(mat)
				else GetMatrix43(mat)
			end;
		
			selected[0].Matrix := mat;
			UpdateSelection;
		end;
		
		Redisplay;
	end;

	if (prop.name = 'att_offset') and ((prop.vtype = 'pose, matrix') or (prop.vtype = 'pose, matrix_43T')) then
	begin
		Scene.UpdateAttaches;	
		Redisplay;
	end;
	
	if (prop.name = 'visual') and (prop.vtype = 'stringz') then
	begin
		selected[0].VisualName := (prop as TStringValue).str;
		UpdateSelection;
		Redisplay;
	end;
	
	if (prop.name = 'startup_animation') and (prop.vtype = 'stringz') then
	begin
		selected[0].Animation := (prop as TStringValue).str;
		UpdateSelection;
		Redisplay;
	end;
	
	// shapes, sphere & box
	if (prop.name = '') and (prop.vtype = 'pose, matrix') or
	   (prop.name = 'h_size') and (prop.vtype = 'vec3f') or
	   (prop.name = 'radius') and (prop.vtype = 'fp32') or
	   (prop.name = 'center') and (prop.vtype = 'vec3f') then
	begin
		selected[0].UpdateShapes;
		Redisplay;
	end;
end;

function property_edit_cb(tree : Ihandle; sect : TSection; prop : TSimpleValue) : Longint; cdecl;
var
	v : TIntegerValue;
	s : TStringValue;
	names : String;
	
	str : String;
	
	sel : TEntity;
	parent : TEntity;
	
	skeleton : T4ASkeleton;
	
	before_change_cb : TPropsBeforeChangeCb;
	after_change_cb  : TPropsAfterChangeCb;
begin
	Result := 1; // 0 - cancel, 1 - default editor, 2 - apply
	
	before_change_cb := TPropsBeforeChangeCb(IupGetAttribute(tree, 'PROPS_BEFORE_CHANGE_CB'));
	after_change_cb := TPropsAfterChangeCb(IupGetAttribute(tree, 'PROPS_AFTER_CHANGE_CB'));
	
	if prop.vtype = 'bool8' then
	begin
		v := prop as TIntegerValue;
		
		names := '';
		
		if v.name = 'oflags' then
			names := 'reflectable,cast_ao,ghost,shadowgen,rws,neversleep,force_realtime,dao_auto';
			
		if v.name = 'physics_flags' then
			names := 'is_physics,after_detach_physics_on,sleeping,kinematic,force_kinematic,block_breacking,raycast,block_ai_los';
			
		if v.name = 'flags0' then
		begin
			sel := Scene.GetSelected[0];
			if (sel.classname <> 'FORCE_FIELD') and // if  not force field
			   (sel.data.GetParam('shapes', 'section') = nil) and // not restrictor
			   (sel.data.GetParam('base_npc_flags', 'bool8') = nil) // and not npc
			then
			  // then it's inventory item object!
				names := 'active,useful_for_player,ammo_for_player,dao_blink_prevent,ready_after_cloned,ui_force_slot,attached_loot';
		end;
			
		if names <> '' then
		begin
			if properties.EditBool8(v, names) then
				Result := 2
			else
				Result := 0;
		end;
	end;
	
	if (prop.vtype = 'u8') and (prop.name = 'ltype') then
	begin
		v := prop as TIntegerValue;
		names := 'directional,omni_normal,omni_shadowed,omni_ambient,spot_normal,spot_shadowed,spot_ambient,quad_normal,quad_shadowed,quad_ambient,elliptic_normal,elliptic_ambient';
		if Scene.GetVersion > sceneVer2033 then
			names := names + ',directional_normal,directional_shadowed,halfomni_normal,halfomni_ambient,IBL_probe';

		if properties.EditEnum(v, names, 'Light Type') then
			Result := 2
		else
			Result := 0;
	end;
	
	if (prop.vtype = 'bone_id') then
	begin
		s := sect.GetParam(prop.name, 'stringz') as TStringValue;
		sel := Scene.GetSelected[0];
		skeleton := sel.GetSkeleton;
		if skeleton <> nil then
		begin
			str := s.str;
			if ChooseBone(skeleton, str) then
			begin
				before_change_cb(s);
				s.str := str;
				after_change_cb(s);
				
				properties.UpdateCaption(tree, s);
				Result := 2;
			end else
				Result := 0;
		end;
	end;
	
	if (prop.vtype = 'locator_str') then
	begin
		s := sect.GetParam(prop.name, 'stringz') as TStringValue;
		sel := Scene.GetSelected[0];
		
		if prop.name = 'att_bone_id' then
		begin
			parent := Scene.EntityById(sel.ParentID);
			if parent <> nil then
				skeleton := parent.GetSkeleton
			else
				skeleton := nil;
		end else
			skeleton := sel.GetSkeleton;
		
		if skeleton <> nil then
		begin		
			str := s.str;
			if ChooseLocator(skeleton, str) then
			begin
				before_change_cb(s);
				s.str := str;
				after_change_cb(s);
				
				properties.UpdateCaption(tree, s);
				Result := 2;
			end else
				Result := 0;
		end;
	end;
	
	if (prop.vtype = 'part_id') then
	begin
		s := sect.GetParam(prop.name, 'stringz') as TStringValue;
		sel := Scene.GetSelected[0];
		skeleton := sel.GetSkeleton;
		if skeleton <> nil then
		begin
			str := s.str;
			if ChooseBonePart(skeleton, str) then
			begin
				before_change_cb(s);
				s.str := str;
				after_change_cb(s);
				
				properties.UpdateCaption(tree, s);
				Result := 2;
			end else
				Result := 0;
		end;
	end;
	
	if (prop.vtype = 'animation_str') then
	begin
		s := sect.GetParam(prop.name, 'stringz') as TStringValue;
		sel := Scene.GetSelected[0];
		skeleton := sel.GetSkeleton;
		if skeleton <> nil then
		begin
			str := s.str;
			if ChooseAnimation(skeleton, str) then
			begin
				before_change_cb(s);
				s.str := str;
				after_change_cb(s);
				
				properties.UpdateCaption(tree, s);
				Result := 2;
			end else
				Result := 0;
		end;
	end;
	
	if (prop.vtype = 'flags8') and (prop.name = 'faces') then
	begin
		v := sect.GetParam(prop.name, 'u8') as TIntegerValue;
		if properties.EditBool8(v, 'Down,Up,Front,Back,Right,Left') then
			Result := 2
		else
			Result := 0;
	end;
end;

function CreateTab : Ihandle;
var
	fr_create : Ihandle;
	tree_templates : Ihandle;
	list_transform : Ihandle;
	tg_select_new : Ihandle;
	btn_add, btn_remove, btn_rename_template : Ihandle;

	fr_entity : Ihandle;
	btn_rename, btn_delete, btn_script : Ihandle;
	t_props : Ihandle;
begin
	tree_templates := IupSetAttributes(IupTree, 'NAME=TREE_TEMPLATES, RASTERSIZE=200x');
	IupSetAttribute(tree_templates, 'ADDEXPANDED', 'NO');
	IupSetAttribute(tree_templates, 'ADDROOT', 'NO');

	list_transform := IupList(nil);
	IupSetAttributes(list_transform, 'NAME=LIST_TRANSFORM, DROPDOWN=YES');
	IupSetAttributes(list_transform, '1="Y = World UP", 2="Y = Normal", 3="Z = Normal", 4="Z = -Normal"');
	IupSetAttribute(list_transform, 'VALUE', '1');
	
	tg_select_new := iup.Toggle('Select', @tg_select_new_cb, select_created);

	btn_add := iup.Button('Add', @btn_add_template_cb);
	btn_remove := iup.Button('Remove', @btn_remove_template_cb);
	btn_rename_template := iup.Button('Rename', @btn_rename_template_cb);

	fr_create := IupFrame(
		IupVBox(
			tree_templates,
			IupSetAttributes(IupHBox(btn_add, btn_remove, btn_rename_template, nil), 'MARGIN=0x0'),
			IupSetAttributes(IupHBox(list_transform, tg_select_new, nil), 'MARGIN=0x0'), 
			nil
		)
	);
	IupSetAttribute(fr_create, 'TITLE', 'Create');

	if uLEOptions.props_two_column then
	begin
		t_props := IupFlatTree;
		IupSetAttribute(t_props, 'EXTRATEXTWIDTH', '200');
	end else
		t_props := IupTree;
		
	IupSetAttributes(t_props, 'NAME=TREE_PROPS, RASTERSIZE=200x');
	IupSetCallback(t_props, 'PROPS_BEFORE_CHANGE_CB', @property_before_change_cb);
	IupSetCallback(t_props, 'PROPS_AFTER_CHANGE_CB', @property_after_change_cb);
	IupSetCallback(t_props, 'PROPS_EDIT_CB', @property_edit_cb);

	btn_rename := iup.Button('Rename', @btn_rename_entity_cb);
	btn_delete := iup.Button('Delete', @btn_delete_entity_cb);
	btn_script := iup.Button('Script', @btn_script_cb);

	fr_entity := IupFrame(
		IupVbox(
			IupSetAttributes(IupHBox(btn_rename, btn_delete, btn_script, nil), 'MARGIN=0x0'),
			t_props, nil
		)
	);
	IupSetAttributes(fr_entity, 'TITLE=Entity, NAME=FRAME_ENTITY, VISIBLE=NO');
	
	Result := IupVBox(fr_create, fr_entity, nil)
end;

procedure UpdateTab;
var
	e : TEntityArray;
	frame, t : Ihandle;
begin
	frame := IupGetDialogChild(MainDialog, 'FRAME_ENTITY');
	t := IupGetDialogChild(MainDialog, 'TREE_PROPS');

	e := Scene.GetSelected;

	if Length(e) = 1 then
	begin
		IupSetAttribute(frame, 'VISIBLE', 'YES');
		SetupProperties(t, e[0].data);
	end
	else
		IupSetAttribute(frame, 'VISIBLE', 'NO');
end;

var
	clipboard : TSection = nil;
	
procedure ProxyFromSelection(e : TEntity);
var
	param_entities, ent : TSection;
	victim : TEntity;
	selected : TEntityArray;
	I : Integer;
begin
	param_entities := e.data.GetSect('entities');
	param_entities.Clear;
	
	selected := Scene.GetSelected;
	param_entities.AddInt('count', Length(selected), 'u32');
	for I := 0 to Length(selected) - 1 do
	begin
		victim := selected[I];
		ent := param_entities.AddSect(victim.Name);
		ent.AddInt('entity', victim.ID, 'entity_link, uobject_link');
	end;
end;

procedure LinkSelectionToWay(way : TEntity);
var
	selected : TEntityArray;
begin
	selected := Scene.GetSelected;
	
	if 
		(length(selected) = 1) and 
		(way.classname = selected[0].classname) and
		selected[0].isWay and
		way.isWay
	then
		selected[0].way_link[0].num := way.ID;
end;
	
procedure CreateEntity(const hit_pos, hit_nrm : TVec3);
var
	dlg : Ihandle;
	t_template : Ihandle;
	l_transform : Ihandle;

	node_id : Longint;
	template : TSection; 
	transform : Integer;
	
	matrix : TMatrix;

	v1,v2,v3 : TVec3;
	d, l : Single;
	
	I : Longint;
	e : TEntityArray;
begin
	dlg := IupGetHandle('MAINDIALOG');
	t_template := IupGetDialogChild(dlg, 'TREE_TEMPLATES');
	l_transform := IupGetDialogChild(dlg, 'LIST_TRANSFORM');
						
	node_id := IupGetInt(t_template, 'VALUE');
	if node_id < 0 then
		Exit;
		
	template := TSection(IupGetAttribute(t_template, PAnsiChar('USERDATA'+IntToStr(node_id))));
	transform := IupGetInt(l_transform, 'VALUE');
						
	// verify if it's template, not folder
	if IsFolder(template) then
		Exit;

	v3 := hit_nrm;
	d := v3.x*0 + v3.y*1 + v3.z*0;
	v2.x := 0-(v3.x*d);
	v2.y := 1-(v3.y*d);
	v2.z := 0-(v3.z*d);
	l := v2.x*v2.x + v2.y*v2.y + v2.z*v2.z;
	if l < 0.001 then
	begin
		//WriteLn('UP = 1, 0, 0');
		d := v3.x*1 + v3.y*0 + v3.z*0;
		v2.x := 1-(v3.x*d);
		v2.y := 0-(v3.y*d);
		v2.z := 0-(v3.z*d);
		l := v2.x*v2.x + v2.y*v2.y + v2.z*v2.z;
	end;
	l := Sqrt(l);
	v2.x := v2.x / l;
	v2.y := v2.y / l;
	v2.z := v2.z / l;
	Cross(v1, v2, v3);

	case transform of
		1: begin // Y = World UP
			matrix[1,1] := 1; matrix[1,2] := 0; matrix[1,3] := 0; matrix[1,4] := 0;
			matrix[2,1] := 0; matrix[2,2] := 1; matrix[2,3] := 0; matrix[2,4] := 0;
			matrix[3,1] := 0; matrix[3,2] := 0; matrix[3,3] := 1; matrix[3,4] := 0;
			matrix[4,1] := hit_pos.x; matrix[4,2] := hit_pos.y; matrix[4,3] := hit_pos.z; matrix[4,4] := 1;
		end;
		2: begin // Y = Normal
			matrix[1,1] := v1.x; matrix[1,2] := v1.y; matrix[1,3] := v1.z; matrix[1,4] := 0;
			matrix[2,1] := v3.x; matrix[2,2] := v3.y; matrix[2,3] := v3.z; matrix[2,4] := 0;
			matrix[3,1] := -v2.x; matrix[3,2] := -v2.y; matrix[3,3] := -v2.z; matrix[3,4] := 0;
			matrix[4,1] := hit_pos.x; matrix[4,2] := hit_pos.y; matrix[4,3] := hit_pos.z; matrix[4,4] := 1;
		end;
		3: begin // Z = Normal
			matrix[1,1] := v1.x; matrix[1,2] := v1.y; matrix[1,3] := v1.z; matrix[1,4] := 0;
			matrix[2,1] := v2.x; matrix[2,2] := v2.y; matrix[2,3] := v2.z; matrix[2,4] := 0;
			matrix[3,1] := v3.x; matrix[3,2] := v3.y; matrix[3,3] := v3.z; matrix[3,4] := 0;
			matrix[4,1] := hit_pos.x; matrix[4,2] := hit_pos.y; matrix[4,3] := hit_pos.z; matrix[4,4] := 1;
		end;
		4: begin // Z = -Normal
			matrix[1,1] := -v1.x; matrix[1,2] := -v1.y; matrix[1,3] := -v1.z; matrix[1,4] := 0;
			matrix[2,1] := v2.x; matrix[2,2] := v2.y; matrix[2,3] := v2.z; matrix[2,4] := 0;
			matrix[3,1] := -v3.x; matrix[3,2] := -v3.y; matrix[3,3] := -v3.z; matrix[3,4] := 0;
			matrix[4,1] := hit_pos.x; matrix[4,2] := hit_pos.y; matrix[4,3] := hit_pos.z; matrix[4,4] := 1;
		end;
	end;
	
	e := CreateEntities(template, matrix);
	
	if (Length(e) = 1) and (e[0].classname = 'PROXY') then
		ProxyFromSelection(e[0]);
	if (Length(e) = 1) and (e[0].isWay) then
		LinkSelectionToWay(e[0]);
		
	if select_created then
	begin
		DeselectAll;
		for I := 0 to Length(e)-1 do
			e[I].selected := True;
		UpdateSelection;
	end;
end;
	
procedure DeselectAll;
var
	selected : TEntityArray;
	I : Longint;
begin
	selected := Scene.GetSelected;
	for I := 0 to Length(selected) - 1 do
		selected[I].selected := False;
end;

procedure DeleteSelection;
var
	selected : TEntityArray;
	I : Integer;
begin
	selected := Scene.GetSelected;
	for I := 0 to Length(selected) - 1 do
		Scene.RemoveEntity(selected[I]);
	
	UpdateSelection;
end;

procedure CutSelection;
begin
	CopySelection;
	DeleteSelection;
end;

procedure CopySelection;
var
	sel : TEntityArray;
	I : Longint;
begin
	sel := Scene.GetSelected;
	
	if Length(sel) > 0 then
	begin
		FreeAndNil(clipboard);
		
		if Length(sel) > 1 then
		begin
			clipboard := TSection.Create;
					
			clipboard.AddBool('is_group', True);
			for I := 0 to Length(sel) - 1 do
				clipboard.items.Add(sel[I].data.Copy);
		end else
			clipboard := sel[0].data.Copy as TSection;
	end;
end;

procedure PasteSelection;
var
	e : TEntityArray;
	I : Longint;
begin
	if Assigned(clipboard) then
	begin
		e := CreateEntities(clipboard);
		
		DeselectAll;
		for I := 0 to Length(e)-1 do
			e[I].selected := True;
		UpdateSelection;
		Redisplay;
	end;
end;

finalization
	clipboard.Free;
	
end.