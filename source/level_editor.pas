program level_Editor;
uses Iup, Windows, GL, GLU, GLExt, sysutils, classes,
  chunkedFile,
  Math, vmath, PhysX, PHGroups,
  Konfig, levelbin,
	common, Engine, Texture,
	Manipulator,
	fouramdl, skeleton,
	uScene, uEntity, uEnvZone, uWeather,
	{$IFDEF HAZ_LEVELEXPORT} uLevelExport, uXRayExport, {$ENDIF}
	uChoose, properties, script_editor, uTemplates, uAttach;
	
type
	TEditMode = (emEntity, emEnvZone);
var
	edit_mode : TEditMode = emEntity;

type
  TManipulatorMode = (mmNone, mmMove, mmRotate, mmScale);
	TManipulatorAxis = (maObject, maWorld, maGroup);
var
	m_mode : TManipulatorMode = mmNone;
	m_move_axis : TManipulatorAxis = maObject;
	m_rotate_axis : TManipulatorAxis = maObject;
	m_uniform_scale : Boolean = False;
	m_t : TManipulator;

var
	level_path : String;

	lineWidth : Longint = 1;

	cam_fov : Single = 70.0;
	position : TVec3;
	direction : TVec3;
	anglex, angley : Single;
	distance : Single = 5.0;

	far_plane : Single = 1500.0;
	rt_width, rt_height : GLuint;
	rt_color, rt_distort : GLuint;
	
var
	clipboard : TSection = nil;
	context_menu : Ihandle;
	
var
	mouse_x, mouse_y : Longint;
	wheel_up, wheel_down : Boolean;
	
	rclick_x, rclick_y : Longint;
	lclick_x, lclick_y : Longint;
	
	selection_rect : Boolean = False;
	
procedure Redisplay;
var
	gl : Ihandle;
begin
	gl := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'GL_CANVAS');
	IupRedraw(gl, 0);
end;

function GetCenter(arr : TEntityArray) : TVec3; overload;
var
	c : TVec3;
	bb : TAABB;
	I : Longint;
begin
	if Length(arr) > 0 then
	begin
		bb := arr[0].bbox;
	
		for I := 1 to Length(arr) - 1 do
			AABBMerge(bb, arr[I].bbox);
		
		AABBCenter(c, bb);
	end else
	begin
		c.x := 0;
		c.y := 0;
		c.z := 0;
	end;
		
	GetCenter := c;
end;

function GetCenter(arr : TEnvZoneArray) : TVec3; overload;
var
	c : TVec3;
	bb, bb2 : TAABB;
	I : Longint;
begin
	if Length(arr) > 0 then
	begin
		arr[0].GetBBox(bb);
	
		for I := 1 to Length(arr) - 1 do
		begin
			arr[I].GetBBox(bb2);
			AABBMerge(bb, bb2);
		end;
		
		AABBCenter(c, bb);
	end else
	begin
		c.x := 0;
		c.y := 0;
		c.z := 0;
	end;
		
	GetCenter := c;
end;

procedure WriteMatrix(const matrix : TMatrix);
begin
	WriteLn(matrix[1,1], matrix[1,2], matrix[1,3], matrix[1,4]);		 
	WriteLn(matrix[2,1], matrix[2,2], matrix[2,3], matrix[2,4]);
	WriteLn(matrix[3,1], matrix[3,2], matrix[3,3], matrix[3,4]);
	WriteLn(matrix[4,1], matrix[4,2], matrix[4,3], matrix[4,4]);
end;

procedure WriteVec3(const v : TVec3);
begin
	WriteLn('x = ', v.x, ' y = ', v.y, ' z = ', v.z);
end;

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

var
	select_created : Boolean;
	
procedure DeselectAll; forward;
procedure UpdateSelection; forward;

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
	if (template.GetParam('id', 'u16') = nil) and not (template.GetBool('is_group', False)) then
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
		
	if select_created then
	begin
		DeselectAll;
		for I := 0 to Length(e)-1 do
			e[I].selected := True;
		UpdateSelection;
	end;
end;

procedure CreateManipulator(const matrix : TMatrix);
begin
	if m_mode = mmMove then
	begin
		m_t := TMoveManipulator.Create(Scene.ph_scene, m_move_axis = maWorld, lineWidth);
		m_t.Matrix := matrix;
	end;
	if m_mode = mmRotate then
	begin
		m_t := TRotateManipulator.Create(Scene.ph_scene, (m_rotate_axis = maWorld) or (m_rotate_axis = maGroup), lineWidth);
		m_t.Matrix := matrix;
	end;
	if m_mode = mmScale then
	begin
		m_t := TScaleManipulator.Create(Scene.ph_scene, m_uniform_scale, lineWidth);
		m_t.Matrix := matrix;
	end;
end;

procedure DestroyManipulator;
begin
	FreeAndNil(m_t);
end;

procedure UpdateManipulator;
var
	selected : TEntityArray;
	z : TEnvZoneArray;
	c : TVec3;
	m : TMatrix;
begin
	DestroyManipulator;
	
	case edit_mode of
	
		emEntity: begin
			selected := Scene.GetSelected;
			
			if Length(selected) > 0 then
			begin
				if Length(selected) = 1 then
				begin
					CreateManipulator(selected[0].Matrix);
				end else
				begin
					c := GetCenter(selected);
					Translate(m, c);
					CreateManipulator(m);
				end;
			end;
		end;
		
		emEnvZone: begin
			z := Scene.GetSelectedEZ;
			
			if Length(z) > 0 then
			begin
				if Length(z) = 1 then
				begin
					CreateManipulator(z[0].Matrix);
				end else
				begin
					c := GetCenter(z);
					Translate(m, c);
					CreateManipulator(m);
				end;
			end;
		end;
		
	end;
end;

procedure UpdateSelection;
var
	e : TEntityArray;
	z : TEnvZoneArray;
	dlg, frame, t, sel_c : Ihandle;
	
	count : Longint;
begin
	UpdateManipulator;
	
	count := 0;
	dlg := IupGetHandle('MAINDIALOG');
	
	case edit_mode of
		emEntity: begin
			frame := IupGetDialogChild(dlg, 'FRAME_ENTITY');
			t := IupGetDialogChild(dlg, 'TREE_PROPS');
			
			e := Scene.GetSelected;
			count := Length(e);
			
			if count = 1 then
			begin
				IupSetAttribute(frame, 'VISIBLE', 'YES');
				SetupProperties(t, e[0].data);
			end
			else
				IupSetAttribute(frame, 'VISIBLE', 'NO');
		end;
		
		emEnvZone: begin
			t := IupGetDialogChild(dlg, 'TREE_PROPS_ENV');
			
			z := Scene.GetSelectedEZ;
			count := Length(z);
			
			if count = 1 then
				SetupProperties(t, z[0].data);
		end;
	end;
	
	// update selection count
	sel_c := IupGetDialogChild(dlg, 'LABEL_SEL');
	IupSetStrAttribute(sel_c, 'TITLE', PAnsiChar('Sel: ' + IntToStr(count)));
end;

procedure DeselectAll;
var
	selected : TEntityArray;
	z : TEnvZoneArray;
	I : Longint;
begin
	case edit_mode of
		emEntity: begin
			selected := Scene.GetSelected;
			for I := 0 to Length(selected) - 1 do
				selected[I].selected := False;
		end;
		
		emEnvZone: begin
			z := Scene.GetSelectedEZ;
			for I := 0 to Length(z) - 1 do
				z[I].Selected := False;
		end;
	end;
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

procedure RecursiveSelect(parent_id : Word);
var
	I : Longint;
	e : TEntity;
begin
	for I := 0 to Scene.entities.Count-1 do
	begin
		e := TEntity(scene.entities[I]);
		if e.ParentID = parent_id then
		begin
			e.selected := True;
			RecursiveSelect(e.ID);
		end;
	end;
end;

procedure AttachSelectionTo(parent : TEntity);
var
	sel : TList;
begin
	sel := Scene.GetSelectedList;
	
	if sel.Count > 0 then
	begin
		AttachEntities(sel, parent)
	end else
		IupMessage('Error', 'Nothing selected to attach');
		
	Scene.UpdateAttaches; // for test
		
	sel.Free;
end;

procedure AttachShapesTo(parent : TEntity);
var
	I : Longint;
	sel : TEntityArray;
begin
	sel := Scene.GetSelected;
	
	for I := 0 to Length(sel)-1 do
		if AddShapes(parent, sel[I]) then
			Scene.RemoveEntity(sel[I]);
			
	UpdateSelection;
end;

procedure LoadMap(const dir : String);
var
	dlg : Ihandle;
	
	startup : TSection;
begin
	DestroyManipulator;
	Scene.LevelUnload;

	LevelPath := dir; // in Engine.pas, for resource loading
	level_path := dir;
	
	Scene.LevelLoad(dir);
	
	if Scene.konf <> nil then
	begin
		startup := Scene.konf.root.GetSect('startup', False);
		if startup <> nil then
			uWeather.SetWeather(startup.GetStrDef('desc_0', ''));
	end;
	
	// load templates
	SaveTemplates;
	case Scene.GetVersion of
		sceneVer2033:			LoadTemplates('editor_data\templates.txt');
		sceneVerLL:				LoadTemplates('editor_data\templates_ll.txt');
		sceneVerRedux:		LoadTemplates('editor_data\templates_redux.txt');
		sceneVerArktika1:	LoadTemplates('editor_data\templates_a1.txt');
		sceneVerExodus:		LoadTemplates('editor_data\templates_exodus.txt');
	end;
	
	dlg := IupGetHandle('MAINDIALOG');
	UpdateTemplates(IupGetDialogChild(dlg, 'TREE_TEMPLATES'));
	IupSetAttribute(dlg, 'TITLE', PAnsiChar('Level Editor - [' + dir + ']'));

	UpdateSelection;
	Redisplay;
end;

procedure UpdateCameraPos;
var
	ih : Ihandle;
	x, y, z : String[32];
begin
	ih := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'LABEL_CAMPOS');

	WriteStr(x, camera_pos.x:1:3);
	WriteStr(y, camera_pos.y:1:3);
	WriteStr(z, camera_pos.z:1:3);

	IupSetAttribute(ih, 'TITLE', PAnsiChar('Cam pos: ' + x + ' ' + y + ' ' + z));
end;

///////////////////////////////////////////////////////////////////////////////
// OpenGL canvas callbacks                                                   //
///////////////////////////////////////////////////////////////////////////////
function gl_map_cb(ih : Ihandle) : Longint; cdecl;
begin
	IupGLMakeCurrent(ih);

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glAlphaFunc(GL_GEQUAL, 0.5);
	glFrontFace(GL_CW);
	glClearColor(0.4, 0.7, 0.8, 0.0);
	
	//glShadeModel(GL_FLAT);

	InitializeRender;
	
	glGenTextures(1, @rt_color);
	glBindTexture(GL_TEXTURE_2D, rt_color);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	
	glGenTextures(1, @rt_distort);
	glBindTexture(GL_TEXTURE_2D, rt_distort);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	
	glBindTexture(GL_TEXTURE_2D, 0);	

	Result := IUP_DEFAULT;
end;

function gl_resize_cb(ih : Ihandle; w, h : Longint) : Longint; cdecl;
var
	aspect : Single;
	m : TMatrix;
begin
	IupGLMakeCurrent(ih);
	glViewport(0, 0, w, h);

	aspect := w / h;

	glMatrixMode(GL_PROJECTION);
	PerspectiveLH(m, cam_fov*(PI/180), aspect, 0.25, far_plane);
	glLoadMatrixf(@m);
	glMatrixMode(GL_MODELVIEW);
	
	glBindTexture(GL_TEXTURE_2D, rt_color);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);

	glBindTexture(GL_TEXTURE_2D, rt_distort);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
	
	glBindTexture(GL_TEXTURE_2D, 0);	
	
	rt_width := w;
	rt_height := h;

	Result := IUP_DEFAULT;
end;

procedure UpdateFrustum;
var
	vp : TMatrix;
begin
	vp := proj;
	Mul44(vp, modelview);
	FrustumFromMatrix(frustum, vp);
end;

function gl_redraw_cb(ih : Ihandle; x, y : Single) : Longint; cdecl;
var
//	sinx, cosx : Single;
//	siny, cosy : Single;
	clear_color : TVec4;
	m : TMatrix;
begin
	IupGLMakeCurrent(ih);

	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

//	sinx := Sin(anglex * (PI/180));
//	cosx := Cos(anglex * (PI/180));

//	siny := Sin(angley * (PI/180));
//	cosy := Cos(angley * (PI/180));

//	glLoadIdentity;
//	gluLookAt(distance*siny*cosx, distance*sinx, distance*cosy*cosx, 0, 0, 0, 0, 1, 0);

//
	glMatrixMode(GL_PROJECTION);
	PerspectiveLH(m, cam_fov*(PI/180), viewport[2] / viewport[3], 0.25, far_plane);
	glLoadMatrixf(@m);
	glMatrixMode(GL_MODELVIEW);
//

	glLoadIdentity;
	glRotatef(anglex, 1, 0, 0);
	glRotatef(angley, 0, 1, 0);
	
	if useWeather then uWeather.RenderSky;
	
	glTranslatef(-position.x, -position.y, -position.z);

	glGetDoublev(GL_MODELVIEW_MATRIX,  @modelview_d);
	glGetDoublev(GL_PROJECTION_MATRIX, @proj_d);
	glGetFloatv(GL_MODELVIEW_MATRIX,  @modelview);
	glGetFloatv(GL_PROJECTION_MATRIX, @proj);
	glGetIntegerv(GL_VIEWPORT, @viewport);

	UpdateFrustum;
	uWeather.Update;
	
	camera_pos.x := position.x;
	camera_pos.y := position.y;
	camera_pos.z := position.z;
	
	UpdateCameraPos;
	
	Scene.RenderPrepare;
	
	if useWireframe then
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	
	Scene.RenderOpaqueFast;
	if Scene.showEnvZones or (edit_mode = emEnvZone) then
		Scene.RenderEnvZones;
	
	glDepthMask(GL_FALSE);
	Scene.RenderBlended;
	glDepthMask(GL_TRUE);
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	glReadBuffer(GL_BACK_LEFT);
	glBindTexture(GL_TEXTURE_2D, rt_color);
	glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, rt_width, rt_height, 0);
	glBindTexture(GL_TEXTURE_2D, 0);
	
	glGetFloatv(GL_COLOR_CLEAR_VALUE, @clear_color);
	glClearColor(0.5, 0.5, 0.0, 0.0);
	glClear(GL_COLOR_BUFFER_BIT);
	
	Scene.RenderDistort;
	
	glClearColor(clear_color.x, clear_color.y, clear_color.z, clear_color.w);
	
	glReadBuffer(GL_BACK_LEFT);
	glBindTexture(GL_TEXTURE_2D, rt_distort);
	glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 0, 0, rt_width, rt_height, 0);
	glBindTexture(GL_TEXTURE_2D, 0);

////
	glActiveTexture(GL_TEXTURE0);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, rt_color);

	glActiveTexture(GL_TEXTURE1);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, rt_distort);
////
	glDisable(GL_DEPTH_TEST);
	
	glMatrixMode(GL_PROJECTION);
	glPushMatrix;
	glLoadIdentity;
	gluOrtho2D(0.0, 1.0, 1.0, 0.0);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix;
	glLoadIdentity;
	
	glEnable(GL_FRAGMENT_PROGRAM_ARB);
	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SCREEN_IMAGE]);
	
	glLineWidth(1);
	glBegin(GL_QUADS);
	
	glTexCoord2f(0.0, 1.0);
	glVertex3f(0.0, 0.0, 0.0);
	glTexCoord2f(1.0, 1.0);
	glVertex3f(1.0, 0.0, 0.0);
	glTexCoord2f(1.0, 0.0);
	glVertex3f(1.0, 1.0, 0.0);
	glTexCoord2f(0.0, 0.0);
	glVertex3f(0.0, 1.0, 0.0);
	
	glEnd;
	
	glDisable(GL_FRAGMENT_PROGRAM_ARB);
	glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
	
	// selection rect
	if (mouse_x <> lclick_x) and (mouse_y <> lclick_y) and selection_rect then
	begin
		glLineWidth(1);
		glBegin(GL_LINE_LOOP);
		glVertex3f(lclick_x / viewport[2], lclick_y / viewport[3], 0);
		glVertex3f(mouse_x / viewport[2], lclick_y / viewport[3], 0);
		glVertex3f(mouse_x / viewport[2], mouse_y / viewport[3], 0);
		glVertex3f(lclick_x / viewport[2], mouse_y / viewport[3], 0);
		glEnd;
	end;
	
	glMatrixMode(GL_PROJECTION);
	glPopMatrix;
	glMatrixMode(GL_MODELVIEW);
	glPopMatrix;
	
	glActiveTexture(GL_TEXTURE1);
	glDisable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
	glActiveTexture(GL_TEXTURE0);
	glDisable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, 0);
////

	if m_t <> nil then
	begin
		m_t.Draw;
	end;
	
	glEnable(GL_DEPTH_TEST);
	
	IupGLSwapBuffers(ih);
	Result := IUP_DEFAULT;
end;

function gl_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	mousexy : Ihandle;
	pos : String;
begin
	pos := IntToStr(x) + ', ' + IntToStr(y);

	mousexy := IupGetDialogChild(ih, 'MOUSEXY');

	IupSetStrAttribute(mousexy, 'TITLE', PAnsiChar(pos));

	if status[4] = '3' then
	begin
		anglex := anglex - (y - mouse_y);
		angley := angley - (x - mouse_x);

		if anglex >= 90.0 then
			anglex := 90.0;
		if anglex <= -90.0 then
			anglex := -90.0;

		direction.x := -Sin(angley*(PI/180)) * Cos(anglex*(PI/180));
		direction.y := Sin(anglex*(PI/180));
		direction.z := Cos(angley*(PI/180)) * Cos(anglex*(PI/180));

		IupRedraw(ih, 0);
	end else
	if status[3] = '2' then
	begin
		distance := distance + ((y - mouse_y) / 8);
		IupRedraw(ih, 0);
	end;
	
	if selection_rect then
		IupRedraw(ih, 0);

	if (m_t <> nil) and (m_t.IsActive) then
	begin
		m_t.Update(x, y);
		IupRedraw(ih, 0);
	end;

	mouse_x := x;
	mouse_y := y;

	Result := IUP_DEFAULT;
end;

function RaycastPoint(const p, dir : TVec3; dist : Single; out point, normal : TVec3; out shape : Pointer) : Boolean;
var
	group : Longword;
begin
	group := PH_GROUP_DEFAULT_MASK;					
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, @point, @normal, nil, group);

	Result := shape <> nil;
end;

function RaycastManipulator(const p, dir : TVec3; dist : Single; out shape : Pointer) : TManipulator;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	group := PH_GROUP_MANIPULATOR_MASK;
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TManipulator then
			Result := TManipulator(sel)
		else
			Result := nil;
	end;
end;

function RaycastEntity(const p, dir : TVec3; dist : Single; out shape : Pointer) : TEntity;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	if Scene.showShapes then
		group := PH_GROUP_DEFAULT_MASK or PH_GROUP_SHAPE_MASK
	else
		group := PH_GROUP_DEFAULT_MASK;
					
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TEntity then
			Result := TEntity(sel)
		else
			Result := nil;
	end;
end;

function RaycastEnvZone(const p, dir : TVec3; dist : Single) : TEnvZone;
var
	group : Longword;
	actor : TPHActor;
	shape : TPHShape;
	sel : TObject;
begin
	group := PH_GROUP_DEFAULT_MASK or PH_GROUP_ENV_ZONE_MASK;				
	shape := PHRaycastClosestShape(Scene.ph_scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TEnvZone then
			Result := TEnvZone(sel)
		else
			Result := nil;
	end;
end;

procedure RaySelect(status : PAnsiChar; const p, dir : TVec3; dist : Single);
var
	e : TEntity;
	z : TEnvZone;
	shape : TPHShape;
begin
	case edit_mode of
	
		emEntity: begin
			e := RaycastEntity(p, dir, dist, shape);
			if e <> nil then
			begin						
				if iup_isshift(status) then
				begin	
					if not e.selected then
						e.selected := True
					else
						e.selected := False;
						
					UpdateSelection;
				end else
				begin
					DeselectAll;
					e.selected := True;
					UpdateSelection;
				end;		
			end else // sel is not TEntity
			begin
				if not iup_isshift(status) then
				begin
					DeselectAll;
					UpdateSelection;
				end;
			end;
		end;
		
		emEnvZone: begin
			z := RaycastEnvZone(p, dir, dist);
			if z <> nil then
			begin						
				if iup_isshift(status) then
				begin	
					if not z.selected then
						z.selected := True
					else
						z.selected := False;
						
					UpdateSelection;
				end else
				begin
					DeselectAll;
					z.selected := True;
					UpdateSelection;
				end;		
			end else // sel is not TEntity
			begin
				if not iup_isshift(status) then
				begin
					DeselectAll;
					UpdateSelection;
				end;
			end;			
		end;
		
	end;
end;

procedure FrustumSelect(status : PAnsiChar; x1, y1, x2, y2 : Longint { rect } );
var
	fovy : Single;
	aspect : Single;
	
	cx, cy : Longint;
	
	p1, p2 : array[1..3] of GLdouble;
	v1, v2 : TVec3;
	
	matrix1, matrix2 : TMatrix;
	frustum : array[1..6] of TPlane;
	
	I : Longint;
	list : TList;
	
	tmp : Longint;
	bb : TAABB;
	
	E : TEntity;
begin
	if (x1 = x2) or (y1 = y2) then
		Exit;
		
	if x1 > x2 then
	begin
		tmp := x2;
		x2 := x1;
		x1 := tmp;
	end;
	
	if y1 > y2 then
	begin
		tmp := y2;
		y2 := y1;
		y1 := tmp;
	end;
		
	WriteLn('y1 = ', y1, ' y2 = ', y2, ' vp[0] = ', viewport[0], ' vp[2] = ', viewport[2]);
	fovy := cam_fov * ((y2-y1) / (viewport[3]-viewport[1]));
	aspect := (x2-x1) / (y2-y1);
	
	WriteLn('fovy = ', fovy, ' aspect = ', aspect);
	
	cx := x1 + (x2-x1) div 2;
	cy := y1 + (y2-y1) div 2;
	
	// in OpenGL origin of window coordinates is a left-bottom corner. That's why viewport[3]-y
	gluUnProject(cx, viewport[3]-cy, 0.9, @modelview_d, @proj_d, @viewport, @p1[1], @p1[2], @p1[3]);
	gluUnProject(cx, viewport[3]-cy, 0.1, @modelview_d, @proj_d, @viewport, @p2[1], @p2[2], @p2[3]);
	
	v1.x := p1[1];
	v1.y := p1[2];
	v1.z := p1[3];

	v2.x := p2[1];
	v2.y := p2[2];
	v2.z := p2[3];
	
	PerspectiveLH(matrix1, fovy*(PI/180), aspect, 0.1, 500);
	LookAtLH(matrix2, v2.x, v2.y, v2.z, v1.x, v1.y, v1.z, 0, 1, 0);
	
	Mul44(matrix1, matrix2);
	
	FrustumFromMatrix(frustum, matrix1);
	
	DeselectAll;
	list := TList.Create;
	
	case edit_mode of
		emEntity: begin
			for I := 0 to Scene.entities.Count-1 do
			begin
				E := TEntity(Scene.entities[I]);
				if E.Visible and AABBVisible(frustum, E.bbox) then
					list.Add(E);
			end;
			
			for I := 0 to list.Count-1 do
				TEntity(list[I]).selected := True;
		end;
		
		emEnvZone: begin
			for I := 0 to Scene.env_zones.Count-1 do
			begin
				TEnvZone(Scene.env_zones[I]).GetBBox(bb);
				if AABBVisible(frustum, bb) then
					list.Add(Scene.env_zones[I]);
			end;
			
			for I := 0 to list.Count-1 do
				TEnvZone(list[I]).selected := True;
		end;
	end;
	
	UpdateSelection;
	list.Free;
end;

procedure ApplyManipulator(var dest : TMatrix; const c : TVec3);
var
	mat : TMatrix;
	l_offset : TVec3;
	l_offset_m : TMatrix;
	
	diff : TMatrix;	
begin
	if m_mode = mmMove then
	begin
		// get local offset					
		l_offset := TMoveManipulator(m_t).local_offset;		
		//WriteLn('x = ', l_offset.x, ' y = ', l_offset.y, ' z = ', l_offset.z);
		Translate(l_offset_m, l_offset);
		
		if m_move_axis = maObject then
		begin
			Mul44(dest, l_offset_m);
		end;
		if m_move_axis = maWorld then
		begin
			mat := l_offset_m;
			Mul44(mat, dest);
			dest := mat;
		end;
	end;
	
	if m_mode = mmRotate then
	begin
		if m_rotate_axis = maGroup then
		begin
			mat := dest;
			mat[4,1] := mat[4,1] - c.x;
			mat[4,2] := mat[4,2] - c.y;
			mat[4,3] := mat[4,3] - c.z;
			diff := m_t.Diff;
			//WriteMatrix(diff);
			Mul44(diff, mat);
			mat := diff;
			mat[4,1] := mat[4,1] + c.x;
			mat[4,2] := mat[4,2] + c.y;
			mat[4,3] := mat[4,3] + c.z;
			
			dest := mat;
		end else
		begin
			// multiply difference by current rotation
			mat[1,1] := m_t.Diff[1,1]*dest[1,1] + m_t.Diff[2,1]*dest[1,2] + m_t.Diff[3,1]*dest[1,3];
			mat[1,2] := m_t.Diff[1,2]*dest[1,1] + m_t.Diff[2,2]*dest[1,2] + m_t.Diff[3,2]*dest[1,3];
			mat[1,3] := m_t.Diff[1,3]*dest[1,1] + m_t.Diff[2,3]*dest[1,2] + m_t.Diff[3,3]*dest[1,3];
		
			mat[2,1] := m_t.Diff[1,1]*dest[2,1] + m_t.Diff[2,1]*dest[2,2] + m_t.Diff[3,1]*dest[2,3];
			mat[2,2] := m_t.Diff[1,2]*dest[2,1] + m_t.Diff[2,2]*dest[2,2] + m_t.Diff[3,2]*dest[2,3];
			mat[2,3] := m_t.Diff[1,3]*dest[2,1] + m_t.Diff[2,3]*dest[2,2] + m_t.Diff[3,3]*dest[2,3];
		
			mat[3,1] := m_t.Diff[1,1]*dest[3,1] + m_t.Diff[2,1]*dest[3,2] + m_t.Diff[3,1]*dest[3,3];
			mat[3,2] := m_t.Diff[1,2]*dest[3,1] + m_t.Diff[2,2]*dest[3,2] + m_t.Diff[3,2]*dest[3,3];
			mat[3,3] := m_t.Diff[1,3]*dest[3,1] + m_t.Diff[2,3]*dest[3,2] + m_t.Diff[3,3]*dest[3,3];
		
			// copy translation & etc
			mat[1,4] := dest[1,4];
			mat[2,4] := dest[2,4];
			mat[3,4] := dest[3,4];
			mat[4,1] := dest[4,1]; mat[4,2] := dest[4,2]; mat[4,3] := dest[4,3]; mat[4,4] := dest[4,4];
		
			dest := mat;
		end;
	end;
	
	if m_mode = mmScale then
	begin
		Mul44(dest, m_t.Diff);
	end;
end;

function gl_button_cb(ih : Ihandle; button, pressed : Longint; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
const
	RAYCAST_DIST = 500.0;
var
	p1, p2 : array[1..3] of GLdouble;
	p, dir : TVec3;
	
	hit_pos, hit_nrm : TVec3;

	shape : TPHShape;
	m_used : Boolean;
	
	I : Longint;
	selected : TEntityArray;
	z : TEnvZoneArray;
	e : TEntity;
	
	mat : TMatrix;
	c : TVec3;
begin	
	if not Assigned(Scene.ph_scene) then
	begin
		Result := IUP_DEFAULT;
		Exit;
	end;

	// in OpenGL origin of window coordinates is a left-bottom corner. That's why viewport[3]-y
	gluUnProject(x, viewport[3]-y, 0.1, @modelview_d, @proj_d, @viewport, @p1[1], @p1[2], @p1[3]);
	gluUnProject(x, viewport[3]-y, 0.9, @modelview_d, @proj_d, @viewport, @p2[1], @p2[2], @p2[3]);

	p.x := p1[1];
	p.y := p1[2];
	p.z := p1[3];

	dir.x := p2[1]-p1[1];
	dir.y := p2[2]-p1[2];
	dir.z := p2[3]-p1[3];
	Normalize(dir);

	m_used := False;
	
	if button = Ord('3') then
	begin
		if pressed = 1 then
		begin
			rclick_x := x;
			rclick_y := y;
		end else
		if pressed = 0 then
		begin
			if (rclick_x = x) and (rclick_y = y) then
				IupPopup(context_menu, IUP_MOUSEPOS, IUP_MOUSEPOS);
		end;
	end else
	if button = Ord('1') then
	begin
		if pressed = 1 then
		begin
			// try raycast manipulator first
			if RaycastManipulator(p, dir, RAYCAST_DIST, shape) <> nil then
			begin
				m_t.Activate(shape, x, y);
				m_used := True;
			end;

			if not m_used then
			begin
				lclick_x := x;
				lclick_y := y;
				
				selection_rect := True;
			end;
		end else
		if pressed = 0 then
		begin
			if (m_t <> nil) and m_t.IsActive then
			begin
				m_t.Deactivate;
				
				
				case edit_mode of
					
					emEntity: begin
						selected := Scene.GetSelected;	
						c := GetCenter(selected);
							
						for I := 0 to Length(selected)-1 do
						begin
							e := selected[I];
							mat := e.Matrix;
							ApplyManipulator(mat, c);
							e.Matrix := mat;
						end;
					end;
					
					emEnvZone: begin
						z := Scene.GetSelectedEZ;
						c := GetCenter(z);
						
						For I := 0 to Length(z)-1 do
						begin
							mat := z[I].Matrix;
							ApplyManipulator(mat, c);
							z[I].Matrix := mat;
						end;	
					end;
					
				end;
				
				m_used := True;
			end;
			
			if not m_used then
			begin	
				if selection_rect and (x <> lclick_x) and (y <> lclick_y) then
				begin
					FrustumSelect(status, lclick_x, lclick_y, x, y);
				end else
				if iup_iscontrol(status) and ((GetASyncKeyState(VK_Z) and $8000) <> 0) then
				begin // attach shape
					e := RaycastEntity(p, dir, RAYCAST_DIST, shape);
					
					if (e <> nil) and not e.selected then
						AttachShapesTo(e);
				end else
				if iup_iscontrol(status) and ((GetASyncKeyState(VK_X) and $8000) <> 0) then
				begin // remove shape
					e := RaycastEntity(p, dir, RAYCAST_DIST, shape);
					if (e <> nil) and e.selected then
					begin
						if PHGetGroup(shape) = PH_GROUP_SHAPE then 
							RemoveShape(e, TSection(PHGetShapeUserdata(shape)));
					end;
				end else
				if iup_iscontrol(status) and iup_isshift(status) then 
				begin // attach
					e := RaycastEntity(p, dir, RAYCAST_DIST, shape);
					
					if (e <> nil) and not e.selected then
						AttachSelectionTo(e);
				end else 
				if iup_iscontrol(status) then 
				begin // create entity
					if RaycastPoint(p, dir, RAYCAST_DIST, hit_pos, hit_nrm, shape) then
						CreateEntity(hit_pos, hit_nrm);
				end else
					RaySelect(status, p, dir, RAYCAST_DIST);
						
				selection_rect := False;		
			end;
		end;
	end;

	IupRedraw(ih, 0);

	Result := IUP_DEFAULT;
end;

function gl_keypress_cb(ih : Ihandle; ic : Longint; press : Longint) : Longint; cdecl;
var
	c : Char;
	sinx, cosx, siny, cosy : Single;
	m : TVec3;
begin
	Result := IUP_DEFAULT;
	
	if press <> 1 then
	begin
		Exit;
	end;

	if ic = $FFFF then // DELETE
	begin
		DeleteSelection;
		IupRedraw(ih, 0);
		Exit;
	end;

	sinx := Sin(anglex * (PI/180));
	cosx := Cos(anglex * (PI/180));
	siny := Sin(angley * (PI/180));
	cosy := Cos(angley * (PI/180));

	c := Char(ic);
	case c of
		'W', 'w' :
		begin
			m.x := -(siny * cosx);
			m.y := sinx;
			m.z := cosy * cosx;
		end;
		'S', 's' :
		begin
			m.x := siny * cosx;
			m.y := -sinx;
			m.z := -(cosy * cosx);
		end;
		'D', 'd' :
		begin
			m.x := cosy;
			m.y := 0;
			m.z := siny;
		end;
		'A', 'a' :
		begin
			m.x := -cosy;
			m.y := 0;
			m.z := -siny;
		end;
		else
		begin
			m.x := 0;
			m.y := 0;
			m.z := 0;
		end;
	end;

	if (GetAsyncKeyState(VK_SHIFT) and $8000) <> 0 then
	begin
		m.x := m.x * 5;
		m.y := m.y * 5;
		m.z := m.z * 5;
	end;

	position.x := position.x + m.x;
	position.y := position.y + m.y;
	position.z := position.z + m.z;

	IupRedraw(ih, 0);
end;

///////////////////////////////////////////////////////////////////////////////
// Toolbox callbacks                                                         //
///////////////////////////////////////////////////////////////////////////////
function list_rm_cb(ih : Ihandle; txt : PAnsiChar; item, state : Longint) : Longint; cdecl;
begin
	case item of
		1 : showAO := False;
		2 : showAO := True;
	end;

	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function tg_weather_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	useWeather := state = 1;

	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_tool_cb(ih : Ihandle) : Longint; cdecl;
var
	t : String;
	
	r_tool_move, r_tool_rotate, r_tool_scale : Ihandle;
	
	procedure Hide(ih : Ihandle);
	begin
		IupSetAttribute(ih, 'FLOATING', 'YES');
		IupSetAttribute(ih, 'VISIBLE', 'NO');
	end;
	
	procedure Show(ih : Ihandle);
	begin
		IupSetAttribute(ih, 'FLOATING', 'NO');
		IupSetAttribute(ih, 'VISIBLE', 'YES');
	end;
begin
	t := IupGetAttribute(ih, 'TITLE');

	r_tool_move := IupGetDialogChild(ih, 'R_TOOL_MOVE');
	r_tool_rotate := IupGetDialogChild(ih, 'R_TOOL_ROTATE');
	r_tool_scale := IupGetDialogChild(ih, 'R_TOOL_SCALE');

	Hide(r_tool_move);
	Hide(r_tool_rotate);
	Hide(r_tool_scale);

	if t = 'Move' then
	begin
		m_mode := mmMove;
		Show(r_tool_move);
	end
	else if t = 'Rotate' then
	begin
		m_mode := mmRotate;
		Show(r_tool_rotate);
	end
	else if t = 'Scale' then
	begin
		m_mode := mmScale;
		Show(r_tool_scale);
	end
	else
	begin
		m_mode := mmNone;
	end;
	
	IupRefresh(IupGetDialogChild(ih,'FRAME_TOOL'));
	
	UpdateManipulator;
	Redisplay;

	Result := IUP_DEFAULT;
end;

function tg_axis_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
var
	t : String;
begin
	if state = 1 then
	begin
		t := IupGetAttribute(ih, 'TITLE');
		
		if m_mode = mmMove then
		begin
			if t = 'Object' then
				m_move_axis := maObject
			else if t = 'World' then
				m_move_axis := maWorld;
		end;
		
		if m_mode = mmRotate then
		begin
			if t = 'Object' then
				m_rotate_axis := maObject
			else if t = 'World' then
				m_rotate_axis := maWorld
			else if t = 'Group' then
				m_rotate_axis := maGroup;
		end;
		
		UpdateManipulator;
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;

function tg_uniform_scale_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	m_uniform_scale := state = 1;
	UpdateManipulator;
	Redisplay;
	Result := IUP_DEFAULT;
end;

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
begin
	selected := Scene.GetSelectedList;

	if selected.Count > 0 then
	begin
		StrPCopy(@name, TEntity(selected[0]).Name);
		pivot := 0;
		
		format := 'Name: %s'#10;
		if selected.Count > 1 then
		begin
			format := format + 'Pivot: %l|<center>|';
			for I := 0 to selected.Count-1 do
				format := format + TEntity(selected[I]).Name + '|';
			format := format + #10;
		end;
		
		if IupGetParam('Add template', nil, nil, PAnsiChar(format), @name, @pivot) = 1 then
		begin
			if name[0] <> #0 then
			begin
				if pivot = 0 then
					NewTemplate(name, selected, nil)
				else
					NewTemplate(name, selected, TEntity(selected[pivot-1]));
	
				t := IupGetDialogChild(ih, 'TREE_TEMPLATES');
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
begin
	t := IupGetDialogChild(ih, 'TREE_TEMPLATES');
	id := IupGetInt(t, 'VALUE');
	if id >= 0 then
	begin
		v := TSection(IupGetAttribute(t, PAnsiChar('USERDATA'+IntToStr(id))));
		DeleteTemplate(v);
		UpdateTemplates(t);
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
			selected[0].Name := PAnsiChar(@name);
			UpdateSelection;
		end;
	end else
		IupMessage('Message', 'Select just one object');

	Result := IUP_DEFAULT;
end;

function btn_delete_entity_cb(ih : Ihandle) : Longint; cdecl;
var
	selected : TList;
begin
	DeleteSelection;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_script_cb(ih : Ihandle) : Longint; cdecl;
var
	s : TSimpleValue;
	selected : TEntityArray;
begin
	selected := Scene.GetSelected;

	if Length(selected) = 1 then
	begin
		s := selected[0].data.GetParam('vss_ver_6', 'section');
		if s = nil then
			s := selected[0].data.GetParam('vss_ver_7', 'section');
		
		if s <> nil then	
			EditScript(s as TSection)
		else
			WriteLn('Warning! Invalid entity, doesn''t contain vss_ver_6 or vss_ver_7 section');
	end else
		IupMessage('Message', 'Select just one object');

	Result := IUP_DEFAULT;
end;

procedure property_changed_cb(prop : TSimpleValue) cdecl;
var
	mat : TMatrix;
	selected : TEntityArray;
begin
	if (prop.name = '') and (prop.vtype = 'pose, matrix') then
	begin		
		selected := Scene.GetSelected;
		
		if prop = selected[0].param_matrix then
		begin
			with prop as TFloatArrayValue do
			begin
				if Length(data) = 16 then GetMatrix44(mat)
				else GetMatrix43(mat)
			end;
		
			selected[0].Matrix := mat;
			UpdateManipulator;
		end;
		
		Redisplay;
	end;

	if (prop.name = 'att_offset') and ((prop.vtype = 'pose, matrix') or (prop.vtype = 'pose, matrix_43T')) then
	begin
		//selected := Scene.GetSelected;
		//selected[0].VisualName := (prop as TStringValue).str;
		Scene.UpdateAttaches;
		
		Redisplay;
	end;
	
	if (prop.name = 'visual') and (prop.vtype = 'stringz') then
	begin
		selected := Scene.GetSelected;
		selected[0].VisualName := (prop as TStringValue).str;
		UpdateSelection;
		Redisplay;
	end;
	
	// shapes, sphere & box
	if (prop.name = '') and (prop.vtype = 'pose, matrix') or
	   (prop.name = 'h_size') and (prop.vtype = 'vec3f') or
	   (prop.name = 'radius') and (prop.vtype = 'fp32') or
	   (prop.name = 'center') and (prop.vtype = 'vec3f') then
	begin
		selected := Scene.GetSelected;
		selected[0].UpdateShapes;
		Redisplay;
	end;
end;

function property_edit_cb(tree : Ihandle; sect : TSection; prop : TSimpleValue) : Longint; cdecl;
var
	v : TIntegerValue;
	s : TStringValue;
	names : String;
	
	sel : TEntity;
	parent : TEntity;
	
	skeleton : T4ASkeleton;
begin
	Result := 1; // 0 - cancel, 1 - default editor, 2 - apply
	
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
			if ChooseBone(skeleton, s.str) then
				Result := 2
			else
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
			if ChooseLocator(skeleton, s.str) then
				Result := 2
			else
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
			if ChooseBonePart(skeleton, s.str) then
				Result := 2
			else
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
			if ChooseAnimation(skeleton, s.str) then
				Result := 2
			else
				Result := 0;
		end;
	end;
end;

function tabs_changepos_cb(ih : Ihandle; new_pos, old_pos : Longint) : Longint; cdecl;
begin
	WriteLn('change tab ', new_pos);
	
	case new_pos of
		0: edit_mode := emEntity;
		1: edit_mode := emEnvZone;
	end;
	
	Redisplay;
	
	Result := IUP_DEFAULT;
end;

///////////////////////////////////////////////////////////////////////////////
// Menu callbacks                                                            //
///////////////////////////////////////////////////////////////////////////////
function menu_file_open_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
begin
	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'DIR');

	if level_path <> '' then
		IupSetStrAttribute(dlg, 'DIRECTORY', PAnsiChar(level_path))
	else
		IupSetStrAttribute(dlg, 'DIRECTORY', PAnsiChar(ResourcesPath + '\maps'));

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		LoadMap(fn);
	end;

	IupDestroy(dlg);
	Result := IUP_DEFAULT;
end;

function menu_file_save_cb(ih : Ihandle) : Longint; cdecl;
var
	kind : Byte;
begin
	if Scene.konf <> nil then
	begin
		case Scene.GetVersion of
			sceneVer2033:		kind := 5;
			sceneVerLL:			kind := 4;
			else						kind := 36;
		end;
			
		SaveLevelBin(level_path + '\level.bin', Scene.konf, kind);
		if Scene.konf_add <> nil then
			SaveLevelBin(level_path + '\level.add.bin', Scene.konf_add, kind);
	end else
		IupMessage('Message', 'Nothing opened');
		
	Result := IUP_DEFAULT;
end;

function menu_file_save_as_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
	
	kind : Byte;
begin
	if Scene.konf <> nil then
	begin
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetAttribute(dlg, 'FILE', PAnsiChar(level_path + '\level.bin'));
		IupSetAttribute(dlg, 'EXTFILTER', 'level.bin|*.bin|All files|*.*|');

		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			case Scene.GetVersion of
				sceneVer2033:		kind := 5;
				sceneVerLL:			kind := 4;
				else						kind := 36;
			end;
		
			fn := IupGetAttribute(dlg, 'VALUE');
			SaveLevelBin(fn, Scene.konf, kind);
		end;

		IupDestroy(dlg);
	end else
		IupMessage('Message', 'Nothing opened');

	Result := IUP_DEFAULT;
end;

function menu_file_save_addon_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
	
	kind : Byte;
begin
	if Scene.konf_add <> nil then
	begin
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetAttribute(dlg, 'FILE', PAnsiChar(level_path + '\level.add.bin'));
		IupSetAttribute(dlg, 'EXTFILTER', 'level.add.bin|*.bin|All files|*.*|');

		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			case Scene.GetVersion of
				sceneVer2033:		kind := 5;
				sceneVerLL:			kind := 4;
				else						kind := 36;
			end;
		
			fn := IupGetAttribute(dlg, 'VALUE');
			SaveLevelBin(fn, Scene.konf_add, kind);
		end;

		IupDestroy(dlg);
	end else
		IupMessage('Message', 'Nothing opened');

	Result := IUP_DEFAULT;
end;

function menu_file_save_env_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
begin
	if Scene.env_zones <> nil then
	begin
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetAttribute(dlg, 'FILE', PAnsiChar(level_path + '\level.environment'));
		IupSetAttribute(dlg, 'EXTFILTER', 'level.environemnt|*.environment|All files|*.*|');

		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			fn := IupGetAttribute(dlg, 'VALUE');
			Scene.SaveEnvironment(fn);
		end;

		IupDestroy(dlg);
	end else
		IupMessage('Message', 'Nothing opened');

	Result := IUP_DEFAULT;
end;

{$IFDEF HAZ_LEVELEXPORT}
function menu_file_export_cb(ih : Ihandle) : Longint; cdecl;
begin
	if Scene.entities <> nil then
	begin
		ExportLevel(ExtractFileName(level_path)); 
	end else
		IupMessage('Message', 'Nothing opened');
		
	Result := IUP_DEFAULT;
end;

function menu_file_export_xray_cb(ih : Ihandle) : Longint; cdecl;
begin
	if Scene.entities <> nil then
	begin
		ExportXrayLevel(ExtractFileName(level_path));
	end else
		IupMessage('Message', 'Nothing opened');
		
	Result := IUP_DEFAULT;
end;
{$ENDIF}

function menu_file_exit_cb(ih : Ihandle) : Longint; cdecl;
begin
	Result := IUP_CLOSE;
end;

function menu_edit_copy_cb(ih : Ihandle) : Longint; cdecl;
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
	
	Result := IUP_DEFAULT;
end;

function menu_edit_paste_cb(ih : Ihandle) : Longint; cdecl;
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
		
	Result := IUP_DEFAULT;
end;

function menu_edit_hide_cb(ih : Ihandle) : Longint; cdecl;
var
	e : TEntityArray;
	I : Longint;
begin
	e := Scene.GetSelected;
	for I := 0 to Length(e)-1 do
		e[I].Visible := False;
		
	Redisplay;
		
	Result := IUP_DEFAULT;
end;

function menu_edit_show_cb(ih : Ihandle) : Longint; cdecl;
var
	e : TEntityArray;
	I : Longint;
begin
	e := Scene.GetSelected;
	for I := 0 to Length(e)-1 do
		e[I].Visible := True;
		
	Redisplay;
		
	Result := IUP_DEFAULT;
end;

function menu_edit_show_all_cb(ih : Ihandle) : Longint; cdecl;
var
	I : Longint;
begin
	for I := 0 to Scene.entities.Count-1 do
		TEntity(Scene.entities[I]).Visible := True;
		
	Redisplay;
		
	Result := IUP_DEFAULT;
end;

function menu_show_cb(ih : Ihandle) : Longint; cdecl;
var
	title : String;
	
	procedure DoIt(var b : Boolean);
	begin
		b := not b;
		IupSetInt(ih, 'VALUE', Integer(b));
	end;
begin
	title := IupGetAttribute(ih, 'TITLE');
	
	if title = 'Flags' then DoIt(Scene.showFlags);
	if title = 'Shapes' then  DoIt(Scene.showShapes);
	if title = 'Environment zones' then DoIt(Scene.showEnvZones);
	if title = 'Decals' then DoIt(Scene.showDecals);
	if title = 'EGeoms' then DoIt(Scene.showEGeoms);
	
	Redisplay;
	Result := IUP_DEFAULT;
end;

function level_options_map_cb(ih : Ihandle) : Longint; cdecl;
var
	startup : TSection;
begin
	startup := Scene.konf.root.GetSect('startup');
	SetupProperties(ih, startup);
	
	Result := IUP_DEFAULT;
end;

function menu_level_options_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg, box, tree : Ihandle;
begin
	if Scene.konf <> nil then
	begin
		tree := IupTree;
		IupSetCallback(tree, 'MAP_CB', @level_options_map_cb);
		
		box := IupVBox(tree, nil);
		IupSetAttribute(box, 'MARGIN', '10x10');
		
		dlg := IupDialog(box);
		IupSetAttribute(dlg, 'TITLE', 'Level options');
		
		IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);
		
		IupDestroy(dlg);
	end;

	Result := IUP_DEFAULT;
end;

function menu_level_select_entity_cb(ih : Ihandle) : Longint; cdecl;
var
	entity : TEntity;
begin
	if Scene.entities <> nil then
	begin
		if ChooseEntity(entity) then
		begin
			DeselectAll;
			if entity <> nil then
				entity.selected := True;
			UpdateSelection;
			Redisplay;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_level_select_id_cb(ih : Ihandle) : Longint; cdecl;
var
	entity : TEntity;
begin
	if Scene.entities <> nil then
	begin
		if ChooseEntity(entity, True) then
		begin
			DeselectAll;
			if entity <> nil then
				entity.selected := True;
			UpdateSelection;
			Redisplay;
		end;
	end;
	
	Result := IUP_DEFAULT;
end;

function set_width(ih: Ihandle; state: Longint) : Longint; cdecl;
var
	value: PAnsiChar;
begin
	lineWidth := IupGetInt(
		IupGetDialogChild(ih, 'Linerange'),
		'VALUE'
	);

	ReloadGLPrograms;
	Redisplay;

	Result := IUP_DEFAULT;
end;

function menu_settings_custom_manipulator(ih : Ihandle) : Longint; cdecl;
var
	dlg_i : Ihandle;
	box: Ihandle;
	lineRange: Ihandle;
	btn: Ihandle;
begin

	lineRange := IupSetAttributes(
		IupVal('HORIZONTAL'),
		'NAME="Linerange", RASTERSIZE=164x32, MIN=1, MAX=24, MARGIN=8x8'
	);

	btn := IupSetAttributes(
		Button('Set width', @set_width),
		'NAME="setwidth", SIZE=78x14, FONTSIZE=10, MARGIN=8x8, PADDING=0x0'
	);

	box := 	IupVbox(
				IupFlatLabel('Line width'),
				lineRange,
				btn,
				nil
			);

	dlg_i := IupDialog(box);

	IupShow(IupSetAttributes(dlg_i, 'TITLE="Settings Editor", SIZE=128x64'));
	
	Result := IUP_DEFAULT;
end;

function menu_level_make_addon_cb(ih : Ihandle) : Longint; cdecl;
var
	arr : TEntityArray;
begin
	if Scene.entities <> nil then
	begin
		arr := Scene.GetSelected;
		Scene.MakeAddon(arr, True);
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_level_make_global_cb(ih : Ihandle) : Longint; cdecl;
var
	arr : TEntityArray;
begin
	if Scene.entities <> nil then
	begin
		arr := Scene.GetSelected;
		Scene.MakeAddon(arr, True);
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_render_wireframe_cb(ih : Ihandle) : Longint; cdecl;
begin
	useWireframe := not useWireframe;
	IupSetInt(ih, 'VALUE', Integer(useWireframe));
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_setfarplane_cb(ih : Ihandle) : Longint; cdecl;
var
	dist : Single;
begin
	dist := far_plane;
	if IupGetParam('Set view distance', nil, nil, 'View distance: %r'#10, @dist) = 1 then
	begin
		far_plane := dist;
		Redisplay;
	end;
	Result := IUP_DEFAULT;
end;

function menu_render_textures_cb(ih : Ihandle) : Longint; cdecl;
begin
	useTextures := not useTextures;
	IupSetInt(ih, 'VALUE', Integer(useTextures));
	
	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_bump_cb(ih : Ihandle) : Longint; cdecl;
begin
	useBump := not useBump;
	IupSetInt(ih, 'VALUE', Integer(useBump));
	
	ReloadGLPrograms;
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_detail_cb(ih : Ihandle) : Longint; cdecl;
begin
	useDetail := not useDetail;
	IupSetInt(ih, 'VALUE', Integer(useDetail));
	Redisplay;
	Result := IUP_DEFAULT;
end;

function menu_render_setbkcolor(ih : Ihandle) : Longint; cdecl;
var
	clr : TVec4;
	gl : Ihandle;
begin
	gl := IupGetDialogChild(ih, 'GL_CANVAS');
	IupGLMakeCurrent(gl);

	glGetFloatv(GL_COLOR_CLEAR_VALUE, @clr);
	if SelectColor(clr) then
	begin
		glClearColor(clr.x, clr.y, clr.z, 1.0);
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;

function menu_render_setquality(ih : Ihandle) : Longint; cdecl;
var
	q : Integer;
begin
	q := textureQuality;
	if IupGetParam('Texture quality', nil, nil, 'Max resolution: %i'#10, @q) = 1 then
		textureQuality := q;

	Result := IUP_DEFAULT;
end;

function menu_focus_on_selection_cb(ih : Ihandle): Longint; cdecl;
var
	sel : TEntityArray;
	I : Longint;
	
	sel_bb : TAABB;
	sel_bs : TSphere;
	
	dist : Single;
begin
	sel := Scene.GetSelected;
	if Length(sel) > 0 then
	begin
	
		sel_bb := sel[0].bbox;
		for I := 1 to Length(sel)-1 do
			AABBMerge(sel_bb, sel[I].bbox);
			
		CalcBSphere(sel_bs, sel_bb);
		
		dist := sel_bs.radius / tan(cam_fov / 2.0 * (PI/180));
		
		position.x := sel_bs.center.x + (-direction.x * dist);
		position.y := sel_bs.center.y + (-direction.y * dist);
		position.z := sel_bs.center.z + (-direction.z * dist);
		
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_select_parent_cb(ih : Ihandle): Longint; cdecl;
var
	sel : TEntityArray;
	p : TEntity;
begin
	sel := Scene.GetSelected;
	if Length(sel) = 1 then
	begin
		sel[0].selected := False;
		p := Scene.EntityById(sel[0].ParentID);
		if p <> nil then p.selected := True;
		
		UpdateSelection;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_select_childs_cb(ih : Ihandle): Longint; cdecl;
var
	sel : TEntityArray;
begin
	sel := Scene.GetSelected;
	if Length(sel) = 1 then
	begin
		RecursiveSelect(sel[0].ID);
		UpdateSelection;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function menu_reset_scale_cb(ih : Ihandle): Longint; cdecl;
var
	I : Longint;
	sel : TEntityArray;
	scale : TVec3;
	mat : TMatrix;
begin
	sel := Scene.GetSelected;
	
	for I := 0 to Length(sel)-1 do
	begin
		mat := sel[I].Matrix;
		
		scale.x := Sqrt(mat[1,1]*mat[1,1] + mat[1,2]*mat[1,2] + mat[1,3]*mat[1,3]);
		scale.y := Sqrt(mat[2,1]*mat[2,1] + mat[2,2]*mat[2,2] + mat[2,3]*mat[2,3]);
		scale.z := Sqrt(mat[3,1]*mat[3,1] + mat[3,2]*mat[3,2] + mat[3,3]*mat[3,3]);
		
		mat[1,1] := mat[1,1] / scale.x; mat[1,2] := mat[1,2] / scale.x; mat[1,3] := mat[1,3] / scale.x;
		mat[2,1] := mat[2,1] / scale.y; mat[2,2] := mat[2,2] / scale.y; mat[2,3] := mat[2,3] / scale.y;
		mat[3,1] := mat[3,1] / scale.z; mat[3,2] := mat[3,2] / scale.z; mat[3,3] := mat[3,3] / scale.z;
		
		sel[I].Matrix := mat;
	end;
	
	UpdateManipulator;
	Redisplay;
	
	Result := IUP_DEFAULT;
end;

///////////////////////////////////////////////////////////////////////////////
// Dialog & menu creation                                                    //
///////////////////////////////////////////////////////////////////////////////
procedure CreateDialog;
var
	gl : Ihandle;
	
	label_sel : Ihandle;
	mousexy : Ihandle;
	campos : Ihandle;
	status_bar : Ihandle;

	list_rm : Ihandle;
	tg_weather : Ihandle;
	box_rm : Ihandle;

	btn_none, btn_move, btn_rotate, btn_scale : Ihandle;
	tg_object, tg_world, tg_group, tg_uniform : Ihandle;
	box_tools, box_tool_opt : Ihandle;
	fr_tool : Ihandle;
	r_tool_move, r_tool_rotate, r_tool_scale : Ihandle;

	tabs : Ihandle;

	fr_create : Ihandle;
	tree_templates : Ihandle;
	list_transform : Ihandle;
	tg_select_new : Ihandle;
	btn_add, btn_remove : Ihandle;

	fr_entity : Ihandle;
	btn_rename, btn_delete, btn_script : Ihandle;
	t_props : Ihandle;
	t_props_env : Ihandle;
	
	toolbox : Ihandle;
	
	menu : Ihandle;
	menu_save_part : Ihandle;
	
	sm_file, sm_edit, sm_show, sm_level, sm_render, sm_settings : Ihandle;

	dlg : Ihandle;
begin
	// OpenGL Window
	gl := IupGLCanvas(nil);
	IupSetAttribute(gl, 'BUFFER', 'DOUBLE');
	IupSetInt(gl, 'DEPTH_SIZE', 32);
	//IupSetAttribute(gl, 'EXPAND', 'YES');
	IupSetAttribute(gl, 'RASTERSIZE', '800x600');
	IupSetCallback(gl, 'ACTION', @gl_redraw_cb);
	IupSetCallback(gl, 'MAP_CB', @gl_map_cb);
	IupSetCallback(gl, 'RESIZE_CB', @gl_resize_cb);
	IupSetCallback(gl, 'MOTION_CB', @gl_motion_cb);
	IupSetCallback(gl, 'BUTTON_CB', @gl_button_cb);
	IupSetCallback(gl, 'KEYPRESS_CB', @gl_keypress_cb);
	IupSetAttribute(gl, 'NAME', 'GL_CANVAS');
	
	// Statusbar
	mousexy := IupLabel('0, 0');
	IupSetAttribute(mousexy, 'NAME', 'MOUSEXY');
	IupSetAttribute(mousexy, 'RASTERSIZE', '100x');
	
	label_sel := IupLabel('Sel: 0');
	IupSetAttribute(label_sel, 'NAME', 'LABEL_SEL');
	IupSetAttribute(label_sel, 'ALIGNMENT', 'RIGHT');
	IupSetAttribute(label_sel, 'RASTERSIZE', '80x');
	
	campos := IupLabel('Cam pos: 0.0 0.0 0.0');
	IupSetAttribute(campos, 'NAME', 'LABEL_CAMPOS');
	IupSetAttribute(campos, 'RASTERSIZE', '250x');
	
	status_bar := IupHBox(mousexy, label_sel, campos, nil);

	// Left Toolbox
	list_rm := IupList(nil);
	IupSetAttributes(list_rm, 'DROPDOWN=YES, 1="Default", 2="AO"');
	IupSetAttribute(list_rm, 'VALUE', '1');
	IupSetCallback(list_rm, 'ACTION', @list_rm_cb);
	
	tg_weather := iup.Toggle('Weather', @tg_weather_cb, useWeather);
	
	box_rm := IupHBox(
		IupLabel('Render Mode'),
		list_rm,
		tg_weather,
		nil
	);
	IupSetAttribute(box_rm, 'MARGIN', '0x0');

	btn_none := iup.Button('None', @btn_tool_cb);
	btn_move := iup.Button('Move', @btn_tool_cb);
	btn_rotate := iup.Button('Rotate', @btn_tool_cb);
	btn_scale := iup.Button('Scale', @btn_tool_cb);
	
	tg_object := iup.Toggle('Object', @tg_axis_cb);
	tg_world := iup.Toggle('World', @tg_axis_cb);
	
	r_tool_move := IupRadio(IupHBox(tg_object, tg_world, nil));
	IupSetAttribute(r_tool_move, 'NAME', 'R_TOOL_MOVE');
	IupSetAttribute(r_tool_move, 'VISIBLE', 'NO');
	IupSetAttribute(r_tool_move, 'FLOATING', 'YES');
	
	tg_object := iup.Toggle('Object', @tg_axis_cb);
	tg_world := iup.Toggle('World', @tg_axis_cb);
	tg_group := iup.Toggle('Group', @tg_axis_cb);
	
	r_tool_rotate := IupRadio(IupHBox(tg_object, tg_world, tg_group, nil));
	IupSetAttribute(r_tool_rotate, 'NAME', 'R_TOOL_ROTATE');
	IupSetAttribute(r_tool_rotate, 'VISIBLE', 'NO');
	IupSetAttribute(r_tool_rotate, 'FLOATING', 'YES');
	
	tg_uniform := iup.Toggle('Uniform', @tg_uniform_scale_cb);
	
	r_tool_scale := IupHBox(tg_uniform, nil);
	IupSetAttribute(r_tool_scale, 'NAME', 'R_TOOL_SCALE');
	IupSetAttribute(r_tool_scale, 'VISIBLE', 'NO');
	IupSetAttribute(r_tool_scale, 'FLOATING', 'YES');
	
	{
	fr_tool := IupFrame(
		IupSetAttributes(IupVBox(
			IupHBox(btn_none, btn_move, btn_rotate, btn_scale, IupFill, nil),
			r_tool_move,
			r_tool_rotate,
			r_tool_scale,
			nil
		), 'MARGIN=5x3')
	);
	IupSetAttribute(fr_tool, 'NAME', 'FRAME_TOOL');
	IupSetAttribute(fr_tool, 'TITLE', 'Tool');
	}
	
	// use variable due to compiler bug
	box_tools := IupHBox(btn_none, btn_move, btn_rotate, btn_scale, IupFill, nil);
	
	fr_tool := IupFrame(
		IupSetAttributes(IupVBox(
			box_tools,
			r_tool_move,
			r_tool_rotate,
			r_tool_scale,
			nil
		), 'MARGIN=5x3')
	);
	IupSetAttribute(fr_tool, 'NAME', 'FRAME_TOOL');
	IupSetAttribute(fr_tool, 'TITLE', 'Tool');
	
	tree_templates := IupSetAttributes(IupTree, 'NAME=TREE_TEMPLATES, RASTERSIZE=200x');
	IupSetAttribute(tree_templates, 'ADDEXPANDED', 'NO');
	IupSetAttribute(tree_templates, 'ADDROOT', 'NO');
	IupSetAttribute(tree_templates, 'IMAGELEAF', 'IMGEMPTY');
	IupSetAttribute(tree_templates, 'HIDELINES', 'YES');

	list_transform := IupList(nil);
	IupSetAttributes(list_transform, 'NAME=LIST_TRANSFORM, DROPDOWN=YES');
	IupSetAttributes(list_transform, '1="Y = World UP", 2="Y = Normal", 3="Z = Normal", 4="Z = -Normal"');
	IupSetAttribute(list_transform, 'VALUE', '1');
	
	tg_select_new := iup.Toggle('Select', @tg_select_new_cb, select_created);

	btn_add := iup.Button('Add', @btn_add_template_cb);
	btn_remove := iup.Button('Remove', @btn_remove_template_cb);

	fr_create := IupFrame(
		IupVBox(
			tree_templates,
			IupSetAttributes(IupHBox(btn_add, btn_remove, nil), 'MARGIN=0x0'),
			IupSetAttributes(IupHBox(list_transform, tg_select_new, nil), 'MARGIN=0x0'), 
			nil
		)
	);
	IupSetAttribute(fr_create, 'TITLE', 'Create');

	t_props := IupSetAttributes(IupTree, 'NAME=TREE_PROPS, RASTERSIZE=200x');
	IupSetCallback(t_props, 'PROPS_VALUECHANGED_CB', @property_changed_cb);
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
	
	t_props_env := IupSetAttributes(IupTree, 'NAME=TREE_PROPS_ENV, RASTERSIZE=200x');

	tabs := IupTabs(
		IupVBox(fr_create, fr_entity, nil),
		IupVBox(IupLabel('nothing to do here!'), t_props_env, nil),
		nil
	);
	
	IupSetAttribute(tabs, 'TABTITLE0', 'Entities');
	IupSetAttribute(tabs, 'TABTITLE1', 'Environment');
	
	IupSetAttribute(tabs, 'TABVISIBLE1', 'NO'); // not ready yet :)
	
	IupSetCallback(tabs, 'TABCHANGEPOS_CB', @tabs_changepos_cb);

	toolbox := IupVBox(box_rm, fr_tool, tabs, nil);
	IupSetAttribute(toolbox, 'MARGIN', '10x10');
	IupSetAttribute(toolbox, 'GAP', '5x5');
	
	// Main Menu
	menu_save_part := IupSubmenu('Save part',
		IupMenu(
			iup.MenuItem('Save level.bin as...', @menu_file_save_as_cb),
			iup.MenuItem('Save level.add.bin as...', @menu_file_save_addon_cb),
			iup.MenuItem('Save level.environemnt as...', @menu_file_save_env_cb),
			nil
		)
	);

	sm_file := IupSubmenu('File',
		IupMenu(
			iup.MenuItem('Open...', @menu_file_open_cb), 
			iup.MenuItem('Save', @menu_file_save_cb), 
			menu_save_part,
			IupSeparator,
			{$IFDEF HAZ_LEVELEXPORT}
			iup.MenuItem('Export...', @menu_file_export_cb),
			iup.MenuItem('Export for X-Ray SDK...', @menu_file_export_xray_cb),
			IupSeparator, 
			{$ENDIF}
			iup.MenuItem('Exit', @menu_file_exit_cb), 
			nil
		)
	);
		
	sm_edit := IupSubmenu('Edit',
		IupMenu(
			iup.MenuItem('Copy', @menu_edit_copy_cb), 
			iup.MenuItem('Paste', @menu_edit_paste_cb), 
			IupSeparator,
			iup.MenuItem('Hide', @menu_edit_hide_cb),
			iup.MenuItem('Show', @menu_edit_show_cb),
			iup.MenuItem('Show all', @menu_edit_show_all_cb),
			nil
		)
	);
		
	sm_show := IupSubmenu('Show',
		IupMenu(
			iup.MenuItem('Flags', @menu_show_cb, Scene.showFlags),
			iup.MenuItem('Shapes', @menu_show_cb, Scene.showShapes),
			iup.MenuItem('Environment zones', @menu_show_cb, Scene.showEnvZones),
			iup.MenuItem('Decals', @menu_show_cb, Scene.showDecals),
			iup.MenuItem('EGeoms', @menu_show_cb, Scene.showEGeoms),
			nil
		)
	);
		
	sm_level := IupSubmenu('Level',
		IupMenu(
			iup.MenuItem('Options...', @menu_level_options_cb), 
			iup.MenuItem('Select entity...', menu_level_select_entity_cb), 
			iup.MenuItem('Select entity (with IDs)...', @menu_level_select_id_cb), 
			IupSeparator, 
			iup.MenuItem('Make addon',  @menu_level_make_addon_cb), 
			iup.MenuItem('Make global', @menu_level_make_global_cb), 
			nil
		)
	);
		
	sm_render := IupSubmenu('Render',
		IupMenu(
			iup.MenuItem('Set view distance', @menu_render_setfarplane_cb), 
			iup.MenuItem('Set background color', @menu_render_setbkcolor), 
			IupSeparator, 
			iup.MenuItem('Wireframe', @menu_render_wireframe_cb, useWireframe), 
			IupSeparator, 
			iup.MenuItem('Textures', @menu_render_textures_cb, useTextures), 
			iup.MenuItem('Bump', @menu_render_bump_cb, useBump), 
			iup.MenuItem('Detail', @menu_render_detail_cb, useDetail), 
			iup.MenuItem('Set texture quality', @menu_render_setquality), 
			nil
		)
	);

	sm_settings := IupSubmenu('Settings',
		IupMenu(
			iup.MenuItem('Customize manipulator', @menu_settings_custom_manipulator),
			nil
		)
	);

	menu := IupMenu(
		sm_file,
		sm_edit,
		sm_show,
		sm_level,
		sm_render,
		sm_settings,
		nil
	);

	// Dialog
	dlg := IupDialog(
		IupSplit( toolbox, IupVBox(gl, status_bar, nil) )
	);
	
	IupSetAttribute(dlg, 'TITLE', 'Level Editor');
	IupSetAttributeHandle(dlg, 'MENU', menu);

	IupSetHandle('MAINDIALOG', dlg);

	IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
end;

procedure CreateContextMenu;
begin	
	context_menu := IupMenu(
		iup.MenuItem('Copy', @menu_edit_copy_cb),
		iup.MenuItem('Paste', @menu_edit_paste_cb),
		IupSeparator,
		iup.MenuItem('Hide', @menu_edit_hide_cb),
		iup.MenuItem('Show', @menu_edit_show_cb),
		iup.MenuItem('Show all', @menu_edit_show_all_cb),
		IupSeparator,
		iup.MenuItem('Focus on selection', @menu_focus_on_selection_cb),
		IupSeparator,
		iup.MenuItem('Select parent', @menu_select_parent_cb),
		iup.MenuItem('Select all childs', @menu_select_childs_cb),
		iup.MenuItem('Reset scale', @menu_reset_scale_cb), // deprecated due to matrix editor?
		nil
	);
end;

begin
	IupOpen(nil, nil);
	IupGLCanvasOpen;
	
	PHInitialize; // initialize PhysX

	if ParamCount > 0 then
		ResourcesPath := ParamStr(1)
	else
	begin
		GetDir(0, ResourcesPath);
		ResourcesPath := ResourcesPath + '\content';
	end;

	InitializeEngine;
	
	Scene := TScene.Create;
	Scene.showFlags := True;
	Scene.showDecals := True;
	Scene.showEGeoms := True;

	CreateDialog;
	CreateContextMenu;
	
	if ParamCount > 1 then
		LoadMap(ParamStr(2));
	
	IupMainLoop();
	IupDestroy(context_menu);

	SaveTemplates;
	UnloadTemplates;
	
	Scene.LevelUnload;
	Scene.Free;
	
	clipboard.Free;
	
	FinalizeEngine;
	
	PHFinalize; // finalize PhysX

	IupClose;
end.
