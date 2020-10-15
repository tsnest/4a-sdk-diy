program model_Editor;
uses common, Iup,
		 GL, GLU, GLExt, 
		 sysutils, classes, chunkedFile, vmath, PhysX, fouramdl,
		 Engine, Texture,
		 //uAO, 
		 uImportStatic, uImportDynamic, uImportMotion, 
		 skeleton, cform_utils, nxcform, uDrawUtils,
		 KonfigLibrary, uChooseMaterial, uChooseTexture, uExportScene;

var
	selected : T4AModel;
	model : T4AModel;
	edit_model : T4AModel;
	
	ph_scene : TPHScene;
	maler : IMaler;
	
	surfaces : TList;
	
	showTangents : Boolean = False;
	showSkeleton : Boolean = False;
	showBoneOBB : Boolean = False;
	
	current_mtlset : Longint;
	mtlsets : P4AMaterialSetArray; // pointer to actual array in model
	
	skeleton : T4ASkeleton;
	
var
	anglex, angley : Single;
	distance : Single = 5.0;

procedure Redisplay;
var
	gl : Ihandle;
begin
	gl := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'GL_CANVAS');
	IupRedraw(gl, 0);
end;

function FileDlg(save : Boolean; out filename : String; const ext : String = '.model'; const desc : String = '4A Model') : Boolean;
var
	dlg : Ihandle;
begin
	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'EXTDEFAULT', PAnsiChar(ext));
	IupSetAttribute(dlg, 'EXTFILTER', PAnsiChar(desc+' (*'+ext+')|*'+ext+'|All files (*.*)|*.*|'));

	if save then
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE')
	else
		IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	Result := False;

	if save then
	begin
		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			filename := IupGetAttribute(dlg, 'VALUE');
			Result := True;
		end
	end else
	begin
		if IupGetInt(dlg, 'STATUS') = 0 then
		begin
			filename := IupGetAttribute(dlg, 'VALUE');
			Result := True;
		end
	end;

	IupDestroy(dlg);
end;
	
procedure UpdateMaler;
var
	mh : T4AModelHierrarhy;
	ms : T4AModelSkeleton;
	mb : T4AModelSoftbody;
	mk : T4AModelSkinned;
begin
	FreeAndNil(maler);
	
	if edit_model is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(edit_model);
		maler := TStaticModelMaler.Create(mh);
	end else 
	if edit_model is T4AModelSkeleton then
	begin
		ms := T4AModelSkeleton(edit_model);
		maler := TSkeletonModelMaler.Create(ms);
	end else 
	if edit_model is T4AModelSoftBody then
	begin
		mb := T4AModelSoftBody(edit_model);
		maler := TSoftbodyModelMaler.Create(mb);
	end else
	if edit_model is T4AModelSkinned then
	begin
		mk := T4AModelSkinned(edit_model);
		maler := TSkinnedModelMaler.Create(nil, mk);
	end;
end;

procedure SelectModel(sel : T4AModel);
var
	ms : T4AModelSimple;
	mk : T4AModelSkinnedMesh;
	
	tex, sh, mtl, name : String;
	
	dlg, fr_material : Ihandle;
	l_tex, l_sh, l_mtl, l_name, t_collision : Ihandle;
	list_surfaces : Ihandle;
begin
	if Assigned(selected) then
		Exclude(selected.editor_flags, flSelected);

	selected := sel;

	dlg := IupGetHandle('MAINDIALOG');
	fr_material := IupGetDialogChild(dlg, 'FRAME_MATERIAL');
	list_surfaces := IupGetDialogChild(dlg, 'LIST_SURFACES');
	
	if Assigned(selected) then
	begin
		Include(selected.editor_flags, flSelected);
	
		if selected is T4AModelSimple then
		begin
			ms := selected as T4AModelSimple;
			tex := ms.texture;
			sh := ms.shader;
			mtl := ms.material;
			name := ms.name;
		end;
		if selected is T4AModelSkinnedMesh then
		begin
			mk := selected as T4AModelSkinnedMesh;
			tex := mk.texture;
			sh := mk.shader;
			mtl := mk.material;
			name := mk.name;
		end;

		IupSetAttribute(fr_material, 'VISIBLE', 'YES');

		l_tex := IupGetDialogChild(dlg, 'TEXT_TEXTURE');
		l_sh := IupGetDialogChild(dlg, 'TEXT_SHADER');
		l_mtl := IupGetDialogChild(dlg, 'TEXT_MATERIAL');
		l_name := IupGetDialogChild(dlg, 'TEXT_NAME');
		t_collision := IupGetDialogChild(dlg, 'TOGGLE_COLLISION');

		IupSetStrAttribute(l_tex, 'VALUE', PAnsiChar(tex));
		IupSetStrAttribute(l_sh, 'VALUE', PAnsiChar(sh));
		IupSetStrAttribute(l_mtl, 'VALUE', PAnsiChar(mtl));
		IupSetStrAttribute(l_name, 'VALUE', PAnsiChar(name));
		if flNoCollision in selected.editor_flags then
			IupSetInt(t_collision, 'VALUE', 0)
		else
			IupSetInt(t_collision, 'VALUE', 1);

		IupSetInt(list_surfaces, 'VALUE', surfaces.IndexOf(selected)+1);
	end
	else
	begin
		IupSetAttribute(fr_material, 'VISIBLE', 'NO');
		IupSetInt(list_surfaces, 'VALUE', 0);
	end;
end;

procedure LoadPhysics(model : T4AModelHierrarhy);
var
	pActor : TPHActor;
	pShapeDesc : Pointer;

	I : Longint;
	
	ph : TList;
begin
	ph := TList.Create;
	MakePhysics(ph, model, True); 

	for I := 0 to Length(model.meshes) - 1 do
	begin
		if ph[I] <> nil then
		begin
			pShapeDesc := PHShapeTrimesh(ph[I]);
			if pShapeDesc = nil then
				raise Exception.Create('PHShapeTrimesh failed');
					
			pActor := PHCreateActor(ph_scene, 0, 1, @pShapeDesc, nil);
			if pActor = nil then
				raise Exception.Create('PHCreateActor failed');
	
			PHSetUserdata(pActor, Pointer(model.meshes[I]));
		end;
	end;
	
	ph.Free;
end;

procedure LoadPhysicsSkinned(model : T4AModelSkinned);
var
	pActor : TPHActor;
	pShapeDesc : Pointer;

	I : Longint;
	
	ph : TList;
begin
	ph := TList.Create;
	MakePhysicsSkinned(ph, model, True); 

	for I := 0 to Length(model.meshes) - 1 do
	begin
		if ph[I] <> nil then
		begin
			pShapeDesc := PHShapeTrimesh(ph[I]);
			if pShapeDesc = nil then
				raise Exception.Create('PHShapeTrimesh failed');
					
			pActor := PHCreateActor(ph_scene, 0, 1, @pShapeDesc, nil);
			if pActor = nil then
				raise Exception.Create('PHCreateActor failed');
	
			PHSetUserdata(pActor, Pointer(model.meshes[I]));
		end;
	end;
	
	ph.Free;
end;

procedure LoadPhysicsSkeleton(model : T4AModelSkeleton);
var
	pActor : TPHActor;
	pShapeDesc : Pointer;
	ph : TList;

	I, J, K : Integer;
begin
	ph := TList.Create;
	MakePhysicsSkeleton(ph, model, True); 
	
	K := 0;
	for I := 0 to Length(model.meshes[0]) - 1 do
		for J := 0 to Length(model.meshes[0,I].meshes) - 1 do
		begin
			if ph[K] <> nil then
			begin
				pShapeDesc := PHShapeTrimesh(ph[K]);
				if pShapeDesc = nil then
					raise Exception.Create('PHShapeTrimesh failed');
				
				pActor := PHCreateActor(ph_scene, 0, 1, @pShapeDesc, nil);
				if pActor = nil then
					raise Exception.Create('PHCreateActor failed');
	
				PHSetUserdata(pActor, Pointer(model.meshes[0,I].meshes[J]));
			end;
			
			Inc(K);
		end;
		
	ph.Free;
end;

procedure LoadEditModel(mdl : T4AModel); 
var
	mh : T4AModelHierrarhy;
	ms : T4AModelSkeleton;
	mk : T4AModelSkinned;
	
	list_surfaces : Ihandle;
	
	I, J : Integer;
	name : String;
	mm : T4AModelSkinnedMesh;
begin
	edit_model := mdl;
	UpdateMaler;
	
	ph_scene := PHCreateScene;
	if ph_scene = nil then
		raise Exception.Create('PHCreateScene failed');

	surfaces := TList.Create;
	list_surfaces := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'LIST_SURFACES');

	if mdl is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(mdl);		
		LoadPhysics(mh);
		
		for I := 0 to Length(mh.meshes) - 1 do
		begin
			if mh.meshes[I].name <> '' then
				name := mh.meshes[I].name
			else
				name := mh.meshes[I].texture;
			
			surfaces.Add(Pointer(mh.meshes[I]));	
			IupSetAttribute(list_surfaces, 'APPENDITEM', PAnsiChar(name));
		end;
	end;

	if mdl is T4AModelSkeleton then
	begin
		ms := T4AModelSkeleton(mdl);
		LoadPhysicsSkeleton(ms);
		
		for I := 0 to Length(ms.meshes[0]) - 1 do
			for J := 0 to Length(ms.meshes[0,I].meshes) - 1 do
			begin
				mm := ms.meshes[0,I].meshes[J];
				if mm.name <> '' then
					name := mm.name
				else
					name := mm.texture;
					
				surfaces.Add(Pointer(mm));
				IupSetAttribute(list_surfaces, 'APPENDITEM', PAnsiChar(name));		
			end;
	end;
	
	if mdl is T4AModelSkinned then
	begin
		mk := T4AModelSkinned(mdl);		
		LoadPhysicsSkinned(mk);
		
		for I := 0 to Length(mk.meshes) - 1 do
		begin
			if mk.meshes[I].name <> '' then
				name := mk.meshes[I].name
			else
				name := mk.meshes[I].texture;
			
			surfaces.Add(Pointer(mk.meshes[I]));	
			IupSetAttribute(list_surfaces, 'APPENDITEM', PAnsiChar(name));
		end;
	end;
end;

procedure UnloadEditModel;
var
	list_surfaces : Ihandle;
begin
	SelectModel(nil);
	FreeAndNil(maler);

	PHDestroyScene(ph_scene);
	ph_scene := nil;
	
	list_surfaces := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'LIST_SURFACES');
	IupSetAttribute(list_surfaces, 'REMOVEITEM', 'ALL');
	FreeAndNil(surfaces);
	
	edit_model := nil;
end;

procedure ModelLoad(mdl : T4AModel); overload;
var
	mh : T4AModelHierrarhy;
	ms : T4AModelSkeleton;
	
	list_mtlsets : Ihandle;
	I : Longint;
begin
	model := mdl;
		
	if model.version >= MODEL_VER_ARKTIKA1 then
		texturesCompressed := True
	else
		texturesCompressed := False;
		
	list_mtlsets := IupGetDialogChild(IupGetHandle('MAINDIALOG'), 'LIST_MTLSETS');
	IupSetAttribute(list_mtlsets, 'REMOVEITEM', 'ALL');
	
	current_mtlset := -1;
	IupSetAttribute(list_mtlsets, 'APPENDITEM', '<none>');

	if model is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(model);
		mtlsets := @mh.mtlsets;
		
		LoadEditModel(mh);
	end;

	if model is T4AModelSkeleton then
	begin
		ms := T4AModelSkeleton(model);
		mtlsets := @ms.mtlsets;
		skeleton := ms.skeleton;
		
		LoadEditModel(ms);
	end; 
	
	if model is T4AModelSoftbody then
		LoadEditModel(model);
		
	if model is T4AModelSkinned then
		LoadEditModel(model);
	
	if Assigned(mtlsets) then
		for I := 0 to Length(mtlsets^) - 1 do
			IupSetAttribute(list_mtlsets, 'APPENDITEM', PAnsiChar(mtlsets^[I].name));
end;

procedure ModelLoad(fn : String); overload;
var
	r, s : TMemoryReader;
	mdl : T4AModel;
begin
	r := nil;
	mdl := nil;
	
	try
		r := TMemoryReader.CreateFromFile(fn);
		mdl := Load4AModel(r, False);

		case mdl.modeltype of
			MODEL_TYPE_HIERRARHY,
			MODEL_TYPE_SKELETON, MODEL_TYPE_ANIMATED,
			MODEL_TYPE_SKINNED,
			MODEL_TYPE_SOFTBODY:
				;
			else
				raise Exception.Create('unsupported model type');
		end;

		if mdl is T4AModelSkeleton then
		begin
			LoadMeshes(T4AModelSkeleton(mdl), fn);
			LoadSkeleton(T4AModelSkeleton(mdl));
		end;
		
		if mdl is T4AModelSoftbody then
		begin
			s := TMemoryReader.CreateFromFile(ChangeFileExt(fn, '.sftmdl_pc'));
			T4AModelSoftbody(mdl).LoadSoftbodyData(s);
			s.Free;
		end;

		r.Free;
	except
		on E: Exception do begin
			IupMessage('Error', PAnsiChar(E.ClassName + ': ' + E.Message));

			FreeAndNil(mdl);
			r.Free;

			Exit;
		end;
	end;

	ModelLoad(mdl);
end;

procedure ModelUnload;
begin
	UnloadEditModel;
	FreeAndNil(model);
	mtlsets := nil;
	skeleton := nil;	
	
	ClearResources;
end;

procedure SetModelVersion(m : T4AModel; ver : Byte);
var
	I : Integer;
	h : T4AModelHierrarhy;
	s : T4AModelSkinned;
begin
	m.version := ver;
	if m is T4AModelHierrarhy then
	begin
		h := T4AModelHierrarhy(m);
		for I := 0 to Length(h.meshes) - 1 do
			h.meshes[I].version := ver;
	end;
	if m is T4AModelSkinned then
	begin
		s := T4AModelSkinned(m);
		for I := 0 to Length(s.meshes) - 1 do
			s.meshes[I].version := ver;
	end;
end;

///////////////////////////////////////////////////////////////////////////////
// OpenGL canvas callbacks                                                   //
///////////////////////////////////////////////////////////////////////////////
function gl_map_cb(ih : Ihandle) : Longint; cdecl;
var
	mdl : T4AModel;
begin
	IupGLMakeCurrent(ih);

	glClearColor(0.4, 0.7, 0.8, 0.0);

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
	glFrontFace(GL_CW);
	glAlphaFunc(GL_GEQUAL, 0.5);

	InitializeRender;

	//mdl := ImportModel('.\test.blend');
	//ModelLoad(mdl);

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
	
	PerspectiveLH(m, 70*(PI/180), aspect, 0.25, 500.0);
	glLoadMatrixf(@m);
	
	glMatrixMode(GL_MODELVIEW);

	Result := IUP_DEFAULT;
end;

function gl_redraw_cb(ih : Ihandle; x, y : Single) : Longint; cdecl;
var
	sinx, cosx : Single;
	siny, cosy : Single;
	
	m : TMatrix;
	
	ms : T4AModelSkeleton;
	I, J : Longint;
begin
	IupGLMakeCurrent(ih);
	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

	sinx := Sin(anglex * (PI/180));
	cosx := Cos(anglex * (PI/180));

	siny := Sin(angley * (PI/180));
	cosy := Cos(angley * (PI/180));

	LookAtLH(m, distance*siny*cosx, distance*sinx, distance*cosy*cosx, 0, 0, 0, 0, 1, 0);
	glLoadMatrixf(@m);

	glGetDoublev(GL_MODELVIEW_MATRIX, @modelview_d);
	glGetDoublev(GL_PROJECTION_MATRIX, @proj_d);
	glGetFloatv(GL_MODELVIEW_MATRIX, @modelview);
	glGetFloatv(GL_PROJECTION_MATRIX, @proj);
	glGetIntegerv(GL_VIEWPORT, @viewport);

	if useWireframe then
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

	try
		if Assigned(maler) then
		begin
			maler.Draw(current_mtlset);
			maler.Draw(current_mtlset, False, True); // blended
		end;
	except
		on E: Exception do
			IupMessage('Error', PAnsiChar(E.Message));
	end;
	
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	if showTangents then
		DrawNormals(model);
	
	if showSkeleton and Assigned(skeleton) then
		DrawSkeleton(skeleton);
	
	if showBoneOBB and (model is T4AModelSkeleton) then
		DrawBoneOBB(T4AModelSkeleton(model));
	
	IupGLSwapBuffers(ih);
	Result := IUP_DEFAULT;
end;

var
	old_x, old_y : Longint;

function gl_motion_cb(ih : Ihandle; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	mousexy : Ihandle;
	pos : String;
begin
	pos := IntToStr(x) + ', ' + IntToStr(y);

	mousexy := IupGetDialogChild(ih, 'MOUSEXY');
	IupSetStrAttribute(mousexy, 'TITLE', PAnsiChar(pos));

	if iup_isbutton3(status) then
	begin
		anglex := anglex + (y - old_y);
		angley := angley + (x - old_x);

		if anglex >= 90.0 then
			anglex := 90.0;
		if anglex <= -90.0 then
			anglex := -90.0;

		IupRedraw(ih, 0);
	end else
	if iup_isbutton2(status) then
	begin
		distance := distance + ((y - old_y) / 8);
		IupRedraw(ih, 0);
	end;

	old_x := x;
	old_y := y;

	Result := IUP_DEFAULT;
end;

function gl_button_cb(ih : Ihandle; button, pressed : Longint; x, y : Longint; status : PAnsiChar) : Longint; cdecl;
var
	p1, p2 : array[1..3] of GLdouble;
	actor : TPHActor;
	shape : TPHShape;
	sel : Pointer;

	p, dir : TVec3;
begin
	if (ph_scene <> nil) and (button = Ord('1')) and (pressed = 0) then
	begin
		// in OpenGL origin of window coordinates is a left-bottom corner
		gluUnProject(x, viewport[3]-y, 0.1, @modelview_d, @proj_d, @viewport, @p1[1], @p1[2], @p1[3]);
		gluUnProject(x, viewport[3]-y, 0.9, @modelview_d, @proj_d, @viewport, @p2[1], @p2[2], @p2[3]);

		p.x := p1[1];
		p.y := p1[2];
		p.z := p1[3];

		dir.x := p2[1]-p1[1];
		dir.y := p2[2]-p1[2];
		dir.z := p2[3]-p1[3];

		Normalize(dir);

		shape := PHRaycastClosestShape(ph_scene, @p, @dir, 500.0);

		if shape <> nil then
		begin
			actor := PHGetActor(shape);
			sel := PHGetUserdata(actor);
			if sel <> nil then
			begin
				//IupMessage('PhysX', 'Touch something');
				SelectModel(sel);
			end;
		end else
			SelectModel(nil);

		IupRefresh(ih);
		IupRedraw(ih, 0);
	end;

	Result := IUP_DEFAULT;
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

function list_lod_cb(ih : Ihandle; txt : PAnsiChar; item, state : Longint) : Longint; cdecl;
var
	mh : T4AModelHierrarhy;
	enable : Longint;
begin
	if item = 1 then	enable := 0
	else							enable := 1;

	IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_LOAD'), 'ACTIVE', enable);
	IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_SAVE'), 'ACTIVE', enable);
	IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_CLEAR'), 'ACTIVE', enable);

	if model is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(model);
		UnloadEditModel;
		case item of
		 1: LoadEditModel(mh);
		 2: if mh.lod1 <> nil then LoadEditModel(mh.lod1);
		 3: if mh.lod0 <> nil then LoadEditModel(mh.lod0);
		end
	end;

	Redisplay;
	Result := IUP_DEFAULT;
end;

function btn_lod_load_cb(ih : Ihandle) : Longint; cdecl;
var
	mh : T4AModelHierrarhy;
	mdl : ^T4AModel;
	lod : Longint;
	
	fn : String;
	r : TMemoryReader;
	mdl_lod : T4AModelHierrarhy;
begin
	if model is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(model);
	
		lod := IupGetInt(IupGetDialogChild(ih, 'LIST_LOD'), 'VALUE');
		case lod of
			2:		mdl := @mh.lod1;
			3:		mdl := @mh.lod0;
			else	mdl := nil;
		end;
		
		if Assigned(mdl) and FileDlg(False, fn) then
		begin
			r := TMemoryReader.CreateFromFile(fn);
			mdl_lod := T4AModelHierrarhy.Create;
			try
				mdl_lod.Load(r);
				
				if Assigned(mdl_lod.lod1) or Assigned(mdl_lod.lod0) then
					IupMessage('Warning', 'Using model with lods as lod itself');
				
				FreeAndNil(mdl^);
				mdl^ := mdl_lod;
			except 
				on E: Exception do
				begin
					IupMessageError(IupGetHandle('MAINDIALOG'), 
					PAnsiChar(E.ClassName + ': ' + E.Message));
					
					mdl_lod.Free;
				end;
			end;
			r.Free;
			
			UpdateMaler;
			Redisplay;
		end;
	end;

	Result := IUP_DEFAULT;
end;

function btn_lod_save_cb(ih : Ihandle) : Longint; cdecl;
var
	mh : T4AModelHierrarhy;
	mdl : T4AModel;
	lod : Longint;
	
	fn : String;
	w : TMemoryWriter;
begin
	if model is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(model);
	
		lod := IupGetInt(IupGetDialogChild(ih, 'LIST_LOD'), 'VALUE');
		case lod of
			2:		mdl := mh.lod1;
			3:		mdl := mh.lod0;
			else	mdl := nil;
		end;
		
		if Assigned(mdl) and FileDlg(True, fn) then
		begin
			w := TMemoryWriter.Create;
			try
				mdl.Save(w);
				w.SaveTo(fn);
			except on E: Exception do
				IupMessageError(IupGetHandle('MAINDIALOG'), PAnsiChar(E.ClassName + ': ' + E.Message));
			end;
			w.Free;
		end;
	end;

	Result := IUP_DEFAULT;
end;

function btn_lod_clear_cb(ih : Ihandle) : Longint; cdecl;
var
	mh : T4AModelHierrarhy;
	lod : Longint;
begin
	if model is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(model);
	
		lod := IupGetInt(IupGetDialogChild(ih, 'LIST_LOD'), 'VALUE');
		case lod of
			2:		FreeAndNil(mh.lod1);
			3:		FreeAndNil(mh.lod0);
		end;
		
		UpdateMaler;
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;
{
procedure HemiYP(var dir : TVec3);
var
	angle, r : Single;
begin
	dir.z := Cos(PI*Random);
	angle := PI*Random;
	r := Sqrt(1-dir.z*dir.z);
	dir.x := Cos(angle) * r;
	dir.y := Sin(angle) * r;
end;

function btn_hemi_yp_cb(ih : Ihandle) : Longint; cdecl;
var
	samples : Ihandle;
	smooth : Ihandle;
begin
	samples := IupGetDialogChild(ih, 'TEXT_SAMPLES');
	smooth := IupGetDialogChild(ih, 'TEXT_SMOOTH');

	if edit_model is T4AModelHierrarhy then
	begin
		CalculateAO(T4AModelHierrarhy(edit_model), HemiYP,
			IupGetInt(samples, 'VALUE'),
			IupGetInt(smooth, 'VALUE'));
			
		UpdateMaler;
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;

procedure FullSphere(var dir : TVec3);
var
	angle, r : Single;
begin
	dir.z := Cos(PI*Random);
	angle := PI*2*Random;
	r := Sqrt(1-dir.z*dir.z);
	dir.x := Cos(angle) * r;
	dir.y := Sin(angle) * r;
end;

function btn_full_cb(ih : Ihandle) : Longint; cdecl;
var
	samples : Ihandle;
	smooth : Ihandle;
begin
	samples := IupGetDialogChild(ih, 'TEXT_SAMPLES');
	smooth := IupGetDialogChild(ih, 'TEXT_SMOOTH');

	if edit_model is T4AModelHierrarhy then
	begin
		CalculateAO(T4AModelHierrarhy(edit_model), FullSphere,
			IupGetInt(samples, 'VALUE'),
			IupGetInt(smooth, 'VALUE'));
			
		UpdateMaler;
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;
}
function list_surfaces_select_cb(ih : Ihandle; title : PAnsiChar; item, state : Longint) : Longint; cdecl;
begin
	if state = 1 then
	begin
		SelectModel(T4AModel(surfaces[item-1]));
		Redisplay;
	end;
		
	Result := IUP_DEFAULT;
end;

function list_mtlsets_select_cb(ih : Ihandle; title : PAnsiChar; item, state : Longint) : Longint; cdecl;
var
	list : Ihandle;
	I : Longint;
begin
	if state = 1 then
	begin
		if item = 1 then
			current_mtlset := -1
		else
			current_mtlset := item-2;
			
		Redisplay;
	end;
	
	list := IupGetDialogChild(ih, 'LIST_MTLSET_SURFACES');
	IupSetAttribute(list, 'REMOVEITEM', 'ALL');
	if current_mtlset <> -1 then
		for I := 0 to Length(mtlsets^[current_mtlset].materials) - 1 do
			IupSetAttribute(list, 'APPENDITEM', PAnsiChar(mtlsets^[current_mtlset].materials[I].surface));
		
		
	Result := IUP_DEFAULT;
end;

function text_texture_changed_cb(ih : Ihandle) : Longint; cdecl;
begin
	if selected is T4AModelSimple then
	begin
		T4AModelSimple(selected).texture := IupGetAttribute(ih, 'VALUE');
		UpdateMaler;
		Redisplay;
	end;
	if selected is T4AModelSkinnedMesh then
	begin
		T4AModelSkinnedMesh(selected).texture := IupGetAttribute(ih, 'VALUE');
		UpdateMaler;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function text_shader_changed_cb(ih : Ihandle) : Longint; cdecl;
begin
	if selected is T4AModelSimple then
	begin
		T4AModelSimple(selected).shader := IupGetAttribute(ih, 'VALUE');
		UpdateMaler;
		Redisplay;
	end;
	if selected is T4AModelSkinnedMesh then
	begin
		T4AModelSkinnedMesh(selected).shader := IupGetAttribute(ih, 'VALUE');
		UpdateMaler;
		Redisplay;
	end;
	
	Result := IUP_DEFAULT;
end;

function text_material_changed_cb(ih : Ihandle) : Longint; cdecl;
begin
	if selected is T4AModelSimple then
		T4AModelSimple(selected).material := IupGetAttribute(ih, 'VALUE');
	if selected is T4AModelSkinnedMesh then
		T4AModelSkinnedMesh(selected).material := IupGetAttribute(ih, 'VALUE');
	Result := IUP_DEFAULT;
end;

function text_name_changed_cb(ih : Ihandle) : Longint; cdecl;
begin
	if selected is T4AModelSimple then
		T4AModelSimple(selected).name := IupGetAttribute(ih, 'VALUE');
	if selected is T4AModelSkinnedMesh then
		T4AModelSkinnedMesh(selected).name := IupGetAttribute(ih, 'VALUE');
	Result := IUP_DEFAULT;
end;

function toggle_collision_cb(ih : Ihandle; state : Longint) : Longint; cdecl;
begin
	if state = 0 then
		Include(selected.editor_flags, flNoCollision)
	else
		Exclude(selected.editor_flags, flNoCollision);
		
	Result := IUP_DEFAULT;
end;

function btn_shader_cb(ih : Ihandle) : Longint; cdecl;
var
	pShader : PString;
begin
	if selected is T4AModelSimple then
		pShader := @T4AModelSimple(selected).shader
	else
	if selected is T4AModelSkinnedMesh then
		pShader := @T4AModelSkinnedMesh(selected).shader
	else
		pShader := nil;
		
	if Assigned(pShader) then
	begin
		if ChooseShader(pShader^) then
		begin
			SelectModel(selected); // update selection
			UpdateMaler;
			Redisplay;
		end;
	end;
		
	Result := IUP_DEFAULT;
end;

function btn_texture_cb(ih : Ihandle) : Longint; cdecl;
var
	pTexture : PString;
begin
	if selected is T4AModelSimple then
		pTexture := @T4AModelSimple(selected).texture
	else
	if selected is T4AModelSkinnedMesh then
		pTexture := @T4AModelSkinnedMesh(selected).texture
	else
		pTexture := nil;
		
	if Assigned(pTexture) then
	begin
		if ChooseTexture(pTexture^) then
		begin
			SelectModel(selected); // update selection
			UpdateMaler;
			Redisplay;
		end;
	end;
		
	Result := IUP_DEFAULT;
end;

function btn_material_cb(ih : Ihandle) : Longint; cdecl;
var
	pMaterial : PString;
begin
	if selected is T4AModelSimple then
		pMaterial := @T4AModelSimple(selected).material
	else
	if selected is T4AModelSkinnedMesh then
		pMaterial := @T4AModelSkinnedMesh(selected).material
	else
		pMaterial := nil;
		
	if Assigned(pMaterial) then
	begin
		if ChooseMaterial(pMaterial^) then
		begin
			SelectModel(selected); // update selection
		end;
	end;
		
	Result := IUP_DEFAULT;
end;

///////////////////////////////////////////////////////////////////////////////
// Menu callbacks                                                            //
///////////////////////////////////////////////////////////////////////////////
function menu_file_open_model_cb(ih : Ihandle) : Longint; cdecl;
var
	fn : String;
begin
	if FileDlg(False, fn, '.model') then
	begin
		ModelUnload;
		ModelLoad(fn);
		
		// reset lod
		IupSetInt(IupGetDialogChild(ih, 'LIST_LOD'), 'VALUE', 1); 
		IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_LOAD'), 'ACTIVE', 0); 
		IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_SAVE'), 'ACTIVE', 0);
		IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_CLEAR'), 'ACTIVE', 0); 
		//
		
		IupSetStrAttribute(IupGetDialog(ih), 'TITLE', PAnsiChar('Model Editor - [' + fn + ']'));
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;

function menu_file_open_mesh_cb(ih : Ihandle) : Longint; cdecl;
var
	fn : String;
begin
	if FileDlg(False, fn, '.mesh') then
	begin
		ModelUnload;
		ModelLoad(fn);
		
		// reset lod
		IupSetInt(IupGetDialogChild(ih, 'LIST_LOD'), 'VALUE', 1); 
		IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_LOAD'), 'ACTIVE', 0); 
		IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_SAVE'), 'ACTIVE', 0);
		IupSetInt(IupGetDialogChild(ih, 'BTN_LOD_CLEAR'), 'ACTIVE', 0); 
		//
		
		IupSetStrAttribute(IupGetDialog(ih), 'TITLE', PAnsiChar('Model Editor - [' + fn + ']'));
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;

function menu_file_save_cb(ih : Ihandle) : Longint; cdecl;
var
	ext : String;
	fn : String;
begin
	if Assigned(model) then
	begin
		if model is T4AModelSkinned then
			ext := '.mesh'
		else
			ext := '.model';
	
		if FileDlg(True, fn, ext) then
		begin
			try
				//SetModelVersion(model, MODEL_VER_2033); // :-)
				model.SaveToFile(fn);
			except
				on E: Exception do
					IupMessage('Error', PAnsiChar(E.ClassName + ': ' + E.Message));
			end;
		end;
	end else
		IupMessage('Message', 'Nothing opened');

	Result := IUP_DEFAULT;
end;

function menu_file_save_cform_cb(ih : Ihandle) : Longint; cdecl;
var
	w : TMemoryWriter;
	cf : TNxCform;
	dlg : Ihandle;
	I : Longint;
	mh : T4AModelHierrarhy;
	ext : String;
begin
	if Assigned(model) and (model is T4AModelHierrarhy) then
	begin
		if model.version >= MODEL_VER_REDUX then
			ext := '.nxcform33x'
		else
		if model.version >= MODEL_VER_LL then
			ext := '.nxcform_xbox'
		else
			ext := '.nxcform_pc';
	
		dlg := IupFileDlg;
		IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
		IupSetAttribute(dlg, 'EXTDEFAULT', PAnsiChar(ext));
		IupSetAttribute(dlg, 'EXTFILTER', PAnsiChar('Cform file (*' + ext + ')|*' + ext + '|All files (*.*)|*.*|'));

		IupPopup(dlg, IUP_CENTER, IUP_CENTER);

		if IupGetInt(dlg, 'STATUS') <> -1 then
		begin
			mh := T4AModelHierrarhy(model);
			
			w := TMemoryWriter.Create;
			cf := TNxCform.Create(False);
			
			for I := 0 to Length(mh.meshes)-1 do
				if not (flNoCollision in mh.meshes[I].editor_flags) then
					cf.Add4AModel(mh.meshes[I]);
			
			try
				if model.version >= MODEL_VER_REDUX then
					raise Exception.Create('not implemented')
				else
				if model.version >= MODEL_VER_LL then
					cf.SaveLL(w)
				else
					cf.Save(w);
				
				w.SaveTo(IupGetAttribute(dlg, 'VALUE'));
			except
				on E: Exception do
					IupMessage('Error', PAnsiChar(E.ClassName + ': ' + E.Message));
			end;
			
			cf.Free;
			w.Free;
		end;

		IupDestroy(dlg);
	end else
		IupMessage('Message', 'Please open static model!');

	Result := IUP_DEFAULT;
end;

function menu_file_import_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
	
	mdl : T4AModel;
begin
	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		ModelUnload;
		
		try
			mdl := ImportModelStatic(fn);
		except
			on E: Exception do
			begin
				IupMessageError(IupGetDialog(ih), PAnsiChar(E.Message));
				mdl := nil;
			end;
		end;
		
		if mdl <> nil then
		begin
			ModelLoad(mdl);
			IupSetStrAttribute(IupGetDialog(ih), 'TITLE', PAnsiChar('Model Editor - [' + fn + ']'));
		end else
			IupSetStrAttribute(IupGetDialog(ih), 'TITLE', 'Model Editor');
			
		Redisplay;
	end;

	IupDestroy(dlg);

	Result := IUP_DEFAULT;
end;

function menu_file_import_dynamic_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn, mesh_fn : String;
	
	mdl : T4AModelSkinned;
begin
	Result := IUP_DEFAULT;
	
	if skeleton = nil then
	begin
		IupMessageError(IupGetDialog(ih), 'Open model with skeleton first!');
		Exit;
	end;

	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		
		try
			mdl := ImportModelDynamic(fn, skeleton);
			SetModelVersion(mdl, model.version);
			mdl.skeleton_crc := skeleton.crc;
		except
			on E: Exception do
			begin
				IupMessageError(IupGetDialog(ih), PAnsiChar(E.Message));
				mdl := nil;
			end;
		end;
		
		if mdl <> nil then
		begin
			if FileDlg(True, mesh_fn, '.mesh') then
				mdl.SaveToFile(mesh_fn);
		
			mdl.Free;
		end;
	end;

	IupDestroy(dlg);
end;

function menu_file_import_motion_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn, mesh_fn : String;
	
	w : TMemoryWriter;
begin
	Result := IUP_DEFAULT;
	
	if skeleton = nil then
	begin
		IupMessageError(IupGetDialog(ih), 'Open model with skeleton first!');
		Exit;
	end;

	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'OPEN');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') = 0 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		
		try
			w := ImportMotion(fn, skeleton);
		except
			on E: Exception do
			begin
				IupMessageError(IupGetDialog(ih), PAnsiChar(E.Message));
				w := nil;
			end;
		end;
		
		if w <> nil then
		begin
			if FileDlg(True, mesh_fn, '.motion', '4A Motion') then
				w.SaveTo(mesh_fn);
		
			w.Free;
		end;
	end;

	IupDestroy(dlg);
end;

function menu_file_export_cb(ih : Ihandle) : Longint; cdecl;
var
	dlg : Ihandle;
	fn : String;
	
	scene : TExportScene;
	model_id : Longint;
	matrix : TMatrix;
	
	ext : String[16];
begin
	dlg := IupFileDlg;
	IupSetAttribute(dlg, 'DIALOGTYPE', 'SAVE');
	IupSetAttribute(dlg, 'EXTFILTER', 'FBX|*.fbx|3DS|*.3ds|OBJ|*.obj|');

	IupPopup(dlg, IUP_CENTER, IUP_CENTER);

	if IupGetInt(dlg, 'STATUS') <> -1 then
	begin
		fn := IupGetAttribute(dlg, 'VALUE');
		
		scene := TExportScene.Create;
		Identity(matrix);
		
		if model is T4AModelSkeleton then
		begin		
			model_id := scene.AddModel('model', T4AModelSkeleton(model), current_mtlset, True);
			scene.AddObject('Object', matrix, model_id);
			
			scene.AddSkeleton(skeleton);
		end;
		
		if model is T4AModelHierrarhy then
		begin
			model_id := scene.AddModel('model', T4AModelHierrarhy(model), current_mtlset);
			scene.AddObject('Object', matrix, model_id);
		end;
		
		if scene.objects.Count > 0 then
		begin
			case IupGetInt(dlg, 'FILTERUSED') of
				1: ext := 'fbx';
				2: ext := '3ds';
				3: ext := 'obj';
			end;
			
			if ExtractFileExt(fn) = '' then
				fn := fn + '.' + ext;
			
			scene.DoExport(ext, fn);
		end;
		
		scene.Free;
	end;

	IupDestroy(dlg);

	Result := IUP_DEFAULT;
end;

function menu_file_exit_cb(ih : Ihandle) : Longint; cdecl;
begin
	Result := IUP_CLOSE;
end;

procedure DoShow(var b : Boolean; ih : Ihandle);
begin
	b := not b;
	IupSetInt(ih, 'VALUE', Integer(b));
	Redisplay;
end;

function menu_show_tangents_cb(ih : Ihandle) : Longint; cdecl;
begin
	DoShow(showTangents, ih);
	Result := IUP_DEFAULT;
end;

function menu_show_skeleton_cb(ih : Ihandle) : Longint; cdecl;
begin
	DoShow(showSkeleton, ih);
	Result := IUP_DEFAULT;
end;

function menu_show_bone_obb_cb(ih : Ihandle) : Longint; cdecl;
begin
	DoShow(showBoneOBB, ih);
	Result := IUP_DEFAULT;
end;

function menu_model_setversion_cb(ih : Ihandle) : Longint; cdecl;
var
	vers : array [0..4] of PAnsiChar;
	ret : Longint;
begin
	vers[0] := 'Metro 2033';
	vers[1] := 'Metro Last Light';
	vers[2] := 'Metro Redux';
	vers[3] := 'Arktika.1';
	vers[4] := 'Metro Exodus';

	ret := IupListDialog(1, 'Set version', 5, @vers, 1, 20, 10, nil);
	if (model <> nil) and (ret <> -1) then
	begin
		case ret of
			0:	SetModelVersion(model, MODEL_VER_2033);
			1:	SetModelVersion(model, MODEL_VER_LL);
			2:	SetModelVersion(model, MODEL_VER_REDUX);
			3:	SetModelVersion(model, MODEL_VER_ARKTIKA1);
			4:	SetModelVersion(model, MODEL_VER_EXODUS);
		end;
	end;
	
	Result := IUP_DEFAULT
end;

function menu_render_wireframe_cb(ih : Ihandle) : Longint; cdecl;
begin
	useWireframe := not useWireframe;
	IupSetInt(ih, 'VALUE', Integer(useWireframe));
	Redisplay;
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
begin
	glGetFloatv(GL_COLOR_CLEAR_VALUE, @clr);
	if SelectColor(clr) then
	begin
		glClearColor(clr.x, clr.y, clr.z, 1.0);
		Redisplay;
	end;

	Result := IUP_DEFAULT;
end;

///////////////////////////////////////////////////////////////////////////////
// Dialog & menu creation                                                    //
///////////////////////////////////////////////////////////////////////////////
procedure CreateDialog;
var
	gl : Ihandle;
	toolbox : Ihandle;

	lod_box, list_lod, btn_lod_load, btn_lod_save, btn_lod_clear : Ihandle;

	list_rm : Ihandle;
	box_rm : Ihandle;
	
	mousexy : Ihandle;
	samples : Ihandle;
	smooth : Ihandle;
	btn_hemi_yp, btn_full : Ihandle;
	fr_ao : Ihandle;

	list_surfaces : Ihandle;
	fr_surfaces : Ihandle;

	fr_mtlsets : Ihandle;
	list_mtlsets : Ihandle;
	list_mtlset_surfaces : Ihandle;

	texture, shader, material, name, collision : Ihandle;
	btn_texture, btn_shader, btn_material : Ihandle;
	box_texture, box_shader, box_material : Ihandle;
	
	fr_material : Ihandle;

	menu : Ihandle;
	sm_file, sm_model, sm_render : Ihandle;

	dlg : Ihandle;
begin
	gl := IupGLCanvas(nil);
	IupSetAttribute(gl, 'NAME', 'GL_CANVAS');
	IupSetAttribute(gl, 'BUFFER', 'DOUBLE');
	IupSetInt(gl, 'DEPTH_SIZE', 32);
	//IupSetAttribute(gl, 'EXPAND', 'YES');
	IupSetAttribute(gl, 'RASTERSIZE', '800x600');
	IupSetCallback(gl, 'ACTION', @gl_redraw_cb);
	IupSetCallback(gl, 'MAP_CB', @gl_map_cb);
	IupSetCallback(gl, 'RESIZE_CB', @gl_resize_cb);
	IupSetCallback(gl, 'MOTION_CB', @gl_motion_cb);
	IupSetCallback(gl, 'BUTTON_CB', @gl_button_cb);

	list_lod := IupList(nil);
	IupSetAttributes(list_lod, 'NAME=LIST_LOD, DROPDOWN=YES, 1="LOD 2", 2="LOD 1", 3="LOD 0"');
	IupSetAttribute(list_lod, 'VALUE', '1');
	IupSetCallback(list_lod, 'ACTION', @list_lod_cb);
	
	btn_lod_load := iup.Button('Load', @btn_lod_load_cb);
	IupSetAttributes(btn_lod_load, 'NAME=BTN_LOD_LOAD, ACTIVE=NO');

	btn_lod_save := iup.Button('Save', @btn_lod_save_cb);
	IupSetAttributes(btn_lod_save, 'NAME=BTN_LOD_SAVE, ACTIVE=NO');
	
	btn_lod_clear := iup.Button('Clear', @btn_lod_clear_cb);
	IupSetAttributes(btn_lod_clear, 'NAME=BTN_LOD_CLEAR, ACTIVE=NO');

	lod_box := IupHBox(list_lod, btn_lod_load, btn_lod_save, btn_lod_clear, nil);
	IupSetAttribute(lod_box, 'MARGIN', '0x0');
	
	list_rm := IupList(nil);
	IupSetAttributes(list_rm, 'DROPDOWN=YES, 1="Default", 2="AO"');
	IupSetAttribute(list_rm, 'VALUE', '1');
	IupSetCallback(list_rm, 'ACTION', @list_rm_cb);
	
	box_rm := IupVBox(
		IupLabel('Render Mode'),
		list_rm, 
		nil
	);
	IupSetAttribute(box_rm, 'MARGIN', '0x0');

	mousexy := IupLabel('0, 0');
	IupSetAttribute(mousexy, 'NAME', 'MOUSEXY');
	IupSetAttribute(mousexy, 'RASTERSIZE', '100x');
{
	samples := IupText(nil);
	IupSetAttribute(samples, 'NAME', 'TEXT_SAMPLES');
	IupSetAttribute(samples, 'MASK', IUP_MASK_UINT);
	IupSetAttribute(samples, 'TIP', 'Samples');
	IupSetInt(samples, 'VALUE', 100);

	smooth := IupText(nil);
	IupSetAttribute(smooth, 'NAME', 'TEXT_SMOOTH');
	IupSetAttribute(smooth, 'MASK', IUP_MASK_UINT);
	IupSetAttribute(smooth, 'TIP', 'Smooth');
	IupSetInt(smooth, 'VALUE', 5);

	btn_hemi_yp := iup.Button('Hemisphere Y+', @btn_hemi_yp_cb);
	btn_full := iup.Button('Full Sphere', @btn_full_cb);

	fr_ao := IupFrame(
		IupVbox(
			samples,
			smooth,
			btn_hemi_yp,
			btn_full, nil)
	);

	IupSetAttribute(fr_ao, 'TITLE', 'Ambient Occlusion');
	IupSetAttribute(fr_ao, 'RASTERSIZE', '240x');
}
	list_surfaces := IupList(nil);
	IupSetAttribute(list_surfaces, 'NAME', 'LIST_SURFACES');
	IupSetAttribute(list_surfaces, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(list_surfaces, 'VISIBLELINES', '10');
	IupSetCallback(list_surfaces, 'ACTION', @list_surfaces_select_cb);
	
	fr_surfaces := IupFrame(IupVBox(list_surfaces, nil));
	IupSetAttribute(fr_surfaces, 'NAME', 'FRAME_SURFACES');
	IupSetAttribute(fr_surfaces, 'TITLE', 'Surfaces'); 
	
	//- mtlsets
	list_mtlsets := IupList(nil);
	IupSetAttribute(list_mtlsets, 'NAME', 'LIST_MTLSETS');
	IupSetAttribute(list_mtlsets, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(list_mtlsets, 'DROPDOWN', 'YES');
	IupSetCallback(list_mtlsets, 'ACTION', @list_mtlsets_select_cb);
	
	list_mtlset_surfaces := IupList(nil);
	IupSetAttribute(list_mtlset_surfaces, 'NAME', 'LIST_MTLSET_SURFACES');
	IupSetAttribute(list_mtlset_surfaces, 'EXPAND', 'HORIZONTAL');
	IupSetAttribute(list_mtlset_surfaces, 'VISIBLELINES', '6');
	
	fr_mtlsets := IupFrame(IupVBox(list_mtlsets, list_mtlset_surfaces, nil));
	IupSetAttribute(fr_mtlsets, 'NAME', 'FRAME_MTLSETS');
	IupSetAttribute(fr_mtlsets, 'TITLE', 'Material sets'); 
	//- mtlsets
	
	texture := IupSetAttributes(IupText(nil), 'NAME=TEXT_TEXTURE, TIP=Texture, EXPAND=HORIZONTAL');
	shader := IupSetAttributes(IupText(nil), 'NAME=TEXT_SHADER, TIP=Shader, EXPAND=HORIZONTAL');
	material := IupSetAttributes(IupText(nil), 'NAME=TEXT_MATERIAL, TIP=Material, EXPAND=HORIZONTAL');
	name := IupSetAttributes(IupText(nil), 'NAME=TEXT_NAME, TIP=Name, EXPAND=HORIZONTAL');
	IupSetCallback(texture, 'VALUECHANGED_CB', @text_texture_changed_cb);
	IupSetCallback(shader, 'VALUECHANGED_CB', @text_shader_changed_cb);
	IupSetCallback(material, 'VALUECHANGED_CB', @text_material_changed_cb);
	IupSetCallback(name, 'VALUECHANGED_CB', @text_name_changed_cb);
	
	btn_texture := iup.Button('Choose', @btn_texture_cb);
	btn_shader := iup.Button('Choose', @btn_shader_cb);
	btn_material := iup.Button('Choose', @btn_material_cb);
	
	box_shader := IupHBox(shader, btn_shader, nil);
	IupSetAttribute(box_shader, 'MARGIN', '0x0');
	
	box_texture := IupHBox(texture, btn_texture, nil);
	IupSetAttribute(box_texture, 'MARGIN', '0x0');
	
	box_material := IupHBox(material, btn_material, nil);
	IupSetAttribute(box_material, 'MARGIN', '0x0');
	
	collision := iup.Toggle('Collision', @toggle_collision_cb);
	IupSetAttribute(collision, 'NAME', 'TOGGLE_COLLISION');
	IupSetAttribute(collision, 'VALUE', '1');

	fr_material := IupFrame(IupVBox(box_texture, box_shader, box_material, name, collision, nil));
	IupSetAttribute(fr_material, 'NAME', 'FRAME_MATERIAL');
	IupSetAttribute(fr_material, 'VISIBLE', 'NO');
	IupSetAttribute(fr_material, 'TITLE', 'Material');

	toolbox := IupVBox(box_rm, mousexy, lod_box, {fr_ao,} fr_surfaces, fr_mtlsets, fr_material, nil);
	IupSetAttribute(toolbox, 'MARGIN', '10x10');
	IupSetAttribute(toolbox, 'GAP', '5x5');

	sm_file := IupSubmenu('File', 
		IupMenu(
			iup.MenuItem('Open .model...', @menu_file_open_model_cb),
			iup.MenuItem('Open .mesh...', @menu_file_open_mesh_cb), 
			iup.MenuItem('Save as...', @menu_file_save_cb), 
			iup.MenuItem('Save NxCform...', @menu_file_save_cform_cb), 
			IupSeparator, 
			iup.MenuItem('Import...', @menu_file_import_cb),
			iup.MenuItem('Import dynamic...', @menu_file_import_dynamic_cb),
			iup.MenuItem('Import motion...', @menu_file_import_Motion_cb),
			iup.MenuItem('Export...', @menu_file_export_cb),
			IupSeparator, 
			iup.MenuItem('Exit', @menu_file_exit_cb), 
			nil
		)
	);
	
	sm_model := IupSubmenu('Model', 
		IupMenu(
			iup.MenuItem('Show Tangent-basis', @menu_show_tangents_cb, showTangents),
			iup.MenuItem('Show skeleton', @menu_show_skeleton_cb, showSkeleton),
			iup.MenuItem('Show bone OBB', @menu_show_bone_obb_cb, showBoneOBB),
			IupSeparator, 
			iup.MenuItem('Set version...', @menu_model_setversion_cb), 
			nil
		)
	);
	
	sm_render := IupSubmenu('Render', 
		IupMenu(
			iup.MenuItem('Wireframe', @menu_render_wireframe_cb, useWireframe), 
			IupSeparator, 
			iup.MenuItem('Textures', @menu_render_textures_cb, useTextures), 
			iup.MenuItem('Bump', @menu_render_bump_cb, useBump), 
			iup.MenuItem('Detail', @menu_render_detail_cb, useDetail), 
			iup.MenuItem('Set background color...', @menu_render_setbkcolor), 
			nil
		)
	);

	menu := IupMenu(
		sm_file,
		sm_model,
		sm_render,
		nil
	);

	dlg := IupDialog(IupSplit(toolbox, gl));
	IupSetAttribute(dlg, 'TITLE', 'Model Editor');
	IupSetAttributeHandle(dlg, 'MENU', menu);
	IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
	
	IupSetHandle('MAINDIALOG', dlg);
end;

begin
	IupOpen(nil, nil);
	IupGLCanvasOpen;
	
	PHInitialize;

	if ParamCount > 0 then
		ResourcesPath := ParamStr(1)
	else
	begin
		GetDir(0, ResourcesPath);
		ResourcesPath := ResourcesPath + '\content';
	end;

	InitializeEngine;
	textureQuality := 2048;

	CreateDialog;
	IupMainLoop();

	ModelUnload;
	
	FinalizeEngine;
	PHFinalize;

	IupClose;
end.
