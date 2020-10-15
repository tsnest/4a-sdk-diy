unit uScene;

interface
uses common, fouramdl, classes, Konfig, PhysX,
	uEntity, uEnvZone;

type
	TSceneVersion = (
		sceneVerNone,
		sceneVer2033,
		sceneVerLL,
		sceneVerRedux,
		sceneVerArktika1,
		sceneVerExodus
	);

type
	TScene = class
	  level_dir : String;
		level : T4ALevel;
		level2 : T4ALevel2;
		maler : ILevelMaler;
		
		egeoms : T4AModelHierrarhy;
		egeoms_maler : TStaticModelMaler;
		
		decals : T4AModelHierrarhy;
		decals_maler : TStaticModelMaler;
		
		konf : TTextKonfig; // level.bin
		konf_entities : TSection;
		
		konf_add : TTextKonfig; // level.add.bin
		konf_add_entities : TSection;
		
		entities : TList;
		env_zones : TList;
		
		showShapes : Boolean;
		showFlags : Boolean;
		showEnvZones : Boolean;
		showDecals : Boolean;
		showEGeoms : Boolean;
		
		ph_scene : TPHScene;
		ph_meshes : array of TPHTriMesh;
		ph_superstatic : TPHActor;
		
		visible : TList;
		
		constructor Create;
		destructor Destroy; override;
		
		procedure LevelLoad(const dir : String);
		procedure LevelUnload;
		procedure LevelReload;
		
		procedure LoadEntities(k_level, k_level_add : TTextKonfig; progress : Boolean = False);
		procedure UnloadEntities;
		
		procedure RenderPrepare;
		procedure RenderOpaque;
		procedure RenderOpaqueFast;
		procedure RenderBlended;
		procedure RenderDistort;
		
		procedure RenderEnvZones;
		
		procedure AddEntity(e : TEntity);
		procedure RemoveEntity(e : TEntity);
		
		function EntityById(id : Word) : TEntity;
		function GenerateId : Word;
		
		function EntityByName(const name : String) : TEntity;
		
		procedure MakeAddon(arr : TEntityArray; addon : Boolean);
		
		function GetSelectedList : TList;
		function GetSelected : TEntityArray;
		
		function GetSelectedEZList : TList;
		function GetSelectedEZ : TEnvZoneArray;
		
		function GetVersion : TSceneVersion;
		
		procedure UpdateAttaches;
		
		
		procedure LoadLevelCform(const dir : String);
		procedure UnloadLevelCform;
		
		procedure LoadEnvironment(const fn : String);
		procedure SaveEnvironment(const fn : String);		
	end;
	
var
	Scene : TScene;

implementation
uses sysutils, GL, GLU, GLExt, Texture, levelbin, Iup, cform_utils, PHGroups, vmath, egeoms;

constructor TScene.Create;
begin
	visible := TList.Create;
end;

destructor TScene.Destroy;
begin
	visible.Free;
end;

procedure TScene.LevelLoad(const dir : String);
var
	ep : TSection;	
	k_level, k_level_add : TTextKonfig;
begin
	if not FileExists(dir + '\level.bin') then
	begin
		IupMessageError(IupGetHandle('MAINDIALOG'),
		PAnsiChar('There is no level.bin file in ''' + dir + '''!'));
		Exit;
	end;
	
	level_dir := dir;

	// load level.bin
	k_level := LoadLevelBin(dir + '\level.bin');
	
	ep := k_level.root.GetSect('entities_params', False);
	if (ep <> nil) and (ep.GetInt('version', 0, 'u16') >= ENTITY_VER_ARKTIKA1) then
		texturesCompressed := True // >= Arktika.1
	else
		texturesCompressed := False;
	
	// load level.add.bin
	if FileExists(dir + '\level.add.bin') then
		k_level_add := LoadLevelBin(dir + '\level.add.bin')
	else
	  k_level_add := nil;

	// load superstatic geometry
	if FileExists(dir + '\level') and 
		(FileExists(dir + '\level.geom_pc') or FileExists(dir + '\level.geom_xbox'))then
	begin
		level := T4ALevel.Create;
		level.Load(dir);

		maler := TLevelMaler.Create(level);
	end else
	if DirectoryExists(dir + '\static') then
	begin
		level2 := T4ALevel2.Create;
		level2.Load(dir);

		maler := TLevel2Maler.Create(level2);		
	end;
	
	// load decals
	if FileExists(dir + '\level.decals') then
	begin
		decals := LoadLevelEGeoms(dir + '\level.decals');
		if Length(decals.meshes) > 0 then
			decals_maler := TStaticModelMaler.Create(decals);
	end;
	
	// load egeoms
	if FileExists(dir + '\level.egeoms') then
	begin
		egeoms := LoadLevelEGeoms(dir + '\level.egeoms');
		if Length(egeoms.meshes) > 0 then
			egeoms_maler := TStaticModelMaler.Create(egeoms);
	end;

	// load cform
	ph_scene := PHCreateScene;
	if ph_scene = nil then
		WriteLn('PHCreateScene failed');

	try
		LoadLevelCform(dir);
	except
		on E : Exception do
			IupMessage('Error',
			PAnsiChar('Level cform loading failed. Reason:'#10 + E.ClassName + ': ' + E.Message));
	end;

	// load entities
  LoadEntities(k_level, k_level_add, True);
	
	// load level.environemt
	env_zones := TList.Create;
	
	if FileExists(dir + '\level.environment') then
		LoadEnvironment(dir + '\level.environment');
end;

procedure TScene.LevelUnload;
var
	I : Integer;
begin
  level_dir := '';
  
	FreeAndNil(maler);
	FreeAndNil(level);
	FreeAndNil(level2);
	
	FreeAndNil(decals_maler);
	FreeAndNil(decals);
	
	FreeAndNil(egeoms_maler);
	FreeAndNil(egeoms);

  UnloadEntities;
	
	if Assigned(env_zones) then
	begin
		for I := 0 to env_zones.Count - 1 do
			TEnvZone(env_zones[I]).Free;
		FreeAndNil(env_zones);
	end;

	UnloadLevelCform;
	PHDestroyScene(ph_scene);
	ph_scene := nil;
end;

procedure TScene.LevelReload;
var
  k_level, k_level_add : TTextKonfig;
begin
  if level_dir <> '' then
  begin
    // load level.bin
  	k_level := LoadLevelBin(level_dir + '\level.bin');
  	
  	// load level.add.bin
  	if FileExists(level_dir + '\level.add.bin') then
  		k_level_add := LoadLevelBin(level_dir + '\level.add.bin')
  	else
  	  k_level_add := nil;
  		
  	UnloadEntities;
  	LoadEntities(k_level, k_level_add);
	end;
end;

procedure TScene.LoadEntities(k_level, k_level_add : TTextKonfig; progress : Boolean);
var
	I : Longint;

	ent : TSection;
	ent_valid : Boolean;
	
	entity_count : Longint;
	dlg : Ihandle;
begin
  konf := k_level;
  konf_entities := konf.root.GetSect('entities');
  
  if Assigned(k_level_add) then
  begin
    konf_add := k_level_add;
    konf_add_entities := konf_add.root.GetSect('entities');
  end;
  
	entity_count := konf_entities.ParamCount;
	if konf_add_entities <> nil then
		entity_count := entity_count + konf_add_entities.ParamCount;
	
	if progress then
	begin
  	dlg := IupProgressDlg;
  	IupSetStrAttribute(dlg, 'TITLE', 'Loading entities');
  	IupSetInt(dlg, 'TOTALCOUNT', entity_count);
  	IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
  end;

	entities := TList.Create;

	for I := 0 to konf_entities.ParamCount - 1 do
	begin
		if konf_entities.GetParam(I) is TSection then
		begin
			ent := TSection(konf_entities.GetParam(I));

			IupSetAttribute(dlg, 'DESCRIPTION', PAnsiChar(ent.name));

			ent_valid :=
				(ent.GetParam('name', 'stringz') <> nil) and
				((ent.GetParam('', 'pose, matrix_43T') <> nil) or (ent.GetParam('', 'pose, matrix') <> nil));

			if ent_valid then
				entities.Add(TEntity.Create(ph_scene, ent));
			
			if progress then	
			  IupSetAttribute(dlg, 'INC', nil);
		end;
	end;

	if konf_add_entities <> nil then
	begin
		for I := 0 to konf_add_entities.ParamCount - 1 do
		begin
			if konf_add_entities.GetParam(I) is TSection then
			begin
				ent := TSection(konf_add_entities.GetParam(I));
	
				IupSetAttribute(dlg, 'DESCRIPTION', PAnsiChar(ent.name));
	
				ent_valid :=
					(ent.GetParam('name', 'stringz') <> nil) and
					((ent.GetParam('', 'pose, matrix_43T') <> nil) or (ent.GetParam('', 'pose, matrix') <> nil));
	
				if ent_valid then
					entities.Add(TEntity.Create(ph_scene, ent));
					
  			if progress then	
  			  IupSetAttribute(dlg, 'INC', nil);
			end;
		end;
	end;
	
	if progress then
	 IupDestroy(dlg);
	
	UpdateAttaches;  
end;

procedure TScene.UnloadEntities;
var
	I : Integer;
begin
	if Assigned(entities) then
	begin
		for I := 0 to entities.Count - 1 do
			TEntity(entities[I]).Free;
		FreeAndNil(entities);
	end;

	FreeAndNil(konf);
	konf_entities := nil;
	
	FreeAndNil(konf_add);
	konf_add_entities := nil;
end;

function EntitySortCb(p1, p2 : Pointer) : Integer;
var
	e1, e2 : TEntity;
begin
	e1 := TEntity(p1);
	e2 := TEntity(p2);
	
	if e1.distance_sqr > e2.distance_sqr then
		EntitySortCb := -1
	else
	if e1.distance_sqr < e2.distance_sqr then
		EntitySortCb := 1
	else
		EntitySortCb := 0;
end;

procedure TScene.RenderPrepare;
var
	I : Longint;
	E : TEntity;
	
	diff : TVec3;
	dist_sq : Single;
begin
	visible.Clear;

	if Assigned(entities) then
	begin		
		if visible.Capacity < entities.Count then
			visible.Capacity := entities.Count;
			
		for I := 0 to entities.Count-1 do
		begin
			E := TEntity(entities[I]);
			if E.Visible and AABBVisible(frustum, E.bbox) then
			begin
				diff.x := camera_pos.x-E.FMatrix[4,1];
				diff.y := camera_pos.y-E.FMatrix[4,2];
				diff.z := camera_pos.z-E.FMatrix[4,3];
				dist_sq := diff.x*diff.x + diff.y*diff.y + diff.z*diff.z;
			
				E.distance_sqr := dist_sq;
				visible.Add(E);
			end;
		end;
		
		visible.Sort(EntitySortCb);
	end;
end;

procedure TScene.RenderOpaque;
var
	I : Longint;
	E : TEntity;
begin
	try
		if Assigned(maler) then
			maler.Draw;
		if showEGeoms and Assigned(egeoms_maler) then
			egeoms_maler.Draw;
		if showDecals and Assigned(decals_maler) then
			decals_maler.Draw;
	except
		on E: Exception do
			IupMessage('Error', PAnsiChar(E.Message));
	end;
		
	for I := visible.Count-1 downto 0 do
	begin
		E := TEntity(visible[I]);
		if showFlags then
			E.DrawFlag;
		E.Draw(False, False);
	end;
end;

procedure TScene.RenderOpaqueFast;
var
	I : Longint;
	E : TEntity;
begin

	try
		if Assigned(maler) then
			maler.Draw;
		if showEGeoms and Assigned(egeoms_maler) then
			egeoms_maler.Draw;
		if showDecals and Assigned(decals_maler) then
			decals_maler.Draw;
	except
		on E: Exception do
			IupMessage('Error', PAnsiChar(E.Message));
	end;
	
	// flags
	if showFlags then
	begin
		for I := visible.Count-1 downto 0 do
		begin
			E := TEntity(visible[I]);
			E.DrawFlag;
		end;
	end;
	
	// softbody ( no shader yet )
	for I := visible.Count-1 downto 0 do
	begin
		E := TEntity(visible[I]);
		
		if Assigned(E.model) and E.model.b_softbody then
		begin
			glPushMatrix;
			glMultMatrixf(@E.FMatrix);
		
			TSoftbodyModelMaler(E.model.maler).Draw(E.mtlset, E.Selected, False, False);
			
			glPopMatrix;
		end;
	end;

	// static
	
	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	if useBump then glEnableVertexAttribArrayARB(2);
	if useBump then glEnableVertexAttribArrayARB(3);
	if useTextures then glEnableVertexAttribArrayARB(4);

{
	//testtesttest
	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	glEnableVertexAttribArrayARB(2);
	//testtesttest
}	
	glEnable(GL_VERTEX_PROGRAM_ARB);
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog[VP_STATIC]);

	for I := visible.Count-1 downto 0 do
	begin
		E := TEntity(visible[I]);
		
		if Assigned(E.model) and E.model.b_static then
		begin
			glPushMatrix;
			glMultMatrixf(@E.FMatrix);
			

		
		if not showAO then
		begin
		
			if (E.model.maler_lod0 <> nil) and (E.distance_sqr > 30*30) then
				TStaticModelMaler(E.model.maler_lod0).Draw2(E.mtlset, E.Selected, False, False)
			else if (E.model.maler_lod1 <> nil) and (E.distance_sqr > 10*10) then
				TStaticModelMaler(E.model.maler_lod1).Draw2(E.mtlset, E.Selected, False, False)
			else
				TStaticModelMaler(E.model.maler).Draw2(E.mtlset, E.Selected, False, False);
				
		end else
			TStaticModelMaler(E.model.maler).Draw2(E.mtlset, E.Selected, False, False);
			
			glPopMatrix;
		end;
	end;
{	
	//testtesttest
	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	glDisableVertexAttribArrayARB(2);
	//testtesttest
}	
	
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glDisable(GL_VERTEX_PROGRAM_ARB);

	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	if useBump then glDisableVertexAttribArrayARB(2);
	if useBump then glDisableVertexAttribArrayARB(3);
	if useTextures then glDisableVertexAttribArrayARB(4);
	
	// dynamic
	glEnableVertexAttribArrayARB(0);
	glEnableVertexAttribArrayARB(1);
	if useBump then glEnableVertexAttribArrayARB(2);
	if useBump then glEnableVertexAttribArrayARB(3);
	if useTextures then glEnableVertexAttribArrayARB(4);

	glEnable(GL_VERTEX_PROGRAM_ARB);
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, prog[VP_DYNAMIC]);
	
	for I := visible.Count-1 downto 0 do
	begin
		E := TEntity(visible[I]);	
		
		if Assigned(E.model) and E.model.b_dynamic then
		begin
			glPushMatrix;
			glMultMatrixf(@E.FMatrix);

			if (E.model.maler_lod0 <> nil) and (E.distance_sqr > 30*30) then
				TSkeletonModelMaler(E.model.maler_lod0).Draw2(E.mtlset, E.Selected, False, False)
			else if (E.model.maler_lod1 <> nil) and (E.distance_sqr > 10*10) then
				TSkeletonModelMaler(E.model.maler_lod1).Draw2(E.mtlset, E.Selected, False, False)
			else
				TSkeletonModelMaler(E.model.maler).Draw2(E.mtlset, E.Selected, False, False);
				
			glPopMatrix;
		end;
	end;
	
	glBindProgramARB(GL_VERTEX_PROGRAM_ARB, 0);
	glDisable(GL_VERTEX_PROGRAM_ARB);
	
	glDisableVertexAttribArrayARB(0);
	glDisableVertexAttribArrayARB(1);
	if useBump then glDisableVertexAttribArrayARB(2);
	if useBump then glDisableVertexAttribArrayARB(3);
	if useTextures then glDisableVertexAttribArrayARB(4);
end;

procedure TScene.RenderBlended;
var
	I : Longint;
	E : TEntity;
begin

	if showEGeoms and Assigned(egeoms_maler) then
		egeoms_maler.Draw(-1, False, True, False);
	if showDecals and Assigned(decals_maler) then
		decals_maler.Draw(-1, False, True, False);

	for I := 0 to visible.Count-1 do
		TEntity(visible[I]).Draw(True, False);
		
	if showShapes then
		for I := 0 to entities.Count-1 do
			if TEntity(entities[I]).Visible then
				TEntity(entities[I]).DrawShapes;

end;

procedure TScene.RenderDistort;
var
	I : Longint;
	E : TEntity;
begin	

	for I := 0 to visible.Count-1 do
		TEntity(visible[I]).Draw(False, True);

end;

procedure TScene.RenderEnvZones;
var
	I : Longint;
	colors : array[0..3] of TVec3;
begin
	if env_zones = nil then
		Exit;
		
	colors[0].x := 0.0;
	colors[0].y := 0.0;
	colors[0].z := 0.8;
	
	colors[1].x := 0.8;
	colors[1].y := 0.0;
	colors[1].z := 0.0;
	
	colors[2].x := 0.8;
	colors[2].y := 0.8;
	colors[2].z := 0.0;
	
	colors[3].x := 0.0;
	colors[3].y := 0.8;
	colors[3].z := 0.8;
		
	glDisable(GL_CULL_FACE);
	
	for I := 0 to env_zones.Count-1 do
	begin
		glPushMatrix;	
		glMultMatrixf(@TEnvZone(env_zones[I]).FMatrix);
			
		glColor3fv(@colors[I mod Length(Colors)]);
		
		glBegin(GL_TRIANGLES);
		
		glVertex3f(-0.5, 0.0, -0.5);
		glVertex3f(-0.5, 0.0,  0.5);
		glVertex3f( 0.5, 0.0,  0.5);
		glVertex3f(-0.5, 0.0, -0.5);
		glVertex3f( 0.5, 0.0, -0.5);
		glVertex3f( 0.5, 0.0,  0.5);
		
		glEnd;
		
		if TEnvZone(env_zones[I]).Selected then
		begin
			glColor3f(0.0, 1.0, 0.0);
			
			glBegin(GL_LINE_LOOP);
			
			glVertex3f(-0.5, 0.0, -0.5);
			glVertex3f(-0.5, 0.0,  0.5);
			glVertex3f( 0.5, 0.0,  0.5);
			glVertex3f( 0.5, 0.0, -0.5);
	
			glEnd;		
		end;
		
		glPopMatrix;
	end;
	
	glColor3f(1.0, 1.0, 1.0);
	glEnable(GL_CULL_FACE);
end;

procedure TScene.AddEntity(e : TEntity);
begin
	entities.Add(e);
	konf_entities.items.Add(e.data);
end;

procedure TScene.RemoveEntity(e : TEntity);
var
	idx : Longint;
begin
	idx := konf_entities.items.IndexOf(e.data);
	if idx <> -1 then
	begin
		konf_entities.items.Delete(idx);
	end else
	begin
		idx := konf_add_entities.items.IndexOf(e.data);
		if idx <> -1 then
			konf_add_entities.items.Delete(idx)
		else
			raise Exception.Create('entity doesn''t belong to level.bin nor level.add.bin');
	end;
	
	e.data.Free;
	entities.Remove(e);
	e.Free;
end;

function TScene.EntityById(id : Word) : TEntity;
var
	I : Integer;
begin
	// are entities use same id-space as ucovers? 
	// added: no.

	Result := nil;

	if Assigned(entities) then
	begin
		for I := 0 to entities.Count - 1 do
		begin
			if TEntity(entities[I]).ID = id then
			begin
				Result := TEntity(entities[I]);
				Exit;
			end;
		end;
	end;
end;

function TScene.GenerateId : Word;
var
	id : Word;
begin
	if Assigned(entities) then
	begin
		id := 256;
		while (id < 65535) and (EntityById(id) <> nil) do
			Inc(id);

		Result := id;
	end else
		Result := 65535;
end;

function TScene.EntityByName(const name : String) : TEntity;
var
	I : Integer;
begin
	Result := nil;

	if Assigned(entities) then
	begin
		for I := 0 to entities.Count - 1 do
		begin
			if TEntity(entities[I]).Name = name then
			begin
				Result := TEntity(entities[I]);
				Exit;
			end;
		end;
	end;
end;

procedure TScene.MakeAddon(arr : TEntityArray; addon : Boolean);
var
	I : Longint;
	data : TSection;
begin
	if Assigned(konf_entities) and Assigned(konf_add_entities) then
	begin
		for I := 0 to Length(arr)-1 do
		begin
			data := arr[I].data;
			if not addon then
			begin
				konf_add_entities.items.Remove(data);
				if konf_entities.items.IndexOf(data) = -1 then
					konf_entities.items.Add(data);
			end else
			begin
				konf_entities.items.Remove(data);
				if konf_add_entities.items.IndexOf(data) = -1 then
					konf_add_entities.items.Add(data);
			end;		
		end;
	end;
end;

function TScene.GetSelectedList : TList;
var
	list : TList;
	I : Integer;
begin
	list := TList.Create;
	
	if Assigned(entities) then
		for I := 0 to entities.Count - 1 do
			if TEntity(entities[I]).selected then
				list.Add(entities[I]);
			
	GetSelectedList := list;
end;

function TScene.GetSelected : TEntityArray;
var
	sel : TEntityArray;
	I : Integer;
begin
	SetLength(sel, 0);

	if Assigned(entities) then
		for I := 0 to entities.Count - 1 do
			if TEntity(entities[I]).selected then
			begin
				SetLength(sel, Length(sel)+1);
				sel[Length(sel)-1] := TEntity(entities[I]);
			end;
			
	GetSelected := sel;
end;

function TScene.GetSelectedEZList : TList;
var
	list : TList;
	I : Integer;
begin
	list := TList.Create;
	
	if Assigned(env_zones) then
		for I := 0 to env_zones.Count - 1 do
			if TEnvZone(env_zones[I]).Selected then
				list.Add(env_zones[I]);
			
	GetSelectedEZList := list;
end;

function TScene.GetSelectedEZ : TEnvZoneArray;
var
	sel : TEnvZoneArray;
	I : Integer;
begin
	SetLength(sel, 0);

	if Assigned(env_zones) then
		for I := 0 to env_zones.Count - 1 do
			if TEnvZone(env_zones[I]).Selected then
			begin
				SetLength(sel, Length(sel)+1);
				sel[Length(sel)-1] := TEnvZone(env_zones[I]);
			end;
			
	GetSelectedEZ := sel;
end;

function TScene.GetVersion : TSceneVersion;
var
	entities_params : TSection;
	entity_ver : Word;
begin
	if konf <> nil then
	begin
		entities_params := konf.root.GetSect('entities_params', False);
		if entities_params <> nil then
		begin
			entity_ver := entities_params.GetInt('version', 'u16');
			
			if entity_ver >= 49 { ENTITY_VER_EXODUS } then 
				Result := sceneVerExodus
			else if entity_ver >= ENTITY_VER_ARKTIKA1 then
				Result := sceneVerArktika1
			else if entity_ver >= ENTITY_VER_REDUX then
				Result := sceneVerRedux
			else
				Result := sceneVerLL;
		end else
			Result := sceneVer2033;
	end else
		Result := sceneVerNone;
end;

procedure TScene.UpdateAttaches;
var
	I : Longint;
	e, parent : TEntity;
	
	att_bone : String;
	att_mat, base_mat : TMatrix;
begin

	for I := 0 to entities.Count-1 do
	begin
		e := TEntity(entities[I]);
		if e.ParentID <> 65535 then
		begin
			parent := EntityById(e.ParentID);
			if parent <> nil then
			begin
				att_mat := e.AttachOffset;
				att_bone := e.AttachBone;
				
				if parent.GetBoneTransform(att_bone, base_mat) then
				begin
					Mul44(base_mat, att_mat);
					e.Matrix := base_mat;
				end;
			end;
		end;
	end;

end;

procedure TScene.LoadLevelCform(const dir : String);
var
	ph : TList;
	shapes : array of Pointer;
	I : Integer;
begin
	ph := TList.Create;
	try
		{if FileExists(dir + '\level.nxcform_pc') then
			LoadPhysics(dir + '\level.nxcform_pc', ph)
		else
		if FileExists(dir + '\level.nxcform_xbox') then
			LoadPhysics(dir + '\level.nxcform_xbox', ph)
		else}
			if level <> nil then
				MakeLevelCform(ph, level);

		if ph.Count > 0 then
		begin
			SetLength(shapes, ph.Count);
			SetLength(ph_meshes, ph.Count);
			for I := 0 to ph.Count - 1 do
			begin
				ph_meshes[I] := ph[I];
				shapes[I] := PHShapeTriMesh(ph[I], nil, nil);
			end;
	
			ph_superstatic := PHCreateActor(ph_scene, 0, Length(shapes), @shapes[0], nil);
			if ph_superstatic = nil then
				WriteLn('level cform loading failed, actor not created');
		end;
	finally
		ph.Free;
	end;
end;

procedure TScene.UnloadLevelCform;
var
	I : Longint;
begin
	PHRemoveActor(ph_scene, ph_superstatic);
	ph_superstatic := nil;
	
	for I := 0 to Length(ph_meshes)-1 do
		PHFreeTriMesh(ph_meshes[I]);
	SetLength(ph_meshes, 0);
end;

procedure TScene.LoadEnvironment(const fn : String);
var
	I : Longint;
	K : TKonfig;
	env_konf : TTextKonfig;
	
	sect_environment : TSection;
	version : Longword;
	
	sect_zones : TSection;
	data : TSection;
begin
	env_konf := TTextKonfig.Create;
		
	K := TKonfig.Create;
	K.Load(fn);
	K.Decompile(env_konf, GetVersion >= sceneVerLL);
	K.Free;
	
	sect_environment := env_konf.root.GetSect('environment');
	version := sect_environment.GetInt('version', 'u32');
	
	if version > 11 then
	begin
		WriteLn('level.environment: unsupported version ', version, ', won''t load');
		env_konf.Free;
		Exit;
	end;
	
	sect_zones := sect_environment.GetSect('zones');
	for I := 1 to sect_zones.ParamCount-1 do
	begin
		data := TSection((sect_zones.GetParam(I) as TSection).Copy);
		data.name := 'env_zone'; // will be renumerated in SaveEnvironment ;)
		env_zones.Add(TEnvZone.Create(ph_scene, data));
	end;
	
	env_konf.Free;
end;

procedure TScene.SaveEnvironment(const fn : String);
var
	I : Longint;
	version : Longint;
	bbox, bbox2 : TAABB;
	
	env_konf : TTextKonfig;
	sect1, sect2 : TSection;
	
	data : TSection;
	n : String[16];
	
	K : TKonfig;
begin
	case GetVersion of
		sceneVer2033:   version := 8;
		sceneVerLL,
		sceneVerRedux:  version := 11;
		else            version := -1;
	end;
	
	if version <> -1 then
	begin
		env_konf := TTextKonfig.Create;
		sect1 := env_konf.root.AddSect('environment');
		sect1.AddInt('version', version, 'u32');
		
		for I := 0 to env_zones.Count - 1 do
		begin
			if I = 0 then
				TEnvZone(env_zones[I]).GetBBox(bbox)
			else
			begin
				TEnvZone(env_zones[I]).GetBBox(bbox2);
				AABBMerge(bbox, bbox2);
			end
		end;
		
		sect1.AddVec3('aabb_min', bbox.min);
		sect1.AddVec3('aabb_max', bbox.max);
		
		sect1.AddHint('zones', 'array');
		sect2 := sect1.AddSect('zones');
		
		sect2.AddInt('count', env_zones.Count, 'u32');
		for I := 0 to env_zones.Count - 1 do
		begin
			data := TSection(TEnvZone(env_zones[I]).data.Copy);
			
			n := IntToStr(I);
			data.name := 'rec_' + StringOfChar('0',4-Length(n)) + n;
			
			sect2.items.Add(data);
		end;
		
		K := TKonfig.Create;
		K.kind := 3;
		K.Compile(env_konf);
		K.Save(fn);
		
		K.Free;
		env_konf.Free;
	end;
end;

end.