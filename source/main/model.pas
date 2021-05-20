program model;
uses chunkedFile, sysutils, classes, vmath, hashTable, fouramdl, OGF, windows,
		 nxcform, OGFImport, LMap;

type
	TLevelFlags = set of (lfSkipMU, lfUseLMap, lfLastLight);

var
	shaderbytexture, materialbytexture : THashTable;

procedure FromOGFTo4A(srcfile, destfile : String);
var
	mdl : T4AModelHierrarhy;
	w : TMemoryWriter;

	I : Longint;
	m : T4AModelSimple;
	sh, mat : PAnsiString;
begin
	mdl := ImportOGF(srcfile);

	Writeln('surfaces: ', Length(mdl.meshes));
	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		m := mdl.meshes[I];
		Write(m.texture, ' ');
		Write('vertex count: ', Length(m.vertices), ' ');
		Write('face count: ', Length(m.indices) div 3);
		Writeln;
		
		sh := shaderbytexture.Get(m.texture);
		if (sh <> nil) and (AnsiCompareText(sh^, '<none>') <> 0) then
			m.shader := sh^
		else
			m.shader := 'geometry\default';
	
		mat := materialbytexture.Get(m.texture);
		if (mat <> nil) and (AnsiCompareText(mat^, '<none>') <> 0) then
			m.material := mat^
		else
			m.material := 'materials\wood';		
	end;

	w := TMemoryWriter.Create;
	mdl.Save(w);
	w.SaveTo(destfile);
	w.Free;
end;

procedure CopyVertices(src : P4AVertStatic; dest : POGFVertStatic; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		UnpackNormal(dest^.normal, src^.normal);
		dest^.tc := src^.tc;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure From4AToOGF(src, dst : String);
var
	I : Integer;
	
	r : TMemoryReader;
	w : TMemoryWriter;

	mdl : T4AModelHierrarhy;
	ogf : TOGFModelHierrarhy;

	s : T4AModelSimple;
	m : TOGFModelSimple;
begin
	r := TMemoryReader.CreateFromFile(src);

	mdl := T4AModelHierrarhy.Create;
	mdl.Load(r);
	r.Free;

	ogf := TOGFModelHierrarhy.Create;
	ogf.version := 4;
	ogf.modeltype := OGF_MT_HIERRARHY;
	ogf.bbox := mdl.bbox;
	ogf.bsphere := mdl.bsphere;

	SetLength(ogf.meshes, Length(mdl.meshes));
	Writeln('surfaces: ', Length(mdl.meshes));
	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		s := mdl.meshes[I];
		Write(s.texture, ' ');
		Write('vertex count: ', Length(s.vertices), ' ');
		Write('face count: ', Length(s.indices) div 3);
		Writeln;

		m := TOGFModelSimple.Create;
		m.version := 4;
		m.modeltype := OGF_MT_NORMAL;
		m.bbox := s.bbox;
		m.bsphere := s.bsphere;
		m.texture := s.texture;
		m.shader := 'default';
		m.vertexformat := OGF_VF_STATIC;
		m.vertexcount := Length(s.vertices);
		SetLength(m.vertices, m.vertexcount*Sizeof(TOGFVertStatic));
		CopyVertices(P4AVertStatic(s.vertices), POGFVertStatic(m.vertices), m.vertexcount);
		SetLength(m.indices, Length(s.indices));
		Move(s.indices[0], m.indices[0], Length(m.indices)*Sizeof(word));

		ogf.meshes[I] := m;
	end;

	w := TMemoryWriter.Create;

	ogf.Save(w);
	w.SaveTo(dst);
	w.Free;

	ogf.Free;
	mdl.Free;
end;

procedure From4AToNxCform(src, dst : String);
var
	m : T4AModelHierrarhy;
	cf : TNxCform;
	I : Integer;

	r : TMemoryReader;
	w : TMemoryWriter;
begin
	m := T4AModelHierrarhy.Create;
	r := TMemoryReader.CreateFromFile(src);
	m.Load(r);
	r.Free;

	cf := TNxCform.Create(False);
	for I := 0 to Length(m.meshes) - 1 do
		cf.Add4AModel(m.meshes[I]);

	w := TMemoryWriter.Create;
	cf.Save(w);
	w.SaveTo(dst);
	w.Free;

	cf.Free;
	m.Free;
end;

procedure CopyVertices(src : P4AVertStatic; dest : P4AVertLevel; count : Longword); overload;
begin
	while count > 0 do
	begin
		dest^.point := src^.point;
		dest^.normal := src^.normal;
		dest^.tangent := src^.tangent;
		dest^.binormal := src^.binormal;
		dest^.tc.x := Trunc(src^.tc.x * 1024.0);
		dest^.tc.y := Trunc(src^.tc.y * 1024.0);
		dest^.lm.x := 0;
		dest^.lm.y := 0;
		Inc(src);
		Inc(dest);
		Dec(count);
	end;
end;

procedure From4AToLevel(l : T4ALevel; model : T4AModelHierrarhy);
var
	I : Longword;
	verts : array of T4AVertLevel;
	rm : T4AModelRef;
	m	: T4AModelSimple;
begin
	for I := 0 to Length(model.meshes) - 1 do
	begin
		m := model.meshes[I];

		rm := T4AModelRef.Create;

		rm.shaderid := l.AddMaterial(m.shader, m.texture, m.material);
		rm.bbox := m.bbox;
		rm.bsphere := m.bsphere;

		SetLength(verts, Length(m.vertices));
		CopyVertices(P4AVertStatic(m.vertices), P4AVertLevel(verts), Length(verts));

		rm.vertexformat := MODEL_VF_LEVEL;
		rm.vertexoffset := l.AddVertexBuffer(verts[0], Length(verts));
		rm.vertexcount := Length(verts);

		rm.indexoffset := l.AddIndexBuffer(m.indices[0], Length(m.indices));
		rm.indexcount := Length(m.indices);

		l.AddVisual(rm);
	end;
end;

procedure CopyVertices(src : TD3DVertexBuffer; dest : P4AVertLevel; count : Longword; lmap_id : Shortint); overload;
var
	tc : TVec2;
	
	// lightmap
	offset : TVec2;
	scale  : TVec2;
begin
	while count > 0 do
	begin
		src.GetPosition(dest^.point);
		src.GetNormal(dest^.normal);
		src.GetTangent(dest^.tangent);
		src.GetBinormal(dest^.binormal);
		src.GetTexcoord1(tc);
		dest^.tc.x := Trunc(tc.x * 1024.0);
		dest^.tc.y := Trunc(tc.y * 1024.0);
		if lmap_id > 0 then
		begin
			offset.x := ((lmap_id-1) mod 4) * 0.25;
			offset.y := ((lmap_id-1) div 4) * 0.25;
			scale.x := 0.25;
			scale.y := 0.25;
			
			src.GetTexcoord2(tc);
			//WriteLn('U: ', tc.x:3:3, ' V: ', tc.y:3:3);
			
			dest^.lm.x := Trunc(((tc.x * scale.x) + offset.x) * 32768.0);
			dest^.lm.y := Trunc(((tc.y * scale.y) + offset.y) * 32768.0);
		end;

		if src.ntype <> 4 then
			dest^.normal := dest^.normal or ($7F shl 24);

		Inc(dest);
		Dec(count);
		src.NextVertex;
	end;
end;

procedure TransformVertices(v : P4AVertLevel; count : Longword; const xform : TMatrix);
var
	n : TVec3;
begin
	while count > 0 do
	begin
		Transform(v^.point, xform);
		UnpackNormal(n, v^.normal);
		Transform33(n, xform); Normalize(n);
		v^.normal := PackNormal(n) or (v^.normal and $FF000000);
		UnpackNormal(n, v^.tangent);
		Transform33(n, xform); Normalize(n);
		v^.tangent := PackNormal(n) or (v^.tangent and $FF000000);
		UnpackNormal(n, v^.binormal);
		Transform33(n, xform); Normalize(n);
		v^.binormal := PackNormal(n) or (v^.binormal and $FF000000);

		Inc(v);
		Dec(count);
	end;
end;

procedure ScaleAO(l : T4ALevel);
var
	I : Longint;
	ao : Byte;
begin
	for I := 0 to Length(l.vbuffer) - 1 do
	begin
		ao := (l.vbuffer[I].normal and $FF000000) shr 24;
		l.vbuffer[I].normal := (l.vbuffer[I].normal and $00FFFFFF) or (LMap.ScaleAO(ao) shl 24);
	end;
end;

procedure CreateSector(l : T4ALevel);
var
	I : Longword;
	s : T4AModelHierrarhyL;
begin
	s := T4AModelHierrarhyL.Create;
	SetLength(s.meshes, Length(l.visuals));
	for I := 0 to Length(l.visuals) - 1 do
	begin
		AABBMerge(s.bbox, l.visuals[I].bbox);
		s.meshes[I] := I;
	end;
	CalcBSphere(s.bsphere, s.bbox);
	l.AddSector(l.AddVisual(s));
end;

procedure GenerateCform(l : T4ALevel; fn : String; ll : Boolean = False);
var
	cf : TNxCform;
	mt : PAnsiString;
	w : TMemoryWriter;
	I : Integer;
begin
	cf := TNxCform.Create(True);
	for I := 0 to Length(l.visuals) - 1 do
		if l.visuals[I] is T4AModelRef then
		begin
			mt := materialbytexture.Get(l.materials[l.visuals[I].shaderid].texture);
			if (mt = nil) or (AnsiCompareText(mt^, '<none>') <> 0) then
			//if (l.materials[l.visuals[I].shaderid,2] = 'terrain\terrain_mp_factory') then
				cf.Add4AModel(T4AModelRef(l.visuals[I]), l);
		end;

	w := TMemoryWriter.Create;
	if ll then cf.SaveLL(w) else cf.Save(w);
	w.SaveTo(fn);

	w.Free;
	cf.Free;
end;

procedure FromModelToLevel(start, count : Integer; destdir : String; params : TLevelFlags);
var
	l : T4ALevel;
	m : T4AModelHierrarhy;
	r : TMemoryReader;

	I : Integer;
begin
	l := T4ALevel.Create;

	for I := start to start+count do
	begin
		m := T4AModelHierrarhy.Create;
		r := TMemoryReader.CreateFromFile(ParamStr(I));
		m.Load(r);
		r.Free;
		From4AToLevel(l, m);
		m.Free;
	end;

	CreateSector(l);

	If lfLastLight in params then
		l.visuals[l.sectors[0]].version := 21;

	If lfLastLight in params then
		GenerateCform(l, destdir + '\level.nxcform_xbox', True)
	else
		GenerateCform(l, destdir + '\level.nxcform_pc');

	ScaleAO(l);

	Writeln('vertex buffer size: ', Length(l.vbuffer)*Sizeof(T4AVertLevel));
	Writeln('index buffer size: ', Length(l.ibuffer)*Sizeof(Word));

	l.SaveTo(destdir);
	l.Free;
end;

procedure FromXRayLevelTo4ALevel(srcdir, destdir : String; params : TLevelFlags);
var
	l : T4ALevel;
	xl : TXRayLevel;

	I, J : Integer;
	verts : array of T4AVertLevel;
	rm : T4AModelRef;
	m	: TOGFModelSimple;

	sh, mt : PAnsiString;
	texture, shader, material : String;
	flags : Longword;

	icount, ioffset : Longword;
	
	comma : Longint;
	lmap_name : String;
	lmap_id : Integer;
begin
	l := T4ALevel.Create;
	
	WriteLn('loading level ''' + srcdir + '''');
	xl := TXRayLevel.Create;
	xl.Load(srcdir);

	// geometry & visuals
	for I := 0 to Length(xl.visuals) - 1 do
		if xl.visuals[I] is TOGFModelSimple then
		begin
			m := TOGFModelSimple(xl.visuals[I]);
			
			if (lfSkipMU in params) and (m is TOGFModelMU) then
				Continue;

			// parse texture name
			texture := LowerCase(xl.materials[m.shaderid,2]);
			comma := Pos(',', texture);
			if comma > 0 then
			begin
				lmap_name := Copy(texture, comma+1);
				SScanf(lmap_name, 'lmap#%d_1', [@lmap_id]);
				
				//WriteLn('lmap_id = ', lmap_id);
				
				if not (lfUseLMap in params) or (lmap_id > 16) then
					lmap_id := -1;
				
				texture := Copy(texture, 1, comma-1);
			end else
				lmap_id := -1;
			
			// guess material by texture name
			sh := shaderbytexture.Get(texture);
			mt := materialbytexture.Get(texture);
			if sh <> nil then shader := sh^
			else shader := 'geometry\default';
			if mt <> nil then material := mt^
			else material := 'materials\wood';

			if AnsiCompareText(shader, '<none>') = 0 then
				Continue;
			if AnsiCompareText(material, '<none>') = 0 then
				material := 'materials\wood';
				
			if lmap_id > 0 then flags := $00010000 else flags := 0;

			// add visual
			rm := T4AModelRef.Create;

			if lfLastLight in params then
				rm.version := 21;
			rm.shaderid := l.AddMaterial(shader, texture, material, flags);
			rm.bbox := m.bbox;
			rm.bsphere := m.bsphere;
			
			// - copy vertices
			SetLength(verts, m.geom_vcount);
			xl.vbuffers[m.geom_vbuffer].currentvert := m.geom_voffset;
			CopyVertices(xl.vbuffers[m.geom_vbuffer], P4AVertLevel(verts), m.geom_vcount, lmap_id);
			if m is TOGFModelMU then
				TransformVertices(P4AVertLevel(verts), Length(verts), (m as TOGFModelMu).xform);

			if (m.modeltype = OGF_MT_TREE_PM) or (m.modeltype = OGF_MT_TREE_ST) then
				for J := 0 to Length(verts) - 1 do // gotcha!
				begin
					verts[J].tc.x := verts[J].tc.x div 2;
					verts[J].tc.y := verts[J].tc.y div 2;
				end;

			rm.vertexformat := MODEL_VF_LEVEL;
			rm.vertexoffset := l.AddVertexBuffer(verts[0], m.geom_vcount);
			rm.vertexcount := m.geom_vcount;

			// - copy indices
			if m.modeltype = OGF_MT_TREE_PM then
			begin
				//Writeln(xl.materials[m.shaderid][2]);
				icount := xl.swibuffers[m.geom_swibuffer][0].nfaces*3;
				ioffset := m.geom_ioffset+xl.swibuffers[m.geom_swibuffer][0].offset;
			end else
			if Length(m.swis) = 0 then
			begin
				icount := m.geom_icount;
				ioffset := m.geom_ioffset;
			end else
			begin
				icount := m.swis[0].nfaces*3;
				ioffset := m.geom_ioffset+m.swis[0].offset;
			end;
			rm.indexoffset := l.AddIndexBuffer(xl.ibuffers[m.geom_ibuffer,ioffset], icount);
			rm.indexcount := icount;

			l.AddVisual(rm);
		end;

	// cform
	WriteLn('generating nxcform...');
	if lfLastLight in params then
		GenerateCform(l, destdir + '\level.nxcform_xbox', True)
	else
		GenerateCform(l, destdir + '\level.nxcform_pc');
		
	// lightmap
	if lfUseLMap in params then
	begin
		WriteLN('converting lightmap...');
		ConvertLightMap(srcdir, destdir + '\level.lmap_pc');
	end;

	// save level
	ScaleAO(l);
	CreateSector(l);
	if lfLastLight in params then
		l.visuals[l.sectors[0]].version := 21;

	Writeln('vertex buffer size: ', Length(l.vbuffer)*Sizeof(T4AVertLevel));
	Writeln('index buffer size: ', Length(l.ibuffer)*Sizeof(Word));

	l.SaveTo(destdir);

	l.Free;
	xl.Free;
end;

procedure From4AToRaw(src, dst : String);
var
	I, J : Integer;

	r : TMemoryReader;
	w : TMemoryWriter;

	mdl : T4AModelHierrarhy;
	s : T4AModelSimple;

	points : array of TVec3;
	faces : array of Longint;
	
	function AddPoint(const p : TVec3) : Longint;
	var
		I : Integer;
	begin
		for I := 0 to Length(points) - 1 do
			if (points[I].x = p.x) and (points[I].y = p.y) and (points[I].z = p.z) then
			begin
				Result := I;
				Exit;
			end;
		
		I := Length(points);
		SetLength(points, I+1); 
		points[I] := p;
		
		Result := I;
	end;

	function AddFace(const p1, p2, p3 : TVec3) : Boolean;
	var
	//	e1, e2, n : TVec3;
		s : Longint;
	begin
		//e1.x := p2.x - p1.x;
		//e1.y := p2.y - p1.y;
		//e1.z := p2.z - p1.z;

		//e2.x := p3.x - p1.x;
		//e2.y := p3.y - p1.y;
		//e2.z := p3.z - p1.z;

		//n.x := e1.y*e2.z - e1.z*e2.y;
		//n.y := e1.z*e2.x - e1.x*e2.z;
		//n.z := e1.x*e2.y - e1.y*e2.x;

		//if n.y >= 0.0 then
		begin
			s := Length(faces);
			SetLength(faces, s+3);
			faces[s] := AddPoint(p1);
			faces[s+1] := AddPoint(p2);
			faces[s+2] := AddPoint(p3);
			AddFace := True;
		end //else
			//AddFace := False;
	end;
begin
	r := TMemoryReader.CreateFromFile(src);
	mdl := T4AModelHierrarhy.Create;
	mdl.Load(r);
	r.Free;

	SetLength(points, 0);
	SetLength(faces, 0);

	for I := 0 to Length(mdl.meshes) - 1 do
	begin
		s := mdl.meshes[I];

		for J := 0 to (Length(s.indices) div 3) - 1 do
		begin
			AddFace(
				s.vertices[s.indices[(J*3)]].point,
				s.vertices[s.indices[(J*3)+1]].point,
				s.vertices[s.indices[(J*3)+2]].point
			);
		end;
	end;
	mdl.Free;

	w := TMemoryWriter.Create;
	w.WriteLongint(1); // Version
	w.WriteLongint(Length(points));
	w.Write(points[0], Length(points)*Sizeof(TVec3));
	w.WriteLongint(Length(faces) div 3);
	w.Write(faces[0], Length(faces)*Sizeof(Longint));

	w.SaveTo(dst);
	w.Free;
end;

procedure LoadList(fn : String; list : THashTable);
var
	f : TextFile;
	line : String;
	pos : Integer;
	s1 : ^String;
begin
	if FileExists(fn) then
	begin
		Assign(f, fn);
		Reset(f);
		while not EOF(f) do
		begin
			ReadLn(f, line);
			pos := AnsiPos('=', line);
			if pos <> 0 then
			begin
				New(s1);
				s1^ := Copy(line, pos+1, Length(line)-(pos-1));
				list.Add(Copy(line, 1, pos-1), s1);
			end;
		end;
		CloseFile(f);
	end;
end;

procedure LoadLists;

begin
	shaderbytexture := THashTable.Create;
	materialbytexture := THashTable.Create;

	LoadList('shadersbytextures.txt', shaderbytexture);
	LoadList('materialsbytextures.txt', materialbytexture);
end;

var
	I : Integer;
	flags : TLevelFlags;
begin
	LoadLists;

	flags := [];

	I := 1;
	if ParamStr(I) = '-nomu' then
	begin
		Include(flags, lfSkipMU);
		Inc(I);
	end;
	
	if ParamStr(I) = '-lmap' then
	begin
		LMap.soc_version := False;
		Include(flags, lfUseLMap);
		Inc(I);
	end;
	
	if ParamStr(I) = '-lmap_soc' then
	begin
		LMap.soc_version := True;
		Include(flags, lfUseLMap);
		Inc(I);
	end;
	
	if ParamStr(I) = '-ao_scale' then
	begin
		LMap.ao_scale := StrToFloat(ParamStr(I+1));
		Inc(I,2);
	end;
	
	if ParamStr(I) = '-ll' then
	begin
		Include(flags, lfLastLight);
		Inc(I);
	end;

	if ParamCount-(I-1) >= 3 then
	begin
		if ParamStr(I) = '-ogf2model' then
		begin
			FromOGFTo4A(ParamStr(I+1), ParamStr(I+2));
		end else
		if ParamStr(I) = '-model2ogf' then
		begin
			From4AToOGF(ParamStr(I+1), ParamStr(I+2));
		end else
		if ParamStr(I) = '-model2nxcform_pc' then
		begin
			From4AToNxCform(ParamStr(I+1), ParamStr(I+2));
		end else
		if ParamStr(I) = '-model2level' then
		begin
			FromModelToLevel(I+1, ParamCount-(I+1)-1, ParamStr(ParamCount), flags);
		end else
		if ParamStr(I) = '-level2level' then
		begin
			FromXRayLevelTo4ALevel(ParamStr(I+1), ParamStr(I+2), flags);
		end else
		if ParamStr(I) = '-model2raw' then
		begin
			From4AToRaw(ParamStr(I+1), ParamStr(I+2));
		end else
			Writeln('Invalid parameter passed!');
	end else
	begin
		WriteLn('Usage:');
		WriteLn(#9'model -ogf2model infile outfile');
		WriteLn(#9'model -model2ogf infile outfile');
		WriteLn(#9'model -model2nxcform_pc infile outfile');
		WriteLn(#9'model -model2level model1 model2 .. modelN leveldir');
		WriteLn(#9'model [-nomu] [-lmap] [-lmap_soc] -level2level xrleveldir leveldir');
		WriteLn('Options:');
		WriteLn(#9'-nomu : skip Multiple Usage models');
		WriteLn(#9'-lmap : convert lightmaps (CS/CoP version) to level.lmap_pc');
		WriteLn(#9'-lmap_soc : convert lightmaps (SoC version) to level.lmap_pc');
	end;
end.