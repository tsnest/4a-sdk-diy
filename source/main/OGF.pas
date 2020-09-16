unit OGF;
{ XRay engine models and levels reader }
interface
uses vmath, chunkedFile;

const
	OGF_MT_NORMAL				 = 0;
	OGF_MT_HIERRARHY			= 1;
	OGF_MT_PROGRESSIVE		= 2;
	OGF_MT_SKELETON_ANIM	= 3;
	OGF_MT_SKELETON_GEOMDEF_PM = 4;
	OGF_MT_SKELETON_GEOMDEF_ST = 5;
	OGF_MT_LOD						= 6;
	OGF_MT_TREE_ST				= 7;
	OGF_MT_SKELETON_RIGID = 10;
	OGF_MT_TREE_PM				= 11;

	OGF_VF_STATIC				 = $112;
	OGF_VF_SKIN1					= $12071980;
	OGF_VF_SKIN2					= $12071980*2;

type
	TOGFVertStatic = record
		point, normal : TVec3;
		tc : TVec2;
	end;

	TOGFVertSkin1 = record
		point, normal, tangent, binormal : TVec3;
		tc : TVec2;
		bone : Longint;
	end;

	TOGFVertSkin2 = record
		bone1, bone2 : Word;
		point, normal, tangent, binormal : TVec3;
		weight : Single;
		tc : TVec2;
	end;

	TOGFVertSkin3 = record
		bone1, bone2, bone3 : Word;
		point, normal, tangent, binormal : TVec3;
		weight1, weight2 : Single;
		tc : TVec2;
	end;

	TOGFVertSkin4 = record
		bone1, bone2, bone3, bone4 : Word;
		point, normal, tangent, binormal : TVec3;
		weight1, weight2, weight3 : Single;
		tc : TVec2;
	end;

	POGFVertStatic = ^TOGFVertStatic;
	POGFVertSkin1 = ^TOGFVertSkin1;
	POGFVertSkin2 = ^TOGFVertSkin2;
	POGFVertSkin3 = ^TOGFVertSkin3;
	POGFVertSkin4 = ^TOGFVertSkin4;

	TOGFModel = class // base
		version, modeltype : Byte;
		shaderid : Word;
		bbox : TAABB;
		bsphere : TSphere;

		procedure Load(reader : TMemoryReader); virtual;
		procedure Save(w : TMemoryWriter); virtual;
	end;

	TOGFModelHierrarhy = class(TOGFModel)
		meshes : array of TOGFModel;
		meshesl : array of Longword; // links (for level)

		destructor Destroy; override;

		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;

	TOGFSlideWindow = packed record
		offset : Longword;
		nfaces, nverts : Word;
	end;

	TOGFModelSimple = class(TOGFModel)
		texture, shader : String;
		vertexformat, vertexcount : Longword;
		vertices : array of Byte;
		indices : array of Word;

		swis : array of TOGFSlideWindow;

		// ref (for level)
		geom_vbuffer, geom_voffset, geom_vcount : Longword;
		geom_ibuffer, geom_ioffset, geom_icount : Longword;
		geom_swibuffer : Longword;

		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;

	TOGFModelMU = class(TOGFModelSimple)
		xform : TMatrix;

		procedure Load(reader : TMemoryReader); override;
		procedure Save(w : TMemoryWriter); override;
	end;

	TD3DVertexBuffer = class
		poffset, ptype : Integer;
		noffset, ntype : Integer;
		toffset, ttype : Integer;
		boffset, btype : Integer;
		tcoffset, tctype : Integer;
		lmoffset, lmtype : Integer;

		currentvert,
		vertexsize,
		vertexcount : Integer;
		data : array of Byte;

		constructor Create;

		procedure GetPosition(var p : TVec3);
		procedure GetNormal(var n : TVec3);	overload;
		procedure GetNormal(var pn : Longword); overload;
		procedure GetTangent(var t : TVec3); overload;
		procedure GetTangent(var pt : Longword); overload;
		procedure GetBinormal(var b : TVec3); overload;
		procedure GetBinormal(var pb : Longword); overload;
		procedure GetTexcoord1(var tc : TVec2);
		procedure GetTexcoord2(var tc : TVec2);

		function NextVertex : Boolean;
	end;

	TXRayLevel = class
		version : Word;
		vbuffers : array of TD3DVertexBuffer;
		ibuffers : array of array of Word;
		swibuffers : array of array of TOGFSlideWindow;
		visuals : array of TOGFModel;
		materials : array of array[1..2] of String;

		constructor Create;
		destructor Destroy; override;

		function Load(from : String) : Boolean;
		
		private
		procedure LoadGeom(reader : TMemoryReader);
	end;

	function LoadOGF(r : TMemoryReader) : TOGFModel;

implementation
uses SysUtils;

type
	TD3DVertexElement9 = record
		stream, offset : Word;
		elemtype, method, usage, usageindex : Byte;
	end;

const
	DECLTYPE_FLOAT2		 = 1;
	DECLTYPE_FLOAT3		 = 2;
	DECLTYPE_D3DCOLOR	 = 4;
	DECLTYPE_SHORT2		 = 6;
	DECLTYPE_SHORT4		 = 7;
	DECLTYPE_UNUSED		 = 17;

	DECLUSAGE_POSITION	= 0;
	DECLUSAGE_NORMAL		= 3;
	DECLUSAGE_TEXCOORD	= 5;
	DECLUSAGE_TANGENT	 = 6;
	DECLUSAGE_BINORMAL	= 7;
	DECLUSAGE_COLOR		 = 10;

function D3DSizeOf(decltype : Integer) : Integer;
begin
	case decltype of
		DECLTYPE_FLOAT2:		D3DSizeOf := 8;
		DECLTYPE_FLOAT3:		D3DSizeOf := 12;
		DECLTYPE_D3DCOLOR:	D3DSizeOf := 4;
		DECLTYPE_SHORT2:		D3DSizeOf := 4;
		DECLTYPE_SHORT4:		D3DSizeOf := 8;
		DECLTYPE_UNUSED:		D3DSizeOf := 0;
		else
			raise Exception.Create('Unknown decltype ' + IntToStr(decltype));
	end;
end;

constructor TD3DVertexBuffer.Create;
begin
	poffset := 0;
	ptype := DECLTYPE_UNUSED;
	noffset := 0;
	ntype := DECLTYPE_UNUSED;
	toffset := 0;
	ttype := DECLTYPE_UNUSED;
	boffset := 0;
	btype := DECLTYPE_UNUSED;
	tcoffset := 0;
	tctype := DECLTYPE_UNUSED;
end;

procedure TD3DVertexBuffer.GetPosition(var p: TVec3);
begin
	case ptype of
		DECLTYPE_FLOAT3:
			Move(data[currentvert*vertexsize+poffset], p, 12);
	end;
end;

procedure TD3DVertexBuffer.GetNormal(var n: TVec3);
var
	pn : Longword;
begin
	case ntype of
		DECLTYPE_FLOAT3:
			Move(data[currentvert*vertexsize+noffset], n, 12);
		DECLTYPE_D3DCOLOR:
		begin
			Move(data[currentvert*vertexsize+noffset], pn, 4);
			UnpackNormal(n, pn);
		end;
	end;
end;

procedure TD3DVertexBuffer.GetNormal(var pn : Longword);
var
	n : TVec3;
begin
	case ntype of
		DECLTYPE_D3DCOLOR:
			Move(data[currentvert*vertexsize+noffset], pn, 4);
		DECLTYPE_FLOAT3:
		begin
			Move(data[currentvert*vertexsize+noffset], n, 12);
			pn := PackNormal(n);
		end;
	end;
end;

procedure TD3DVertexBuffer.GetTangent(var t: TVec3);
var
	pt : Longword;
begin
	case ttype of
		DECLTYPE_FLOAT3:
			Move(data[currentvert*vertexsize+toffset], t, 12);
		DECLTYPE_D3DCOLOR:
		begin
			Move(data[currentvert*vertexsize+toffset], pt, 4);
			UnpackNormal(t, pt);
		end;
	end;
end;

procedure TD3DVertexBuffer.GetTangent(var pt : Longword);
var
	t : TVec3;
begin
	case ttype of
		DECLTYPE_D3DCOLOR:
			Move(data[currentvert*vertexsize+toffset], pt, 4);
		DECLTYPE_FLOAT3:
		begin
			Move(data[currentvert*vertexsize+toffset], t, 12);
			pt := PackNormal(t);
		end;
	end;
end;

procedure TD3DVertexBuffer.GetBinormal(var b: TVec3);
var
	pb : Longword;
begin
	case btype of
		DECLTYPE_FLOAT3:
			Move(data[currentvert*vertexsize+boffset], b, 12);
		DECLTYPE_D3DCOLOR:
		begin
			Move(data[currentvert*vertexsize+boffset], pb, 4);
			UnpackNormal(b, pb);
		end;
	end;
end;

procedure TD3DVertexBuffer.GetBinormal(var pb : Longword);
var
	b : TVec3;
begin
	case btype of
		DECLTYPE_D3DCOLOR:
			Move(data[currentvert*vertexsize+boffset], pb, 4);
		DECLTYPE_FLOAT3:
		begin
			Move(data[currentvert*vertexsize+boffset], b, 12);
			pb := PackNormal(b);
		end;
	end;
end;

procedure TD3DVertexBuffer.GetTexcoord1(var tc: TVec2);
var
	ptc : array[1..2] of Smallint;
begin
	case tctype of
		DECLTYPE_FLOAT2:
			Move(data[currentvert*vertexsize+tcoffset], tc, 8);
		DECLTYPE_SHORT2, DECLTYPE_SHORT4:
		begin
			Move(data[currentvert*vertexsize+tcoffset], ptc, 4);
			tc.x := ptc[1] / 1024.0;
			tc.y := ptc[2] / 1024.0;
		end;
	end;
end;

procedure TD3DVertexBuffer.GetTexcoord2(var tc: TVec2);
var
	ptc : array[1..2] of Smallint;
begin
	case lmtype of
		DECLTYPE_FLOAT2:
			Move(data[currentvert*vertexsize+lmoffset], tc, 8);
		DECLTYPE_SHORT2, DECLTYPE_SHORT4:
		begin
			Move(data[currentvert*vertexsize+lmoffset], ptc, 4);
			tc.x := ptc[1] / 32768.0;
			tc.y := ptc[2] / 32768.0;
		end;
	end;
end;

function TD3DVertexBuffer.NextVertex : Boolean;
begin
	if currentvert < vertexcount-1 then
	begin
		Inc(currentvert);
		Result := True;
	end else
		Result := False;
end;

const
	OGF_CHUNK_HEADER			= 1;
	OGF_CHUNK_TEXTURE		 = 2;
	OGF_CHUNK_VERTICES		= 3;
	OGF_CHUNK_INDICES		 = 4;
	OGF_CHUNK_SWIDATA		 = 6;
	OGF_CHUNK_VCONTAINER	= 7;
	OGF_CHUNK_ICONTAINER	= 8;
	OGF_CHUNK_CHILD			 = 9;
	OGF_CHUNK_CHILD_L		 = 10;
	OGF_CHUNK_TREEDEF		 = 12;
	OGF_CHUNK_SWICONTAINER = 20;
	OGF_CHUNK_GCONTAINER	= 21;

function LoadOGF(r : TMemoryReader) : TOGFModel;
var
	m : TOGFModel;
	mt : Byte;
begin
	m := TOGFModel.Create;
	m.Load(r);
	mt := m.modeltype;
	m.Free;

	case mt of
		OGF_MT_NORMAL, OGF_MT_PROGRESSIVE, OGF_MT_SKELETON_GEOMDEF_ST,
		OGF_MT_SKELETON_GEOMDEF_PM:
			m := TOGFModelSimple.Create;
		OGF_MT_HIERRARHY, OGF_MT_SKELETON_RIGID, OGF_MT_SKELETON_ANIM:
			m := TOGFModelHierrarhy.Create;
		OGF_MT_LOD:
			m := nil; // not used
		OGF_MT_TREE_ST, OGF_MT_TREE_PM:
			m := TOGFModelMU.Create;
		else
		begin
			Writeln('Unknown OGF model type ' + IntToStr(mt) + ', skipped');
			m := nil;
		end;
	end;

	if m <> nil then
		m.Load(r);

	Result := m;
end;

procedure TOGFModel.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
begin
	r := reader.OpenChunk(OGF_CHUNK_HEADER);
	try
		version := r.ReadByte;
		if version <> 4 then
			raise Exception.Create('Unsupported OGF version');
		modeltype := r.ReadByte;
		shaderid := r.ReadWord;
		r.Read(bbox, Sizeof(bbox));
		r.Read(bsphere, Sizeof(bsphere));
	finally
		r.Free;
	end;
end;

procedure TOGFModel.Save(w: TMemoryWriter);
begin
	w.OpenChunk(OGF_CHUNK_HEADER);
	w.WriteByte(version);
	w.WriteByte(modeltype);
	w.WriteWord(shaderid);
	w.Write(bbox, Sizeof(bbox));
	w.Write(bsphere, Sizeof(bsphere));
	w.CloseChunk;
end;

procedure TOGFModelSimple.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
	count : Longword;
begin
	inherited Load(reader);
{
	case modeltype of
		OGF_MT_NORMAL, OGF_MT_SKELETON_GEOMDEF_ST :
			;
		OGF_MT_PROGRESSIVE, OGF_MT_SKELETON_GEOMDEF_PM :
			Writeln('Warning! Progressive OGF Model');
		else
			raise Exception.Create('Unknown OGF model type');
	end;
}
	try
		r := reader.OpenChunk(OGF_CHUNK_GCONTAINER);
		if r <> nil then
		begin
			geom_vbuffer := r.ReadLongword;
			geom_voffset := r.ReadLongword;
			geom_vcount := r.ReadLongword;
			geom_ibuffer := r.ReadLongword;
			geom_ioffset := r.ReadLongword;
			geom_icount := r.ReadLongword;
			FreeAndNil(r);
		end else
		begin
			r := reader.OpenChunk(OGF_CHUNK_VCONTAINER);
			if r <> nil then
			begin
				geom_vbuffer := r.ReadLongword;
				geom_voffset := r.ReadLongword;
				geom_vcount := r.ReadLongword;
				FreeAndNil(r);
				
				r := reader.OpenChunk(OGF_CHUNK_ICONTAINER);
				geom_ibuffer := r.ReadLongword;
				geom_ioffset := r.ReadLongword;
				geom_icount := r.ReadLongword;
				FreeAndNil(r);
			end else
			begin
				r := reader.OpenChunk(OGF_CHUNK_TEXTURE);
				texture := r.ReadStringZ;
				shader := r.ReadStringZ;
				FreeAndNil(r);
	
				r := reader.OpenChunk(OGF_CHUNK_VERTICES);
				vertexformat := r.ReadLongword;
				vertexcount := r.ReadLongword;
				SetLength(vertices, r.size-8);
				r.Read(vertices[0], r.size-8);
				FreeAndNil(r);
	
				r := reader.OpenChunk(OGF_CHUNK_INDICES);
				count := r.ReadLongword;
				SetLength(indices, count);
				r.Read(indices[0], count*Sizeof(word));
				FreeAndNil(r);
			end;
		end;

		r := reader.OpenChunk(OGF_CHUNK_SWIDATA);
		if r <> nil then
		begin
			Inc(r.pos, 16); // reserved
			SetLength(swis, r.ReadLongword);
			r.Read(swis[0], Sizeof(TOGFSlideWindow)*Length(swis));
			FreeAndNil(r);
		end;

		r := reader.OpenChunk(OGF_CHUNK_SWICONTAINER);
		if r <> nil then
		begin
			geom_swibuffer := r.ReadLongword;
			FreeAndNil(r);
		end;
	finally
		r.Free;
	end;
end;

procedure TOGFModelSimple.Save(w : TMemoryWriter);
begin
	inherited Save(w);
	w.OpenChunk(OGF_CHUNK_TEXTURE);
	w.WriteStringZ(texture);
	w.WriteStringZ(shader);
	w.CloseChunk;

	w.OpenChunk(OGF_CHUNK_VERTICES);
	w.WriteLongword(vertexformat);
	w.WriteLongword(vertexcount);
	w.Write(vertices[0], Length(vertices));
	w.CloseChunk;

	w.OpenChunk(OGF_CHUNK_INDICES);
	w.WriteLongword(Length(indices));
	w.Write(indices[0], Length(indices)*Sizeof(Word));
	w.CloseChunk;
end;

destructor TOGFModelHierrarhy.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(meshes) - 1 do
		meshes[I].Free;

	inherited Destroy;
end;

procedure TOGFModelHierrarhy.Load(reader : TMemoryReader);
var
	r, r2 : TMemoryReader;
	I : Integer;
begin
	inherited Load(reader);
	case modeltype of
		OGF_MT_HIERRARHY, OGF_MT_SKELETON_ANIM, OGF_MT_SKELETON_RIGID: ;
		else
			raise Exception.Create('Invalid OGF model type, must be hierrarhy');
	end;

	r := nil;
	r2 := nil;
	try
		r := reader.OpenChunk(OGF_CHUNK_CHILD);
		if r <> nil then
		begin
			I := 0;
			r2 := r.OpenChunk(I);
			while r2 <> nil do
			begin
				SetLength(meshes, I+1);
				meshes[I] := LoadOGF(r2);
				FreeAndNil(r2);

				Inc(I);
				r2 := r.OpenChunk(I);
			end;
			FreeAndNil(r);
		end;
		r := reader.OpenChunk(OGF_CHUNK_CHILD_L);
		if r <> nil then
		begin
			SetLength(meshesl, r.ReadLongword);
			r.Read(meshesl[0], Length(meshesl)*Sizeof(Longword));
			FreeAndNil(r);
		end;
	finally
		r.Free;
		r2.Free;
	end;
end;

procedure TOGFModelHierrarhy.Save(w : TMemoryWriter);
var
	I : Integer;
begin
	inherited Save(w);
	if Length(meshes) > 0 then
	begin
		w.OpenChunk(OGF_CHUNK_CHILD);
		for I := 0 to Length(meshes) - 1 do
		begin
			w.OpenChunk(I);
			meshes[I].Save(w);
			w.CloseChunk;
		end;
	w.CloseChunk;
	end;
	if Length(meshesl) > 0 then
	begin
		w.OpenChunk(OGF_CHUNK_CHILD_L);
		w.WriteLongword(Length(meshesl));
		w.Write(meshesl[0], Length(meshesl)*Sizeof(Longword));
		w.CloseChunk;
	end;
end;

procedure TOGFModelMU.Load(reader: TMemoryReader);
var
	r : TMemoryReader;
begin
	inherited Load(reader);
	r := reader.OpenChunk(OGF_CHUNK_TREEDEF);
	try
		r.Read(xform, Sizeof(xform));
		// other parameters unused
	finally
		r.Free;
	end;
end;

procedure TOGFModelMU.Save(w: TMemoryWriter);
begin
	inherited Save(w);
end;

const
	FSL_CHUNK_VERSION	= 1;
	FSL_CHUNK_TEXTURES = 2;
	FSL_CHUNK_VISUALS	= 3;
	FSL_CHUNK_VBUFFERS = 9;
	FSL_CHUNK_IBUFFERS = 10;
	FSL_CHUNK_SWIBUFFERS = 11;
	
	FSL12_CHUNK_VBUFFERS = 10;
	FSL12_CHUNK_IBUFFERS = 9;

constructor TXrayLevel.Create;
begin
	version := 13;
end;

destructor TXRayLevel.Destroy;
var
	I : Integer;
begin
	for I := 0 to Length(vbuffers) - 1 do
		vbuffers[I].Free;

	inherited Destroy;
end;

function TXRayLevel.Load(from : String) : Boolean;
var
	reader, reader_geom, r, r2 : TMemoryReader;
	I : Integer;

	s : String;
	pos : Integer;
begin
	WriteLn('loading level...');
	reader := TMemoryReader.CreateFromFile(from + '\level');
	
	r := reader.OpenChunk(FSL_CHUNK_VERSION);
	version := r.ReadWord;
	r.Free;	
	
	if version >= 13 then
	begin
		WriteLn('loading level.geom... ');
		reader_geom := TMemoryReader.CreateFromFile(from + '\level.geom');
		LoadGeom(reader_geom);
		reader_geom.Free;
	end else
	begin
		LoadGeom(reader);
	end;
	
	Writeln('loading visuals and materials...');
	r := reader.OpenChunk(FSL_CHUNK_VISUALS);
	I := 0;
	r2 := r.OpenChunk(I);
	while r2 <> nil do
	begin
		SetLength(visuals, I+1);
		visuals[I] := LoadOGF(r2);
		Inc(I);
		r2.Free;
		r2 := r.OpenChunk(I);
	end;
	r.Free;
	r := reader.OpenChunk(FSL_CHUNK_TEXTURES);
	SetLength(materials, r.ReadLongword);
	for I := 0 to Length(materials) - 1 do
	begin
		s := r.ReadStringZ;
		pos := AnsiPos('/', s);
		materials[I,1] := Copy(s, 1, pos-1);
		materials[I,2] := Copy(s, pos+1, Length(s)-pos);
	end;
	r.Free;
	reader.Free;

	Load := True;
end;

procedure TXrayLevel.LoadGeom(reader : TMemoryReader);
var
	I : Integer;
	r : TMemoryReader;
	
	elem : TD3DVertexElement9;
	
	vb_chunk, ib_chunk : Longword;
begin
	if version >= 13 then
	begin
		vb_chunk := FSL_CHUNK_VBUFFERS;
		ib_chunk := FSL_CHUNK_IBUFFERS;
	end else
	begin
		vb_chunk := FSL12_CHUNK_VBUFFERS;
		ib_chunk := FSL12_CHUNK_IBUFFERS;
	end;

	r := reader.OpenChunk(vb_chunk);
	SetLength(vbuffers, r.ReadLongword);
	for I := 0 to Length(vbuffers) - 1 do
	begin
		vbuffers[I] := TD3DVertexBuffer.Create;
		repeat
			r.Read(elem, Sizeof(elem));
			if elem.elemtype = DECLTYPE_UNUSED then
				Break;
			//WriteLn('usage = ', elem.usage);
			case elem.usage of
				DECLUSAGE_POSITION:
				begin
					vbuffers[I].poffset := elem.offset;
					vbuffers[I].ptype := elem.elemtype;
				end;
				DECLUSAGE_NORMAL:
				begin
					vbuffers[I].noffset := elem.offset;
					vbuffers[I].ntype := elem.elemtype;
				end;
				DECLUSAGE_TANGENT:
				begin
					vbuffers[I].toffset := elem.offset;
					vbuffers[I].ttype := elem.elemtype;
				end;
				DECLUSAGE_BINORMAL:
				begin
					vbuffers[I].boffset := elem.offset;
					vbuffers[I].btype := elem.elemtype;
				end;
				DECLUSAGE_TEXCOORD:
					case elem.usageindex of 
						0: begin // base
							vbuffers[I].tcoffset := elem.offset;
							vbuffers[I].tctype := elem.elemtype;
						end;
						1: begin // lightmap
							vbuffers[I].lmoffset := elem.offset;
							vbuffers[I].lmtype := elem.elemtype;
						end;
					end;
				DECLUSAGE_COLOR: ;
				else
					raise Exception.Create('Unknown usage ' + IntToStr(elem.usage));
			end;
			Inc(vbuffers[I].vertexsize, D3DSizeOf(elem.elemtype));
		until elem.elemtype = DECLTYPE_UNUSED;
		vbuffers[I].vertexcount := r.ReadLongword;
		SetLength(vbuffers[I].data, vbuffers[I].vertexcount*vbuffers[I].vertexsize);
		r.Read(vbuffers[I].data[0], Length(vbuffers[I].data));
	end;
	r.Free;
	r := reader.OpenChunk(ib_chunk);
	SetLength(ibuffers, r.ReadLongword);
	for I := 0 to Length(ibuffers) - 1 do
	begin
		SetLength(ibuffers[I], r.ReadLongword);
		r.Read(ibuffers[I][0], Length(ibuffers[I])*Sizeof(Word));
	end;
	r.Free;
	r := reader.OpenChunk(FSL_CHUNK_SWIBUFFERS);
	if r <> nil then
	begin
		SetLength(swibuffers, r.ReadLongword);
		for I := 0 to Length(swibuffers) - 1 do
		begin
			Inc(r.pos, 16);
			SetLength(swibuffers[I], r.ReadLongword);
			r.Read(swibuffers[I][0], Length(swibuffers[I])*Sizeof(TOGFSlideWindow));
		end;
		r.Free;
	end;
end;

end.