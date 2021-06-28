unit skeleton;

interface
uses vmath, chunkedFile, Konfig;

type
	T4ABone = record
		name : String;
		parent_name : String;
		orientation : TVec3; // TODO use quaterion ?
		q : TVec4;
		position : TVec3;
		bone_part : Word;
		
		parent_id : Longint;
	end;
	P4ABone = ^T4ABone;
	
	T4ALocator = record
		name : String;
		parent_name : String;
		orientation : TVec3; // TODO use quaterion ?
		q : TVec4;
		position : TVec3;
		flags : Byte; // LL 
		
		parent_id : Longint;
	end;
	P4ALocator = ^T4ALocator;
	
	T4ABonePart = record
		name : String;
		weights : array of Byte;
	end;
	P4ABonePart = ^T4ABonePart;
	
type
	T4ASkeleton = class
		version : Longword;
		crc : Longword;
		bones : array of T4ABone;
		locators : array of T4ALocator;
		bone_parts : array of T4ABonePart;
		anim_path : String;
		
		procedure Load2033(reader : TMemoryReader);
		procedure LoadKonfig(r : TMemoryReader); overload;
		procedure LoadKonfig(k : TKonfig); overload;
		
		function  GetBoneID(const name : String) : Longint;
		function  GetLocatorID(const name : String) : Longint;
		
		function  GetTransform(const name : String; out transform : TMatrix) : Boolean;
		function  GetTransformLocal(const name : String; out transform : TMatrix) : Boolean;
		
		function  GetBoneTransform(id : Longint; out transform : TMatrix) : Boolean;
		function  GetBoneTransformLocal(id : Longint; out transform : TMatrix) : Boolean;
		function  GetLocatorTransform(id : Longint; out transform : TMatrix) : Boolean;
		function  GetLocatorTransformLocal(id : Longint; out transform : TMatrix) : Boolean;
	end;

implementation
uses sysutils, Konfig_reader, math;

const
	CHUNK_VERSION			= 1;	// probably
	CHUNK_BONES				= 13;
	CHUNK_LOCATORS		= 14;
	CHUNK_BONE_PARTS	= 17;
	CHUNK_ANIM_PATH		= 19;
	CHUNK_PARAMS			= 27;
	
procedure T4ASkeleton.Load2033(reader : TMemoryReader);
var
	I, count : Longint;
	r : TMemoryReader;
	o, p : TVec3;
begin
	try
		r := reader.OpenChunk(CHUNK_VERSION);
		version := r.ReadLongword;
		FreeAndNil(r);
		
		r := reader.OpenChunk(CHUNK_BONES);
		crc := r.ReadLongword;
		count := r.ReadWord;
		SetLength(bones, count);
		for I := 0 to count-1 do
			with bones[I] do
			begin
				name := r.ReadStringZ;
				parent_name := r.ReadStringZ;
				r.Read(o, Sizeof(TVec3));
				orientation.x := o.x;
				orientation.y := o.y;
				orientation.z := o.z;
				r.Read(p, Sizeof(TVec3));
				position.x := p.x;
				position.y := p.y;
				position.z := p.z;
				bone_part := r.ReadWord;
			end;
		FreeAndNil(r);
		
		r := reader.OpenChunk(CHUNK_LOCATORS);
		count := r.ReadWord;
		SetLength(locators, count);
		for I := 0 to count-1 do
			with locators[I] do
			begin
				name := r.ReadStringZ;
				parent_name := r.ReadStringZ;
				r.Read(orientation, Sizeof(TVec3));
				r.Read(position, Sizeof(TVec3));
				flags := 0;
			end;
		FreeAndNil(r);
		
		r := reader.OpenChunk(CHUNK_BONE_PARTS);
		count := r.ReadWord;
		SetLength(bone_parts, count);
		for I := 0 to count-1 do
			with bone_parts[I] do
			begin
				name := r.ReadStringZ;
				SetLength(weights, Length(bones));
				r.Read(weights[0], Length(bones));
			end;
		FreeAndNil(r);
		
		r := reader.OpenChunk(CHUNK_ANIM_PATH);
		if r <> nil then
		begin
			anim_path := r.ReadStringZ;
			FreeAndNil(r);
		end;
	finally
		r.Free;
	end;
	
	for I := 0 to Length(bones) - 1 do
		bones[I].parent_id := GetBoneID(bones[I].parent_name);
	for I := 0 to Length(locators) - 1 do
		locators[I].parent_id := GetBoneID(locators[I].parent_name);
end;

procedure Quaterion2Angles(out angles : TVec3; const q : TVec4);
var
	yaw, pitch, roll : Single;
begin
	// copy-paste from https://stackoverflow.com/questions/5782658/extracting-yaw-from-a-quaternion
	yaw 	:= arctan2(2.0*(q.y*q.z + q.w*q.x), q.w*q.w - q.x*q.x - q.y*q.y + q.z*q.z);
	pitch := arcsin(-2.0*(q.x*q.z - q.w*q.y));
	roll 	:= arctan2(2.0*(q.x*q.y + q.w*q.z), q.w*q.w + q.x*q.x - q.y*q.y - q.z*q.z);
	
	angles.x := PI*2-yaw;
	angles.y := PI*2-Pitch;
	angles.z := PI*2-roll;
end;

procedure T4ASkeleton.LoadKonfig(r : TMemoryReader);
var
	k : TKonfig;
begin
	k := TKonfig.Create;
	try
		k.Load(r);
		LoadKonfig(k);
	finally
		k.Free;
	end;
end;

procedure T4ASkeleton.LoadKonfig(k : TKonfig);
var
	reader : TKonfigReader;
	I, count : Longint;
	quat : TVec4; // TQuaterion
	
	skeleton : TKonfigReader;
	bones_arr, b : TKonfigReader;
	locators_arr, l : TKonfigReader;
	aux_bones_arr, ab : TKonfigReader;
	driven_bones_arr, drb : TKonfigReader;
	dynamic_bones_arr, dnb : TKonfigReader;
	partitions_arr, p : TKonfigReader;
	
	procedural : TKonfigReader;
begin
	reader := TKonfigReader.Create(k, nil);
	try
		skeleton := reader.ReadSection('skeleton');
		try
			version := skeleton.ReadU32('ver');
			crc := skeleton.ReadU32('crc');
			skeleton.ReadString('facefx');
			anim_path := skeleton.ReadString('motions');
			
			count := 0; // suppress compiler warning
			
			bones_arr := skeleton.ReadArray('bones', @count);
			try
				SetLength(bones, count);
				for I := 0 to count-1 do
				begin
					b := bones_arr.ReadSection('', False);
					try
						bones[I].name := b.ReadString('name');
						bones[I].parent_name := b.ReadString('parent');
						quat := b.ReadVec4('q');
						Quaterion2Angles(bones[I].orientation, quat);
						bones[I].q := quat;
						bones[I].position := b.ReadVec3('t');
						bones[I].bone_part := b.ReadU16('bp');
					finally
						b.Free;
					end;
				end;
			finally
				bones_arr.Free;
			end;
			
			locators_arr := skeleton.ReadArray('locators', @count);
			try
				SetLength(locators, count);
				for I := 0 to count-1 do
				begin
					l := locators_arr.ReadSection('', False);
					try
						locators[I].name := l.ReadString('name');
						locators[I].parent_name := l.ReadString('parent');
						quat := l.ReadVec4('q');
						Quaterion2Angles(locators[I].orientation, quat);
						locators[I].q := quat;
						locators[I].position := l.ReadVec3('t');
						locators[I].flags := l.ReadU8('fl', 'bool8');
					finally
						l.Free;
					end;
				end;
			finally
				locators_arr.Free;
			end;
			
			if version >= 7 then
			begin
				aux_bones_arr := skeleton.ReadArray('aux_bones', @count);
				try
					for I := 0 to count-1 do
					begin
						ab := aux_bones_arr.ReadSection('', False);
						try
							ab.ReadString('name');
							ab.ReadString('parent');
							ab.ReadVec4('q');
							ab.ReadVec3('t');
						finally
							ab.Free;
						end;
					end;
				finally
					aux_bones_arr.Free;
				end;
			end;
			
			if version >= 11 then // Arktika.1
			begin
				procedural := skeleton.ReadSection('procedural');
				procedural.Free;
			end else // Redux
			begin
				if version >= 7 then
				begin
					driven_bones_arr := skeleton.ReadArray('driven_bones', @count);
					try
						for I := 0 to count-1 do
						begin
							drb := driven_bones_arr.ReadSection('', False);
							try
								drb.ReadHintStr('bone', 'choose');
								drb.ReadHintStr('driver', 'choose');
								drb.ReadHintStr('driver_parent', 'choose');
								drb.ReadU8('component');
								drb.ReadString('twister');
								drb.ReadFP32('value_min');
								drb.ReadFP32('value_max');
							finally
								drb.Free;
							end;
						end;
					finally
						driven_bones_arr.Free;
					end;
				end;
			
				if version >= 8 then
				begin
					dynamic_bones_arr := skeleton.ReadArray('dynamic_bones', @count);
					try
						for I := 0 to count-1 do
						begin
							dnb := dynamic_bones_arr.ReadSection('', False);
							try
								dnb.ReadHintStr('bone', 'choose');
								dnb.ReadFP32('inertia');
								dnb.ReadFP32('damping');
								dnb.ReadVec3('contraints');
							finally
								dnb.Free;
							end;
						end;
					finally
						dynamic_bones_arr.Free;
					end;
				end;
			end;
			
			partitions_arr := skeleton.ReadArray('partitions', @count);
			try
				SetLength(bone_parts, count);
				for I := 0 to count-1 do
				begin
					p := partitions_arr.ReadSection('', False);
					try
						bone_parts[I].name := p.ReadString('name');
						bone_parts[I].weights := p.ReadU8Array('infl');
					finally
						p.Free;
					end;
				end;
			finally
				partitions_arr.Free;
			end;
		finally
			skeleton.Free;
		end;
	finally
		reader.Free;
	end;
	
	for I := 0 to Length(bones) - 1 do
		bones[I].parent_id := GetBoneID(bones[I].parent_name);
	for I := 0 to Length(locators) - 1 do
		locators[I].parent_id := GetBoneID(locators[I].parent_name);
end;

function T4ASkeleton.GetBoneID(const name : String) : Longint;
var
	I : Longint;
begin
	Result := -1;
	
	for I := 0 to Length(bones)-1 do
		if bones[I].name = name then
		begin
			Result := I;
			Exit;
		end;
end;

function T4ASkeleton.GetLocatorID(const name : String) : Longint;
var
	I : Longint;
begin
	Result := -1;
	
	for I := 0 to Length(locators)-1 do
		if locators[I].name = name then
		begin
			Result := I;
			Exit;
		end;
end;

// translate * rotatey * rotatex * rotatez
procedure MatrixBone2033(out m : TMatrix; yaw, pitch, roll : Single; x, y, z : Single);
var
	sh, ch, sp, cp, sb, cb : Single;
begin
	sh := Sin(yaw);
	ch := Cos(yaw);
	sp := Sin(pitch);
	cp := Cos(pitch);
	sb := Sin(roll);
	cb := Cos(roll);

	m[1][1] := ch * cb + sh * sp * sb;
	m[1][2] := sb * cp;
	m[1][3] := -sh * cb + ch * sp * sb;
	m[1][4] := 0.0;
	m[2][1] := -ch * sb + sh * sp * cb;
	m[2][2] := cb * cp;
	m[2][3] := sb * sh + ch * sp * cb;
	m[2][4] := 0.0;
	m[3][1] := sh * cp;
	m[3][2] := -sp;
	m[3][3] := ch * cp;
	m[3][4] := 0.0;
	m[4][1] := x;
	m[4][2] := y;
	m[4][3] := z;
	m[4][4] := 1.0;
end;

procedure MatrixQuaterion(out m : TMatrix; const q : TVec4; x, y, z : Single);
begin
	RotateQuaternion(m, q);
	m[4,1] := x; m[4,2] := y; m[4,3] := z; m[4,4] := 1.0;
end;

function T4ASkeleton.GetTransform(const name : String; out transform : TMatrix) : Boolean;
var
	I : Longint;
begin
	I := GetBoneID(name);
	if I <> -1 then
	begin
		Result := GetBoneTransform(I, transform);
	end else
	begin
		I := GetLocatorID(name);
		if I <> -1 then		
			Result := GetLocatorTransform(I, transform)
		else
			Result := False;
	end;
end;

function T4ASkeleton.GetTransformLocal(const name : String; out transform : TMatrix) : Boolean;
var
	I : Longint;
begin
	I := GetBoneID(name);
	if I <> -1 then
	begin
		Result := GetBoneTransformLocal(I, transform);
	end else
	begin
		I := GetLocatorID(name);
		if I <> -1 then		
			Result := GetLocatorTransformLocal(I, transform)
		else
			Result := False;
	end;
end;

function T4ASkeleton.GetBoneTransform(id : Longint; out transform : TMatrix) : Boolean;
var
	mat, t : TMatrix;
begin
	if (id >= 0) and (id < Length(bones)) then
	begin
		if bones[id].parent_id <> -1 then
			GetBoneTransform(bones[id].parent_id, mat)
		else
			Identity(mat);

		if version >= 5 then
		begin
			with bones[id] do
				MatrixQuaterion(t, 
				q, 
				position.x, position.y, position.z);
				
			Mul44(mat, t);
		end else
		begin
			with bones[id] do
				MatrixBone2033(t, 
				orientation.y, orientation.x, orientation.z, 
				position.x, position.y, position.z);
				
			Mul44(mat, t);
		end;
			
		transform := mat;
		Result := True;
	end else
		Result := False;
end;

function T4ASkeleton.GetBoneTransformLocal(id : Longint; out transform : TMatrix) : Boolean;
var
	t : TMatrix;
begin
	if (id >= 0) and (id < Length(bones)) then
	begin
		if version >= 5 then
		begin
			with bones[id] do
				MatrixQuaterion(t, 
				q, 
				position.x, position.y, position.z);
		end else
		begin
			with bones[id] do
				MatrixBone2033(t, 
				orientation.y, orientation.x, orientation.z, 
				position.x, position.y, position.z);
		end;
			
		transform := t;
		Result := True;
	end else
		Result := False;
end;

function T4ASkeleton.GetLocatorTransform(id : Longint; out transform : TMatrix) : Boolean;
var
	mat, t : TMatrix;
begin
	if (id >= 0) and (id < Length(locators)) then
	begin
		if locators[id].parent_id <> -1 then
			GetBoneTransform(locators[id].parent_id, mat)
		else
			Identity(mat);

		if version >= 5 then
		begin
			with locators[id] do
				MatrixQuaterion(t, 
				q, 
				position.x, position.y, position.z);
				
			Mul44(mat, t);
		end else
		begin
			with locators[id] do
				MatrixBone2033(t, 
				orientation.y, orientation.x, orientation.z, 
				position.x, position.y, position.z);
				
			Mul44(mat, t);
		end;
			
		transform := mat;
		Result := True;
	end else
		Result := False;
end;

function T4ASkeleton.GetLocatorTransformLocal(id : Longint; out transform : TMatrix) : Boolean;
var
	t : TMatrix;
begin
	if (id >= 0) and (id < Length(locators)) then
	begin
		if version >= 5 then
		begin
			with locators[id] do
				MatrixQuaterion(t, 
				q, 
				position.x, position.y, position.z);
		end else
		begin
			with locators[id] do
				MatrixBone2033(t, 
				orientation.y, orientation.x, orientation.z, 
				position.x, position.y, position.z);
		end;
			
		transform := t;
		Result := True;
	end else
		Result := False;
end;

end.