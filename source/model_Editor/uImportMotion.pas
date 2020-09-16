unit uImportMotion;

interface
uses chunkedFile, skeleton;

function ImportMotion(const path : String; skeleton : T4ASkeleton) : TMemoryWriter;

implementation

uses sysutils, uCrc, vmath, Iup, uImport, 
     assimp, aiAnimation, aiTypes, aiVector3D, aiQuaternion, aiScene;

function GetCRC(data : TMemoryWriter) : Longint;
var buffer : String;
begin
	// well, it's stupid, but i'm lazy
	SetLength(buffer, data.size);
	Move(data.data[0], buffer[1], data.size);
	Result := GetStringCrc(buffer);
end;

function FindBone(a : PAIAnimation; const name : String) : PAINodeAnim;
var
	I : Longint;
begin
	I := 0;
	while (I < a.mNumChannels) and (name <> aiStringToDelphiString(a.mChannels^[I].mNodeName)) do
		Inc(I);
		
	if I >= a.mNumChannels then
		raise Exception.Create('Bone ''' + name + ''' not found in animation ''' + aiStringToDelphiString(a.mName) + '''!');
		
	Result := a.mChannels^[I];
end;

function EvalTranslation(n : PAINodeAnim; time : Double) : TAIVector3D;
var
	J : Longint;
	t1, t2 : TAIVectorKey;
begin
	J := 0;
	while (J < n.mNumPositionKeys) and (n.mPositionKeys^[J].mTime < time) do
		Inc(J);
		
	if J = 0 then // just copy first key
		Result := n.mPositionKeys^[0].mValue
	else if J >= n.mNumPositionKeys then // just copy last key
		Result := n.mPositionKeys^[n.mNumPositionKeys-1].mValue
	else begin // interpolate
		t1 := n.mPositionKeys^[J-1];
		t2 := n.mPositionKeys^[J];
		
		Result := aiVector3D.Interpolate(t1.mValue, t2.mValue, (time - t1.mTime) / (t2.mTime - t1.mTime));
	end;
end;

function EvalRotation(n : PAINodeAnim; time : Double) : TAIQuaternion;
var
	J : Longint;
	q1, q2 : TAIQuatKey;
	
	//m : Single;
begin
	J := 0;
	while (J < n.mNumRotationKeys) and (n.mRotationKeys^[J].mTime < time) do
		Inc(J);
		
	if J = 0 then // just copy first key
		Result := n.mRotationKeys^[0].mValue
	else if J >= n.mNumPositionKeys then // just copy last key
		Result := n.mRotationKeys^[n.mNumRotationKeys-1].mValue
	else begin // interpolate
		q1 := n.mRotationKeys^[J-1];
		q2 := n.mRotationKeys^[J];
		
		Result := aiQuaternion.Interpolate(q1.mValue, q2.mValue, (time - q1.mTime) / (q2.mTime - q1.mTime));
	end;
	
	{
	// normalize
	m := Sqrt(q.w*q.w + q.x*q.x + q.z*q.z + q.y*q.y);
	q.w := q.w / m;
	q.x := q.x / m;
	q.y := q.y / m;
	q.z := q.z / m;
	}
	
	if Result.w < 0.0 then
		with Result do
		begin
			w := -w;
			x := -x;
			y := -y;
			z := -z;
		end;
end;

function FromAISceneToMotion(scene : PAIScene; motion_id : Longint; cflags : TConvFlags; s : T4ASkeleton) : TMemoryWriter;
var
	w : TMemoryWriter;
	motion : PAIAnimation;
	
	flags : Byte;
	
	index : Byte;
	mask : set of Byte;
	
	speed : Single;
	accrue : Single;
	falloff : Single;
	frame_count : Longint;
	
	bone : PAINodeAnim;
	useRotation, usePosition : Boolean;
	
	I, J : Longint;
	
	q : TAIQuaternion;
	t : TAIVector3D;
	
	t_keys : array of TAIVector3D;
	t_origin : TVec3;
	t_scale : TVec3;
	
	vec, half_size : TVec3;
	bb : TAABB;
	
	data : TMemoryWriter;
	xdata : TMemoryWriter; // big-endian, only for crc calculation
	
	x, y, z : Smallint;
begin
	motion := scene.mAnimations^[motion_id];

	WriteLN('mDuration = ', motion.mDuration);
	WriteLN('mTicksPerSecond = ', motion.mTicksPerSecond);
	
	useRotation := True;
	usePosition := True;
	speed := 1.0;
	accrue := 1.0;
	falloff := 1.0;
	
	if 0 = IupGetParam(
		'Options', nil, nil, 
		'Use rotation: %b'#10'Use position: %b'#10'Speed: %r'#10'Accrue(?): %r'#10'Falloff(?): %r'#10,
		@useRotation, @usePosition, @speed, @accrue, @falloff)
	then begin
		Result := nil;
		Exit;
	end;
	
	frame_count := Trunc(motion.mDuration / motion.mTicksPerSecond * 30.0); // force 30 FPS

	data := TMemoryWriter.Create;
	xdata := TMemoryWriter.Create;

	w := TMemoryWriter.Create;
	
	w.OpenChunk(0);
	w.WriteLongword(8); // version
	w.WriteLongword(s.crc);
	w.WriteWord(Length(s.bones));
	
	mask := [];
	for index := 0 to Length(s.bones)-1 do
		Include(mask, index);
	w.Write(mask, 16);
	
	w.CloseChunk;
	
	w.OpenChunk(1);
	w.WriteWord(0); // flags ?
	w.WriteSingle(speed);
	w.WriteSingle(accrue);
	w.WriteSingle(falloff);
	w.WriteLongword(frame_count); // frame count
	w.WriteLongword(0);
	
	mask := [];
	for index := 0 to 255 do
		Include(mask, index);
	w.Write(mask, 32);
		
	w.CloseChunk;
	
	w.OpenChunk(2);
	for I := 0 to Length(s.bones)-1 do
	begin
		flags := 0;
		if usePosition then
			flags := flags or $01;
		if useRotation then
			flags := flags or $02;
			
		w.WriteByte(flags);
		
		bone := FindBone(motion, s.bones[I].name);
		
		//for J := 0 to bone.mNumPositionKeys-1 do
		//	with bone.mPositionKeys^[J] do
		//		WriteLN('time ', mTime:3:3, ' X: ', mValue.x:3:3, ' Y: ', mValue.y:3:3, ' Z: ', mValue.z:3:3);
		
		// calculate rotation keys
		if useRotation then
		begin
			for J := 0 to frame_count-1 do
			begin
				q := EvalRotation(bone, J / (frame_count-1) * motion.mDuration);
			
				if cfMirrorZ in cflags then
					q.z := -q.z;
					
				x := Trunc(q.x * 32767);
				y := Trunc(q.y * 32767);
				z := Trunc(q.z * 32767);
				
				data.WriteSmallint(x);
				data.WriteSmallint(y);
				data.WriteSmallint(z);
				
				xdata.WriteSmallint(NtoBE(x));
				xdata.WriteSmallint(NtoBE(y));
				xdata.WriteSmallint(NtoBE(z));
			end;
		end else
		begin
			q := EvalRotation(bone, 0.0);
		
			if cfMirrorZ in cflags then
				q.z := -q.z;
				
			x := Trunc(q.x * 32767);
			y := Trunc(q.y * 32767);
			z := Trunc(q.z * 32767);
			
			data.WriteSmallint(x);
			data.WriteSmallint(y);
			data.WriteSmallint(z);
			
			xdata.WriteSmallint(NtoBE(x));
			xdata.WriteSmallint(NtoBE(y));
			xdata.WriteSmallint(NtoBE(z));
		end;
		
		// write rotation data
		w.WriteLongint(GetCRC(data));
		w.WriteLongint(GetCRC(xdata));
		w.Write(data.data[0], data.size);
		
		data.Clear;
		xdata.Clear;
		
		if usePosition then
		begin
			// calculate position keys & bounding box
			SetLength(t_keys, frame_count);
			for J := 0 to frame_count-1 do
			begin
				t_keys[J] := EvalTranslation(bone, J / (frame_count-1) * motion.mDuration);
				
				if cfMirrorZ in cflags then
					t_keys[J].z := -t_keys[J].z;
				
				Move(t_keys[J], vec, Sizeof(TVec3));
				if J = 0 then
				begin
					bb.min := vec;
					bb.max := vec;
				end else
					AABBMergePoint(bb, vec);	
			end;
			
			half_size.x := (bb.max.x - bb.min.x) / 2;
			half_size.y := (bb.max.y - bb.min.y) / 2;
			half_size.z := (bb.max.z - bb.min.z) / 2;
			
			AABBCenter(t_origin, bb);
			t_scale.x := 1 / (32767 / half_size.x);
			t_scale.y := 1 / (32767 / half_size.y);
			t_scale.z := 1 / (32767 / half_size.z);
			
			// write position data
			for J := 0 to frame_count-1 do
			begin
				t := t_keys[J];
				
				x := Trunc((t.x - t_origin.x) / t_scale.x);
				y := Trunc((t.y - t_origin.y) / t_scale.y);
				z := Trunc((t.z - t_origin.z) / t_scale.z);
				
				data.WriteSmallint(x);
				data.WriteSmallint(y);
				data.WriteSmallint(z);
				
				xdata.WriteSmallint(NtoBE(x));
				xdata.WriteSmallint(NtoBE(y));
				xdata.WriteSmallint(NtoBE(z));
			end;
			
			w.WriteLongint(GetCRC(data));
			w.WriteLongint(GetCRC(xdata));
			w.Write(data.data[0], data.size);
			
			data.Clear;
			xdata.Clear;	
			
			w.Write(t_scale, Sizeof(TVec3));
			w.Write(t_origin, Sizeof(TVec3));
		end else
		begin
			t := EvalTranslation(bone, 0.0);
			w.Write(t, Sizeof(TVec3));
		end;
		
	end;
	w.CloseChunk;
	
	Result := w;
	
	data.Free;
	xdata.Free;
end;

function ImportMotion(const path : String; skeleton : T4ASkeleton) : TMemoryWriter;
var
	scene : PAIScene;
	flags : TConvFlags;
	
	I : Longint;
	anims : array of String;
	ret : Longint;
begin
	Result := nil;
	scene := ImportAIScene(path, flags);
	try
		if scene^.mNumAnimations > 0 then
		begin	
			SetLength(anims, scene^.mNumAnimations);
			for I := 0 to Length(anims) - 1 do
				anims[I] := aiStringToDelphiString(scene^.mAnimations[I].mName);
			
			ret := iup.ListDialog('Select Animation', anims, 0, 24, 40);
			if ret <> -1 then
				Result := FromAISceneToMotion(scene, ret, flags, skeleton);
		end else
		begin
			IupMessage('Error', 'Imported scene doesn''t have any motion');
		end;
	finally
		aiReleaseImport(scene);
	end;
end;

end.