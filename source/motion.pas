unit motion;

interface
uses vmath, chunkedFile;

const
	flUsePosition = $01;
	flUseRotation = $02;
	flModernRotation = $20;

type
	TBoneMotion = record
		flags : Byte;
		rotation : array of Smallint;
		position : array of Smallint;
		position_origin : TVec3;
		position_scale : TVec3;
	end;

	T4AMotion = class
		version : Longint;
		skeleton_crc : Longint;
		affected_bones : set of Byte;
		
		unk1 : Word;
		speed : Single;
		accrue : Single;
		falloff : Single;
		frame_count : Longint;
		unk2 : Longint;
		unk3 : set of Byte;
		
		data : array of TBoneMotion;
	
		constructor CreateAndLoad(const fn : String); overload;
		constructor CreateAndLoad(reader : TMemoryReader); overload;
	
		procedure Load(const fn : String); overload;
		procedure Load(reader : TMemoryReader); overload;
		
		procedure Save(w : TMemoryWriter);
		
		function  LengthSec : Single;
		function  AffectsBone(id : Longint) : Boolean;
		procedure GetTransform(id : Longint; t : Single; out m : TMatrix);
	end;

implementation
uses Math, uCrc; // for Min, Max

constructor T4AMotion.CreateAndLoad(const fn : String);
begin
	inherited Create;
	Load(fn);
end;

constructor T4AMotion.CreateAndLoad(reader : TMemoryReader);
begin
	inherited Create;
	Load(reader);
end;

procedure T4AMotion.Load(const fn : String); overload;
var
	r : TMemoryReader;
begin
	r := TMemoryReader.CreateFromFile(fn);
	try
		Load(r);
	finally
		r.Free;
	end;
end;

procedure T4AMotion.Load(reader : TMemoryReader); overload;
var
	I : Longint;
	r : TMemoryReader;
begin
	r := reader.OpenChunk(0);
	try
		version := r.ReadLongword;
		skeleton_crc := r.ReadLongword;
		SetLength(data, r.ReadWord);
		r.Read(affected_bones, 16);
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(1);
	
	try
		unk1 := r.ReadWord; // flags ?
		speed := r.ReadSingle;
		accrue := r.ReadSingle;
		falloff := r.ReadSingle;
		frame_count := r.ReadLongword;
		unk2 := r.ReadLongword; 
		r.Read(unk3, 32);
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(2);
	try
		for I := 0 to Length(data) - 1 do
			if AffectsBone(I) then
			begin
				data[I].flags := r.ReadByte;
				
				r.ReadLongword; // CRC
				r.ReadLongword; // Big-Endian CRC
				
				if (data[I].flags and flUseRotation) <> 0 then
					SetLength(data[I].rotation, frame_count*3)
				else
					SetLength(data[I].rotation, 3);
					
				r.Read(data[I].rotation[0], Length(data[I].rotation) * Sizeof(Smallint));
				
				if (data[I].flags and flUsePosition) <> 0 then
				begin
					r.ReadLongword; // CRC
					r.ReadLongword; // Big-Endian CRC
					SetLength(data[I].position, frame_count*3);
					r.Read(data[i].position[0], Length(data[I].position) * Sizeof(Smallint));
					
					r.Read(data[I].position_scale, Sizeof(TVec3));
					r.Read(data[I].position_origin, Sizeof(TVec3));
				end else
					r.Read(data[I].position_origin, Sizeof(TVec3));
			end;
	finally
		r.Free;
	end;
end;

procedure T4AMotion.Save(w : TMemoryWriter);
var
	I : Longint;
	
	function GetCRC(data : array of Smallint) : Longint;
	var buffer : String;
	begin
		// well, it's stupid, but i'm lazy
		SetLength(buffer, Length(data)*Sizeof(Smallint));
		Move(data[0], buffer[1], Length(buffer));
		Result := GetStringCrc(buffer);
	end;
	
	function GetCRC_BE(data : array of Smallint) : Longint;
	var
		data_be : array of Smallint;
		I : Longint;
	begin
		SetLength(data_be, Length(data));
		for I := 0 to Length(data_be)-1 do
			data_be[I] := NtoBE(data[I]);
			
		Result := GetCRC(data_be);
	end;
begin
	w.OpenChunk(0);
	
	w.WriteLongword(version);
	w.WriteLongword(skeleton_crc);
	w.WriteWord(Length(data)); // bone count
	w.Write(affected_bones, 16);
	
	w.CloseChunk;
	
	w.OpenChunk(1);
	
	w.WriteWord(unk1);
	w.WriteSingle(speed);
	w.WriteSingle(accrue);
	w.WriteSingle(falloff);
	w.WriteLongword(frame_count);
	w.WriteLongword(unk2);
	w.Write(unk3, 32);
	
	w.CloseChunk;
	
	w.OpenChunk(2);
	
	for I := 0 to Length(data) - 1 do
		if AffectsBone(I) then
		begin
			w.WriteByte(data[I].flags);
			
			w.WriteLongword(GetCRC(data[I].rotation)); // CRC
			w.WriteLongword(GetCRC_BE(data[I].rotation)); // Big-Endian CRC
				
			w.Write(data[I].rotation[0], Length(data[I].rotation) * Sizeof(Smallint));
			
			if (data[I].flags and flUsePosition) <> 0 then
			begin
				w.WriteLongword(GetCRC(data[I].position)); // CRC
				w.WriteLongword(GetCRC_BE(data[I].position)); // Big-Endian CRC
				
				w.Write(data[I].position[0], Length(data[I].position) * Sizeof(Smallint));
				
				w.Write(data[I].position_scale, Sizeof(TVec3));
				w.Write(data[I].position_origin, Sizeof(TVec3));
			end else
				w.Write(data[I].position_origin, Sizeof(TVec3));
		end;
	
	w.CloseChunk;
end;

function T4AMotion.LengthSec : Single;
begin
	LengthSec := frame_count / 30;
end;

function T4AMotion.AffectsBone(id : Longint) : Boolean;
begin
	Result := Byte(id) in affected_bones;
end;

procedure T4AMotion.GetTransform(id : Longint; t : Single; out m : TMatrix);
var
	I, rm : Longint;
	x, y, z : Smallint;
	
	q : TVec4;
	
	values : array[0..3] of Single;
	scale : Single;
	
	p : TVec3;
	
	function _Sqrt(v : Single) : Single;
	begin
		if v < 0.0001 then
			_Sqrt := 0.0
		else
			_Sqrt := Sqrt(v)
	end;
begin
	I := Trunc(t * frame_count);
	I := Min(I, frame_count-1);
	I := Max(I, 0);
	
	if (data[id].flags and flUseRotation) <> 0 then
	begin
		x := data[id].rotation[I*3  ];
		y := data[id].rotation[I*3+1];
		z := data[id].rotation[I*3+2];
	end else
	begin
		x := data[id].rotation[0];
		y := data[id].rotation[1];
		z := data[id].rotation[2];
	end;
	
	if (data[id].flags and flModernRotation) <> 0 then
	begin
		rm := ((x and $01) shl 1) or (y and $01);
		x := x and $FFFE;
		y := y and $FFFE;
		
		scale := (Sqrt(2.0) / 2.0) / 32767;

		values[0] := x * scale;
		values[1] := y * scale;
		values[2] := z * scale;
		values[3] := _Sqrt(1.0 - values[0]*values[0] - values[1]*values[1] - values[2]*values[2]);
		
		case rm of
			0: begin
				q.x := values[3];
				q.y := values[0];
				q.z := values[1];
				q.w := values[2];
			end;
			1: begin
				q.x := values[0];
				q.y := values[3];
				q.z := values[1];
				q.w := values[2];
			end;
			2: begin
				q.x := values[0];
				q.y := values[1];
				q.z := values[3];
				q.w := values[2];
			end;
			3: begin
				q.x := values[0];
				q.y := values[1];
				q.z := values[2];
				q.w := values[3];
			end;
		end;
	end else
	begin
		scale := 1.0 / 32767;
		
		q.x := x * scale;
		q.y := y * scale;
		q.z := z * scale;
		q.w := _Sqrt(1.0 - q.x*q.x - q.y*q.y - q.z*q.z);
	end;
	
	if (data[id].flags and flUsePosition) <> 0 then
	begin
		x := data[id].position[I*3  ];
		y := data[id].position[I*3+1];
		z := data[id].position[I*3+2];
		
		p.x := data[id].position_origin.x + (x / 32767) * data[id].position_scale.x;
		p.y := data[id].position_origin.y + (y / 32767) * data[id].position_scale.y;
		p.z := data[id].position_origin.z + (z / 32767) * data[id].position_scale.z;
	end else
		p := data[id].position_origin;
		
	RotateQuaternion(m, q);
	m[4,1] := p.x; m[4,2] := p.y; m[4,3] := p.z;
end;

end.