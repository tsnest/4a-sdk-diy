unit motion;

interface
uses vmath, chunkedFile;

type 
	I4AMotion = class
		function  LengthSec : Single; virtual; abstract;
		function  AffectsBone(id : Longint) : Boolean; virtual; abstract;
		procedure GetTransform(id : Longint; t : Single; out m : TMatrix); virtual; abstract;
	end;

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

	T4AMotion = class(I4AMotion)
		version : Longint;
		skeleton_crc : Longint;
		
		unk1 : Word;
		speed : Single;
		accrue : Single;
		falloff : Single;
		frame_count : Longint;
		unk2 : Longint;
		unk3 : set of Byte;
		affected_bones : set of Byte;
		
		data : array of TBoneMotion;
	
		constructor CreateAndLoad(const fn : String); overload;
		constructor CreateAndLoad(reader : TMemoryReader); overload;
	
		procedure Load(const fn : String); overload;
		procedure Load(reader : TMemoryReader); overload;
		
		procedure Save(w : TMemoryWriter);
		
		function  LengthSec : Single; override;
		function  AffectsBone(id : Longint) : Boolean; override;
		procedure GetTransform(id : Longint; t : Single; out m : TMatrix); override;
	end;

type
	TBoneMotionLL = record
		rot_type : Byte;
		rot_time_divisor : Single;
		rot_time : array of Word;
		rot_value : array of Smallint;
		rot_constant : TVec4;
		
		pos_type : Byte;
		pos_time_divisor : Single;
		pos_time : array of Word;
		pos_origin : TVec3;
		pos_scale : TVec3;
		pos_value : array of Smallint;
		{
		scale_type : Byte;
		scale_time_divisor : Single;
		scale_time : array of Word;
		scale_origin : TVec3;
		scale_scale : TVec3;
		scale_value : array of Smallint;
		}
	end;
	
	T4AMotionLL = class(I4AMotion)
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
		
		motions_size : Longword;
		curves_offset : Longword;
	
		affected_bones2 : set of Byte;
	
		data : array of TBoneMotionLL;
		
		constructor CreateAndLoad(const fn : String); overload;
		constructor CreateAndLoad(reader : TMemoryReader); overload;
		
		procedure Load(const fn : String); overload;
		procedure Load(reader : TMemoryReader); overload;
		
		function  LengthSec : Single; override;
		function  AffectsBone(id : Longint) : Boolean; override;
		procedure GetTransform(id : Longint; t : Single; out m : TMatrix); override;
	end;

implementation
uses Math, uCrc, aiQuaternion;

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
		
		p.x := data[id].position_origin.x + x * data[id].position_scale.x;
		p.y := data[id].position_origin.y + y * data[id].position_scale.y;
		p.z := data[id].position_origin.z + z * data[id].position_scale.z;
	end else
		p := data[id].position_origin;
		
	RotateQuaternion(m, q);
	m[4,1] := p.x; m[4,2] := p.y; m[4,3] := p.z;
end;

constructor T4AMotionLL.CreateAndLoad(const fn : String);
begin
	inherited Create;
	Load(fn);
end;

constructor T4AMotionLL.CreateAndLoad(reader : TMemoryReader);
begin
	inherited Create;
	Load(reader);
end;

procedure T4AMotionLL.Load(const fn : String); overload;
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

procedure T4AMotionLL.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
	I, J : Longint;
	
	num_bones : Longint;
	offsets : array of Longword;
	
	header : LongWord;
	num_points : Longint;
	
	temp : array[0..3] of Longword;
	
	isLE : Boolean;
	function _BEtoNw(w:Word):Word;
	begin
		if isLE then _BEtoNw := w
		else _BEtoNw := BEtoN(w);
	end;
	function _BEtoNl(w:LongWord):LongWord;
	begin
		if isLE then _BEtoNl := w
		else _BEtoNl := BEtoN(w);
	end;	
begin
	r := reader.OpenChunk(0);
	try
		version := r.ReadLongword;
		skeleton_crc := r.ReadLongword;
		num_bones := r.ReadWord;
		SetLength(data, num_bones);
		// TODO incomplete
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
		r.Read(unk3, 16);
		motions_size := r.ReadLongword;
		curves_offset := r.ReadLongword;
		r.Read(affected_bones, 16);
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(9);
	try
		temp[0] := BEtoN(r.ReadLongword);
		temp[1] := BEtoN(r.ReadLongword);
		temp[2] := BEtoN(r.ReadLongword);
		temp[3] := BEtoN(r.ReadLongword);
		Move(temp, affected_bones2, 16);
		
		Inc(r.pos, 16); // skip funky stuff
		SetLength(offsets, num_bones*3);
		
		// read curve offsets ( Is it really necessary? I think curves go in fixed order in any way )
		for I := 0 to num_bones - 1 do
		begin
			if AffectsBone(I) then
			begin
				offsets[I*3  ] := BEtoN(r.ReadLongword);
				offsets[I*3+1] := BEtoN(r.ReadLongword);
				if self.version >= 15 then
					offsets[I*3+2] := BEtoN(r.ReadLongword);
			end;
		end;
		
		// read curves
		for I := 0 to num_bones - 1 do
		begin
			if AffectsBone(I) then
			begin
				// read bone rotation 
				if offsets[I*3  ] > r.size then
				begin
					isLE := True;
					r.pos := BEtoN(offsets[I*3  ]);
				end else
				begin
					isLE := False;
					r.pos := offsets[I*3  ];
				end;
				
				header := _BEtoNl(r.ReadLongword);
				num_points := (header and $FFFF);
				
				data[I].rot_type := (header shr 16) and $000F;
				case data[I].rot_type of
					2: begin // constant
						data[I].rot_constant.x := Single(_BEtoNl(r.ReadLongword));
						data[I].rot_constant.y := Single(_BEtoNl(r.ReadLongword));
						data[I].rot_constant.z := Single(_BEtoNl(r.ReadLongword));
						data[I].rot_constant.w := Single(_BEtoNl(r.ReadLongword));
					end;
					5: begin // compressed quaternion
						SetLength(data[I].rot_time, num_points);
						SetLength(data[I].rot_value, num_points*3);
						
						data[I].rot_time_divisor := Single(_BEtoNl(r.ReadLongword));
						
						for J := 0 to num_points - 1 do
							data[I].rot_time[J] := _BEtoNw(r.ReadWord);
						
						for J := 0 to num_points - 1 do
						begin
							data[I].rot_value[J*3  ] := _BEtoNw(r.ReadWord);
							data[I].rot_value[J*3+1] := _BEtoNw(r.ReadWord);
							data[I].rot_value[J*3+2] := _BEtoNw(r.ReadWord);
						end;		
					end;
					7: ; // empty
					else
						WriteLn('Unsupported rotation type ', data[I].rot_type);
				end;
				
				// read bone position
				if offsets[I*3+1] > r.size then
				begin
					isLE := True;
					r.pos := BEtoN(offsets[I*3+1]);
				end else
				begin
					isLE := False;
					r.pos := offsets[I*3+1];
				end;
				
				header := _BEtoNl(r.ReadLongword);
				num_points := (header and $FFFF);
				
				data[I].pos_type := (header shr 16) and $000F;
				case data[I].pos_type of
					2: begin // constant
						data[I].pos_origin.x := Single(_BEtoNl(r.ReadLongword));
						data[I].pos_origin.y := Single(_BEtoNl(r.ReadLongword));
						data[I].pos_origin.z := Single(_BEtoNl(r.ReadLongword));
					end;
					4: begin // compressed position
						SetLength(data[I].pos_time, num_points);
						SetLength(data[I].pos_value, num_points*3);
						
						data[I].pos_time_divisor := Single(_BEtoNl(r.ReadLongword));
						
						data[I].pos_scale.x := Single(_BEtoNl(r.ReadLongword));
						data[I].pos_scale.y := Single(_BEtoNl(r.ReadLongword));
						data[I].pos_scale.z := Single(_BEtoNl(r.ReadLongword));
						
						data[I].pos_origin.x := Single(_BEtoNl(r.ReadLongword));
						data[I].pos_origin.y := Single(_BEtoNl(r.ReadLongword));
						data[I].pos_origin.z := Single(_BEtoNl(r.ReadLongword));	
						
						for J := 0 to num_points - 1 do
							data[I].pos_time[J] := _BEtoNw(r.ReadWord);			
						
						for J := 0 to num_points - 1 do
						begin
							data[I].pos_value[J*3  ] := _BEtoNw(r.ReadSmallint);
							data[I].pos_value[J*3+1] := _BEtoNw(r.ReadSmallint);
							data[I].pos_value[J*3+2] := _BEtoNw(r.ReadSmallint);
						end;		
					end;
					7: ; // empty
					else
						WriteLn('Unsupported position type ', data[I].pos_type);
				end;	
				
				// read bone scale
				{
				if version >= 15 then
				begin
					if offsets[I*3+2] > r.size then
					begin
						isLE := True;
						r.pos := BEtoN(offsets[I*3+2]);
					end else
					begin
						isLE := False;
						r.pos := offsets[I*3+2];
					end;
					
					header := _BEtoNl(r.ReadLongword);
					num_points := (header and $FFFF);
					
					data[I].scale_type := (header shr 16) and $000F;
					case data[I].scale_type of
						2: begin // constant
							data[I].scale_origin.x := Single(_BEtoNl(r.ReadLongword));
							data[I].scale_origin.y := Single(_BEtoNl(r.ReadLongword));
							data[I].scale_origin.z := Single(_BEtoNl(r.ReadLongword));
						end;
						4: begin // compressed position
							SetLength(data[I].scale_time, num_points);
							SetLength(data[I].scale_value, num_points*3);
							
							data[I].scale_time_divisor := Single(_BEtoNl(r.ReadLongword));
							
							data[I].scale_scale.x := Single(_BEtoNl(r.ReadLongword));
							data[I].scale_scale.y := Single(_BEtoNl(r.ReadLongword));
							data[I].scale_scale.z := Single(_BEtoNl(r.ReadLongword));
							
							data[I].scale_origin.x := Single(_BEtoNl(r.ReadLongword));
							data[I].scale_origin.y := Single(_BEtoNl(r.ReadLongword));
							data[I].scale_origin.z := Single(_BEtoNl(r.ReadLongword));	
							
							for J := 0 to num_points - 1 do
								data[I].scale_time[J] := _BEtoNw(r.ReadWord);			
							
							for J := 0 to num_points - 1 do
							begin
								data[I].scale_value[J*3  ] := _BEtoNw(r.ReadSmallint);
								data[I].scale_value[J*3+1] := _BEtoNw(r.ReadSmallint);
								data[I].scale_value[J*3+2] := _BEtoNw(r.ReadSmallint);
							end;		
						end;
						7: ; // empty
						else
							WriteLn('Unsupported scale type ', data[I].pos_type);
					end;
				end else
					data[I].scale_type := 7;
				}
			end; // if AffectsBone(I)
		end;
	finally
		r.Free;
	end;
end;

function T4AMotionLL.LengthSec : Single;
begin
	LengthSec := frame_count / 30;
end;

function T4AMotionLL.AffectsBone(id : Longint) : Boolean;
begin
	Result := (Byte(id) in affected_bones) and (Byte(id) in affected_bones2);
end;

procedure T4AMotionLL.GetTransform(id : Longint; t : Single; out m : TMatrix);
var
	I, L : Longint;
	
	q : TVec4;
	p : TVec3;
	//s : TVec3;
	
	q1 : TVec4;
	q2 : TVec4;
	
	p1 : TVec3;
	p2 : TVec3;
	p3 : TVec3;
	
	aq : TaiQuaternion absolute q;
	aq1 : TaiQuaternion absolute q1;
	aq2 : TaiQuaternion absolute q2;
	
	time1 : Single;
	time2 : Single;
	factor : Single;
	
	//matrix : TMatrix;
	
	function _Sqrt(v : Single) : Single;
	begin
		if v < 0.0001 then
			_Sqrt := 0.0
		else
			_Sqrt := Sqrt(v)
	end;
	
	function UnpackQuat(x, y, z : Smallint) : TVec4;
	var	
		rm : Longint;
		scale : Single;
		values : array[0..3] of Single;
	begin
		rm := ((x and $01) shl 1) or (y and $01);
		x := x and $FFFE;
		y := y and $FFFE;
		
		scale := (Sqrt(2.0) / 2.0) / 32767;
		
		values[0] := x * scale;
		values[1] := y * scale;
		values[2] := z * scale;
		values[3] := _Sqrt(1.0 - values[0]*values[0] - values[1]*values[1] - values[2]*values[2]);
		if (z and 1) <> 0 then
			values[3] := -values[3];
		
		case rm of
			0: begin
				Result.x := values[3];
				Result.y := values[0];
				Result.z := values[1];
				Result.w := values[2];
			end;
			1: begin
				Result.x := values[0];
				Result.y := values[3];
				Result.z := values[1];
				Result.w := values[2];
			end;
			2: begin
				Result.x := values[0];
				Result.y := values[1];
				Result.z := values[3];
				Result.w := values[2];
			end;
			3: begin
				Result.x := values[0];
				Result.y := values[1];
				Result.z := values[2];
				Result.w := values[3];
			end;
		end;
	end;
	
begin
	case data[id].rot_type of
		2: q := data[id].rot_constant;
		5: begin
			I := 0;
			L := Length(data[id].rot_time);
			while (I < L) and ((data[id].rot_time[I] / 65535) < t) do
				Inc(I);
				
			if I = 0 then
			begin
				// copy first frame
				q := UnpackQuat(
					data[id].rot_value[0], 
					data[id].rot_value[1], 
					data[id].rot_value[2]
				);
			end else
			if I = L then
			begin
				// copy last frame
				q := UnpackQuat(
					data[id].rot_value[(L-1)*3+0], 
					data[id].rot_value[(L-1)*3+1], 
					data[id].rot_value[(L-1)*3+2]
				);
			end else
			begin
				// interpolate
				q1 := UnpackQuat(
					data[id].rot_value[(I-1)*3+0], 
					data[id].rot_value[(I-1)*3+1], 
					data[id].rot_value[(I-1)*3+2]
				);
				
				q2 := UnpackQuat(
					data[id].rot_value[(I)*3+0], 
					data[id].rot_value[(I)*3+1], 
					data[id].rot_value[(I)*3+2]
				);
				
				time1 := data[id].rot_time[I-1] / 65535;
				time2 := data[id].rot_time[I  ] / 65535;
				
				aq := aiQuaternion.Interpolate(aq1, aq2, (t - time1) / (time2 - time1));
				Normalize(q);
			end;
		end;
		7: begin
			q.x := 0;
			q.y := 0;
			q.z := 0;
			q.w := 1;
		end;
	end;
	
	case data[id].pos_type of
		2: p := data[id].pos_origin;
		4: begin
			I := 0;
			L := Length(data[id].pos_time);
			while (I < L) and ((data[id].pos_time[I] / 65535) < t) do
				Inc(I);
				
			if I = 0 then
			begin
				// copy first frame
				p.x := data[id].pos_origin.x + (data[id].pos_value[0] / 32767) * data[id].pos_scale.x;
				p.y := data[id].pos_origin.y + (data[id].pos_value[1] / 32767) * data[id].pos_scale.y;
				p.z := data[id].pos_origin.z + (data[id].pos_value[2] / 32767) * data[id].pos_scale.z;
			end else
			if I = L then
			begin
				// copy last frame
				p.x := data[id].pos_origin.x + (data[id].pos_value[(L-1)*3  ] / 32767) * data[id].pos_scale.x;
				p.y := data[id].pos_origin.y + (data[id].pos_value[(L-1)*3+1] / 32767) * data[id].pos_scale.y;
				p.z := data[id].pos_origin.z + (data[id].pos_value[(L-1)*3+2] / 32767) * data[id].pos_scale.z;
			end else
			begin
				// interpolate
				p1.x := data[id].pos_value[(I-1)*3  ] / 32767;
				p1.y := data[id].pos_value[(I-1)*3+1] / 32767;
				p1.z := data[id].pos_value[(I-1)*3+2] / 32767;
			
				p2.x := data[id].pos_value[I*3  ] / 32767;
				p2.y := data[id].pos_value[I*3+1] / 32767;
				p2.z := data[id].pos_value[I*3+2] / 32767;
				
				time1 := data[id].pos_time[I-1] / 65535;
				time2 := data[id].pos_time[I  ] / 65535;
				factor := (t - time1) / (time2 - time1);
				
				p3.x := p1.x + (p2.x - p1.x) * factor;
				p3.y := p1.y + (p2.y - p1.y) * factor;
				p3.z := p1.z + (p2.z - p1.z) * factor;
				
				p.x := data[id].pos_origin.x + p3.x * data[id].pos_scale.x;
				p.y := data[id].pos_origin.y + p3.y * data[id].pos_scale.y;
				p.z := data[id].pos_origin.z + p3.z * data[id].pos_scale.z;
			end;
		end;
		7: begin
			p.x := 0;
			p.y := 0;
			p.z := 0;
		end;
	end;
	{
	case data[id].scale_type of
		2: p := data[id].scale_origin;
		4: begin
			I := 0;
			L := Length(data[id].scale_time);
			while (I < L) and ((data[id].scale_time[I] / 65535) < t) do
				Inc(I);
				
			if I = 0 then
			begin
				// copy first frame
				s.x := data[id].scale_origin.x + (data[id].scale_value[0] / 32767) * data[id].scale_scale.x;
				s.y := data[id].scale_origin.y + (data[id].scale_value[1] / 32767) * data[id].scale_scale.y;
				s.z := data[id].scale_origin.z + (data[id].scale_value[2] / 32767) * data[id].scale_scale.z;
			end else
			if I = L then
			begin
				// copy last frame
				s.x := data[id].scale_origin.x + (data[id].scale_value[(L-1)*3  ] / 32767) * data[id].scale_scale.x;
				s.y := data[id].scale_origin.y + (data[id].scale_value[(L-1)*3+1] / 32767) * data[id].scale_scale.y;
				s.z := data[id].scale_origin.z + (data[id].scale_value[(L-1)*3+2] / 32767) * data[id].scale_scale.z;
			end else
			begin
				// interpolate
				p1.x := data[id].scale_value[(I-1)*3  ] / 32767;
				p1.y := data[id].scale_value[(I-1)*3+1] / 32767;
				p1.z := data[id].scale_value[(I-1)*3+2] / 32767;
			
				p2.x := data[id].scale_value[I*3  ] / 32767;
				p2.y := data[id].scale_value[I*3+1] / 32767;
				p2.z := data[id].scale_value[I*3+2] / 32767;
				
				time1 := data[id].scale_time[I-1] / 65535;
				time2 := data[id].scale_time[I  ] / 65535;
				factor := (t - time1) / (time2 - time1);
				
				p3.x := p1.x + (p2.x - p1.x) * factor;
				p3.y := p1.y + (p2.y - p1.y) * factor;
				p3.z := p1.z + (p2.z - p1.z) * factor;
				
				s.x := data[id].scale_origin.x + p3.x * data[id].scale_scale.x;
				s.y := data[id].scale_origin.y + p3.y * data[id].scale_scale.y;
				s.z := data[id].scale_origin.z + p3.z * data[id].scale_scale.z;
			end;
		end;
		7: begin
			s.x := 1;
			s.y := 1;
			s.z := 1;
		end;
	end;
	}	
	RotateQuaternion(m, q);
	m[4,1] := p.x; m[4,2] := p.y; m[4,3] := p.z;
	
	//Scale(matrix, s);
	//Mul44(m, matrix);
end;


end.
