unit Konfig_reader;

interface
uses chunkedFile, Konfig, mujs, vmath;

type
	TStringRecoverFunc = function(crc : Longint; userdata : Pointer) : String;
	
type
	TU8Array = array of Byte;
	TU16Array = array of Word;
	TU32Array = array of Longword;
	TStrArray = array of String;
	TFP32Array = array of Single;

type
	TKonfigReader = class
		bin_flags : Byte;
		data : TMemoryReader;
		dictionary : array of String;
		dest : TSection;
		section_name : String;
		own_data : Boolean;
		
		WarnIfDataLeft : Boolean; static;
		PrintDebugInfo : Boolean; static;

		constructor Create(K : TKonfig; D : TSection); overload;
		constructor Create(parent : TKonfigReader; const name : String; r : TMemoryReader; D : TSection; own_data : Boolean = False); overload;
		destructor Destroy; override;

		function More : Boolean;

		function ReadSection(const name : String; name_check : Boolean = True) : TKonfigReader;
		function ReadArray(const name : String; count : PLongword = nil) : TKonfigReader;
		function ReadArray16(const name : String; count : PLongword = nil) : TKonfigReader;
		
		function TryReadSection(const name : String) : TKonfigReader;
		function TryReadArray(const name : String; count : PLongword = nil) : TKonfigReader;
		function TryReadArray16(const name : String; count : PLongword = nil) : TKonfigReader;

		function ReadName(const nm : String) : String;
		function ReadLang(const nm : String) : String;
		function ReadStringCrc(const nm : String; f : TStringRecoverFunc; userdata : Pointer = nil) : String;
		procedure ReadHint(const nm : String; const tp : String);
		function ReadHintStr(const nm : String; const tp : String) : String;

		function ReadS8(const nm : String; const tp : String = 's8') : Shortint;
		function ReadS16(const nm : String; const tp : String = 's16') : Smallint;
		function ReadS32(const nm : String; const tp : String = 's32') : Longint;
		function ReadS64(const nm : String; const tp : String = 's64') : Int64;

		function ReadU8(const nm : String; const tp : String = 'u8') : Byte;
		function ReadU16(const nm : String; const tp : String = 'u16') : Word;
		function ReadU32(const nm : String; const tp : String = 'u32') : Longword;
		function ReadU64(const nm : String; const tp : String = 'u64') : QWord;

		function ReadFP32(const nm : String; const tp : String = 'fp32') : Single;

		function ReadString(const nm : String; const tp : String = 'stringz') : String;
		function ReadBool(const nm : String; const tp : String = 'bool') : Boolean;

		function ReadVec2(const nm : String; const tp : String = 'vec2f') : TVec2;
		function ReadVec3(const nm : String; const tp : String = 'vec3f') : TVec3;
		function ReadVec4(const nm : String; const tp : String = 'vec4f') : TVec4;
		function ReadVec4S16(const nm : String; const tp : String = 'vec4s16') : TVec4S16;
		procedure ReadVec3i(const nm : String; const tp : String = 'vec3i');
		procedure ReadVec4i(const nm : String; const tp : String = 'vec3i');

		procedure ReadMatrix44(const nm : String; const tp : String = 'pose, matrix');
		procedure ReadMatrix43(const nm : String; const tp : String = 'pose, matrix_43T');
		
		function ReadU8Array(const nm : String; const tp : String = 'u8_array') : TU8Array;
		function ReadU16Array(const nm : String; const tp : String = 'u16_array') : TU16Array;
		function ReadU32Array(const nm : String; const tp : String = 'u32_array') : TU32Array;
		function ReadFP32Array(const nm : String; const tp : String = 'fp32_array') : TFP32Array;
		function ReadU16Array16(const nm : String; const tp : String = 'u16_array') : TU16Array;
		function ReadU32Array16(const nm : String; const tp : String = 'u32_array16') : TU32Array;
		function ReadFP32Array16(const nm : String; const tp : String = 'fp32_array16') : TFP32Array;
		
		function ReadStrArray16(const nm : String; const tp : String = 'str_array16') : TStrArray;
		function ReadStrArray32(const nm : String; const tp : String = 'str_array32') : TStrArray;

		protected

		procedure _ReadDebugInfo(const nm, tp : String; raiseiffail : Boolean = False);
		function _ReadString : String;
		function _ReadSection(const name : String; name_check : Boolean = True) : TKonfigReader;
	end;

procedure Script_Init(J : js_State);
procedure Script_Finish(J : js_State);
procedure Script_Push(J : js_State; K : TKonfigReader);

implementation
uses uCrc, sysutils;

constructor TKonfigReader.Create(K : TKonfig; D : TSection);
begin
	Inherited Create;

	bin_flags := K.kind;
	if (bin_flags and konfMultiChunk) = 0 then
		data := TMemoryReader.Create(K.data[0], Length(K.data))
	else
		data := TMemoryReader.Create(K.data[8], Length(K.data)-8); // cut arch_chunk_0 section
	dictionary := K.dictionary;
	dest := D;
	section_name := '<ROOT>';
	own_data := True;
end;

constructor TKonfigReader.Create(parent : TKonfigReader; const name : String; r : TMemoryReader; D : TSection; own_data : Boolean);
begin
	inherited Create;

	bin_flags := parent.bin_flags;
	data := r;
	dictionary := parent.dictionary;
	dest := D;
	section_name := name;
	self.own_data := own_data;
end;

destructor TKonfigReader.Destroy;
begin
	if WarnIfDataLeft AND More then 
		WriteLn('section ', section_name, ' data left');
	
	if own_data then
		data.Free;

	inherited;
end;

function TKonfigReader.More : Boolean;
begin
	More := data.pos < data.size;
end;

function TKonfigReader.ReadSection(const name : String; name_check : Boolean) : TKonfigReader;
var
	sect : TKonfigReader;
begin
	sect := _ReadSection(name, name_check);
	if sect <> nil then
	begin
		if Assigned(dest) then
			sect.dest := dest.AddSect(name);
		Result := sect;
	end else
		raise Exception.Create('cannot open section ''' + name + ''' (' + IntToHex(GetStringCrc(name), 8) + ')');
end;

function TKonfigReader.ReadArray(const name : String; count : PLongword) : TKonfigReader;
var
	arr : TKonfigReader;
	cnt : Longword;
begin
	ReadHint(name, 'array');
	arr := ReadSection(name);
	cnt := arr.ReadU32('count');
	
	if count <> nil then
		count^ := cnt;

	Result := arr;
end;

function TKonfigReader.ReadArray16(const name : String; count : PLongword) : TKonfigReader;
var
	arr : TKonfigReader;
	cnt : Longword;
begin
	ReadHint(name, 'array');
	arr := ReadSection(name);
	cnt := arr.ReadU16('count');
	
	if count <> nil then
		count^ := cnt;

	Result := arr;
end;

function TKonfigReader.TryReadSection(const name : String) : TKonfigReader;
var
	sect : TKonfigReader;
begin
	sect := _ReadSection(name, True);
	if sect <> nil then
	begin
		if Assigned(dest) then
			sect.dest := dest.AddSect(name);
			
		Result := sect;
	end else
		Result := nil;
end;

function TKonfigReader.TryReadArray(const name : String; count : PLongword) : TKonfigReader;
var
	saved_pos : Longword;
	sect : TKonfigReader;
	cnt : Longword;
begin
	saved_pos := data.pos;

	try 
		_ReadDebugInfo(name, 'array', True);
	except
		on E: Exception do
		begin
			data.pos := saved_pos;
			Result := nil;
			Exit;
		end
	end;
	
	sect := _ReadSection(name, True);
	if sect <> nil then
	begin
		if Assigned(dest) then
		begin
			dest.AddHint(name, 'array');
			sect.dest := dest.AddSect(name);
		end;
		
		cnt := sect.ReadU32('count');
		
		if count <> nil then
			count^ := cnt;
		
		Result := sect;
	end else
		Result := nil;
end;

function TKonfigReader.TryReadArray16(const name : String; count : PLongword) : TKonfigReader;
var
	saved_pos : Longword;
	sect : TKonfigReader;
	cnt : Longword;
begin
	saved_pos := data.pos;

	try 
		_ReadDebugInfo(name, 'array', True);
	except
		on E: Exception do
		begin
			data.pos := saved_pos;
			Result := nil;
			Exit;
		end
	end;
	
	sect := _ReadSection(name, True);
	if sect <> nil then
	begin
		if Assigned(dest) then
		begin
			dest.AddHint(name, 'array');
			sect.dest := dest.AddSect(name);
		end;
		
		cnt := sect.ReadU16('count');
		
		if count <> nil then
			count^ := cnt;
		
		Result := sect;
	end else
		Result := nil;
end;

function TKonfigReader.ReadName(const nm : String) : String;
var
	n : String;
begin
	ReadHint(nm, 'name');
	n := ReadString(nm, 'stringz');
	if Assigned(dest) then dest.name := n;

	Result := n;
end;

function TKonfigReader.ReadLang(const nm : String) : String;
var
	n : String;
begin
	ReadHint(nm, 'lang');
	n := ReadString(nm, 'stringz');
	if Assigned(dest) then dest.name := n;

	Result := n;
end;

function TKonfigReader.ReadStringCrc(const nm : String; f : TStringRecoverFunc; userdata : Pointer) : String;
var
	crc : Longint;
	n : String;
begin
	_ReadDebugInfo(nm, 'u32');
	crc := data.ReadLongint;
	n := f(crc, userdata);
	if Assigned(dest) then dest.AddStr(nm, n);
	Result := n;
end;

procedure TKonfigReader.ReadHint(const nm : String; const tp : String);
begin
	_ReadDebugInfo(nm, tp);
	if Assigned(dest) then dest.AddHint(nm, tp);
end;

function TKonfigReader.ReadHintStr(const nm : String; const tp : String) : String;
begin
	ReadHint(nm, tp);
	Result := ReadString(nm);
end;

function TKonfigReader.ReadS8(const nm : String; const tp : String) : Shortint;
var
	I : Shortint;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadShortint;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadS16(const nm : String; const tp : String) : Smallint;
var
	I : Smallint;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadSmallint;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadS32(const nm : String; const tp : String) : Longint;
var
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadLongint;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadS64(const nm : String; const tp : String) : Int64;
var
	I : Int64;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadInt64;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadU8(const nm : String; const tp : String) : Byte;
var
	I : Byte;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadByte;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadU16(const nm : String; const tp : String) : Word;
var
	I : Word;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadWord;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadU32(const nm : String; const tp : String) : Longword;
var
	I : Longword;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadLongword;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadU64(const nm : String; const tp : String) : QWord;
var
	I : QWord;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadQWord;
	if Assigned(dest) then dest.AddInt(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadFP32(const nm : String; const tp : String) : Single;
var
	I : Single;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadSingle;
	if Assigned(dest) then dest.AddFloat(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadString(const nm : String; const tp : String) : String;
var
	I : String;
begin
	_ReadDebugInfo(nm, tp);
	I := _ReadString;

	if Assigned(dest) then dest.AddStr(nm, I, tp);

	Result := I;
end;

function TKonfigReader.ReadBool(const nm : String; const tp : String) : Boolean;
var
	I : Boolean;
begin
	_ReadDebugInfo(nm, tp);
	I := data.ReadByte <> 0;
	if Assigned(dest) then dest.AddBool(nm, I, tp);
	Result := I;
end;

function TKonfigReader.ReadVec2(const nm : String; const tp : String) : TVec2;
var
	v : TVec2;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	if Assigned(dest) then dest.AddVec2(nm, v, tp);
	Result := v;
end;

function TKonfigReader.ReadVec3(const nm : String; const tp : String) : TVec3;
var
	v : TVec3;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	if Assigned(dest) then dest.AddVec3(nm, v, tp);
	Result := v;
end;

function TKonfigReader.ReadVec4(const nm : String; const tp : String) : TVec4;
var
	v : TVec4;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	if Assigned(dest) then dest.AddVec4(nm, v, tp);
	Result := v;
end;

function TKonfigReader.ReadVec4S16(const nm : String; const tp : String) : TVec4S16;
var
	v : TVec4S16;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(v, Sizeof(v));
	if Assigned(dest) then dest.AddVec4S16(nm, v, tp);
	Result := v;
end;

procedure TKonfigReader.ReadVec3i(const nm : String; const tp : String);
var
	v : TIntegerArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	v := TIntegerArrayValue.Create(nm, tp);
	v.FromReaderS32(data, 3);
	if Assigned(dest) then dest.items.Add(v);
end;

procedure TKonfigReader.ReadVec4i(const nm : String; const tp : String);
var
	v : TIntegerArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	v := TIntegerArrayValue.Create(nm, tp);
	v.FromReaderS32(data, 4);
	if Assigned(dest) then dest.items.Add(v);
end;

procedure TKonfigReader.ReadMatrix44(const nm : String; const tp : String);
var
	mat : array[1..16] of Single;
	v : TFloatArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(mat, Sizeof(mat));
	
	if Assigned(dest) then
	begin
		v := TFloatArrayValue.Create(nm, tp);
		SetLength(v.data, 16);
		Move(mat, v.data[0], Sizeof(mat));
		dest.items.Add(v);
	end;
end;

procedure TKonfigReader.ReadMatrix43(const nm : String; const tp : String);
var
	mat : array[1..12] of Single;
	v : TFloatArrayValue;
begin
	_ReadDebugInfo(nm, tp);
	data.Read(mat, Sizeof(mat));
	
	if Assigned(dest) then
	begin
		v := TFloatArrayValue.Create(nm, tp);
		SetLength(v.data, 12);
		Move(mat, v.data[0], Sizeof(mat));
		dest.items.Add(v);
	end;
end;

function TKonfigReader.ReadU8Array(const nm : String; const tp : String) : TU8Array;
var
	ret : TU8Array;
	arr : TByteArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadByte;
		
	if Assigned(dest) then 
	begin
		arr := TByteArrayValue.Create(nm, tp);
		arr.data := Copy(ret, 0, len);
		dest.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU16Array(const nm : String; const tp : String) : TU16Array;
var
	ret : TU16Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadWord;
		
	if Assigned(dest) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU32Array(const nm : String; const tp : String) : TU32Array;
var
	ret : TU32Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadLongword;
		
	if Assigned(dest) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadFP32Array(const nm : String; const tp : String) : TFP32Array;
var
	ret : TFP32Array;
	arr : TFloatArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadLongword;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadSingle;
		
	if Assigned(dest) then 
	begin
		arr := TFloatArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU16Array16(const nm : String; const tp : String) : TU16Array;
var
	ret : TU16Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadWord;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadWord;
		
	if Assigned(dest) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadU32Array16(const nm : String; const tp : String) : TU32Array;
var
	ret : TU32Array;
	arr : TIntegerArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadWord;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadLongword;
		
	if Assigned(dest) then 
	begin
		arr := TIntegerArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadFP32Array16(const nm : String; const tp : String) : TFP32Array;
var
	ret : TFP32Array;
	arr : TFloatArrayValue;
	len : Longint;
	I : Longint;
begin
	_ReadDebugInfo(nm, tp);
	
	len := data.ReadWord;
	SetLength(ret, len);
	
	for I := 0 to len - 1 do
		ret[I] := data.ReadSingle;
		
	if Assigned(dest) then 
	begin
		arr := TFloatArrayValue.Create(nm, tp);
		SetLength(arr.data, len);
		for I := 0 to len - 1 do
			arr.data[I] := ret[I];
		dest.items.Add(arr);
	end;
	
	Result := ret;
end;

function TKonfigReader.ReadStrArray16(const nm : String; const tp : String) : TStrArray;
var
	I, len : Longint;
	ret : TStrArray;
begin
	ReadHint(nm, tp);
	len := ReadU16(nm);
	SetLength(ret, len);
	for I := 0 to len-1 do
		ret[I] := ReadString(nm);
		
	Result := ret;
end;

function TKonfigReader.ReadStrArray32(const nm : String; const tp : String) : TStrArray;
var
	I, len : Longint;
	ret : TStrArray;
begin
	ReadHint(nm, tp);
	len := ReadU32(nm);
	SetLength(ret, len);
	for I := 0 to len-1 do
		ret[I] := ReadString(nm);
		
	Result := ret;
end;

procedure TKonfigReader._ReadDebugInfo(const nm, tp : String; raiseiffail : Boolean);
var
	n, t : String;
	
	procedure Error(const msg : String);
	begin
		if raiseiffail then
			raise Exception.Create(msg)
		else
			WriteLn('Error! ' + msg);
	end;
begin
	if PrintDebugInfo then
		WriteLn(nm, ' ', tp);
		
	if (bin_flags and konfDebugInfo) <> 0 then
	begin
		n := _ReadString;
		t := _ReadString;
		if (n <> nm) or (t <> tp) then
			Error('name or type doesn''t match! ' + nm + ' : ' + tp + ' <> ' + n + ' : ' + t);
	end;
end;

function TKonfigReader._ReadString : String;
var
	index : Longword;
begin
	if (bin_flags and konfDiktionary) <> 0 then
	begin
		index := data.ReadLongword;
		if index >= Length(dictionary) then
			raise Exception.Create('word index out of range');
		_ReadString := dictionary[index];
	end else
		_ReadString := data.ReadStringZ;
end;

function TKonfigReader._ReadSection(const name : String; name_check : Boolean) : TKonfigReader;
var
	crc : Longint;
	size : Longword;
	
	n : String;
	saved_pos : Longword;
	start_pos : Longword;
begin
	if (bin_flags and konfNoSections) <> 0 then
	begin
		// TODO ReadHint(name, 'section') ?
		Result := TKonfigReader.Create(
			self,
			name,
			data,
			nil,
			False
		);
		Exit;
	end;

	if data.size - data.pos < 8 then
	begin
		Result := nil;
		Exit;
	end;
	
	saved_pos := data.pos;

	crc := data.ReadLongint;
	if (not name_check) or (GetStringCrc(name) = crc) then
	begin
		size := data.ReadLongword;

		start_pos := data.pos;

		// TODO other kinds(LL skeleton)
		if (bin_flags and konfMultiChunk) = 0 then
		begin
			if data.ReadByte <> bin_flags then
				raise Exception.Create('buffer[0] <> bin_flags ololo blea');
		end;
		
		if (bin_flags and konfDebugInfo) <> 0 then
		begin
			n := _ReadString;
			if name_check and (name <> n) then
			begin
				data.pos := saved_pos;
				Result := nil;
				Exit;
			end;
		end;
		
		size := size - (data.pos - start_pos);

		Result := TKonfigReader.Create(
			self,
			name,
			TMemoryReader.Create((data.data+data.pos)^, size),
			nil
		);

		Inc(data.pos, size);
	end else
	begin
		data.pos := saved_pos;
		Result := nil;
	end;
end;

// ------------------------------------
// JS part
// ------------------------------------

const
	TAG = 'TKONFIGREADER';

procedure ArgsThis(J : js_State; var this : TKonfigReader);
begin
	this := TKonfigReader(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TKonfigReader');
end;

procedure ArgsThis1Str(J : js_State; var this : TKonfigReader; var s : PAnsiChar; def : PAnsiChar);
begin
	if js_gettop(J) >= 2 then 
		s := js_trystring(J, 1, def)
	else 
		s := def;

	this := TKonfigReader(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TKonfigReader');
		
	if s = nil then
		js_typeerror(J, 'missing argument 1 to call function');
end;

procedure ArgsThis2Str(J : js_State; var this : TKonfigReader; var s1, s2 : PAnsiChar; def1, def2 : PAnsiChar);
begin
	if js_gettop(J) >= 2 then 
		s1 := js_trystring(J, 1, def1)
	else 
		s1 := def1;

	if js_gettop(J) >= 3 then 
		s2 := js_trystring(J, 2, def2)
	else 
		s2 := def2;

	this := TKonfigReader(js_touserdata(J, 0, TAG));
	if this = nil then
		js_typeerror(J, 'cannot convert ''this'' to TKonfigReader');
		
	if s1 = nil then
		js_typeerror(J, 'missing argument 1 to call function');
	if s2 = nil then
		js_typeerror(J, 'missing argument 2 to call function');
end;

procedure S_Size(J : js_State); cdecl;
var
	this : TKonfigReader;
begin
	ArgsThis(J, this);
	
	try
		js_pushnumber(J, this.data.size)
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_More(J : js_State); cdecl;
var
	this : TKonfigReader;
begin
	ArgsThis(J, this);
	
	try
		js_pushboolean(J, Longint(this.More))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadSection(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
	check : Boolean;
begin
	if js_gettop(J) = 1 then
	begin
		// no args
		ArgsThis(J, this);
		name := '';
		check := False;
	end else
	begin
		ArgsThis1Str(J, this, name, nil);
	
		if js_gettop(J) >= 3 then check := (js_tryboolean(J, 2, 1) = 1)
		else check := True;
	end;

	try 
		Script_Push(J, this.ReadSection(name, check));
		
		if ((this.bin_flags and konfNoSections) <> 0) and (js_isnull(J, -1) = 0) then
		begin	
			// hack - store reference to parent object
			// to guarantee it's not freed before new object
			js_copy(J, 0);
			js_setproperty(J, -2, '____dataref');
		end;
		
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadArray(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
	count : Longword;
begin
	ArgsThis1Str(J, this, name, nil);

	try
		Script_Push(J, this.ReadArray(name, @count));
	
		js_pushnumber(J, count);
		js_setproperty(J, -2, 'count');
	
		if (this.bin_flags and konfNoSections) <> 0 then
		begin	
			// hack - store reference to parent object
			// to guarantee it's not freed before new object
			js_copy(J, 0);
			js_setproperty(J, -2, '____dataref');
		end;
	
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadArray16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
	count : Longword;
begin
	ArgsThis1Str(J, this, name, nil);

	try
		Script_Push(J, this.ReadArray16(name, @count));
	
		js_pushnumber(J, count);
		js_setproperty(J, -2, 'count');
	
		if (this.bin_flags and konfNoSections) <> 0 then
		begin	
			// hack - store reference to parent object
			// to guarantee it's not freed before new object
			js_copy(J, 0);
			js_setproperty(J, -2, '____dataref');
		end;
	
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_TryReadSection(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
begin
	ArgsThis1Str(J, this, name, nil);

	try
		Script_Push(J, this.TryReadSection(name));

		if ((this.bin_flags and konfNoSections) <> 0) and (js_isnull(J, -1) = 0) then
		begin	
			// hack - store reference to parent object
			// to guarantee it's not freed before new object
			js_copy(J, 0);
			js_setproperty(J, -2, '____dataref');
		end;
		
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_TryReadArray(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
	count : Longword;
begin
	ArgsThis1Str(J, this, name, nil);

	try
		Script_Push(J, this.TryReadArray(name, @count));
		
		if js_isnull(J, -1) = 0 then
		begin
			js_pushnumber(J, count);
			js_setproperty(J, -2, 'count');
		
			if (this.bin_flags and konfNoSections) <> 0 then
			begin	
				// hack - store reference to parent object
				// to guarantee it's not freed before new object
				js_copy(J, 0);
				js_setproperty(J, -2, '____dataref');
			end;
		end;
		
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_TryReadArray16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
	count : Longword;
begin
	ArgsThis1Str(J, this, name, nil);

	try
		Script_Push(J, this.TryReadArray16(name, @count));
		
		if js_isnull(J, -1) = 0 then
		begin
			js_pushnumber(J, count);
			js_setproperty(J, -2, 'count');
		
			if (this.bin_flags and konfNoSections) <> 0 then
			begin	
				// hack - store reference to parent object
				// to guarantee it's not freed before new object
				js_copy(J, 0);
				js_setproperty(J, -2, '____dataref');
			end;
		end;
		
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadName(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
begin
	ArgsThis1Str(J, this, name, nil);

	try
		js_pushstring(J, PAnsiChar(this.ReadName(name)))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadLang(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
begin
	ArgsThis1Str(J, this, name, nil);

	try
		js_pushstring(J, PAnsiChar(this.ReadLang(name)))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

function ReadStringCrc_helper(crc : Longint; userdata : Pointer) : String;
var
	J : js_State;
begin
	J := js_State(userdata);
	
	js_copy(J, 2);					// push function to call
	js_copy(J, 0);					// push 'this'
	js_pushnumber(J, Longword(crc));	// push crc as 1st argument
	
	js_pcall(J, 1);
	Result := js_trystring(J, js_gettop(J)-1, '');
end;

procedure S_ReadStringCrc(J : js_State); cdecl;
var
	this : TKonfigReader;
	name : PAnsiChar;
	s : String;
begin
	ArgsThis1Str(J, this, name, nil);
	
	if js_iscallable(J, 2) = 0 then
		js_typeerror(J, 'TKonfigReader.ReadStringCrc: argument 2 is not callable');
	
	try
		s := this.ReadStringCrc(name, ReadStringCrc_helper, Pointer(J));
		js_pushstring(J, PAnsiChar(s));
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadHint(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, nil);

	try
		this.ReadHint(name, _type);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;

	js_pushundefined(J);
end;

procedure S_ReadHintStr(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, nil);

	try
		this.ReadHint(name, _type);
		js_pushstring(J, PAnsiChar(this.ReadString(name)));
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS8(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's8');
	
	try
		js_pushnumber(J, this.ReadS8(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's16');

	try
		js_pushnumber(J, this.ReadS16(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS32(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's32');

	try
		js_pushnumber(J, this.ReadS32(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadS64(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 's64');

	try
		this.ReadS64(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU8(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u8');

	try
		js_pushnumber(J, this.ReadU8(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u16');

	try
		js_pushnumber(J, this.ReadU16(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU32(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u32');

	try
		js_pushnumber(J, this.ReadU32(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU64(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u64');

	try
		this.ReadU64(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadFP32(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'fp32');

	try
		js_pushnumber(J, this.ReadFP32(name, _type))
	except on E: Exception do
		js_error(J, '%s', PAnsiChar(E.Message));
	end;
end;

procedure S_ReadString(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'stringz');

	try
		js_pushstring(J, PAnsiChar(this.ReadString(name, _type)))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadBool(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'bool');

	try
		js_pushboolean(J, Longint(this.ReadBool(name, _type)))
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;


procedure S_ReadVec2(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec2f');

	try
		this.ReadVec2(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec3(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec3f');

	try
		this.ReadVec3(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec4(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec4f');

	try
		this.ReadVec4(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec4S16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec4s16');

	try
		this.ReadVec4S16(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec3i(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec3i');

	try
		this.ReadVec3i(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadVec4i(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'vec4i');

	try
		this.ReadVec4i(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadMatrix44(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'pose, matrix');

	try
		this.ReadMatrix44(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadMatrix43(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'pose, matrix_43T');

	try
		this.ReadMatrix43(name, _type);
		js_pushundefined(J);
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU8Array(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TU8Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u8_array');

	try
		arr := this.ReadU8Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU16Array(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TU16Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u16_array');

	try
		arr := this.ReadU16Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU32Array(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TU32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u32_array');

	try
		arr := this.ReadU32Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadFP32Array(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TFP32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'fp32_array');

	try
		arr := this.ReadFP32Array(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU16Array16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TU16Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u16_array');

	try
		arr := this.ReadU16Array16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadU32Array16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TU32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'u32_array16');

	try
		arr := this.ReadU32Array16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadFP32Array16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TFP32Array;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'fp32_array16');

	try
		arr := this.ReadFP32Array16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newnumber(J, arr[I]);
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadStrArray16(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TStrArray;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'str_array16');

	try
		arr := this.ReadStrArray16(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newstring(J, PAnsiChar(arr[I]));
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure S_ReadStrArray32(J : js_State); cdecl;
var
	this : TKonfigReader;
	name, _type : PAnsiChar;
	arr : TStrArray;
	I : Longint;
begin
	ArgsThis2Str(J, this, name, _type, nil, 'str_array32');

	try
		arr := this.ReadStrArray32(name, _type);
		js_newarray(J);
		for I := 0 to Length(arr)-1 do
		begin
			js_newstring(J, PAnsiChar(arr[I]));
			js_setindex(J, -2, I);
		end;
	except on E: Exception do
		js_error(J, PAnsiChar(E.Message));
	end;
end;

procedure Script_Init(J : js_State);

	procedure DefFunc(f : js_CFunction; name : PAnsiChar; nargs : Longint = 1);
	begin
		js_newcfunction(J, f, name, nargs);
		js_defproperty(J, -2, name, JS_DONTENUM);
	end;
begin
	js_newobject(J);

	DefFunc(S_More, 'More', 0);
	DefFunc(S_Size, 'Size', 0);

	DefFunc(S_ReadSection, 'ReadSection', 0);
	DefFunc(S_ReadArray, 'ReadArray');
	DefFunc(S_ReadArray16, 'ReadArray16');
	
	DefFunc(S_TryReadSection, 'TryReadSection');
	DefFunc(S_TryReadArray, 'TryReadArray');
	DefFunc(S_TryReadArray16, 'TryReadArray16');

	DefFunc(S_ReadName, 'ReadName');
	DefFunc(S_ReadLang, 'ReadLang');
	DefFunc(S_ReadStringCrc, 'ReadStringCrc', 2);
	DefFunc(S_ReadHint, 'ReadHint', 2);
	DefFunc(S_ReadHintStr, 'ReadHintStr', 2);

	DefFunc(S_ReadS8,	'ReadS8');
	DefFunc(S_ReadS16, 'ReadS16');
	DefFunc(S_ReadS32, 'ReadS32');
	DefFunc(S_ReadS64, 'ReadS64');

	DefFunc(S_ReadU8,	'ReadU8');
	DefFunc(S_ReadU16, 'ReadU16');
	DefFunc(S_ReadU32, 'ReadU32');
	DefFunc(S_ReadU64, 'ReadU64');

	DefFunc(S_ReadFP32, 'ReadFP32');

	DefFunc(S_ReadString, 'ReadString');
	DefFunc(S_ReadBool, 'ReadBool');

	// should they return vectors to JS ??
	DefFunc(S_ReadVec2, 'ReadVec2');
	DefFunc(S_ReadVec3, 'ReadVec3');
	DefFunc(S_ReadVec4, 'ReadVec4');
	DefFunc(S_ReadVec4S16, 'ReadVec4S16');
	DefFunc(S_ReadVec3i, 'ReadVec3i');
	DefFunc(S_ReadVec4i, 'ReadVec4i');

	DefFunc(S_ReadMatrix44, 'ReadMatrix44');
	DefFunc(S_ReadMatrix43, 'ReadMatrix43');
	
	DefFunc(S_ReadU8Array, 'ReadU8Array');
	DefFunc(S_ReadU16Array, 'ReadU16Array');
	DefFunc(S_ReadU32Array, 'ReadU32Array');
	DefFunc(S_ReadFP32Array, 'ReadFP32Array');
	DefFunc(S_ReadU16Array, 'ReadU32Array16');
	DefFunc(S_ReadU16Array16, 'ReadU16Array16');
	DefFunc(S_ReadFP32Array16, 'ReadFP32Array16');
	
	DefFunc(S_ReadStrArray16, 'ReadStrArray16');
	DefFunc(S_ReadStrArray32, 'ReadStrArray32');

	js_setregistry(J, 'TKonfigReader_prototype');
end;

procedure Script_Finish(J : js_State);
begin
	js_delregistry(J, 'TKonfigReader_prototype');
end;

procedure TKonfigReader_finalize(J : js_State; p : Pointer); cdecl;
var
	kr : TKonfigReader;
begin
	kr := TKonfigReader(p);
	kr.Free;
end;

procedure Script_Push(J : js_State; K : TKonfigReader);
begin
	if Assigned(K) then
	begin
		js_getregistry(J, 'TKonfigReader_prototype');
		js_newuserdata(J, TAG, Pointer(K), TKonfigReader_finalize);
	end else
		js_pushnull(J);
end;

end.