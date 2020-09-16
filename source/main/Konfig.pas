unit Konfig;

interface
uses classes, sysutils, chunkedFile, vmath;

type
	EParamNotFound = class(Exception)
		public
		param_name, param_type : String;
		constructor Create(const p_name, p_type : String);
	end;

	TSimpleValue = class
		name, vtype : String;
		constructor Create(nm, tp : String); overload;

		function Copy : TSimpleValue; virtual;
	end;
	TSection = class(TSimpleValue)
		items: TList;
		constructor Create; overload;
		constructor Create(nm : String); overload;
		constructor Create(nm, tp : String); overload;
		destructor Destroy; override;

		function Copy : TSimpleValue; override;
		procedure Clear;
		
		function ParamCount : Longint;
		function GetParam(index : Longint) : TSimpleValue; overload;
		function GetParam(const nm, tp : String) : TSimpleValue; overload;
		
		procedure AddHint(const nm, tp : String);
		procedure AddFloat(const nm : String; f : Single; const tp : String = 'fp32');
		procedure AddStr(const nm, val : String; const tp : String = 'stringz');
		procedure AddVec2(const nm : String; const v : TVec2; const tp : String = 'vec2f');
		procedure AddVec3(const nm : String; const v : TVec3; const tp : String = 'vec3f');
		procedure AddVec4(const nm : String; const v : TVec4; const tp : String = 'vec4f');
		procedure AddVec4S16(const nm : String; const v : TVec4S16; const tp : String = 'vec4s16');
		procedure AddBool(const nm : String; b : Boolean; const tp : String = 'bool');
		procedure AddInt(const nm : String; val : Int64; const tp : String);
		function	AddSect(const nm : String) : TSection;
		
		function GetFloat(const nm : String; const tp : String = 'fp32') : Single; overload;
		function GetFloat(const nm : String; def : Single; const tp : String = 'fp32') : Single; overload;
		function GetStr(const nm : String; const tp : String = 'stringz') : String; overload;
		function GetStrDef(const nm : String; const def : String; const tp : String = 'stringz') : String; overload;
		function GetVec2(const nm : String; const tp : String = 'vec2f') : TVec2; overload;
		function GetVec2(const nm : String; const def : TVec2; const tp : String = 'vec2f') : TVec2; overload;
		function GetVec3(const nm : String; const tp : String = 'vec3f') : TVec3; overload;
		function GetVec3(const nm : String; const def : TVec3; const tp : String = 'vec3f') : TVec3; overload;
		function GetVec4(const nm : String; const tp : String = 'vec4f') : TVec4; overload;
		function GetVec4(const nm : String; const def : TVec4; const tp : String = 'vec4f') : TVec4; overload;		
		function GetBool(const nm : String; const tp : String = 'bool') : Boolean; overload;
		function GetBool(const nm : String; def : Boolean; const tp : String = 'bool') : Boolean; overload;
		function GetInt(const nm : String; const tp : String) : Int64; overload;
		function GetInt(const nm : String; def : Int64; const tp : String) : Int64; overload;
		function GetSect(const nm : String; raiseiffail : Boolean = True) : TSection;
		
		procedure Replace(old, new : TSimpleValue);
	end;
	TIntegerValue = class(TSimpleValue)
		num : Int64;
		constructor Create(nm, tp : String; i : Int64); overload;
		function Copy : TSimpleValue; override;
	end;
	TSingleValue = class(TSimpleValue)
		num : Single;
		constructor Create(nm, tp : String; f : Single); overload;
		function Copy : TSimpleValue; override;
	end;
	TStringValue = class(TSimpleValue)
		str : String;
		constructor Create(nm, val : String); overload;
		function Copy : TSimpleValue; override;
	end;
	TByteArrayValue = class(TSimpleValue)
		data : array of Byte;
		function Copy : TSimpleValue; override;
		procedure FromStrings(const strs : array of String);
		procedure FromReader(r : TMemoryReader; count : Integer);
	end;
	TIntegerArrayValue = class(TSimpleValue)
		data : array of Integer;
		function Copy : TSimpleValue; override;
		procedure FromStrings(const strs : array of String);
		procedure FromReaderS16(r : TMemoryReader; count : Integer);
		procedure FromReaderS32(r : TMemoryReader; count : Integer);
	end;
	TFloatArrayValue = class(TSimpleValue)
		data : array of Single;
		function Copy : TSimpleValue; override;
		procedure FromStrings(const strs : array of String);
		procedure FromReader(r : TMemoryReader; count : Integer);
		
		procedure GetMatrix44(out m : TMatrix);
		procedure GetMatrix43(out m : TMatrix);
		procedure GetMatrix43T(out m : TMatrix); // transpose
		
		procedure SetMatrix44(const m : TMatrix);
		procedure SetMatrix43(const m : TMatrix);
		procedure SetMatrix43T(const m : TMatrix); // transpose
	end;
	TBoolValue = class(TSimpleValue)
		bool : Boolean;
		function Copy : TSimpleValue; override;
		constructor Create(nm, tp : String; val : Boolean);
	end;

	TTextKonfig = class
		root : TSection;

		constructor Create; overload;
		constructor Create(root : TSection); overload;
		destructor Destroy; override;
		
		function Copy : TTextKonfig;
		
		function Load(const str : String) : Boolean;
		procedure Save(var str : String);
		function LoadFromFile(const filename : String) : Boolean;
		procedure SaveToFile(const filename : String);
	end;

const
	konfDebugInfo		= $1;
	konfEditor			= $2;  // what does it mean ??
	konfDiktionary	= $4;
	konfPlain				= $8;  // also unknown
	konfNoSections	= $10;
	konfMultiChunk	= $20;

type
	TKonfig = class
	public
		kind : Byte; // flags (see above)
		data : array of Byte;
		dictionary : array of String;

	public
		function Load(stream : TStream) : Boolean; overload;
		procedure Save(stream : TStream); overload;

		function Load(r : TMemoryReader) : Boolean; overload;
		procedure Save(w : TMemoryWriter); overload;

		function Load(const filename : String) : Boolean; overload;
		procedure Save(const filename : String); overload;

		procedure Compile(from : TTextKonfig; ll : Boolean = False);
		procedure Decompile(tk : TTextKonfig; ll : Boolean = False);
	private
		function AddWord(w : String) : Longint;
		procedure CompileSection(sect : TSection; w : TMemoryWriter; ll : Boolean);
		procedure CompileConfig(items : TList; w : TMemoryWriter; ll : Boolean);
		function DecompileSection(r : TMemoryReader; ll : Boolean) : TSection;
		procedure DecompileConfig(items : TList; kind : Byte; r : TMemoryReader; ll : Boolean);
	end;

implementation
uses uCrc, windows;

type
	TStringArray = array of String;

	TParser = class
		source : String;
		pos : Longint;

		constructor Create(const src : String);

		procedure Error(const msg : String; p : Longint = 0);
		function NextToken(var token : String; allowEOF : Boolean = False) : Boolean;
	end;

constructor TParser.Create(const src: string);
begin
	inherited Create;
	source := src;
	pos := 1;
end;

procedure TParser.Error(const msg : String; p : Longint);
var
	I : Longint;
	line, ch : Integer;
begin
	I := 1;
	line := 1;
	ch := 1;

	if p = 0 then
		p := pos;

	while I < p do
	begin
		if source[I] = #10 then
		begin
			Inc(line);
			ch := 1;
		end else
			Inc(ch);

		Inc(I);
	end;

	raise Exception.Create(IntToStr(line) + ':' + IntToStr(ch) + ': ' + msg);
end;

function TParser.NextToken(var token : String; allowEOF : Boolean) : Boolean;
	function IsDelim : Boolean;
	begin
		IsDelim := source[pos] in [',', ':', ';', '=', '[', ']'];
	end;
	function IsSpace : Boolean;
	begin
		IsSpace := source[pos] in [' ', #10, #13, #9];
	end;
var
	start : Longint;
begin
	while (pos <= Length(source)) and IsSpace do
		Inc(pos);

	if (pos <= Length(source)) and (source[pos] = '{') then
	begin
		start := pos;
		while (pos <= Length(source)) and (source[pos] <> '}') do
			Inc(pos);

		if pos > Length(source) then
			Error('unclosed comment', start)
		else
			Inc(pos); // skip }
	end;

	while (pos <= Length(source)) and IsSpace do
		Inc(pos);

	if pos <= Length(source) then
	begin
		if IsDelim then
		begin
			token := source[pos];
			Inc(pos);
		end else
		if (source[pos] = '''') or (source[pos] = '"') then
		begin
			start := pos;
			Inc(pos);
			while (source[pos] <> source[start]) and (pos <= Length(source)) do
				Inc(pos);

			if pos > Length(source) then
				Error('unclosed string', start);

			token := Copy(source, start+1, pos-(start+1));
			Inc(pos); // skip '
		end else
		begin
			start := pos;
			while (not IsDelim) and (not IsSpace) and (pos <= Length(source)) do
				Inc(pos);

			token := Copy(source, start, pos-start);
		end;

		Result := True;
	end else
	begin
		Result := False;
		if not allowEOF then
			Error('unexpected end of file');
	end;
end;

// Exception implementation
constructor EParamNotFound.Create(const p_name, p_type : String);
begin
	inherited Create('Parameter not found (' + p_name + ': ' + p_type + ')');
	param_name := p_name;
	param_type := p_type;
end;

// utilities
function ParseFloat(src : String) : Single;
var I : Longint;
begin
	I := Pos('.', src);
	if I > 0 then
		src[I] := DecimalSeparator;
		
	Result := StrToFloat(src);
end;

function PrintFloat(f : Single) : String;
var I : Longint;
begin
	Result := FloatToStr(f);
	I := Pos(DecimalSeparator, Result);
	if I > 0 then
		Result[I] := '.';
end;

// value implementation
constructor TSimpleValue.Create(nm: string; tp: string);
begin
	name := nm;
	vtype := tp;
end;

function TSimpleValue.Copy : TSimpleValue;
begin
	Result := TSimpleValue.Create(name, vtype);
end;

constructor TSection.Create;
begin
	inherited Create('unnamed_section', 'section');
	items := TList.Create;
end;

constructor TSection.Create(nm: string);
begin
	inherited Create(nm, 'section');
	items := TList.Create;
end;

constructor TSection.Create(nm: string; tp: string);
begin
	inherited Create(nm, tp);
	items := TList.Create;
end;

destructor TSection.Destroy;
begin
	Clear;
	items.Free;
	inherited;
end;

function TSection.Copy : TSimpleValue;
var
	sect : TSection;
	I : Integer;
begin
	sect := TSection.Create(name);
	sect.items.Count := items.Count;

	for I := 0 to items.Count - 1 do
		sect.items[I] := TSimpleValue(items[I]).Copy;

	Result := sect;
end;

procedure TSection.Clear;
var
	I : Integer;
begin
	for I := 0 to items.Count - 1 do
		TSimpleValue(items[I]).Free;
	items.Clear;
end;

function TSection.ParamCount : Longint;
begin
	Result := items.Count;
end;

function TSection.GetParam(index : Longint) : TSimpleValue; overload;
begin
	Result := TSimpleValue(items[index]);
end;

function TSection.GetParam(const nm: string; const tp: string) : TSimpleValue; overload;
var
	item : TSimpleValue;
	I : Integer;
begin
	for I := 0 to items.Count - 1 do
	begin
		item := TSimpleValue(items[I]);
		if (item.name = nm) and (item.vtype = tp) then
		begin
			Result := item;
			Exit;
		end;
	end;

	Result := nil;
end;

procedure TSection.AddHint(const nm: string; const tp: string);
begin
	items.Add(TSimpleValue.Create(nm, tp));
end;

procedure TSection.AddFloat(const nm: string; f: Single; const tp: string);
begin
	items.Add(TSingleValue.Create(nm, tp, f));
end;

procedure TSection.AddStr(const nm: string; const val: string; const tp: string);
var
	s : TStringValue;
begin
	s := TStringValue.Create(nm, val);
	s.vtype := tp;
	items.Add(s);
end;

procedure TSection.AddVec2(const nm: string; const v: TVec2; const tp: string);
var
	a : TFloatArrayValue;
begin
	a := TFloatArrayValue.Create(nm, tp);
	SetLength(a.data, 2);
	a.data[0] := v.x;
	a.data[1] := v.y;
	items.Add(a);
end;

procedure TSection.AddVec3(const nm: string; const v: TVec3; const tp: string);
var
	a : TFloatArrayValue;
begin
	a := TFloatArrayValue.Create(nm, tp);
	SetLength(a.data, 3);
	a.data[0] := v.x;
	a.data[1] := v.y;
	a.data[2] := v.z;
	items.Add(a);
end;

procedure TSection.AddVec4(const nm: string; const v: TVec4; const tp: string);
var
	a : TFloatArrayValue;
begin
	a := TFloatArrayValue.Create(nm, tp);
	SetLength(a.data, 4);
	a.data[0] := v.x;
	a.data[1] := v.y;
	a.data[2] := v.z;
	a.data[3] := v.w;
	items.Add(a);
end;

procedure TSection.AddVec4S16(const nm: string; const v: TVec4S16; const tp: string);
var
	a : TIntegerArrayValue;
begin
	a := TIntegerArrayValue.Create(nm, tp);
	SetLength(a.data, 4);
	a.data[0] := v.x;
	a.data[1] := v.y;
	a.data[2] := v.z;
	a.data[3] := v.w;
	items.Add(a);
end;

procedure TSection.AddBool(const nm: string; b: Boolean; const tp: string);
begin
	items.Add(TBoolValue.Create(nm, tp, b));
end;

procedure TSection.AddInt(const nm: String; val : Int64; const tp : String);
begin
	items.Add(TIntegerValue.Create(nm, tp, val));
end;

function TSection.AddSect(const nm : String) : TSection;
var
	s : TSection;
begin
	s := TSection.Create(nm);
	items.Add(s);

	Result := s;
end;

function TSection.GetFloat(const nm : String; const tp : String) : Single;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TSingleValue).num
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetFloat(const nm : String; def : Single; const tp : String) : Single;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TSingleValue).num
	else Result := def;
end;

function TSection.GetStr(const nm, tp : String) : String;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TStringValue).str
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetStrDef(const nm : String; const def : String; const tp : String) : String;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TStringValue).str
	else Result := def;
end;

function TSection.GetVec2(const nm, tp : String) : TVec2;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec2;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		Result := r
	end else 
		raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetVec2(const nm : String; const def : TVec2; const tp : String) : TVec2;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec2;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		Result := r
	end else 
		Result := def;
end;

function TSection.GetVec3(const nm, tp : String) : TVec3;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec3;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		Result := r
	end else 
		raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetVec3(const nm : String; const def : TVec3; const tp : String) : TVec3;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec3;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		Result := r
	end else 
		Result := def;
end;

function TSection.GetVec4(const nm, tp : String) : TVec4;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec4;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		r.w := f.data[3];
		Result := r
	end else 
		raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetVec4(const nm : String; const def : TVec4; const tp : String) : TVec4;
var
	v : TSimpleValue;
	f : TFloatArrayValue;
	r : TVec4;
begin
	v := GetParam(nm, tp);
	if v <> nil then
	begin 
		f := v as TFloatArrayValue;
		r.x := f.data[0];
		r.y := f.data[1];
		r.z := f.data[2];
		r.w := f.data[3];
		Result := r
	end else 
		Result := def;
end;

function TSection.GetBool(const nm, tp : String) : Boolean;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TBoolValue).bool
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetBool(const nm : String; def : Boolean; const tp : String) : Boolean;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TBoolValue).bool
	else Result := def;
end;

function TSection.GetInt(const nm, tp : String) : Int64;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TIntegerValue).num
	else raise EParamNotFound.Create(nm, tp);
end;

function TSection.GetInt(const nm : String; def : Int64; const tp : String) : Int64;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, tp);
	if v <> nil then Result := (v as TIntegerValue).num
	else Result := def;
end;

function TSection.GetSect(const nm : String; raiseiffail : Boolean) : TSection;
var
	v : TSimpleValue;
begin
	v := GetParam(nm, 'section');
	if v <> nil then Result := v as TSection
	else Result := nil;
	
	if raiseiffail and (Result = nil) then
		raise EParamNotFound.Create(nm, 'section');
end;

procedure TSection.Replace(old, new : TSimpleValue);
var
	idx : Longint;
begin
	idx := items.IndexOf(old);
	if idx <> -1 then
	begin
		TSimpleValue(items[idx]).Free;
		items[idx] := new;
	end;
end;

constructor TIntegerValue.Create(nm: string; tp: string; i: Int64);
begin
	inherited Create(nm, tp);
	num := i;
end;

function TIntegerValue.Copy : TSimpleValue;
begin
	Result := TIntegerValue.Create(name, vtype, num);
end;

constructor TSingleValue.Create(nm: string; tp: string; f: Single);
begin
	inherited Create(nm, tp);
	num := f;
end;

function TSingleValue.Copy : TSimpleValue;
begin
	Result := TSingleValue.Create(name, vtype, num);
end;

constructor TStringValue.Create(nm: string; val: string);
begin
	inherited Create(nm, 'stringz');
	str := val;
end;

function TStringValue.Copy : TSimpleValue;
begin
	Result := TStringValue.Create(name, str);
end;

function TByteArrayValue.Copy : TSimpleValue;
var
	arr : TByteArrayValue;
begin
	arr := TByteArrayValue.Create(name, vtype);
	SetLength(arr.data, Length(data));
	Move(data[0], arr.data[0], Length(data)*Sizeof(Byte));

	Result := arr;
end;

procedure TByteArrayValue.FromStrings(const strs: array of string);
var I : Integer;
begin
	SetLength(data, Length(strs));
	for I := 0 to Length(data) - 1 do
		data[I] := StrToInt(strs[I]);
end;

procedure TByteArrayValue.FromReader(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadByte;
end;

function TIntegerArrayValue.Copy : TSimpleValue;
var
	arr : TIntegerArrayValue;
begin
	arr := TIntegerArrayValue.Create(name, vtype);
	SetLength(arr.data, Length(data));
	Move(data[0], arr.data[0], Length(data)*Sizeof(Int64));

	Result := arr;
end;

procedure TIntegerArrayValue.FromStrings(const strs: array of string);
var I : Integer;
begin
	SetLength(data, Length(strs));
	for I := 0 to Length(data) - 1 do
		data[I] := StrToInt(strs[I]);
end;

procedure TIntegerArrayValue.FromReaderS16(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadSmallint;
end;

procedure TIntegerArrayValue.FromReaderS32(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadLongint;
end;

function TFloatArrayValue.Copy : TSimpleValue;
var
	arr : TFloatArrayValue;
begin
	arr := TFloatArrayValue.Create(name, vtype);
	SetLength(arr.data, Length(data));
	Move(data[0], arr.data[0], Length(data)*Sizeof(Single));

	Result := arr;
end;

procedure TFloatArrayValue.FromStrings(const strs: array of string);
var I : Integer;
begin
	SetLength(data, Length(strs));
	for I := 0 to Length(data) - 1 do
		data[I] := ParseFloat(strs[I]);
end;

procedure TFloatArrayValue.FromReader(r: TMemoryReader; count: Integer);
var I : Integer;
begin
	SetLength(data, count);
	for I := 0 to count - 1 do
		data[I] := r.ReadSingle;
end;

procedure TFloatArrayValue.GetMatrix44(out m : TMatrix);
begin
	if Length(data) <> 16 then
		raise Exception.Create('4x4 matrix must contain 16 elements');
		
	Move(data[0], m, Sizeof(TMatrix));
end;

procedure TFloatArrayValue.GetMatrix43(out m : TMatrix);
begin
	if Length(data) <> 12 then
		raise Exception.Create('4x3 matrix must contain 12 elements');
		
	m[1,1] := data[0]; m[1,2] := data[1]; m[1,3] := data[2]; m[1,4] := 0.0;
	m[2,1] := data[3]; m[2,2] := data[4]; m[2,3] := data[5]; m[2,4] := 0.0;
	m[3,1] := data[6]; m[3,2] := data[7]; m[3,3] := data[8]; m[3,4] := 0.0;
	m[4,1] := data[9]; m[4,2] := data[10]; m[4,3] := data[11]; m[4,4] := 1.0;
end;

procedure TFloatArrayValue.GetMatrix43T(out m : TMatrix);
begin
	if Length(data) <> 12 then
		raise Exception.Create('4x3 matrix must contain 12 elements');
		
	m[1,1] := data[0];
	m[1,2] := data[4];
	m[1,3] := data[8];
	m[1,4] := 0.0;

	m[2,1] := data[1];
	m[2,2] := data[5];
	m[2,3] := data[9];
	m[2,4] := 0.0;

	m[3,1] := data[2];
	m[3,2] := data[6];
	m[3,3] := data[10];
	m[3,4] := 0.0;

	m[4,1] := data[3];
	m[4,2] := data[7];
	m[4,3] := data[11];
	m[4,4] := 1.0;
end;

procedure TFloatArrayValue.SetMatrix44(const m : TMatrix);
begin
	SetLength(data, 16);
	Move(m, data[0], Sizeof(TMatrix));
end;

procedure TFloatArrayValue.SetMatrix43(const m : TMatrix);
begin
	SetLength(data, 12);
	data[0] := m[1,1]; data[1] := m[1,2]; data[2] := m[1,3];
	data[3] := m[2,1]; data[4] := m[2,2]; data[5] := m[2,3];
	data[6] := m[3,1]; data[7] := m[3,2]; data[8] := m[3,3];
	data[9] := m[4,1]; data[10] := m[4,2]; data[11] := m[4,3];	
end;

procedure TFloatArrayValue.SetMatrix43T(const m : TMatrix);
begin
	SetLength(data, 12);
	data[0] := m[1,1];
	data[4] := m[1,2];
	data[8] := m[1,3];

	data[1] := m[2,1];
	data[5] := m[2,2];
	data[9] := m[2,3];

	data[2] := m[3,1];
	data[6] := m[3,2];
	data[10] := m[3,3];

	data[3] := m[4,1];
	data[7] := m[4,2];
	data[11] := m[4,3];
end;

constructor TBoolValue.Create(nm: string; tp: string; val: Boolean);
begin
	inherited Create(nm, tp);
	bool := val;
end;

function TBoolValue.Copy : TSimpleValue;
begin
	Result := TBoolValue.Create(name, vtype, bool);
end;

// TKonfig
function TKonfig.Load(stream : TStream) : Boolean;
var
	r : TMemoryReader;
begin
	r := TMemoryReader.Create(stream);
	Result := Load(r);
	r.Free;
end;

procedure TKonfig.Save(stream : TStream);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	Save(w);
	stream.Write(w.data[0], w.size);
	w.Free;
end;

function TKonfig.Load(r : TMemoryReader) : Boolean;
var
	rdata, rdict : TMemoryReader;
	count, I : Longint;
begin
	Load := False;

	kind := r.ReadByte;
	if (kind and konfDiktionary) <> 0 then
	begin
		rdata := r.OpenChunk(1, True);
		if rdata <> nil then
		begin
			SetLength(data, rdata.size);
			rdata.Read(data[0], rdata.size);

			rdict := r.OpenChunk(2, True);
			if rdict <> nil then
			begin
				count := rdict.ReadLongint;
				SetLength(dictionary, count);

				for I := 0 to count-1 do
					dictionary[I] := rdict.ReadStringZ;

				Load := True;
			end;
			rdict.Free;
		end;
		rdata.Free;
	end else
	begin
		SetLength(data, r.size-r.pos);
		r.Read(data[0], r.size-r.pos);
		Load := True;
	end;
end;

procedure TKonfig.Save(w : TMemoryWriter);
var
	I : Integer;
begin
	w.WriteByte(kind);
	if (kind and konfDiktionary) <> 0 then
	begin
		w.OpenChunk(1);
		w.Write(data[0], Length(data));
		w.CloseChunk;

		w.OpenChunk(2);
		w.WriteLongint(Length(dictionary));
		for I := 0 to Length(dictionary) - 1 do
			w.WriteStringZ(dictionary[I]);
		w.CloseChunk;
	end else
		w.Write(data[0], Length(data));
end;

function TKonfig.Load(const filename : String) : Boolean;
var
	r : TMemoryReader;
begin
	r := TMemoryReader.CreateFromFile(filename);
	Result := Load(r);
	r.Free;
end;

procedure TKonfig.Save(const filename : String);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	Save(w);
	w.SaveTo(filename);
end;

procedure TKonfig.Compile(from: TTextKonfig; ll : Boolean);
var
	w : TMemoryWriter;
begin
	SetLength(dictionary, 0); // cleanup

	w := TMemoryWriter.Create;
	CompileConfig(from.root.items, w, ll);
	if (kind and konfMultiChunk) = 0 then
	begin
		SetLength(data, w.size);
		Move(w.data[0], data[0], w.size);
	end else
	begin
		SetLength(data, w.size+8);
		PLongint(@data[0])^ := GetStringCrc('arch_chunk_0');
		PLongint(@data[4])^ := w.size;
		Move(w.data[0], data[8], w.size);
	end;
	w.Free;
end;

procedure TKonfig.Decompile(tk: TTextKonfig; ll : Boolean);
var
	r : TMemoryReader;
begin
	if (kind = 3) or (kind = 5) then
	begin
		r := TMemoryReader.Create(data[0], Length(data));
		DecompileConfig(tk.root.items, kind, r, ll);
		r.Free;
	end else
		raise Exception.Create('Cannot decompile config with kind='+IntToStr(kind));
end;

function TKonfig.AddWord(w : String) : Longint;
var
	I : Integer;
begin
	Result := -1;
	for I := 0 to Length(dictionary) - 1 do
		if dictionary[I] = w then
		begin
			Result := I;
			Break;
		end;

	if Result = -1 then
	begin
		I := Length(dictionary);
		SetLength(dictionary, I+1);
		dictionary[I] := w;
		Result := I;
	end;
end;

procedure TKonfig.CompileSection(sect: TSection; w: TMemoryWriter; ll : Boolean);
begin
	if (kind and konfNoSections) = 0 then
	begin
		w.OpenChunk(GetStringCrc(sect.name));
		
		if (kind and konfMultiChunk) = 0 then
			w.WriteByte(kind);
			
		if (kind and konfDebugInfo) <> 0 then
		begin
			if (kind and konfDiktionary) <> 0 then
				w.WriteLongint(AddWord(sect.name))
			else
				w.WriteStringZ(sect.name)
		end;
	end;
	
	CompileConfig(sect.items, w, ll);
	
	if (kind and konfNoSections) = 0 then
		w.CloseChunk;
end;

procedure TKonfig.CompileConfig(items: TList; w: TMemoryWriter; ll : Boolean);
var
	I, J : Integer;
	item : TSimpleValue;
	int : TIntegerValue;
	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;
begin
	for I := 0 to items.Count - 1 do
	begin
		item := TSimpleValue(items[I]);
		if item is TSection then
			CompileSection(item as TSection, w, ll)
		else
		begin
			if (kind and konfDebugInfo) <> 0 then
			begin
				if (kind and konfDiktionary) <> 0 then
				begin
					w.WriteLongint(AddWord(item.name));
					w.WriteLongint(AddWord(item.vtype));
 				end else
				begin
					w.WriteStringZ(item.name);
					w.WriteStringZ(item.vtype);
				end
			end;

			if item is TIntegerValue then
			begin
				int := item as TIntegerValue;
				if int.vtype = 'u8' then
					w.WriteByte(int.num)
				else if int.vtype = 's8' then
					w.WriteShortint(int.num)
				else if int.vtype = 'u16' then
					w.WriteWord(int.num)
				else if int.vtype = 's16' then
					w.WriteSmallint(int.num)
				else if int.vtype = 'u32' then
					w.WriteLongword(int.num)
				else if int.vtype = 's32' then
					w.WriteLongint(int.num)
				else if (int.vtype = 'fp32_q8') or (int.vtype = 'bool8') then
					w.WriteByte(int.num)
				else if int.vtype = 'u64' then
					w.WriteQWord(int.num)
				else if int.vtype = 's64' then
					w.WriteInt64(int.num)
				else if (int.vtype = 'entity_link, uobject_link') or
					 (int.vtype = 'cover_link, ucover_link') then
					w.WriteWord(int.num)
				else
					raise Exception.Create('define integer type ' + int.vtype + ', please');
			end else
			if item is TSingleValue then
				w.WriteSingle((item as TSingleValue).num)
			else
			if item is TStringValue then
			begin
				if (kind and konfDiktionary) <> 0 then
					w.WriteLongint(AddWord((item as TStringValue).str))
				else
					w.WriteStringZ((item as TStringValue).str)
			end else
			if item is TByteArrayValue then
			begin
				u8arr := item as TByteArrayValue;
				w.WriteLongword(Length(u8arr.data));
				w.Write(u8arr.data[0], Length(u8arr.data));
			end else
			if item is TIntegerArrayValue then
			begin
				intarr := item as TIntegerArrayValue;
				if item.vtype = 'vec4s16' then
				begin
					w.WriteSmallint(intarr.data[0]);
					w.WriteSmallint(intarr.data[1]);
					w.WriteSmallint(intarr.data[2]);
					w.WriteSmallint(intarr.data[3]);
				end else
				if item.vtype = 'vec3i' then
				begin
					w.WriteLongint(intarr.data[0]);
					w.WriteLongint(intarr.data[1]);
					w.WriteLongint(intarr.data[2]);
				end else
				if item.vtype = 'vec4i' then
				begin
					w.WriteLongint(intarr.data[0]);
					w.WriteLongint(intarr.data[1]);
					w.WriteLongint(intarr.data[2]);
					w.WriteLongint(intarr.data[3]);
				end else
				if item.vtype = 'u32_array' then
				begin
					if ll then
						w.WriteWord(Length(intarr.data))
					else
						w.WriteLongword(Length(intarr.data));
					for J := 0 to Length(intarr.data) - 1 do
						w.WriteLongword(intarr.data[J]);
				end else
					raise Exception.Create('Unknown TIntegerArrayValue type!');
			end else
			if item is TFloatArrayValue then
			begin
				floatarr := item as TFloatArrayValue;
				
				if item.vtype = 'fp32_array' then
				begin
					if ll then
						w.WriteWord(Length(floatarr.data))
					else
					  w.WriteLongword(Length(floatarr.data));
				end;
				
				for J := 0 to Length(floatarr.data) - 1 do
					w.WriteSingle(floatarr.data[J]);
			end else
			if item is TBoolValue then
			begin
				if (item as TBoolValue).bool then
					w.WriteByte(1)
				else
					w.WriteByte(0);
			end;
		end;
	end;
end;

function TKonfig.DecompileSection(r: TMemoryReader; ll : Boolean) : TSection;
var
	s : TSection;
	skind : Byte;
	sname : String;
begin
	skind := r.ReadByte;
	
	if (skind and konfDebugInfo) <> 0 then
	begin
		if (skind and konfDiktionary) <> 0 then
			sname := dictionary[r.ReadLongint]
		else
			sname := r.ReadStringZ;
	end;

	s := TSection.Create(sname);
	DecompileConfig(s.items, skind, r, ll);
	Result := s;
end;

procedure TKonfig.DecompileConfig(items : TList; kind : Byte; r : TMemoryReader; ll : Boolean);
var
	vname, vtype : String;
	sectdata : TMemoryReader;
	I : Longint;

	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;

	function SectionComing : Boolean;
	var
		savedpos, remain, crc: Longword;
		skind : Byte;

		sname : String;
		wordid : Longword;
	begin
		SectionComing := False;
		SavedPos := r.pos;

		remain := r.size-r.pos;
		if remain > 9 then // crc+size+kind
		begin
			crc := r.ReadLongword;
			r.ReadLongword; // size
			skind := r.ReadByte;
			remain := r.size-r.pos;

			case skind of
			3 : begin
						try
							sname := r.ReadStringZ;
							if GetStringCrc(sname) = Longint(crc) then
								SectionComing := True;
						except
							SectionComing := False;
						end;
					end;
			5 : if remain >= 4 then begin
						wordid := r.ReadLongword;
						if wordid < Longword(Length(dictionary)) then
							if GetStringCrc(dictionary[wordid]) = Longint(crc) then
								SectionComing := True;
					end;
			end;
		end;

		//Writeln('crc = ', crc, ' remain = ', remain, ' sectioncoming = ', Result);

		r.pos := savedpos;
	end;
begin
	while r.pos < r.size do
	begin
		if SectionComing then
		begin
			sectdata := r.OpenChunk;
			items.Add(DecompileSection(sectdata, ll));
			sectdata.Free;
		end else
		begin
		
			if (kind and konfDebugInfo) <> 0 then
			begin
				if (kind and konfDiktionary) <> 0 then
				begin
					vname := dictionary[r.ReadLongword];
					vtype := dictionary[r.ReadLongword];
				end else 
				begin
					vname := r.ReadStringZ;
					vtype := r.ReadStringZ;
				end;
				
				//WriteLn(vname, ' ', vtype);
			end;

			if (vtype = 'array') or
				 (vtype = 'camera_track, str_shared') or
				 (vtype = 'texture, str_shared') or
				 (vtype = 'choose_array, str_shared') or
				 (vtype = 'particles_modifier, str_shared') or
				 (vtype = 'particles, str_shared') or
				 (vtype = 'vs_ref, str_shared') or
				 (vtype = 'material, str_shared') or
				 (vtype = 'shader, str_shared') or
				 (vtype = 'locator_str') or
				 (vtype = 'locator_id') or
				 (vtype = 'name') or
				 (vtype = 'ref_model') or
				 (vtype = 'animation_str') or
				 (vtype = 'part_id') or
				 (vtype = 'sound') or
				 (vtype = 'str_array') or
				 (vtype = 'str_array16') or
				 (vtype = 'str_array32') or
				 (vtype = 'bone_id') or
				 (vtype = 'ref_coloranim') or
				 (vtype = 'flags8') or
				 (vtype = 'flags32') or
				 (vtype = 'bone_str') or
				 (vtype = 'choose') then
			begin
				items.Add(TSimpleValue.Create(vname, vtype));
			end else
			begin
				if (vtype = 'u8') or (vtype = 'bool8') then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadByte))
				else
				if vtype = 's8' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadShortint))
				else
				if vtype = 'u16' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadWord))
				else
				if vtype = 's16' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadSmallint))
				else
				if vtype = 'u32' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadLongword))
				else
				if vtype = 's32' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadLongint))
				else
				if (vtype = 'fp32') or (vtype = 'angle, fp32') then
					items.Add(TSingleValue.Create(vname, vtype, r.ReadSingle))
				else
				if vtype = 'fp32_q8' then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadByte))
				else
				if vtype = 'fp32_array' then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					if ll then
						floatarr.FromReader(r, r.ReadWord)
					else
						floatarr.FromReader(r, r.ReadLongword);
					items.Add(floatarr);
				end else
				if vtype = 'u8_array' then
				begin
					u8arr := TByteArrayValue.Create(vname, vtype);
					u8arr.FromReader(r, r.ReadLongword);
					items.Add(u8arr);
				end else
				if vtype = 'u32_array' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					if ll then
						SetLength(intarr.data, r.ReadWord)
					else
						SetLength(intarr.data, r.ReadLongword);
					for I := 0 to Length(intarr.data) - 1 do
						intarr.data[I] := r.ReadLongword;
				end else
				if vtype = 'stringz' then
				begin
					if (kind and konfDiktionary) <> 0 then
						items.Add(TStringValue.Create(vname, dictionary[r.ReadLongint]))
					else
						items.Add(TStringValue.Create(vname, r.ReadStringZ));			
				end else
				if vtype = 'pose, matrix' then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					if ll then // in Last Light matrices became 4x3
						floatarr.FromReader(r, 12)
					else
						floatarr.FromReader(r, 16);
					items.Add(floatarr);
				end else
				if vtype = 'pose, matrix_43T' then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 12);
					items.Add(floatarr);
				end else
				if vtype = 'bool' then
					items.Add(TBoolValue.Create(vname, vtype, r.ReadByte <> 0))
				else
				if (vtype = 'vec2f') or (vtype = 'ang2f') then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 2);
					items.Add(floatarr);
				end else
				if vtype = 'vec3f' then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 3);
					items.Add(floatarr);
				end else
				if (vtype = 'vec4f') or (vtype = 'color, vec4f') then
				begin
					floatarr := TFloatArrayValue.Create(vname, vtype);
					floatarr.FromReader(r, 4);
					items.Add(floatarr);
				end else
				if vtype = 'vec4s16' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					intarr.FromReaderS16(r, 4);
					items.Add(intarr)
				end else
				if vtype = 'vec3i' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					intarr.FromReaderS32(r, 3);
					items.Add(intarr)
				end else
				if vtype = 'vec4i' then
				begin
					intarr := TIntegerArrayValue.Create(vname, vtype);
					intarr.FromReaderS32(r, 4);
					items.Add(intarr)
				end else
				if (vtype = 'entity_link, uobject_link') or
					 (vtype = 'cover_link, ucover_link') then
					items.Add(TIntegerValue.Create(vname, vtype, r.ReadWord))
				else
					raise Exception.Create('Unknown type ' + vtype + ' name=' + vname);
			end;

		end;
	end;
end;

// TTextKonfig utils
function ParseValue(parser : TParser) : String;
var
	t1, t2 : String;
begin
	parser.NextToken(t1);
	if t1 <> '=' then
		parser.Error('''='' expected');

	parser.NextToken(t2);
	Result := t2;
end;

function ParseInteger(parser : TParser) : Int64;
begin
	Result := StrToInt64(ParseValue(parser));
end;

function ParseArray(parser : TParser) : TStringArray;
var
	t : String;
	count : Longint;
	arr : TStringArray;
begin
	parser.NextToken(t);
	if t <> '=' then
		parser.Error('''='' expected');

	parser.NextToken(t);
	if t <> '[' then
		parser.Error('''['' expected');

	SetLength(arr, 0);
	count := 0;

	parser.NextToken(t);
	while t <> ']' do
	begin
		Inc(count);
		SetLength(arr, count);
		arr[count-1] := t;

		parser.NextToken(t);
		if (t <> ',') and (t <> ']') then
			parser.Error('expected '','' or '']''');

		if t = ',' then
			parser.NextToken(t);
	end;

	Result := arr;
end;

procedure ParseConfig(sect : Longword; items : TList; parser : TParser);
var
	t1, t2, t3, t4 : String;
	arr : TStringArray;

	section : TSection;
	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;
begin
	while parser.NextToken(t1, True) do
	begin
		parser.NextToken(t2);
		if t2 = ':' then // variable
		begin
			parser.NextToken(t3); // type

			if (t3 = 'array') or
				 (t3 = 'camera_track, str_shared') or
				 (t3 = 'particles, str_shared') or
				 (t3 = 'texture, str_shared') or
				 (t3 = 'choose_array, str_shared') or
				 (t3 = 'particles_modifier, str_shared') or
			 	 (t3 = 'vs_ref, str_shared') or
			 	 (t3 = 'material, str_shared') or
			 	 (t3 = 'shader, str_shared') or
				 (t3 = 'locator_str') or
				 (t3 = 'locator_id') or
				 (t3 = 'name') or
				 (t3 = 'ref_model') or
				 (t3 = 'animation_str') or
				 (t3 = 'part_id') or
				 (t3 = 'sound') or
				 (t3 = 'str_array') or
				 (t3 = 'str_array16') or
				 (t3 = 'str_array32') or
				 (t3 = 'bone_id') or
				 (t3 = 'ref_coloranim') or
				 (t3 = 'flags8') or
				 (t3 = 'flags32') or
				 (t3 = 'bone_str') or
				 (t3 = 'choose') then
			begin
				items.Add(TSimpleValue.Create(t1, t3))
			end else
			if (t3 = 'u8') or
				 (t3 = 's8') or
				 (t3 = 'u16') or
				 (t3 = 's16') or
				 (t3 = 'u32') or
				 (t3 = 's32') or
				 (t3 = 'u64') or
				 (t3 = 's64') or
				 (t3 = 'entity_link, uobject_link') or
				 (t3 = 'cover_link, ucover_link') or
				 (t3 = 'bool8') or
				 (t3 = 'fp32_q8') then
			begin
				items.Add(TIntegerValue.Create(t1, t3, ParseInteger(parser)));
			end else
			if (t3 = 'fp32') or (t3 = 'angle, fp32') then
				items.Add(TSingleValue.Create(t1, t3, ParseFloat(ParseValue(parser))))
			else
			if t3 = 'fp32_array' then
			begin
				arr := ParseArray(parser);
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'u8_array' then
			begin
				arr := ParseArray(parser);
				u8arr := TByteArrayValue.Create(t1, t3);
				u8arr.FromStrings(arr);
				items.Add(u8arr);
			end else
			if t3 = 'u32_array' then
			begin
				arr := ParseArray(parser);
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
			if t3 = 'stringz' then
			begin
				items.Add(TStringValue.Create(t1, ParseValue(parser)));
			end else
			if t3 = 'pose, matrix' then
			begin
				arr := ParseArray(parser);
				if (Length(arr) <> 16) and (Length(arr) <> 12) then
					parser.Error('matrix must contain 12 or 16 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'pose, matrix_43T' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 12 then
					parser.Error('matrix43_T must contain 12 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'bool' then
			begin
				items.Add(TBoolValue.Create(t1, t3,
				AnsiCompareText(ParseValue(parser), 'True') = 0));
			end else
			if (t3 = 'vec2f') or (t3 = 'ang2f') then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 2 then
					parser.Error('vec2f must contain 2 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'vec3f' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 3 then
					parser.Error('vec3f must contain 3 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if (t3 = 'vec4f') or (t3 = 'color, vec4f') then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 4 then
					parser.Error('vec4f must contain 4 elements');
				floatarr := TFloatArrayValue.Create(t1, t3);
				floatarr.FromStrings(arr);
				items.Add(floatarr);
			end else
			if t3 = 'vec4s16' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 4 then
					parser.Error('vec4s16 must contain 4 elements');
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
			if t3 = 'vec3i' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 3 then
					parser.Error('vec3i must contain 3 elements');
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
			if t3 = 'vec4i' then
			begin
				arr := ParseArray(parser);
				if Length(arr) <> 4 then
					parser.Error('vec4i must contain 4 elements');
				intarr := TIntegerArrayValue.Create(t1, t3);
				intarr.FromStrings(arr);
				items.Add(intarr);
			end else
				parser.Error('unknown type ''' + t3 + '''');

			parser.NextToken(t4);
			if t4 <> ';' then
				parser.Error('syntax error, '';'' expected');
		end else
		if t2 = '=' then // section
		begin
			parser.NextToken(t3);
			if t3 <> 'begin' then
				parser.Error('''begin'' expected');

			section := TSection.Create(t1);
			ParseConfig(sect+1, section.items, parser);
			items.Add(section);
		end else
		begin
			if (t1 = 'end') and (t2 = ';')	then
			begin
				if sect > 0 then
					Exit
				else
					parser.Error('unexpected ''end''');
			end else
				parser.Error('syntax error');
		end;
	end;

	if sect > 0 then
		parser.Error('''end'' expected');
end;

function Quote(s : String ; bforcequote : Boolean = False) : String;
var
	bquote : Boolean;
	qch : Char;
	I : Integer;
begin
	bquote := False;
	qch := '''';
	for I := 1 to Length(s) do
	begin
		if s[I] in [' ', ',', #10] then
			bquote := True;
		if s[I] = '''' then
			qch := '"';
	end;

	if bquote or bforcequote or (s = '') then
		Result := qch + s + qch
	else
		Result := s;
end;

procedure PrintConfig(ident : Integer; w : TMemoryWriter; items : TList);
var
	I, J : Integer;
	item : TSimpleValue;

	u8arr : TByteArrayValue;
	intarr : TIntegerArrayValue;
	floatarr : TFloatArrayValue;

	procedure Print(const l : String);
	begin
		w.WriteString(l);
	end;
	procedure Println(const l : String);
	begin
		w.WriteString(l + #13#10);
	end;
begin
	for I := 0 to items.Count - 1 do
	begin
		item := TSimpleValue(items[I]);
		if item is TSection then
		begin
			Println(StringOfChar(#9, ident) + Quote(item.name) + ' = begin');
			PrintConfig(ident+1, w, TSection(item).items);
			Println(StringOfChar(#9, ident) + 'end;');
		end else
		begin
			Print(StringOfChar(#9, ident) + Quote(item.name) + ' : ' + Quote(item.vtype));
			if item is TIntegerValue then
				Print(' = ' + IntToStr((item as TIntegerValue).num))
			else if item is TSingleValue then
				Print(' = ' + PrintFloat((item as TSingleValue).num))
			else if item is TStringValue then
				Print(' = ' + Quote((item as TStringValue).str, True))
			else if item is TByteArrayValue then
			begin
				Print(' = [');
				u8arr := item as TByteArrayValue;
				for J := 0 to Length(u8arr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(IntToStr(u8arr.data[J]));
				end;
				Print(']');
			end
			else if item is TIntegerArrayValue then
			begin
				Print(' = [');
				intarr := item as TIntegerArrayValue;
				for J := 0 to Length(intarr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(IntToStr(intarr.data[J]));
				end;
				Print(']');
			end
			else if item is TFloatArrayValue then
			begin
				Print(' = [');
				floatarr := item as TFloatArrayValue;
				for J := 0 to Length(floatarr.data) - 1 do
				begin
					if J > 0 then
						Print(', ');
					Print(PrintFloat(floatarr.data[J]));
				end;
				Print(']');
			end
			else if item is TBoolValue then
			begin
				if (item as TBoolValue).bool then
					Print(' = True')
				else
					Print(' = False')
			end;

			Println(';');
		end;
	end;
end;

// TTextKonfig
constructor TTextKonfig.Create;
begin
	inherited;
	root := TSection.Create('root');
end;

constructor TTextKonfig.Create(root : TSection);
begin
	inherited Create;
	self.root := root;
end;

destructor TTextKonfig.Destroy;
begin
	root.Free;
	inherited;
end;

function TTextKonfig.Copy : TTextKonfig;
begin
	Result := TTextKonfig.Create(TSection(root.Copy));
end;

function TTextKonfig.Load(const str: String) : Boolean;
var
	parser : TParser;
begin
	parser := TParser.Create(str);
	ParseConfig(0, root.items, parser);
	parser.Free;

	Load := True;
end;

procedure TTextKonfig.Save(var str: string);
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	w.delta := 512*1024;
	PrintConfig(0, w, root.items);
	w.WriteByte(0);
	
	str := PAnsiChar(w.data);
	
	w.Free;
end;

function TTextKonfig.LoadFromFile(const filename : String) : Boolean;
var
	f : TFileStream;
	S : String;
begin
	f := TFileStream.Create(filename, fmOpenRead);
	
	LoadFromFile := True; // ?
	
	try
		SetLength(S, f.Size);
		f.ReadBuffer(S[1], Length(S));
		Load(S);
	finally
		f.Free;
	end;
end;

procedure TTextKonfig.SaveToFile(const filename : String);
var
	f : TFileStream;
	S : String;
begin
	Save(S);
	
	f := TFileStream.Create(filename, fmCreate);
	try
		f.WriteBuffer(S[1], Length(S));
	finally
		f.Free;
	end;
end;

end.