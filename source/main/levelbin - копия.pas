unit levelbin;

interface
uses Konfig;

// these two mustdie
//procedure LoadTypedStrings(const fn : String);
//procedure UnloadTypedStrings;

// automatic version detection
function LoadLevelBin(const fn : String) : TTextKonfig;
procedure SaveLevelBin(const fn : String; tk : TTextKonfig; kind : Integer);

// manual version select
function LoadLevelBin2033(const fn : String) : TTextKonfig;
function LoadLevelBinLL(const fn : String) : TTextKonfig;
function LoadLevelBinA1(const fn : String) : TTextKonfig;
//function LoadLevelBinExodus(const fn : String) : TTextKonfig;

procedure SaveLevelBin2033(const fn : String; tk : TTextKonfig; kind : Integer);
procedure SaveLevelBinLL(const fn : String; tk : TTextKonfig; kind : Integer);
procedure SaveLevelBinA1(const fn : String; tk : TTextKonfig; kind : Integer);

implementation
uses m2033unp, classes, sysutils, chunkedFile, uCrc, fgl, framework;

const 
	LEVEL_BIN_MAGIC			= $6C76656C; // ASCII levl

const
  ENTITY_VER_LL       = 29;
  ENTITY_VER_ARKTIKA1 = 43;
  ENTITY_VER_EXODUS   = 50;

const
	CRC_STATICPROP			= $2301C4EF;
	CRC_O_ENTITY				= $0F10B43B;
	CRC_EFFECT					= $46985674;
	CRC_EFFECTM					= $8D41FFD3;
	CRC_AMMO						= $D91A939E;
	CRC_MEDKIT					= $B713A3C2;
	CRC_FILTER					= $8F3C989B;
	CRC_o_hlamp					= $0CBBE8A6;
	CRC_VISUALSCRIPT		= $54571BFA;
	CRC_PATROL_POINT		= $B42EA669;
	CRC_PROXY						= $84260B0E;
	CRC_O_HELPERTEXT		= $A7F76A1E;
	CRC_PLAYER					= $68E1BDE3;
	CRC_WOMAN						= $8F22A0ED;
	CRC_O_AIPOINT				= $27DCDAF2;
	CRC_O_BASEZONE			= $05275E6E;

type
  TCrcStringMap = TFPGMap<Longint, String>;
var
  typed_strings : TCrcStringMap;

procedure LoadTypedStrings(const fn : String);
var
  I, count : Longint;
  str : String;
  f : TMemoryReader;
begin
  typed_strings := TCrcStringMap.Create;
  f := TMemoryReader.CreateFromFile(fn);

  f.ReadLongword; // unknown 1 (zero)
  count := f.ReadLongword;  
  typed_strings.Capacity := count;

  //for I := 0 to count - 1 do
  while f.pos < f.size do
  begin
    f.ReadWord; // dictionary index ?
    str := f.ReadStringZ;
    
    typed_strings.Add(GetStringCrc(str), str);  
  end;

  typed_strings.Sort;

  f.Free;
end;

procedure UnloadTypedStrings;
begin
  typed_strings.Free;
end; 

function TypedString(crc : Longint) : String;
var
  idx : Longint;
begin
  idx := typed_strings.IndexOf(crc);
  if idx <> -1 then
  begin
    TypedString := typed_strings.Data[idx];
  end else
  begin
    WriteLn('Cannot find typed string with crc $', IntToHex(crc, 8));
    TypedString := '';
  end;
end; 

procedure AddTypedString(const str : String);
begin
	typed_strings.Add(GetStringCrc(str),str);
end;

// временно глобальные
// а вообще надо в класс обернуть
var
  bin_flags : Byte;
  entity_version : Longint;
  dictionary : array of String;

//
function _ReadSection(r : TMemoryReader) : TMemoryReader;
var
  s : TMemoryReader;
  size : longword;
  buffer : array of Byte;
begin
  size := r.ReadLongword;
  SetLength(buffer, size);
  r.Read(buffer[0], size);

  if bin_flags = 4 then
  begin
    if buffer[0] <> bin_flags then
      raise Exception.Create('buffer[0] <> bin_flags ololo blea');

    s := TMemoryReader.Create(buffer[1], size-1);
  end else
    s := TMemoryReader.Create(buffer[0], size);

  Result := s;
end;

function ReadSection(r : TMemoryReader; const name : String) : TMemoryReader; overload;
var
  crc : Longint;
begin
  crc := r.Readlongint;
  if GetStringCrc(name) = crc then
    Result := _ReadSection(r)
  else
    raise Exception.Create('cannot open section ''' + name + ''' (' + IntToHex(crc, 8) + ')');
end;

function ReadSection(r : TMemoryReader; crc : Longword) : TMemoryReader; overload;
begin
  if r.ReadLongword = crc then
    Result := _ReadSection(r)
  else
    raise Exception.Create('cannot open section $'+IntToHex(crc,8));
end;

function ReadSection(r : TMemoryReader) : TMemoryReader; overload; // without name check
begin
	r.ReadLongword;
	Result := _ReadSection(r);
end;

function RecStr(const pref : String; n : Integer; min_digits : Integer) : String;
var
	ns : String;
begin
	ns := IntToStr(n);
	RecStr := pref + StringOfChar('0', min_digits-Length(ns)) + ns;
end;

function LoadLevelBin(const fn : String) : TTextKonfig;
var
	f : TFileStream;
	magic, dword, filesize : Longword;
begin
	f := TFileStream.Create(fn, fmOpenRead);
	filesize := f.Size;
	f.ReadBuffer(magic, Sizeof(magic));
	f.ReadBuffer(dword, Sizeof(dword));
	f.Free;

	if magic = LEVEL_BIN_MAGIC then
	begin

		if dword = $00000124 then
			Result := LoadLevelBinA1(fn) // exodus too
		else 
		if dword = (filesize-8) then
			Result := LoadLevelBinLL(fn)
		else
			Result := LoadLevelBin2033(fn);

	end else
	begin
		WriteLn('"', fn, '" is not level bin file!');
		Result := nil;
	end;
end;

procedure SaveLevelBin(const fn : String; tk : TTextKonfig; kind : Integer);
begin
	if tk.root.GetParam('arch_chunk_0', 'section') <> nil then
		SaveLevelBinA1(fn, tk, kind)
	else
	if tk.root.GetParam('entities_params', 'section') <> nil then
		SaveLevelBinLL(fn, tk, kind)
	else
		SaveLevelBin2033(fn, tk, kind);
end;

// Metro 2033

function LoadLevelBin2033(const fn : String) : TTextKonfig;
var
  infile : TFileStream;
  temp1, temp2 : array of byte;
  magic, size1, size2 : Longword;

  mem : TMemoryStream;
  K : TKonfig;
  TK : TTextKonfig;
begin
  TK := nil;

  infile := TFileStream.Create(fn, fmOpenRead);
  infile.Read(magic, Sizeof(magic));
  if magic = LEVEL_BIN_MAGIC then // ASCII levl
  begin
    infile.Read(size1, Sizeof(size1));
    SetLength(temp1, size1);
    size2 := infile.Size - 8;
    SetLength(temp2, size2);
    infile.Read(temp2[0], size2);
    //Writeln('Decompressing...');
    m2033unp.decompress(@temp2[0], @temp1[0], size2);

    mem := TMemoryStream.Create;
    mem.Write(temp1[0], size1);
    mem.Seek(0, soBeginning);
//  mem.SaveToFile('aaa.bin');
    K := TKonfig.Create;
    if K.Load(mem) then
    begin
      TK := TTextKonfig.Create;
      K.Decompile(TK);
    end;
    K.Free;
    mem.Free;
  end;

  infile.Free;
  Result := TK;
end;

procedure SaveLevelBin2033(const fn : String; tk : TTextKonfig; kind : Integer);
var
  outfile : TFileStream;
  mem : TMemoryStream;
  K : TKonfig;

  magic : Longword;
  flags : Byte;
  len : Longword;

  blocksize, remain, blocksizeplus9, remainplus9 : Longword;
  blocks : Longword;
  buf : array[0..$1FFFF] of Byte;
begin
  K := TKonfig.Create;
  K.kind := kind;
  K.Compile(tk);

  mem := TMemoryStream.Create;
  K.Save(mem);
  K.Free;

  mem.Seek(0, soBeginning);

//  mem := TMemoryStream.Create;
//  mem.LoadFromFile('level.cfg.bin');

  outfile := TFileStream.Create(fn, fmCreate);
  magic := LEVEL_BIN_MAGIC;
  flags := 2;
  len := mem.Size;
  outfile.Write(magic, Sizeof(magic));
  outfile.Write(len, Sizeof(len));

  blocksize := $020000;
  blocksizeplus9 := blocksize + 9;
  blocks := len div blocksize;
  remain := len mod blocksize;
  remainplus9 := remain + 9;
  while blocks > 0 do
  begin
    outfile.Write(flags, Sizeof(flags));
    outfile.Write(blocksizeplus9, Sizeof(blocksizeplus9));
    outfile.Write(blocksize, Sizeof(blocksize));
    mem.Read(buf[0], blocksize);
    outfile.Write(buf[0], blocksize);
    Dec(blocks);
  end;

  outfile.Write(flags, Sizeof(flags));
  outfile.Write(remainplus9, Sizeof(remainplus9));
  outfile.Write(remain, Sizeof(remain));
  mem.Read(buf[0], remain);
  outfile.Write(buf[0], remain);

  outfile.Free;
  mem.Free;
end;

// end Metro 2033

// Metro Last Light
function LoadLevelBinLL(const fn : String) : TTextKonfig;
var
	r : TMemoryReader;
	K : TKonfig;
begin
	r := TMemoryReader.CreateFromFile(fn);

	if r.ReadLongword = LEVEL_BIN_MAGIC then
	begin
		r.ReadLongword; // skip size

		K := TKonfig.Create;
		K.Load(r);

		framework.Initialize;
		Result := framework.DecompileKonfig(K, 'js\levelbin_ll.js');
		framework.Finalize;		

	end else
	begin
		WriteLn('"', fn, '" is not level bin file!');
		Result := nil;
	end;
end;

procedure SaveLevelBinLL(const fn : String; tk : TTextKonfig; kind : Integer);
var
	K : TKonfig;
	w : TMemoryWriter;

	outf : TFileStream;
	magic, size : Longword;
begin
	K := TKonfig.Create;
	K.kind := kind;
	K.Compile(tk);

	w := TMemoryWriter.Create;
	K.Save(w);

	outf := TFileStream.Create(fn, fmCreate);
	magic := LEVEL_BIN_MAGIC;
	size := w.size;
	outf.Write(magic, Sizeof(magic));
	outf.Write(size, Sizeof(size));
	outf.Write(w.data[0], w.size);
	outf.Free;

	w.Free;
	K.Free;
end;
// end Metro Last Light

// Arktika.1 (and exodus)
function LoadLevelBinA1(const fn : String) : TTextKonfig;
var
	r : TMemoryReader;
	K : TKonfig;
begin
	r := TMemoryReader.CreateFromFile(fn);

	if r.ReadLongword = LEVEL_BIN_MAGIC then
	begin
		K := TKonfig.Create;
		K.Load(r);

		framework.Initialize;
		Result := framework.DecompileKonfig(K, 'js\levelbin_exodus.js');
		framework.Finalize;		

	end else
	begin
		WriteLn('"', fn, '" is not level bin file!');
		Result := nil;
	end;
end;

procedure SaveLevelBinA1(const fn : String; tk : TTextKonfig; kind : Integer);
var
	K : TKonfig;

	outf : TFileStream;
	magic : Longword;
begin
	K := TKonfig.Create;
	K.kind := kind;
	K.Compile(tk);

	outf := TFileStream.Create(fn, fmCreate);
	magic := LEVEL_BIN_MAGIC;
	outf.Write(magic, Sizeof(magic));
	K.Save(outf);
	outf.Free;

	K.Free;
end;
// end Arktika.1 (and exodus)


// those parameters are common for all entity classes
procedure ReadCommon(e : TMemoryReader; sect : TSection);
var
	att_mat : TFloatArrayValue;
begin
	sect.AddStr('att_bone_id', dictionary[e.ReadLongint]);
	sect.AddInt('id', e.ReadWord, 'u16');
	sect.AddInt('parent_id', e.ReadWord, 'u16');
	att_mat := TFloatArrayValue.Create('att_offset', 'pose, matrix_43T');
	att_mat.FromReader(e, 12);
	sect.items.Add(att_mat);
  if entity_version >= ENTITY_VER_ARKTIKA1 then
	 sect.AddBool('att_root', e.ReadByte <> 0);
end;

// visual script
// maybe rewrite this part in javascript, eh?
const
	CRC_ACTIONS_ENGINE_SIGNAL 				= $E5727F1E;
	CRC_LOGIC_DELAY										= $DE1F2B0E; 
	CRC_LOGIC_RANDOM									= $963238B5;
	CRC_LOGIC_LOCKER_BASE							= $EC0B00B7;
	CRC_TRIGGERS_STARTGAME						= $475F0709;
	CRC_TRIGGERS_ENGINE_SIGNAL				= $0627CE24;

procedure ReadBlock(e : TMemoryReader; sect : TSection);
var
	clsid_crc : Longword;
	clsid : String;
begin
	clsid_crc := e.ReadLongint;
	clsid := TypedString(clsid_crc);
	sect.AddStr('clsid', clsid);
	sect.AddInt('posx', e.ReadWord, 'u16');
	sect.AddInt('posy', e.ReadWord, 'u16');
      
  // other properties here	
  case clsid_crc of
  	CRC_ACTIONS_ENGINE_SIGNAL: begin
  		sect.AddHint('signal', 'choose');
  		sect.AddStr('signal', dictionary[e.ReadLongword]);
  		sect.AddInt('mode', e.ReadByte, 'u8');
  	end;
  	CRC_LOGIC_DELAY: begin
  		sect.AddHint('min', 'time');
  		sect.AddInt('min', e.ReadLongword, 'u32');
  		sect.AddHint('max', 'time');
  		sect.AddInt('max', e.ReadLongword, 'u32');
  		sect.AddInt('dflags', e.ReadByte, 'bool8');
  	end;
  	CRC_LOGIC_RANDOM: begin
  		sect.AddInt('quant', e.ReadByte, 'u8');
  	end;
  	CRC_LOGIC_LOCKER_BASE: begin
  		sect.AddInt('flags', e.ReadByte, 'bool8');
  	end;
  	CRC_TRIGGERS_STARTGAME: BEGIN
  		sect.AddBool('active', e.ReadByte <> 0);
  		sect.AddInt('flags', e.ReadByte, 'bool8');
  	end;
  	CRC_TRIGGERS_ENGINE_SIGNAL: begin
  		sect.AddBool('active', e.ReadByte <> 0);
  		sect.AddHint('signal', 'choose');
  		sect.AddStr('signal', dictionary[e.ReadLongword]);
  	end;  		
  end;
end;

procedure ReadVssVer6(r : TMemoryReader; sect : TSection);
var
  I, J : Longint;
  vs_count, block_count : Longint;

  n : String;

  arr, vs : TMemoryReader;
  vss : TSection;

  groups, blocks : TMemoryReader;
  sect_groups, sect_blocks : TSection;

  block : TMemoryReader;
  sect_block : TSection;

  clsid_crc : Longint;

  link_count : Longword;
  link : TIntegerArrayValue;
begin
	arr := ReadSection(r, 'vss_ver_6');

  vs_count := arr.ReadLongint;
  sect.AddInt('count', vs_count, 'u32');

  I := 0;
  while arr.pos < arr.size do
  begin
    n := RecStr('rec_', I, 4);
    Inc(I);

    vs := ReadSection(arr);
    vss := sect.AddSect(n);

    groups := ReadSection(vs, 'groups');
    sect_groups := vss.AddSect('groups');
    sect_groups.AddInt('count', groups.ReadLongword, 'u32');
    // it's even used ???
    groups.Free;

    blocks := ReadSection(vs, 'blocks');
    sect_blocks := vss.AddSect('blocks');
    sect_blocks.AddInt('version', blocks.ReadWord, 'u16');
    sect_blocks.AddInt('block_count', blocks.ReadLongword, 'u32'); // this is always 0xFFFFFFFF
    sect_blocks.AddInt('count', blocks.ReadLongword, 'u32'); // this is real count
    
    J := 0;
    while blocks.pos < blocks.size do
    begin
      n := RecStr('block_', J, 4);
      Inc(J);  

      block := ReadSection(blocks);
      sect_block := sect_blocks.AddSect(n);

			ReadBlock(block, sect_block);

      block.Free;
    end;

    blocks.Free;

    link_count := vs.ReadLongword;
    vss.AddInt('link_count', link_count, 'u32');

    J := 0;
    while link_count > 0 do
    begin
      link := TIntegerArrayValue.Create(IntToStr(J), 'vec4s16');
      SetLength(link.data, 4);
      link.data[0] := vs.ReadWord;  
      link.data[1] := vs.ReadWord; 
      link.data[2] := vs.ReadWord; 
      link.data[3] := vs.ReadWord; 

      vss.items.Add(link);

      Inc(J);
      Dec(link_count);
    end;

    vs.Free;
  end;
  
  arr.Free;
end;

// Arktika.1

procedure ParseStartupA1(r : TMemoryReader; s : TSection);
begin

end;

procedure ParseEntitiesA1(r : TMemoryReader; s : TSection);
var
  mat : TFloatArrayValue;
  sect : TSection;

  name : String;
  classcrc : Longint;
  staticdatakeycrc : Longint;

  e : TMemoryReader;
begin
  s.AddInt('count', r.ReadLongword, 'u32');
  while r.pos < r.size do
  begin
    e := ReadSection(r, 0);
    classcrc := e.ReadLongint;
    staticdatakeycrc := e.ReadLongint;

    if classcrc = CRC_STATICPROP then
    begin
      sect := s.AddSect('ENTITY');
      sect.AddStr('class', 'STATICPROP');

      ReadCommon(e, sect);

      name := dictionary[e.ReadLongint];
      sect.name := name;
      sect.AddStr('name', name);
      e.ReadLongword; // unknown 1
      e.ReadLongword; // unknown 2

      mat := TFloatArrayValue.Create('xform', 'pose, matrix_43T');
      mat.FromReader(e, 12);
      sect.items.Add(mat);

      sect.AddStr('visual', dictionary[e.ReadLongint]);
    end;
    e.Free;
  end;
end;

function LoadLevelBinA1(const fn : String) : TTextKonfig;
var
  r, s : TMemoryReader;
  magic : Longword;
  tk : TTextKonfig;

  I : Integer;
  data, dict : TMemoryReader;

  sect : TSection;
begin
  tk := TTextKonfig.Create;

  r := TMemoryReader.CreateFromFile(fn);
  magic := r.ReadLongword;
  bin_flags := r.ReadByte;
  if (magic = $6C76656C) and (bin_flags = 36) then
  begin
    data := r.OpenChunk(1, True);

    dict := r.OpenChunk(2, True);
    SetLength(dictionary, dict.ReadLongword);
    for I := 0 to Length(dictionary) - 1 do
      dictionary[I] := dict.ReadStringZ;
    dict.Free;

    // unknown root section
    data.ReadLongword;
    data.ReadLongword;

    // section startup
    sect := tk.root.AddSect('startup');
    s := ReadSection(data, 'startup');
    ParseStartupA1(s, sect);

    // common params for all entities, i guess
    sect := tk.root.AddSect('entities_params');
    s := ReadSection(data, 'entities_params');
    entity_version := s.ReadWord;
    sect.AddInt('version', entity_version, 'u16');
    s.Free;

    // section entities
    sect := tk.root.AddSect('entities');
    s := ReadSection(data, 'entities');
    ParseEntitiesA1(s, sect);

    data.Free;
  end;
  r.Free;
  Result := tk;
end;

// Metro Last Light

procedure ReadInterest_LL(e : TMemoryReader; sect : TSection);
var
	i : TMemoryReader;
  interest : TSection;
begin
  i := ReadSection(e, 'interest');
  interest := sect.AddSect('interest');
  interest.AddInt('min_importance', i.ReadWord, 'u16');
  interest.AddInt('max_importance', i.ReadWord, 'u16');
  interest.AddInt('interest_type', i.ReadByte, 'u8');
  interest.AddInt('duration', i.ReadWord, 'u16');
  interest.AddFloat('speed', i.ReadSingle);
  interest.AddFloat('distance', i.ReadSingle);
  i.Free;
end;

procedure ReadCommonsVs_LL(e : TMemoryReader; sect : TSection);
var
	arr : TMemoryReader;
	ref : TMemoryReader;

	cv, vr : TSection;

	I : Integer;
begin
	arr := ReadSection(e, 'commons_vs');
	cv := sect.AddSect('commons_vs');
	cv.AddInt('count', arr.ReadLongword, 'u32');
	I := 0;
	while (arr.pos < arr.size) do
	begin
		ref := ReadSection(arr);
		vr := cv.AddSect(RecStr('rec_', I, 4));
		vr.AddStr('vs_name', dictionary[ref.ReadLongint]);
		vr.AddBool('vs_debug', ref.ReadByte <> 0);
		vr.AddHint('vs_ref', 'vs_ref, str_shared');
		vr.AddStr('vs_ref, str_shared', dictionary[ref.ReadLongint]);
		vr.AddBool('vs_ref_dyn_state_exist', ref.ReadByte <> 0);
		ref.Free;		

		Inc(I);
	end;
	arr.Free;
end;

procedure ReadUObject_LL(e : TMemoryReader; sect : TSection);
var
	name : String;
	mat : TFloatArrayValue;
begin
  // class uobject
	name := dictionary[e.ReadLongint];
	sect.name := name;
	sect.AddHint('name', 'name');
	sect.AddStr('name', name);
	sect.AddInt('oflags', e.ReadByte, 'bool8');
  sect.AddInt('sflags', e.ReadByte, 'bool8');
	
	mat := TFloatArrayValue.Create('xform', 'pose, matrix_43T');
	mat.FromReader(e, 12);
	sect.items.Add(mat);
	
	sect.AddHint('visual', 'ref_model');
	sect.AddStr('visual', dictionary[e.ReadLongint]);

  sect.AddInt('dao_val', e.ReadWord, 'u16');
  ReadVssVer6(e, sect.AddSect('vss_ver_6'));
  sect.AddInt('spatial_sector', e.ReadWord, 'u16');
end;

procedure ReadUObjectEffect_LL(e : TMemoryReader; sect : TSection);
var
	I, labels : Integer;
begin
	ReadUObject_LL(e, sect);

	// class uobject_effect
	sect.AddStr('startup_animation', dictionary[e.ReadLongint]);
	sect.AddHint('bone_part', 'part_id');
	sect.AddStr('bone_part', dictionary[e.ReadLongint]);
	sect.AddInt('start_frame', e.ReadWord, 'u16');
	sect.AddBool('force_paused', e.ReadByte <> 0);
	sect.AddInt('force_looped', e.ReadByte, 'u8');
	sect.AddHint('sound', 'sound');
	sect.AddStr('sound', dictionary[e.ReadLongint]);
	sect.AddInt('sound_volume', e.ReadByte, 'fp32_q8');
	sect.AddInt('sound_filter', e.ReadByte, 'u8');
	sect.AddHint('particles', 'particles, str_shared');
	sect.AddStr('particles', dictionary[e.ReadLongint]);
	sect.AddInt('particle_flags', e.ReadByte, 'bool8');
	ReadInterest_LL(e, sect);
	sect.AddHint('labels', 'str_array16');
	labels := e.ReadWord;
	sect.AddInt('labels', labels, 'u16');
	for I := 0 to labels-1 do
		sect.AddStr('labels', dictionary[e.ReadLongint]); 
	ReadCommonsVs_LL(e, sect);
end;

procedure ParseEntities_LL(r : TMemoryReader; s : TSection);
var
  mat : TFloatArrayValue;
  sect : TSection;

  name : String;
  classcrc, staticdatakeycrc : Longword;
  _class, _staticdatakey : String;
  
  
  e : TMemoryReader;
begin
  s.AddInt('count', r.ReadLongword, 'u32');
  while r.pos < r.size do
  begin
    e := ReadSection(r);
    
    classcrc := e.ReadLongword;
		staticdatakeycrc := e.ReadLongword;

		_class := TypedString(classcrc);
		_staticdatakey := TypedString(staticdatakeycrc);
		
		WriteLn(_class, ' ', _staticdatakey);
    
    sect := TSection.Create('ENTITY');
    sect.AddStr('class', _class);
    sect.AddStr('static_data_key', _staticdatakey);
    
    ReadCommon(e, sect);
		
		case classcrc of
			CRC_STATICPROP: begin
				ReadUObject_LL(e, sect);

        // class uobject_static
        sect.AddInt('flags', e.ReadByte, 'bool8');
        sect.AddInt('collision_group', e.ReadByte, 'u8');
			end;
      CRC_O_ENTITY, CRC_o_hlamp: begin
        sect.AddFloat('health', e.ReadSingle);
        sect.AddFloat('stamina', e.ReadSingle);
        sect.AddInt('dying_mask', e.ReadLongword, 'u32');
        ReadUObjectEffect_LL(e, sect);
      end;
      CRC_EFFECT: begin
        ReadUObjectEffect_LL(e, sect);
      end;
			else
				FreeAndNil(sect);
		end;
		
		if sect <> nil then
			s.items.Add(sect);
    
    e.Free;
  end;
end;

function LoadLevelBinLL(const fn : String) : TTextKonfig;
var
  r, s : TMemoryReader;
  magic, size : Longword;
  tk : TTextKonfig;

  I : Integer;
  data, dict : TMemoryReader;

  sect : TSection;
begin
  tk := TTextKonfig.Create;

  r := TMemoryReader.CreateFromFile(fn);
  magic := r.ReadLongword;
  size := r.ReadLongword;
  bin_flags := r.ReadByte;
  if (magic = $6C76656C) and (bin_flags = 4) then
  begin
    data := r.OpenChunk(1, True);

    dict := r.OpenChunk(2, True);
    SetLength(dictionary, dict.ReadLongword);
    for I := 0 to Length(dictionary) - 1 do
      dictionary[I] := dict.ReadStringZ;
    dict.Free;

    // section startup
    sect := tk.root.AddSect('startup');
    s := ReadSection(data, 'startup');
    //ParseStartupA1(s, dictionary, sect);
    s.Free;
    
    // common params for all entities, i guess
    sect := tk.root.AddSect('entities_params');
    s := ReadSection(data, 'entities_params');
    entity_version := s.ReadWord;
    sect.AddInt('version', entity_version, 'u16');
    s.Free;

    // section entities
    sect := tk.root.AddSect('entities');
    s := ReadSection(data, 'entities');
    ParseEntities_LL(s, sect);
    s.Free;

    data.Free;
  end;
  r.Free;
  Result := tk;
end;

// Metro Exodus

procedure ReadInterest_Exodus(e : TMemoryReader; sect : TSection);
var
	i : TMemoryReader;
  interest : TSection;
begin
  i := ReadSection(e, 'interest');
  interest := sect.AddSect('interest');
  interest.AddInt('min_importance', i.ReadWord, 'u16');
  interest.AddInt('max_importance', i.ReadWord, 'u16');
  interest.AddInt('interest_type', i.ReadByte, 'u8');
  interest.AddInt('duration', i.ReadWord, 'u16');
  interest.AddFloat('speed', i.ReadSingle);
  interest.AddFloat('distance', i.ReadSingle);
  interest.AddFloat('max_angle_x', i.ReadSingle, 'angle, fp32');
  interest.AddFloat('max_angle_y', i.ReadSingle, 'angle, fp32');
  interest.AddFloat('angle_coef', i.ReadSingle);
  i.Free;
end;

procedure ReadPhysicsShell_Exodus(e : TMemoryReader; sect : TSection);
var
	ps : TMemoryReader;
begin
	ps := ReadSection(e, 'physics_shell');
	// TODO
	ps.Free;
end;

// C++ class uobject
procedure ReadUObject_Exodus(e : TMemoryReader; sect : TSection);
var
	mat : TFloatArrayValue;
	name : String;
begin
  name := dictionary[e.ReadLongint];
  sect.name := name;
  sect.AddStr('name', name);
  sect.AddInt('oflags', e.ReadByte, 'bool8');
  sect.AddInt('sflags', e.ReadByte, 'bool8');
  sect.AddFloat('cull_distance', e.ReadSingle);
  mat := TFloatArrayValue.Create('xform', 'pose, matrix_43T');
  mat.FromReader(e, 12);
  sect.items.Add(mat);
  sect.AddHint('visual', 'choose');
  sect.AddStr('visual', dictionary[e.ReadLongint]);    
  sect.AddInt('dao_val', e.ReadWord, 'u16');
  mat := TFloatArrayValue.Create('render_aux_val', 'color, vec4f');
  mat.FromReader(e, 4);
  sect.items.Add(mat);
  ReadVssVer6(e, sect.AddSect('vss_ver_6'));
  sect.AddBool('vs_active', e.ReadByte <> 0);
  sect.AddInt('spatial_sector', e.ReadWord, 'u16');
  sect.AddInt('qsave_chunk', e.ReadByte, 'u8');
end;

// C++ class uobject_static
procedure ReadUObjectStatic_Exodus(e : TMemoryReader; sect : TSection);
begin
	ReadUObject_Exodus(e, sect);
			
	sect.AddInt('flags', e.ReadByte, 'bool8');
	sect.AddInt('collision_group', e.ReadByte, 'u8');
			
	ReadInterest_Exodus(e, sect);
end;

// C++ class uobject_static_breakbale
procedure ReadUObjectStaticBreakable_Exodus(e : TMemoryReader; sect : TSection);
var
	commons_vs, removed_vs : TMemoryReader;
begin
	// class uobject_hit<>
	sect.AddFloat('health', e.ReadSingle);

	ReadUObject_Exodus(e, sect);
	
	commons_vs := ReadSection(e, 'commons_vs'); // TODO need parser
  commons_vs.Free;
  removed_vs := ReadSection(e, 'removed_vs');
  removed_vs.Free;
	
	sect.AddInt('flags', e.ReadByte, 'bool8');
	sect.AddInt('collision_group', e.ReadByte, 'u8');
			
	ReadInterest_Exodus(e, sect);
	
	// TODO
end;

// C++ class uobject_effect
procedure ReadUObjectEffect_Exodus(e : TMemoryReader; sect : TSection);
var
	commons_vs, removed_vs : TMemoryReader;
	I, labels : Longword;
begin
	ReadUObject_Exodus(e, sect);
	
	// class uobject_effect
  commons_vs := ReadSection(e, 'commons_vs'); // TODO need parser
  commons_vs.Free;
  removed_vs := ReadSection(e, 'removed_vs');
  removed_vs.Free;
  sect.AddHint('startup_animation', 'animation_str');
  sect.AddStr('startup_animation', dictionary[e.ReadLongword]);
  sect.AddHint('bone_part', 'part_str');
  sect.AddStr('bone_part', dictionary[e.ReadLongword]);
  sect.AddInt('start_frame', e.ReadWord, 'u16');
  sect.AddFloat('speed', e.ReadSingle);
  sect.AddInt('startup_animation_flags', e.ReadByte, 'bool8');
  sect.AddInt('force_looped', e.ReadByte, 'u8');
  sect.AddStr('sound', dictionary[e.ReadLongword]);
  sect.AddInt('sound_volume', e.ReadByte, 'fp32_q8');
  sect.AddInt('sound_filter', e.ReadByte, 'u8');
  sect.AddInt('partice_flags', e.ReadByte, 'bool8');
  sect.AddHint('particles', 'choose');
  sect.AddStr('particles', dictionary[e.ReadLongword]);
	ReadInterest_Exodus(e, sect);

  sect.AddHint('labels', 'str_array32');
	labels := e.ReadLongword;
  sect.AddInt('labels', labels, 'u32');
  I := 0;
  while I < labels do
  begin
  	sect.AddStr('labels', dictionary[e.ReadLongword]);
  	Inc(I);
  end;
end;

// C++ class centity
procedure ReadCEntity_Exodus(e : TMemoryReader; sect : TSection);
var
	ph_shell_writed : Boolean;
begin
	// class uobject_hit<>
	sect.AddFloat('health', e.ReadSingle);
	
	// unknown class
	sect.AddInt('dying_mask', e.ReadLongword, 'u32');
	sect.AddInt('physics_flags', e.ReadByte, 'bool8');
	sect.AddInt('physics_flags1', e.ReadByte, 'bool8');
	sect.AddInt('physics_flags2', e.ReadByte, 'bool8');

	ReadUObjectEffect_Exodus(e, sect);
	
	// class centity
  sect.AddInt('friend_type', e.ReadByte, 'u8');
  sect.AddInt('reaction_type', e.ReadByte, 'u8');
  sect.AddHint('fixed_bones', 'choose_array, str_shared');
  sect.AddStr('fixed_bones', dictionary[e.ReadLongword]);
  sect.AddFloat('break_impulse_threshold', e.ReadSingle);
  sect.AddInt('collisions_group', e.ReadByte, 'u8');
  sect.AddInt('scene_type', e.ReadByte, 'u8');
  sect.AddHint('break_particles_break', 'choose');
  sect.AddStr('break_particels_break', dictionary[e.ReadLongword]);
  sect.AddHint('break_particles_death', 'choose');
  sect.AddStr('break_particels_death', dictionary[e.ReadLongword]);
  sect.AddHint('break_sound_death', 'choose');
  sect.AddStr('break_sound_death', dictionary[e.ReadLongword]);
  sect.AddInt('break_sound_death_ai_type', e.ReadByte, 'u8');
  sect.AddHint('type_mask', 'flags64');
  sect.AddInt('type_mask', e.ReadQWord, 'u64');
  sect.AddInt('ph_shell_model_src', e.ReadLongword, 'u32');
  sect.AddInt('ph_shell_skltn_src', e.ReadLongword, 'u32');
  sect.AddInt('ph_shell_skltn_bcount', e.ReadLongword, 'u32');
  ph_shell_writed := e.ReadByte() <> 0;
  sect.AddBool('ph_shell_writed', ph_shell_writed);
  if ph_shell_writed then
  	ReadPhysicsShell_Exodus(e, sect);
  sect.AddBool('attach_with_joint', e.ReadByte <> 0);
  sect.AddFloat('footprint_size', e.ReadSingle);
  sect.AddFloat('footprint_power', e.ReadSingle);
end;

// o_hlamp
procedure ReadOHlamp_Exodus(e : TMemoryReader; sect : TSection);
var
	vec : TFloatArrayValue;
begin
	ReadCEntity_Exodus(e, sect);
	
	// class lamp
	sect.AddBool('initial_state', e.ReadByte <> 0);
	sect.AddInt('die_sound_type', e.ReadByte, 'u8');
	sect.AddHint('die_sound', 'choose');
	sect.AddStr('die_sound', dictionary[e.ReadLongword]);
	sect.AddHint('die_particle', 'choose');
	sect.AddStr('die_particle', dictionary[e.ReadLongword]);
	sect.AddHint('light_main_bone', 'attp_str');
	sect.AddStr('light_main_bone', dictionary[e.ReadLongword]);
	sect.AddHint('dark_bone', 'attp_str');
	sect.AddStr('dark_bone', dictionary[e.ReadLongword]);
	sect.AddHint('broken_bone', 'attp_str');
	sect.AddStr('broken_bone', dictionary[e.ReadLongword]);
	
	e.ReadLongword; // unknown section crc C846EFB1 name "main_light"
	e.ReadLongword; 
	
	sect.AddInt('type', e.ReadByte, 'u8');
	vec := TFloatArrayValue.Create('color', 'color, vec4f');
	vec.FromReader(e, 4);
	sect.items.Add(vec);
	sect.AddFloat('brightness', e.ReadSingle);
	sect.AddFloat('range_far', e.ReadSingle);
	sect.AddFloat('lod_scale', e.ReadSingle);
	vec := TFloatArrayValue.Create('data1', 'vec3f');
	vec.FromReader(e, 3);
	sect.items.Add(vec);
	vec := TFloatArrayValue.Create('data2', 'vec2f');
	vec.FromReader(e, 2);
	sect.items.Add(vec);
	sect.AddFloat('ibl_gen_radius', e.ReadSingle);
	sect.AddFloat('range_near', e.ReadSingle);	
	sect.AddFloat('source_size', e.ReadSingle);
	sect.AddFloat('cone', e.ReadSingle, 'angle, fp32');
	sect.AddFloat('quality', e.ReadSingle);
	vec := TFloatArrayValue.Create('position', 'vec3f');
	vec.FromReader(e, 3);
	sect.items.Add(vec);
	vec := TFloatArrayValue.Create('direction', 'vec3f');
	vec.FromReader(e, 3);
	sect.items.Add(vec);	
	vec := TFloatArrayValue.Create('right', 'vec3f');
	vec.FromReader(e, 3);
	sect.items.Add(vec);
	sect.AddHint('color_ca', 'choose');
	sect.AddStr('color_ca', dictionary[e.ReadLongword]);
	sect.AddHint('texture', 'choose');
	sect.AddStr('texture', dictionary[e.ReadLongword]);
	sect.AddHint('faces', 'flags8');
	sect.AddInt('faces', e.ReadByte, 'u8');
	sect.AddInt('light_flags1', e.ReadByte, 'bool8');
	sect.AddInt('light_flags2', e.ReadByte, 'bool8');
	
	// end unknown section
	
	sect.AddBool('color_to_aux', e.ReadByte <> 0);
	sect.AddBool('sync_color_to_aux', e.ReadByte <> 0);
	sect.AddInt('secondary_type', e.ReadByte, 'u8');
	sect.AddHint('secondary_bone', 'attp_str');
	sect.AddStr('secondary_bone', dictionary[e.ReadLongword]);
	sect.AddFloat('secondary_power', e.ReadSingle);
	sect.AddFloat('secondary_radius', e.ReadSingle);
	sect.AddBool('secondary_mul_by_ao', e.ReadByte <> 0);
	sect.AddBool('backlight', e.ReadByte <> 0);
	sect.AddInt('backlight_ref', e.ReadWord, 'entity_link, uobject_link');
	sect.AddFloat('backlight_dist', e.ReadSingle);
	sect.AddBool('backlight_dynamic', e.ReadByte <> 0);
	sect.AddBool('backlight_ignore_parents', e.ReadByte <> 0);
	sect.AddBool('backlight_brightness_compensation', e.ReadByte <> 0);
	sect.AddFloat('backlight_force_offset', e.ReadSingle);
	sect.AddHint('backlight_ray', 'choose');
	sect.AddStr('backlight_ray', dictionary[e.ReadLongword]);
	sect.AddHint('backlight_ray_particles', 'choose');
	sect.AddStr('backlight_ray_particles', dictionary[e.ReadLongword]);
	sect.AddBool('backlight_trace_npc_only', e.ReadByte <> 0);
	sect.AddInt('master', e.ReadWord, 'entity_link, uobject_link');
	
	e.ReadLongword; // unknown section crc DF9C622B
	e.ReadLongword;
	
	sect.AddHint('name', 'choose');
	sect.AddStr('name', dictionary[e.ReadLongword]);
	sect.AddHint('bone', 'choose');
	sect.AddStr('bone', dictionary[e.ReadLongword]);
	sect.AddInt('axis', e.ReadByte, 'u8');
	vec := TFloatArrayValue.Create('cmul', 'color, vec4f');
	vec.FromReader(e, 4);
	sect.items.Add(vec);
	
	// end unknown section
end;

// C++ class inventory_item_object
procedure ReadInventoryItemObject_Exodus(e : TMemoryReader; sect : TSection);
begin
	// class inventory_item
	sect.AddInt('flags0', e.ReadByte, 'bool8');
	sect.AddInt('trade_weight', e.ReadWord, 'u16');
	sect.AddInt('ui_force_slot_id', e.ReadByte, 'u8');
	
	ReadCEntity_Exodus(e, sect);
end;

// C++ class upgrade_item
procedure ReadUpgradeItem_Exodus(e : TMemoryReader; sect : TSection);
begin
	ReadInventoryItemObject_Exodus(e, sect);
    		
  // class chud_item_container (probably)
	sect.AddBool('anim_simplification', e.ReadByte <> 0);
	
	// class upgrade_item
	sect.AddHint('upgrade_id', 'choose');
	sect.AddStr('upgrade_id', dictionary[e.ReadLongword]);
	sect.AddBool('free_on_level', e.ReadByte <> 0);
end;

// C++ class uobject_aipoint
procedure ReadUObjectAIPoint_Exodus(e : TMemoryReader; sect : TSection);
var
	link : TSection;
	l : TMemoryReader;
	
	I : Integer;
	n : String;
begin
	ReadUObject_Exodus(e, sect);
	
	// class uobject_aipoint
	for I := 0 to 3 do
	begin
		n := 'link_' + IntToStr(I);
		link := sect.AddSect(n);
		
		l := ReadSection(e, n);
		link.AddInt('object', l.ReadWord, 'entity_link, uobject_link');
		link.AddFloat('weight', l.ReadSingle);
		l.Free;
	end;
	
	sect.AddInt('ai_map', e.ReadByte, 'bool8');
	sect.AddHint('cover_group', 'choose');
	sect.AddStr('cover_group', dictionary[e.ReadLongword]);
end;

// C++ class patrol_point
procedure ReadPatrolPoint_Exodus(e : TMemoryReader; sect : TSection);
begin
	ReadUObjectAIPoint_Exodus(e, sect);
	
	// class patrol point
	sect.AddInt('min_wait_time', e.ReadLongword, 'u32');
	sect.AddInt('max_wait_time', e.ReadLongword, 'u32');
	
	// class patrol_state
	sect.AddStr('body_state', dictionary[e.ReadLongword]);
	sect.AddStr('anim_state', dictionary[e.ReadLongword]);
	sect.AddStr('movement_type', dictionary[e.ReadLongword]);
	sect.AddStr('weapon_state', dictionary[e.ReadLongword]);
	sect.AddStr('action', dictionary[e.ReadLongword]);
	sect.AddInt('target', e.ReadWord, 'entity_link, uobject_link');
	sect.AddHint('flags', 'flags32');
	sect.AddInt('flags', e.ReadLongword, 'u32');
	sect.AddFloat('anim_state_approach_speed', e.ReadSingle);
	sect.AddFloat('approaching_accel', e.ReadSingle);
end;

// C++ class uobject_proxy
procedure ReadUObjectProxy_Exodus(e : TMemoryReader; sect : TSection);
var
	entities, rec : TSection;
	ee, eee : TMemoryReader;
	
	I : Integer;
begin
	ReadUObject_Exodus(e, sect);
	
	// class uobject_proxy
	sect.AddInt('slice_count', e.ReadWord, 'u16');
	sect.AddHint('entities', 'array');
	entities := sect.AddSect('entities');
	ee := ReadSection(e, 'entities');
	
	entities.AddInt('count', ee.ReadLongword, 'u32');
	I := 0;
	while ee.pos < ee.size do
	begin
		eee := ReadSection(ee);
		rec := entities.AddSect(RecStr('entity_', I, 4));
		rec.AddInt('entity', eee.ReadWord, 'entity_link, uobject_link');
		eee.Free;
		Inc(I);
	end;
	
	ee.Free;
end;

procedure ReadPlayer_Exodus(e : TMemoryReader; sect : TSection);
begin
	// ???
	sect.AddBool('reviving', e.ReadByte <> 0);
	sect.AddBool('fully_dead', e.ReadByte <> 0);
	sect.AddInt('dying_from', e.ReadByte, 'u8');
	
	ReadCEntity_Exodus(e, sect);
	
	// TODO
end;

procedure ReadWoman_Exodus(e : TMemoryReader; sect : TSection);
begin
	// ???
	sect.AddInt('unknown1', e.ReadByte, 'u8');
	sect.AddInt('unknown2', e.ReadByte, 'u8');
	
	ReadCEntity_Exodus(e, sect);
	
	// TODO
end;

// C++ class text_helper
procedure ReadTextHelper_Exodus(e : TMemoryReader; sect : TSection);
var
	clr : TFloatArrayValue;
begin
	ReadUObject_Exodus(e, sect); 
	
	// class text_helper
	sect.AddStr('text', dictionary[e.ReadLongword]);
	sect.AddHint('text_key', 'choose');
	sect.AddStr('text_key', dictionary[e.ReadLongword]);
	sect.AddFloat('size', e.ReadSingle);
	clr := TFloatArrayValue.Create('color', 'color, vec4f');
	clr.FromReader(e, 4);
	sect.items.Add(clr);
	sect.AddHint('font', 'choose');
	sect.AddStr('font', dictionary[e.ReadLongword]);
	sect.AddInt('flags0', e.ReadByte, 'bool8');
	sect.AddFloat('width', e.ReadSingle);
	sect.AddFloat('height', e.ReadSingle);
	sect.AddInt('h_alignment', e.ReadByte, 'u8');
	sect.AddFloat('display_dist', e.ReadSingle);
end;

// C++ class u_object_effect_mleaf
procedure ReadUObjectEffectMLeaf_Exodus(e : TMemoryReader; sect : TSection);
var
	clr : TFloatArrayValue;
begin
	ReadUObjectEffect_Exodus(e, sect);
	
	if entity_version < $2F then
	begin
		// in arktika.1 type was color, u32
		sect.AddInt('particles_color', e.ReadLongword, 'color, u32');
	end else
	begin
		clr := TFloatArrayValue.Create('particles_color', 'color, vec4f');
		clr.FromReader(e, 4);
		sect.items.Add(clr);
	end;
end;

// C++ class uobject_zone
procedure ReadUObjectZone_Exodus(e : TMemoryReader; sect : TSection);
var
	mat, vec : TFloatArrayValue;
	shapes, shape, box, sphere : TMemoryReader;
	
	I : Integer;
	shape_type : Longword;
	
	sect_shapes, sect_shape : TSection;
	sect_sphere, sect_box : TSection;
begin
	ReadUObject_Exodus(e, sect);
	
	// class uobject_restrictor
	sect.AddStr('label', dictionary[e.ReadLongword]);
	
	shapes := ReadSection(e, 'shapes');
	sect_shapes := sect.AddSect('shapes');
	sect_shapes.AddInt('count', shapes.ReadLongword, 'u32');
	
	I := 0;
	while shapes.pos < shapes.size do
	begin
		shape := ReadSection(shapes);
		sect_shape := sect_shapes.AddSect(RecStr('shape_', I, 2));
		
		shape_type := shape.ReadLongword;
		sect_shape.AddInt('type', shape_type, 'u32');
		
		case shape_type of
			0: begin
				sphere := ReadSection(shape, 'sphere');
				sect_sphere := sect_shape.AddSect('sphere');
				
				vec := TFloatArrayValue.Create('center', 'vec3f');
				vec.FromReader(sphere, 3);
				sect_sphere.items.Add(vec);
				sect_sphere.AddFloat('radius', sphere.ReadSingle);
				
				sphere.Free;
			end;
			1: begin
				box := ReadSection(shape, 'box');
				sect_box := sect_shape.AddSect('box');
				
				mat := TFloatArrayValue.Create('', 'pose, matrix_43T');
				mat.FromReader(box, 12);
				sect_box.items.Add(mat);
				vec := TFloatArrayValue.Create('h_size', 'vec3f');
				vec.FromReader(box, 3);
				sect_box.items.Add(vec);
				
				box.Free;
			end;
			else
				raise Exception.Create('Unknown shape type ' + IntToStr(shape_type));
		end;
		
		shape.Free;
		Inc(I);
	end;
	shapes.Free;
	
	sect.AddInt('collisions_group', e.ReadByte, 'u8');
	sect.AddInt('obstacle_collision_group', e.ReadByte, 'u8');
	sect.AddInt('flags0', e.ReadByte, 'bool8');
	sect.AddInt('block_ai_vision', e.ReadByte, 'u8'); // if entity_version >= ENTITY_VER_EXODUS
	sect.AddInt('scene_type', e.ReadByte, 'u8');
	sect.AddHint('step_gmtrl', 'choose');
	sect.AddStr('step_gmtrl', dictionary[e.ReadLongword]);
	sect.AddInt('dynamic_mode', e.ReadByte, 'u8');
	
	// class uobject_zone
	sect.AddHint('type_mask', 'flags64');
	sect.AddInt('type_mask', e.ReadQWord, 'u64');
	sect.AddBool('type_filter_on', e.ReadByte <> 0);
end;

const	
	CRC_COSTUME_UPGRADE 					= $FFA46402;
	CRC_COMPASS										= $6F437257;
	CRC_DEVICE_UPGRADE						= $2F8EB5EC;
	CRC_VISOR											= $0B13293A;
	CRC_PLAYER_TIMER_UPGRADE 			= $CEA8466E;
	CRC_MAP_PAD_UPGRADE						= $6904B9B8;
	CRC_METAL_DETECTOR_UPGRADE		= $7A4790C5;
	CRC_MOTION_SENSOR_UPGRADE			= $874C8770;
	CRC_PULSOMETER_UPGRADE				= $6B6195DD;
	CRC_WEAPON_ITEM								= $EBC6700D;
	CRC_WEAPON_ITEM_AMMO 					= $3F38477D;
	CRC_WEAPON_ITEM_SILENCER 			= $267CE390;
	CRC_WEAPON_ITEM_SPEEDLOADER 	= $7B68434A;
	CRC_WEAPON_ITEM_OPTIC					= $4BB2C43B;
	CRC_WEAPON_ITEM_MAGAZINE 			= $512EC1FF;
	CRC_WEAPON_ITEM_LASER					= $27FDF76A;
	CRC_WEAPON_ITEM_VR						= $A4C5F75E;
	CRC_WEAPON_ITEM_VR_ATTACH			= $4F830E36;
	CRC_WEAPON_ITEM_PRESET				= $284A7374;
  CRC_SCRIPTED_ENTITY           = $DAEF9A87;
  CRC_WICK_VISUAL               = $B42BC33C;
  CRC_STATICPROP_MOVABLE				= $A0F671AC;
  CRC_STATICPROP_BREAKABLE			= $AA177401;
  CRC_O_INTEREST                = $EEFFE3BE;

procedure ParseEntitiesExodus(r : TMemoryReader; s : TSection);
var
  mat : TFloatArrayValue;
  sect : TSection;

  name : String;
  classcrc, staticdatakeycrc : Longword;
  e : TMemoryReader;

  _class, _staticdatakey : String;
begin
  s.AddInt('count', r.ReadLongword, 'u32');
  while r.pos < r.size do
  begin
    e := ReadSection(r);
    classcrc := e.ReadLongint;
    staticdatakeycrc := e.ReadLongword;
          
    _class := TypedString(classcrc);
    _staticdatakey := TypedString(staticdatakeycrc);
    
    //WriteLn(_class, ' ', _staticdatakey);
    
    sect := TSection.Create('ENTITY');
    sect.AddStr('class', _class);
    sect.AddStr('static_data_key', _staticdatakey);
      
		ReadCommon(e, sect);    

    case classcrc of
    	CRC_STATICPROP,
    	CRC_STATICPROP_MOVABLE: begin
				ReadUObjectStatic_Exodus(e, sect);
    	end;
    	CRC_STATICPROP_BREAKABLE: begin
    		ReadUObjectStaticBreakable_Exodus(e, sect);
    	end;
    	CRC_O_ENTITY,
      CRC_SCRIPTED_ENTITY: begin
				ReadCEntity_Exodus(e, sect);
				//WriteLn('O_ENTITY bytes left ', e.size-e.pos);
    	end;
			CRC_EFFECT: begin
				ReadUObjectEffect_Exodus(e, sect);
				//WriteLn('EFFECT bytes left ', e.size-e.pos);
    	end;
    	CRC_EFFECTM: begin
    		ReadUObjectEffectMLeaf_Exodus(e, sect);
    	end;
		  CRC_AMMO: begin
    		ReadInventoryItemObject_Exodus(e, sect);

				// class weapon_ammo
      	sect.AddInt('box_value', e.ReadWord, 'u16');
    	end;
    	CRC_MEDKIT: begin
    		ReadInventoryItemObject_Exodus(e, sect);

				// ???
      	sect.AddBool('anim_simplification', e.ReadByte <> 0);
      	sect.AddBool('use_hud_offset', e.ReadByte <> 0);
      
      	// class medkit
      	sect.AddInt('ampulas_num', e.ReadLongword, 'u32');	
    	end;
    	CRC_FILTER: begin
    		ReadInventoryItemObject_Exodus(e, sect);

				// ???
      	sect.AddBool('anim_simplification', e.ReadByte <> 0);
      	sect.AddBool('use_hud_offset', e.ReadByte <> 0);
      
      	// class filter
      	sect.AddFloat('timer', e.ReadSingle);    
			end;
   	 	CRC_o_hlamp: begin
    		ReadOHlamp_Exodus(e, sect);
    	end;
    	CRC_VISUALSCRIPT: begin
				ReadUObject_Exodus(e, sect);
				//WriteLn('VISUALSCRIPT bytes left ', e.size-e.pos);
    	end;
    	CRC_O_AIPOINT: begin
    		ReadUObjectAIPoint_Exodus(e, sect);
    	end;
    	CRC_PATROL_POINT: begin
				ReadPatrolPoint_Exodus(e, sect);
    	end; 
    	CRC_PROXY: begin
    		ReadUObjectProxy_Exodus(e, sect);
    	end;
    	CRC_O_HELPERTEXT: begin
    		ReadTextHelper_Exodus(e, sect);
    	end;
    	CRC_COSTUME_UPGRADE, 
    	CRC_COMPASS,
    	CRC_DEVICE_UPGRADE, 
    	CRC_VISOR,
    	CRC_PLAYER_TIMER_UPGRADE,
    	CRC_MAP_PAD_UPGRADE,
    	CRC_METAL_DETECTOR_UPGRADE,
    	CRC_MOTION_SENSOR_UPGRADE,
    	CRC_PULSOMETER_UPGRADE,
    	CRC_WEAPON_ITEM,
    	CRC_WEAPON_ITEM_AMMO,
    	CRC_WEAPON_ITEM_SILENCER,
    	CRC_WEAPON_ITEM_OPTIC,
    	CRC_WEAPON_ITEM_LASER,
    	CRC_WEAPON_ITEM_VR,
    	CRC_WEAPON_ITEM_PRESET: begin
				ReadUpgradeItem_Exodus(e, sect);
    	end;
    	CRC_WEAPON_ITEM_SPEEDLOADER,
    	CRC_WEAPON_ITEM_MAGAZINE: begin
    		// ???
    		sect.AddBool('loot_ammo_limited_by_mag_size', e.ReadByte <> 0);
    		sect.AddInt('loot_ammo_count', e.ReadWord, 'u16');
    		
    		ReadUpgradeItem_Exodus(e, sect);
    	end;
    	CRC_WEAPON_ITEM_VR_ATTACH: begin
    		ReadUpgradeItem_Exodus(e, sect);
    		sect.AddHint('preview_model', 'choose');
    		sect.AddStr('preview_model', dictionary[e.ReadLongword]);
    	end;
    	CRC_PLAYER: begin
    		ReadPlayer_Exodus(e, sect);
    	end;
    	CRC_WOMAN: begin
    		ReadWoman_Exodus(e, sect);
    	end;
      CRC_WICK_VISUAL: begin
        // TODO
        sect.AddInt('unk1', e.ReadLongword, 'u32');
        ReadCEntity_Exodus(e, sect);
        sect.AddInt('unk2', e.ReadByte, 'u8');
        sect.AddInt('unk3', e.ReadLongword, 'u32');

        WriteLn('WICK_VISUAL ', sect.name);
      end;
      CRC_O_BASEZONE: begin
      	ReadUObjectZone_Exodus(e, sect);
      end;
      CRC_O_INTEREST: begin
        ReadUObject_Exodus(e, sect);
        ReadInterest_Exodus(e, sect);
      end;
    		
    	else begin
    		// unknown class
    		WriteLn(_class, ' ', _staticdatakey);
    		FreeAndNil(sect);
    	end;
    end;
    
    if sect <> nil then
    	s.items.Add(sect);
    
    e.Free;
  end;
end;

function LoadLevelBinExodus(const fn : String) : TTextKonfig;
var
  r, s : TMemoryReader;
  magic : Longword;
  tk : TTextKonfig;

  I : Integer;
  data, dict : TMemoryReader;

  sect : TSection;
begin
  AddTypedString('actions/engine/signal');
  AddTypedString('triggers/engine/signal');

  tk := TTextKonfig.Create;

  r := TMemoryReader.CreateFromFile(fn);
  magic := r.ReadLongword;
  bin_flags := r.ReadByte;
  if (magic = $6C76656C) and (bin_flags = 36) then
  begin
    data := r.OpenChunk(1, True);

    dict := r.OpenChunk(2, True);
    SetLength(dictionary, dict.ReadLongword);
    for I := 0 to Length(dictionary) - 1 do
      dictionary[I] := dict.ReadStringZ;
    dict.Free;

    // unknown root section
    data.ReadLongword;
    data.ReadLongword;

    // section startup
    sect := tk.root.AddSect('startup');
    s := ReadSection(data, 'startup');
    //ParseStartupA1(s, dictionary, sect);
    s.Free;
    
    // common params for all entities, i guess
    sect := tk.root.AddSect('entities_params');
    s := ReadSection(data, 'entities_params');
    entity_version := s.ReadWord;
    sect.AddInt('version', entity_version, 'u16');
    s.Free;

    // section entities
    sect := tk.root.AddSect('entities');
    s := ReadSection(data, 'entities');
    ParseEntitiesExodus(s, sect);
    s.Free;

    data.Free;
  end;
  r.Free;
  Result := tk;
end;

end.