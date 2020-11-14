unit script_block_descs;

interface
uses Konfig, Iup, glfont;

const
	BlockSizeX = 150;
	BlockSizeY = 150;
	
	BlockMarginX = 10;
	BlockMarginY = 5;
	BlockMarginPointX = 20;

type
	TBlockDesc = class
		clsid : String;
		
		in_names : array of String;
		out_names : array of String;
		
		props : TSection;
		
		constructor Create;
		destructor Destroy; override;
	end;
	
var	
	block_descs : array of TBlockDesc;
	
procedure LoadDescs(const fn : String; tree : Ihandle);
procedure UnloadDescs;

function GetBlockDesc(blk : TSection; out unique : Boolean) : TBlockDesc;

procedure CalcBlockSize(desc : TBlockDesc; font : TGLFont; out width, height : Longint);

implementation
uses classes, sysutils, Math;

constructor TBlockDesc.Create;
begin
	inherited Create;
	props := TSection.Create;
end;

destructor TBlockDesc.Destroy;
begin
	props.Free;
	inherited;
end;

procedure _LoadDescs(sect : TSection; tree : Ihandle; ref : Longint);
var
	I, J : Integer;
	
	s : TSection;
	desc : TBlockDesc;
	
	names : TStringList;
	props : TSection;
begin
	for I := sect.ParamCount - 1 downto 0 do
	begin
		s := sect.GetParam(I) as TSection;
		
		if s.GetParam('clsid', 'stringz') <> nil then
		begin
			J := Length(block_descs);
			SetLength(block_descs, J+1);
			
			block_descs[J] := TBlockDesc.Create;
			desc := block_descs[J];
			
			desc.clsid := s.GetStr('clsid');
			
			names := TStringList.Create;
			
			names.CommaText := s.GetStr('in_names');
			SetLength(desc.in_names, names.Count);
			for J := 0 to names.Count - 1 do
				desc.in_names[J] := names[J];
				
			names.CommaText := s.GetStr('out_names');
			SetLength(desc.out_names, names.Count);
			for J := 0 to names.Count - 1 do
				desc.out_names[J] := names[J];
				
			names.Free;
			
			props := s.GetSect('properties', False);
			if Assigned(props) then
				for J := 0 to props.items.Count - 1 do
					desc.props.items.Add(TSimpleValue(props.items[J]).Copy);
					
			IupSetStrAttribute(tree, PAnsiChar('ADDLEAF' + IntToStr(ref)), PAnsiChar(s.name));
			J := IupGetInt(tree, 'LASTADDNODE');
			IupSetAttribute(tree, PAnsiChar('USERDATA' + IntToStr(J)), Pointer(desc));
		end else
		begin
			IupSetAttribute(tree, PAnsiChar('ADDBRANCH' + IntToStr(ref)), PAnsiChar(s.name));
			J := IupGetInt(tree, 'LASTADDNODE');
			IupSetAttribute(tree, PAnsiChar('USERDATA' + IntToStr(J)), nil);
			
			_LoadDescs(s, tree, J);
		end;
	end;
end;
	
procedure LoadDescs(const fn : String; tree : Ihandle);
var
	I : Integer;
	k : TTextKonfig;
begin
	k := TTextKonfig.Create;
	k.LoadFromFile(fn);
	
	SetLength(block_descs, 1);
	
	// block_descs[0] is default desc
	block_descs[0] := TBlockDesc.Create;
	SetLength(block_descs[0].in_names, 5);
	for I := 0 to Length(block_descs[0].in_names) - 1 do
		block_descs[0].in_names[I] := 'in'+IntToStr(I);
	SetLength(block_descs[0].out_names, 5);
	for I := 0 to Length(block_descs[0].out_names) - 1 do
		block_descs[0].out_names[I] := 'out'+IntToStr(I);
		
	_LoadDescs(k.root, tree, -1);
	
	k.Free;
end;

procedure UnloadDescs;
var
	I : Integer;
begin
	for I := 0 to Length(block_descs) - 1 do
		block_descs[I].Free;
	SetLength(block_descs, 0);
end;

function GetBlockDesc(blk : TSection; out unique : Boolean) : TBlockDesc;
var
	desc : TBlockDesc;
	I : Longint;
	clsid : String;
	cnt : Longint;
begin
	clsid := blk.GetStr('clsid');

	if clsid = 'trade/trade trigger' then
	begin
		unique := True;
		
		desc := TBlockDesc.Create;
		desc.clsid := clsid;
		
		SetLength(desc.in_names, 2);
		desc.in_names[0] := 'enable';
		desc.in_names[1] := 'disable';
		
		cnt := blk.GetInt('objects_count', 'u32');
		SetLength(desc.out_names, 7 + (cnt * 2));
		desc.out_names[0] := 'nomoney';
		desc.out_names[1] := 'success';
		desc.out_names[2] := 'toomany';
		desc.out_names[3] := 'finish';
		desc.out_names[4] := 'the same';
		desc.out_names[5] := 'no buy (for ammo)';
		desc.out_names[6] := 'no sell (for ammo)';
		for I := 0 to cnt -1 do
		begin
			desc.out_names[7+I*2  ] := 'Object ' + IntToStr(I) + ' activate';
			desc.out_names[7+I*2+1] := 'Object ' + IntToStr(I) + ' deactivate';
		end;
		
	end else
	begin
		unique := False;
		
		desc := block_descs[0]; // default
		for I := 0 to Length(block_descs) - 1 do
			if block_descs[I].clsid = clsid then
			begin
				desc := block_descs[I];
				Break;
			end;
	end;
	
	Result := desc;
end;

procedure CalcBlockSize(desc : TBlockDesc; font : TGLFont; out width, height : Longint);
const
	GAP_POINT = 10;
var
	I : Longint;
	maxln : Longint;
	w : Longint;
begin
	width := Max(BlockSizeX, font.StringWidth(desc.clsid) + BlockMarginX*2);
	
	maxln := Max(Length(desc.in_names), Length(desc.out_names));
	for I := 0 to maxln-1 do
	begin
		w := GAP_POINT + BlockMarginPointX*2;
		if I < Length(desc.in_names) then
			Inc(w, font.StringWidth(desc.in_names[I]));
		if I < Length(desc.out_names) then
			Inc(w, font.StringWidth(desc.out_names[I]));
			
		width := Max(w, width);
	end;
	
	height := BlockMarginY*2 + font.cell_height*2 + maxln * font.cell_height;
end;

end.