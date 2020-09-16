unit script_block_descs;

interface
uses Konfig;

const
	BlockSizeX = 150;
	BlockSizeY = 150;

type
	TBlockDesc = class
		clsid : String;
		sizex, sizey : Longint;
		
		in_names : array of String;
		out_names : array of String;
		
		props : TSection;
		
		constructor Create;
		destructor Destroy; override;
	end;
	
var	
	block_descs : array of TBlockDesc;
	
procedure LoadDescs(const fn : String);
procedure UnloadDescs;

function GetBlockDesc(blk : TSection; out unique : Boolean) : TBlockDesc;

implementation
uses classes, sysutils;

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
	
procedure LoadDescs(const fn : String);
var
	I, J : Integer;
	
	k : TTextKonfig;
	s : TSection;
	
	names : TStringList;
	props : TSection;
begin
	k := TTextKonfig.Create;
	k.LoadFromFile(fn);
	
	SetLength(block_descs, k.root.items.Count + 1);
	
	// block_descs[0] is default desc
	block_descs[0] := TBlockDesc.Create;
	block_descs[0].sizex := BlockSizeX;
	block_descs[0].sizey := BlockSizeY;
	SetLength(block_descs[0].in_names, 5);
	for I := 0 to Length(block_descs[0].in_names) - 1 do
		block_descs[0].in_names[I] := 'in'+IntToStr(I);
	SetLength(block_descs[0].out_names, 5);
	for I := 0 to Length(block_descs[0].out_names) - 1 do
		block_descs[0].out_names[I] := 'out'+IntToStr(I);
		
	for I := 1 to Length(block_descs) - 1 do
	begin
		s := TSimpleValue(k.root.items[I-1]) as TSection;
		
		block_descs[I] := TBlockDesc.Create;
		block_descs[I].clsid := s.GetStr('clsid');
		block_descs[I].sizex := s.GetInt('sizex', BlockSizeX, 'u32');
		block_descs[I].sizey := s.GetInt('sizey', BlockSizeY, 'u32');
		
		names := TStringList.Create;
		
		names.CommaText := s.GetStr('in_names');
		SetLength(block_descs[I].in_names, names.Count);
		for J := 0 to names.Count - 1 do
			block_descs[I].in_names[J] := names[J];
			
		names.CommaText := s.GetStr('out_names');
		SetLength(block_descs[I].out_names, names.Count);
		for J := 0 to names.Count - 1 do
			block_descs[I].out_names[J] := names[J];
			
		names.Free;
		
		props := s.GetSect('properties', False);
		if Assigned(props) then
			for J := 0 to props.items.Count - 1 do
				block_descs[I].props.items.Add(TSimpleValue(props.items[J]).Copy);
	end;
	
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
		desc.sizex := BlockSizeX;
		desc.sizey := BlockSizeY;
		
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

end.