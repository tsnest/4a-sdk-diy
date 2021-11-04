program binunp;
uses classes, sysutils, Konfig, texturePrefs, levelbin,
		 framework, Konfig_reader;

function EndsWith(const s1 : String; const s2 : String) : Boolean;
var
	l1, l2 : Longint;
begin
	l1 := Length(s1);
	l2 := Length(s2);

	EndsWith := False;

	if l1 >= l2 then
		if Copy(s1, l1-l2+1, l2) = s2 then
			EndsWith := True;
end;

procedure CompileLevel(infn, outfn : String; kind : Integer);
var
	tk : TTextKonfig;
begin
	tk := TTextKonfig.Create;
	tk.LoadFromFile(infn);

	SaveLevelBin(outfn, tk, kind);

	tk.Free;
end;

procedure DecompileLevel(inf, outf : String);
var
	TK : TTextKonfig;
begin
	TK := LoadLevelBin(inf);
	if TK <> nil then
	begin
		Writeln('Saving to ', outf);
		TK.SaveToFile(outf);
		TK.Free;
	end else
		Writeln('This is not level.bin file!');
end;

procedure CompileConfig(inf, outf : String; kind : Integer; last_light : Boolean);
var
	K : TKonfig;
	T : TTextKonfig;
begin
	T := TTextKonfig.Create;
	T.LoadFromFile(inf);

	K := TKonfig.Create;
	K.kind := kind;
	K.Compile(T, last_light);

	K.Save(outf);

	K.Free;
	T.Free;
end;

procedure DecompileConfig(inf, outf : String; last_light : Boolean);
var
	K : TKonfig;
	T : TTextKonfig;
begin
	K := TKonfig.Create;
	if K.Load(inf) then
	begin
		WriteLn('kind = ', K.kind);
		T := TTextKonfig.Create;
		K.Decompile(T, last_light);
		T.SaveToFile(outf);
		T.Free;
	end;
	K.Free;
end;

procedure DecompileSpecial(script, inf, outf : String);
var
	k : TKonfig;
	tk : TTextKonfig;
	t : TTexturesBin;
begin
	k := TKonfig.Create;
	if k.Load(inf) then
	begin
		WriteLn('kind = ', K.kind);
		tk := TTextKonfig.Create;

		if EndsWith(inf, 'textures.bin') then
		begin
			t := TTexturesBin.Create;
			t.Load(k);
			WriteLn('loading ok');
			t.Save(tk);
			t.Free;
		end else
		begin
			framework.Initialize;
			tk := framework.DecompileKonfig(k, script);
			WriteLn('loading ok');
			framework.Finalize
		end;

		tk.SaveToFile(outf);
		tk.Free;
	end;
	k.Free;
end;

procedure Usage;
begin
	Writeln('Usage: ');
	Writeln(#9'binunp [-l] [-ll] -d infile outfile');
	Writeln(#9'binunp [-l] [-ll] [-k n] -c infile outfile');
	Writeln(#9'binunp -s script infile outfile');
	Writeln;
	Writeln('-l option = compile/decompile level.bin');
	Writeln('-ll option = compile/decompile bin from Last Light or later versions');
	Writeln('-k option = config type, may be 3, 4 or 5, by default 5');
end;

var
	I, kind : Integer;
	level : Boolean;
	last_light : Boolean;
begin
	kind := 5;
	level := False;
	last_light := False;

	I := 1;
	while I <= ParamCount do
	begin
		if ParamStr(I) = '-v' then
			TKonfigReader.WarnIfDataLeft := True
		else if ParamStr(I) = '-vv' then
			TKonfigReader.PrintDebugInfo := True
		else if ParamStr(I) = '-l' then
			level := True
		else if ParamStr(I) = '-ll' then	
			last_light := True
		else if ParamStr(I) = '-k' then
		begin
			kind := StrToInt(ParamStr(I+1));
			Inc(I, 1);
			
			if not (kind in [3, 4, 5, 16, 36]) then
			begin
				Writeln('Unsupported config type ', kind, ', reset to 5');
				kind := 5;
			end;
		end else
		if ParamStr(I) = '-d' then
		begin
			if (ParamCount - I) >= 2 then
			begin
				if level then
					DecompileLevel(ParamStr(I+1), ParamStr(I+2))
				else
					DecompileConfig(ParamStr(I+1), ParamStr(I+2), last_light);
					
				Inc(I, 2);
			end else
				WriteLn('Not enough parameters for -d');
		end else
		if ParamStr(I) = '-c' then
		begin
			if (ParamCount - I) >= 2 then
			begin
				if level then
					CompileLevel(ParamStr(I+1), ParamStr(I+2), kind)
				else
					CompileConfig(ParamStr(I+1), ParamStr(I+2), kind, last_light);
					
				Inc(I, 2);
			end else
				WriteLn('Not enough parameters for -c');
		end else
		if ParamStr(I) = '-s' then
		begin
			if (ParamCount - I) >= 3 then
			begin
				DecompileSpecial(ParamStr(I+1), ParamStr(I+2), ParamStr(I+3));
				Inc(I, 3);
			end else
				WriteLn('Not enough parameters for -s');
		end else		
			Writeln('Unknown parameter ', ParamStr(I));
		
		Inc(I);
	end;
end.