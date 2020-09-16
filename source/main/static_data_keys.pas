uses classes, sysutils, Konfig;

var
	sr : TSearchRec;
	T : TTextKonfig;
	list : TStringList;
	entities, E : TSection;
	I : Longint;
	fn : String;
	
begin
	list := TStringList.Create;
	
  if FindFirst('maps\*', faDirectory, sr) = 0 then
  begin
    repeat
      if not ((sr.Name = '.') or (sr.Name = '..')) then
      begin
        T := TTextKonfig.Create;
        T.LoadFromFile('maps\' + sr.Name + '\level.txt');
        
        entities := T.root.GetSect('entities');
        
        for I := 1 to entities.ParamCount-1 do
        begin
        	E := entities.GetParam(I) as TSection;
        	fn := 'static_data\' + LowerCase(E.GetStr('class') + '_' + E.GetStr('static_data_key'));
        	
        	if list.IndexOf(fn) < 0 then
        		list.Add(fn);
        end;
        
        T.Free;
      end;
       
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  
  for I := 0 to list.Count-1 do
  	WriteLn(list[I]);
	
	list.Free;
end.