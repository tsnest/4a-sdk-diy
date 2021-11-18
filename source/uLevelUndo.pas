unit uLevelUndo;

interface

procedure SetUndoLimit(limit : Longint);

procedure DoUndo;
procedure DoRedo;

procedure UndoClearHistory;
procedure UndoSave;

implementation
uses classes, Konfig, uScene, Windows;

type 
  TUndoRec = record
    konf : TTextKonfig;
    konf_add : TTextKonfig;
    konf_env : TTextKonfig;
  end;
  
var
  undo_buffer : array of TUndoRec;
  next : Longint = 0;
  
var
  redo_buffer : array of TUndoRec;
  redo_next : Longint = 0;
 
procedure SetUndoLimit(limit : Longint);
begin
  UndoClearHistory;
  SetLength(undo_buffer, limit);
  SetLength(redo_buffer, limit);
end;
 
procedure UndoClear;
var
  I : Longint;
begin
  for I := 0 to next-1 do
  begin
    undo_buffer[I].konf.Free;
    undo_buffer[I].konf_add.Free;
    undo_buffer[I].konf_env.Free;
  end;
  
  next := 0;
end;
  
procedure UndoPush;
begin
  if Length(undo_buffer) = 0 then
    Exit;
  
  if next > High(undo_buffer) then
  begin
    undo_buffer[0].konf.Free;
    undo_buffer[0].konf_add.Free;
    undo_buffer[0].konf_env.Free;
    
    Move(undo_buffer[1], undo_buffer[0], (Length(undo_buffer)-1)*Sizeof(TUndoRec) );
    Dec(next);
  end;
  
  undo_buffer[next].konf := Scene.konf.Copy;
  undo_buffer[next].konf_add := Scene.konf_add.Copy;
  undo_buffer[next].konf_env := Scene.SaveEnvironment;
    
  Inc(next);
  
  WriteLn('undo_push next = ', next);
end;

procedure UndoPop;
begin
  Scene.UnloadEntities;
  Scene.LoadEntities(undo_buffer[next-1].konf.Copy, undo_buffer[next-1].konf_add.Copy);
  
  Scene.UnloadEnvironment;
  Scene.env_zones := TList.Create;
  Scene.LoadEnvironment(undo_buffer[next-1].konf_env);
  
  Dec(next);
  
  WriteLn('undo_pop next = ', next);
end;

procedure RedoClear;
var
  I : Longint;
begin
  for I := 0 to redo_next-1 do
  begin
    redo_buffer[I].konf.Free;
    redo_buffer[I].konf_add.Free;
    redo_buffer[I].konf_env.Free;
  end;
  
  redo_next := 0;
end;

procedure RedoPush;
begin
  if Length(redo_buffer) = 0 then
    Exit;
  
  if redo_next > High(redo_buffer) then
  begin
    redo_buffer[0].konf.Free;
    redo_buffer[0].konf_add.Free;
    redo_buffer[0].konf_env.Free;
    
    Move(redo_buffer[1], redo_buffer[0], (Length(redo_buffer)-1)*Sizeof(TUndoRec) );
    Dec(next);
  end;
  
  redo_buffer[redo_next].konf := Scene.konf.Copy;
  redo_buffer[redo_next].konf_add := Scene.konf_add.Copy;
  redo_buffer[redo_next].konf_env := Scene.SaveEnvironment;
    
  Inc(redo_next);
  
  WriteLn('redo_push next = ', redo_next);
end;

procedure RedoPop;
begin
  Scene.UnloadEntities;
  Scene.LoadEntities(redo_buffer[redo_next-1].konf.Copy, redo_buffer[redo_next-1].konf_add.Copy);
  
  Scene.UnloadEnvironment;
  Scene.env_zones := TList.Create;
  Scene.LoadEnvironment(redo_buffer[redo_next-1].konf_env);
  
  Dec(redo_next);
  
  WriteLn('redo_pop next = ', redo_next);
end;

procedure DoUndo;
begin
  if next > 0 then
  begin
    RedoPush;
    UndoPop;
  end;
end;

procedure DoRedo;
begin
  if redo_next > 0 then
  begin
    UndoPush;
    RedoPop;
  end;
end;

procedure UndoClearHistory;
begin
  UndoClear;
  RedoClear;
end;

procedure UndoSave;
begin
  RedoClear;
  UndoPush;
end;

end.