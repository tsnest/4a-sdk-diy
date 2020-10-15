unit uLevelUndo;

interface

procedure DoUndo;
procedure DoRedo;

procedure UndoClearHistory;
procedure UndoSave;

implementation
uses Konfig, uScene;

type 
  TUndoRec = record
    konf : TTextKonfig;
    konf_add : TTextKonfig;
  end;
  
var
  undo_buffer : array[0..63] of TUndoRec;
  next : Longint = 0;
  
var
  redo_buffer : array[0..63] of TUndoRec;
  redo_next : Longint = 0;
 
procedure UndoClear;
var
  I : Longint;
begin
  for I := 0 to next-1 do
  begin
    undo_buffer[I].konf.Free;
    undo_buffer[I].konf_add.Free;
  end;
  
  next := 0;
end;
  
procedure UndoPush;
begin
  if next > High(undo_buffer) then
  begin
    undo_buffer[0].konf.Free;
    undo_buffer[0].konf_add.Free;
    
    Move(undo_buffer[1], undo_buffer[0], (Length(undo_buffer)-1)*Sizeof(TUndoRec) );
    Dec(next);
  end;
  
  undo_buffer[next].konf := Scene.konf.Copy;
  undo_buffer[next].konf_add := Scene.konf_add.Copy;
    
  Inc(next);
  
  WriteLn('undo_push next = ', next);
end;

procedure UndoPop;
begin
  Scene.UnloadEntities;
  Scene.LoadEntities(undo_buffer[next-1].konf.Copy, undo_buffer[next-1].konf_add.Copy);
  
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
  end;
  
  redo_next := 0;
end;

procedure RedoPush;
begin
  if redo_next > High(redo_buffer) then
  begin
    redo_buffer[0].konf.Free;
    redo_buffer[0].konf_add.Free;
    
    Move(redo_buffer[1], redo_buffer[0], (Length(redo_buffer)-1)*Sizeof(TUndoRec) );
    Dec(next);
  end;
  
  redo_buffer[redo_next].konf := Scene.konf.Copy;
  redo_buffer[redo_next].konf_add := Scene.konf_add.Copy;
    
  Inc(redo_next);
  
  WriteLn('redo_push next = ', redo_next);
end;

procedure RedoPop;
begin
  Scene.UnloadEntities;
  Scene.LoadEntities(redo_buffer[redo_next-1].konf.Copy, redo_buffer[redo_next-1].konf_add.Copy);
  
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