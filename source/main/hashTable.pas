unit hashTable;

interface

type
  PHashTableItem = ^THashTableItem;
  THashTableItem = record
    str : String;
    hash : Longint;
    value : Pointer;

    next, prev : PHashTableItem;
  end;

  THashTable = class
    private
    list : PHashTableItem;

    function NewItem(const key : String; value : Pointer) : PHashTableItem;
    procedure DeleteItem(item : PHashTableItem);
    function Find(hash : Longint; const str : String) : PHashTableItem;

    function FindValue(const key : String) : Pointer;
    procedure SetValue(const key : String; value : Pointer);

    procedure FreeValue(value : Pointer); virtual;

    public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const key : String; p : Pointer);
    procedure Remove(const key : String);
    function  Get(const key : String) : Pointer;

    property Items[const key : String] : Pointer read FindValue write SetValue; default;
  end;

  THashTableObject = class(THashTable)
    procedure FreeValue(value : Pointer); override;
  end;

implementation
uses uCrc;

function THashTable.NewItem(const key : String; value : Pointer) : PHashTableItem;
var
  I : PHashTableItem;
begin
  New(I);
  I^.str := key;
  I^.hash := GetStringCrc(key);
  I^.value := value;

  I^.next := list;
  if I^.next <> nil then
    I^.next^.prev := I;
  list := I;
  I^.prev := nil;

  Result := I;
end;

procedure THashTable.DeleteItem(item : PHashTableItem);
begin
  if item^.next <> nil then
    item^.next^.prev := item^.prev;
  if item^.prev <> nil then
    item^.prev^.next := item^.next
  else // item.prev = nil so item = list
    list := item^.next;

  FreeValue(item^.value);
  Dispose(item);
end;

function THashTable.Find(hash : Longint; const str : String) : PHashTableItem;
var
  I : PHashTableItem;
begin
  I := list;
  while I <> nil do
  begin
    if I^.hash = hash then
      if I^.str = str then
      begin
        Result := I;
        Exit;
      end;

    I := I^.next;
  end;

  Result := Nil;
end;

function THashTable.FindValue(const key : String) : Pointer;
var
  I : PHashTableItem;
begin
  I := Find(GetStringCrc(key), key);
  if I <> nil then
    FindValue := I^.value
  else
    FindValue := nil;
end;

procedure THashTable.SetValue(const key : String; value : Pointer);
var
  I : PHashTableItem;
begin
  I := Find(GetStringCrc(key), key);
  if I <> nil then
    I^.value := value
  else
    NewItem(key, value);
end;

procedure THashTable.FreeValue(value : Pointer);
begin

end;

constructor THashTable.Create;
begin
  inherited Create;
  list := nil;
end;

destructor THashTable.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THashTable.Clear;
var
  I, next : PHashTableItem;
begin
  I := list;
  while I <> nil do
  begin
    next := I^.next;
    FreeValue(I^.value);
    Dispose(I);
    I := next;
  end;
  list := nil;
end;

procedure THashTable.Add(const key: string; p: Pointer);
begin
  Items[key] := p;
end;

procedure THashTable.Remove(const key: string);
var
  I : PHashTableItem;
begin
  I := Find(GetStringCrc(key), key);
  if I <> nil then
    DeleteItem(I);
end;

function THashTable.Get(const key: string) : Pointer;
begin
  Result := Items[key];
end;

procedure THashTableObject.FreeValue(value : Pointer);
begin
  TObject(value).Free;
end;

end.