unit uEditorUtils;

interface
uses vmath, Iup;

function MainDialog : Ihandle;

procedure Redisplay;
procedure ShowError(const message : String);

procedure WriteMatrix(const matrix : TMatrix);
procedure WriteVec3(const v : TVec3);

type TCondFunc = function(const s : String) : Boolean;
function GenerateName(src : String; cond : TCondFunc) : String;

function SelectColor(var clr : TVec4; alpha : Boolean = False) : Boolean;

implementation
uses sysutils;

function MainDialog : Ihandle;
begin
	Result := IupGetHandle('MAINDIALOG')
end;

procedure Redisplay;
var
	gl : Ihandle;
begin
	gl := IupGetDialogChild(MainDialog, 'GL_CANVAS');
	IupRedraw(gl, 0);
end;

procedure ShowError(const message : String);
begin
	IupMessageError(MainDialog, PAnsiChar(Message));
end;

procedure WriteMatrix(const matrix : TMatrix);
begin
	WriteLn(matrix[1,1], matrix[1,2], matrix[1,3], matrix[1,4]);		 
	WriteLn(matrix[2,1], matrix[2,2], matrix[2,3], matrix[2,4]);
	WriteLn(matrix[3,1], matrix[3,2], matrix[3,3], matrix[3,4]);
	WriteLn(matrix[4,1], matrix[4,2], matrix[4,3], matrix[4,4]);
end;

procedure WriteVec3(const v : TVec3);
begin
	WriteLn('x = ', v.x, ' y = ', v.y, ' z = ', v.z);
end;

function GenerateName(src : String; cond : TCondFunc) : String;
var
	I : Longint;
	tail : String;
	cut : Boolean;
	num, name : String;
	
	function IsDigit(c : Char) : Boolean;
	begin
		IsDigit := (c >= '0') and (c <= '9');
	end;
begin
	if cond(src) then
	begin
		GenerateName := src
	end else
	begin
		// cut "_NNNN" tail if exist
		if Length(src) > 5 then
		begin
			tail := Copy(src, Length(src)-4, 5);
			cut := (tail[1] = '_') and 
							IsDigit(tail[2]) and 
							IsDigit(tail[3]) and 
							IsDigit(tail[4]) and 
							IsDigit(tail[5]);
							
			if cut then
				src := Copy(src, 1, Length(src)-5);
		end;
				
		I := 0;
		repeat
			num := IntToStr(I);
			name := src + '_' + StringOfChar('0',4-Length(num)) + num;
			Inc(I);
		until cond(name);
		
		GenerateName := name;
	end;
end;

function SelectColor(var clr : TVec4; alpha : Boolean) : Boolean;
var
	str : String;
	I, E : Integer;
	dlg : Ihandle;
begin
	dlg := IupColorDlg;

	str := IntToStr(Trunc(clr.x * 255)) + ' ' + IntToStr(Trunc(clr.y * 255)) + ' ' + IntToStr(Trunc(clr.z * 255));
	IupSetStrAttribute(dlg, 'VALUE', PAnsiChar(str));

	if alpha then
	begin
		str := IntToStr(Trunc(clr.w * 255));
		IupSetStrAttribute(dlg, 'ALPHA', PAnsiChar(str));
		IupSetAttribute(dlg, 'SHOWALPHA', 'YES');
	end;

	IupPopup(dlg, IUP_CURRENT, IUP_CURRENT);

	if IupGetInt(dlg, 'STATUS') = 1 then
	begin
		str := IupGetAttribute(dlg, 'VALUE');
		I := 1; E := 1;
		while str[E] <> ' ' do Inc(E);
		clr.x := StrToInt(Copy(str, I, E-I)) / 255;
		I := E+1; E := E+1;
		while str[E] <> ' ' do Inc(E);
		clr.y := StrToInt(Copy(str, I, E-I)) / 255;
		I := E+1; E := E+1;
		while (E <= Length(str)) and (str[E] <> ' ') do Inc(E); 
		clr.z := StrToInt(Copy(str, I, E-I)) / 255;

		if alpha then
		begin
			clr.w := IupGetInt(dlg, 'ALPHA') / 255;
		end;

		Result := True;
	end else
		Result := False;

	IupDestroy(dlg);
end;

end.