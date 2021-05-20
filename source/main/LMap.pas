unit LMap;

interface

var ao_scale : Single = 1.0;
function ScaleAO(ao : Byte) : Byte;

var soc_version : Boolean = False;
procedure ConvertLightMap(const path, outf : String); 

implementation
uses sysutils, FreeImage;

function ScaleAO(ao : Byte) : Byte;
var
	s : Single;
begin
	s := ao * ao_scale;
	if s < 0 then s := 0;
	if s > 255.0 then s := 255;
	ScaleAO := Trunc(s);
end;

procedure LoadLMap(var data : array of Byte; id : Shortint; const fn : String);
var
	dib, dib32 : FIBITMAP;
	src : PByte;
	I, J, offset : Longint;
begin
	dib := FreeImage_Load(FIF_DDS, PAnsiChar(fn));
	if dib <> nil then
	begin
		if (FreeImage_GetWidth(dib) = 1024) and (FreeImage_GetHeight(dib) = 1024) then
		begin
			if FreeImage_GetBPP(dib) <> 32 then
			begin
				dib32 := FreeImage_ConvertTo32Bits(dib);
				FreeImage_Unload(dib);
				dib := dib32;
			end;
			
			for I := 0 to 1024 - 1 do
			begin
				src := FreeImage_GetScanLine(dib, 1023-I);
				offset := ((id div 4) * (1024*1024*4)) + (I*4096) + ((id mod 4) * 1024);
				
				for J := 0 to 1024 - 1 do
				begin
						
					if soc_version then
					begin
						// SoC version
						data[offset + J] := ScaleAO( ((src+0)^ + (src+1)^ + (src+2)^) div 3 ); 
						// в лайтмапах из Тени Чернобыля AO хранится в RGB, поэтому возьмём среднее арифметическое из всех трёх каналов
					end else
					begin
						// CS/CoP version
						data[offset + J] := ScaleAO( (src+3)^ ); // в Чистом Небе и Зове Припяти AO хранится в альфе, берём как есть
					end;
					
					Inc(src, 4);
				end;
			end;
			
		end else
			WriteLn('Invalid lightmap ''', fn, ''' resolution, must be 1024x1024');
		
		FreeImage_Unload(dib);
	end else
		WriteLn('Lightmap ''', fn, ''' loading failed');
end;

procedure ConvertLightMap(const path, outf : String);
var
	I : Longint;
	name : String;
	
	data : array of Byte;
	f : File;
begin
	SetLength(data, 4096*4096);
	FillChar(data[0], Length(data), #0);
	
	for I := 1 to 16 do
	begin
		name := path + '\lmap#' + IntToStr(I) + '_2.dds';
		if FileExists(name) then
			LoadLMap(data, I-1, name);	
	end;
		
	Assign(f, outf);
	Rewrite(f,1);
	
	BlockWrite(f, data[0], Length(data));
	
	Close(f); 
end;

end.