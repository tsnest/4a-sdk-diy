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
				
				for J := 0 to 1024 - 1 do
				begin		
					offset := id*(1024*1024*4) + I*1024*4 + J*4;
					
					if soc_version then
					begin
						// SoC version
						data[offset + 0] := ScaleAO((src+0)^);
						data[offset + 1] := ScaleAO((src+1)^);
						data[offset + 2] := ScaleAO((src+2)^);
						data[offset + 3] := ScaleAO((src+0)^);
					end else
					begin
						// CS/CoP version
						data[offset + 0] := ScaleAO((src+3)^);
						data[offset + 1] := ScaleAO((src+3)^);
						data[offset + 2] := ScaleAO((src+3)^);
						data[offset + 3] := ScaleAO((src+3)^);
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
	data : array of Byte;
	f : File;
begin
	SetLength(data, 4*1024*1024*4);
	FillChar(data[0], Length(data), #0);
	
	if FileExists(path + '\lmap#1_2.dds') then
		LoadLMap(data, 0, path + '\lmap#1_2.dds');
	if FileExists(path + '\lmap#2_2.dds') then
		LoadLMap(data, 1, path + '\lmap#2_2.dds');
	if FileExists(path + '\lmap#3_2.dds') then
		LoadLMap(data, 2, path + '\lmap#3_2.dds');
	if FileExists(path + '\lmap#4_2.dds') then
		LoadLMap(data, 3, path + '\lmap#4_2.dds');
		
	Assign(f, outf);
	Rewrite(f,1);
	
	BlockWrite(f, data[0], Length(data));
	
	Close(f); 
end;

end.