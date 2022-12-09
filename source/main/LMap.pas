unit LMap;

interface

var ao_scale : Single = 1.0;
function ScaleAO(ao : Byte) : Byte;

var soc_version : Boolean = False;
procedure ConvertLightMap(const path, outf : String; targetXBox : Boolean); 

implementation
uses sysutils, FreeImage, crnlib;

const
	LMAP_SIZE = 4096;

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
						// � ��������� �� ���� ��������� AO �������� � RGB, ������� ������ ������� �������������� �� ���� ��� �������
					end else
					begin
						// CS/CoP version
						data[offset + J] := ScaleAO( (src+3)^ ); // � ������ ���� � ���� ������� AO �������� � �����, ���� ��� ����
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

// a bit reworked code from Xenia emulator
// I don't understand how it works
// https://github.com/xenia-project/xenia/blob/7dd715ea6fdd008c242f3d8b6d53d82e0ac348ad/src/xenia/gpu/texture_conversion.cc#L93
function TiledBlockCoord(x, y, width : Longint) : Longint;
const
	DXT1_BLOCK_SZ = 8;
var
	log2_bpp : Longint;
	macro1, micro1 : Longint;
	row_offset : Longint;
	macro2, micro2 : Longint;
	offset1, offset2 : Longint;
	block_id : Longint;
begin
	log2_bpp := (DXT1_BLOCK_SZ div 4) + ((DXT1_BLOCK_SZ div 2) shr (DXT1_BLOCK_SZ div 4));
	macro1 := ((y div 32) * (width div 32)) shl (log2_bpp + 7);
	micro1 := ((y and 6) shl 2) shl log2_bpp;
	row_offset := macro1 + ((micro1 and (not $F)) shl 1) + (micro1 and $F) + ((y and 8) shl (3 + log2_bpp)) + ((y and 1) shl 4);
	macro2 := (x div 32) shl (log2_bpp + 7);
	micro2 := (x and 7) shl log2_bpp;
	offset1 := row_offset + (macro2 + ((micro2 and (not $F)) shl 1) + (micro2 and $F));
	offset2 := ((offset1 and (not $1FF)) shl 3) + ((offset1 and $1C0) shl 2) + (offset1 and $3F) + ((y and 16) shl 7) + (((((y and 8) shr 2) + (x shr 3)) and 3) shl 6);
	block_id := offset2 shr log2_bpp;
	TiledBlockCoord := block_id;
end;

procedure ConvertLightMap(const path, outf : String; targetXBox : Boolean);
var
	I : Longint;
	name : String;
	
	data : array of Byte;
	f : File;
	
	X, Y, B : Longint;
	pixels : array[0..15] of Longword;
	dxt1 : array of QWord;
	pContext : crn_block_compressor_context_t;
	crn_params : crn_comp_params;
	
	function MakePel(ao : Byte) : Longword;
	begin
		MakePel := ao or (ao shl 8) or (ao shl 16) or (ao shl 24);
	end;
begin
	SetLength(data, LMAP_SIZE*LMAP_SIZE);
	FillChar(data[0], Length(data), #0);
	
	for I := 1 to 16 do
	begin
		name := path + '\lmap#' + IntToStr(I) + '_2.dds';
		if FileExists(name) then
			LoadLMap(data, I-1, name);	
	end;
		
	Assign(f, outf);
	Rewrite(f,1);
	
	if targetXBox then
	begin
		// compress to DXT1 & swizzle
		SetLength(dxt1, (LMAP_SIZE div 4) * (LMAP_SIZE div 4));
		
		crn_params.Clear;
		pContext := crn_create_block_compressor(crn_params);
		
		for X := 0 to (LMAP_SIZE div 4) - 1 do
		begin
			for Y := 0 to (LMAP_SIZE div 4) - 1 do
			begin
				pixels[0] := MakePel(data[(Y*4+0) * LMAP_SIZE + X*4+0]);
				pixels[1] := MakePel(data[(Y*4+0) * LMAP_SIZE + X*4+1]);
				pixels[2] := MakePel(data[(Y*4+0) * LMAP_SIZE + X*4+2]);
				pixels[3] := MakePel(data[(Y*4+0) * LMAP_SIZE + X*4+3]);
				
				pixels[4] := MakePel(data[(Y*4+1) * LMAP_SIZE + X*4+0]);
				pixels[5] := MakePel(data[(Y*4+1) * LMAP_SIZE + X*4+1]);
				pixels[6] := MakePel(data[(Y*4+1) * LMAP_SIZE + X*4+2]);
				pixels[7] := MakePel(data[(Y*4+1) * LMAP_SIZE + X*4+3]);
				
				pixels[8] := MakePel(data[(Y*4+2) * LMAP_SIZE + X*4+0]);
				pixels[9] := MakePel(data[(Y*4+2) * LMAP_SIZE + X*4+1]);
				pixels[10] := MakePel(data[(Y*4+2) * LMAP_SIZE + X*4+2]);
				pixels[11] := MakePel(data[(Y*4+2) * LMAP_SIZE + X*4+3]);
				
				pixels[12] := MakePel(data[(Y*4+3) * LMAP_SIZE + X*4+0]);
				pixels[13] := MakePel(data[(Y*4+3) * LMAP_SIZE + X*4+1]);
				pixels[14] := MakePel(data[(Y*4+3) * LMAP_SIZE + X*4+2]);
				pixels[15] := MakePel(data[(Y*4+3) * LMAP_SIZE + X*4+3]);
				
				B := TiledBlockCoord(X, Y, (LMAP_SIZE div 4));
				crn_compress_block(pContext, @pixels[0], @dxt1[B]);
			end;
		end;
		
		crn_free_block_compressor(pContext);
		
		BlockWrite(f, dxt1[0], Length(dxt1)*Sizeof(QWord));
	end else
		BlockWrite(f, data[0], Length(data));
	
	Close(f); 
end;

end.