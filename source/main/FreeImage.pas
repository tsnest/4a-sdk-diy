unit FreeImage;

interface

type
	FIBITMAP = Pointer;
	FIBOOL = Longint;
	
const
	FI_DLL = 'FreeImage.dll';
	FIF_DDS = 24;
	
{$IFDEF WIN64}
function  FreeImage_GetFIFFromFilename(filename : PAnsiChar) : Longword; stdcall; external FI_DLL;
	
function  FreeImage_Load(format : Longword; filename : PAnsiChar; flags : Longint = 0) : FIBITMAP; stdcall; external FI_DLL;
procedure FreeImage_Unload(dib : FIBITMAP); stdcall; external FI_DLL;

function  FreeImage_GetBits(dib : FIBITMAP) : PByte; stdcall; external FI_DLL;
function  FreeImage_GetScanLine(dib : FIBITMAP; scanline : Longint) : PByte; stdcall; external FI_DLL;

function  FreeImage_GetBPP(dib : FIBITMAP) : Longword; stdcall; external FI_DLL;
function  FreeImage_GetWidth(dib : FIBITMAP) : Longword; stdcall; external FI_DLL;
function  FreeImage_GetHeight(dib : FIBITMAP) : Longword; stdcall; external FI_DLL;

function  FreeImage_ConvertTo32Bits(dib : FIBITMAP) : FIBITMAP; stdcall; external FI_DLL;

function  FreeImage_FlipVertical(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL;
function  FreeImage_FlipHorizontal(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL;
{$ELSE}
function  FreeImage_GetFIFFromFilename(filename : PAnsiChar) : Longword; stdcall; external FI_DLL name '_FreeImage_GetFIFFromFilename@4';
	
function  FreeImage_Load(format : Longword; filename : PAnsiChar; flags : Longint = 0) : FIBITMAP; stdcall; external FI_DLL name '_FreeImage_Load@12';
procedure FreeImage_Unload(dib : FIBITMAP); stdcall; external FI_DLL name '_FreeImage_Unload@4';

function  FreeImage_GetBits(dib : FIBITMAP) : PByte; stdcall; external FI_DLL name '_FreeImage_GetBits@4';
function  FreeImage_GetScanLine(dib : FIBITMAP; scanline : Longint) : PByte; stdcall; external FI_DLL name '_FreeImage_GetScanLine@8';

function  FreeImage_GetBPP(dib : FIBITMAP) : Longword; stdcall; external FI_DLL name '_FreeImage_GetBPP@4';
function  FreeImage_GetWidth(dib : FIBITMAP) : Longword; stdcall; external FI_DLL name '_FreeImage_GetWidth@4';
function  FreeImage_GetHeight(dib : FIBITMAP) : Longword; stdcall; external FI_DLL name '_FreeImage_GetHeight@4';

function  FreeImage_ConvertTo32Bits(dib : FIBITMAP) : FIBITMAP; stdcall; external FI_DLL name '_FreeImage_ConvertTo32Bits@4';

function  FreeImage_FlipVertical(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL name '_FreeImage_FlipVertical@4';
function  FreeImage_FlipHorizontal(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL name '_FreeImage_FlipHorizontal@4';
{$ENDIF}

implementation

end.