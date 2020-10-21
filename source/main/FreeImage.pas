unit FreeImage;

interface

type
	FIBITMAP = Pointer;
	FIBOOL = Longint;
	
const
	FI_DLL = 'FreeImage.dll';
	FIF_DDS = 24;
	
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

implementation

end.