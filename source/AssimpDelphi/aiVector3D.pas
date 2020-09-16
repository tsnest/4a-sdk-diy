unit aiVector3D;

interface
uses aiTypes;

type TaiVector3D = packed record
   x, y, z: single;
end;
type PaiVector3D = ^TaiVector3D;
type PaiVector3DArray = array [0.._AI_MAX_ARRAY] of PaiVector3D;

type TaiVector3DArray = array[0.._AI_MAX_ARRAY] of TaiVector3D;
type PTaiVector3DArray = ^TaiVector3DArray;

function Interpolate(const vStart, vEnd : TaiVector3D; factor : Single) : TaiVector3D;

implementation

function Interpolate(const vStart, vEnd : TaiVector3D; factor : Single) : TaiVector3D;
begin
	Result.x := vStart.x + (vEnd.x - vStart.x) * factor;
	Result.y := vStart.y + (vEnd.y - vStart.y) * factor;
	Result.z := vStart.z + (vEnd.z - vStart.z) * factor;
end;

end.
