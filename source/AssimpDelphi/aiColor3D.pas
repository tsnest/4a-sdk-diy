unit aiColor3D;

interface
uses aiTypes;

type TaiColor3D = packed record
   r, g, b: single;
end;
type PaiColor3D = ^TaiColor3D;

type TaiColor3DArray = array[0.._AI_MAX_ARRAY] of TaiColor3D;
type PTaiColor3DArray = ^TaiColor3DArray;

implementation

end.
