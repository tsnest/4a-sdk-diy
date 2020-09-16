unit vmath;
interface

type
  TVec2 = record
    x, y : Single;
  end;
  PVec2 = ^TVec2;

  TVec2S16 = record
    x, y : Smallint;
  end;
  PVec2S16 = ^TVec2S16;

  TVec3 = record
    x, y, z : Single;
  end;
  PVec3 = ^TVec3;

  TVec4 = record
    x, y, z, w : Single;
  end;
  PVec4 = ^TVec4;

  TVec4S16 = record
    x, y, z, w : Smallint;
  end;
  PVec4S16 = ^TVec4S16;

  TMatrix = array[1..4,1..4] of Single;
  TMatrix33 = array[1..3,1..3] of Single;

  TAABB = record
    min, max : TVec3;
  end;

  TSphere = record
    center : TVec3;
    radius : Single;
  end;

  TPlane = record
    x, y, z, d : Single;
  end;

procedure Normalize(var v : TVec3); overload;
procedure Normalize(var v : TVec2); overload;
function Dot(const v1 : TVec3; const v2 : TVec3) : Single; overload;
function Dot(const v1 : TVec2; const v2 : TVec2) : Single; overload;

procedure Cross(out c : TVec3; const v1, v2 : TVec3);

procedure Transform(var v : TVec3; const m : TMatrix);
procedure Transform33(var v : TVec3; const m : TMatrix);

procedure AABBMerge(var a : TAABB; const b : TAABB);
procedure AABBMergePoint(var a : TAABB; const p : TVec3);
procedure AABBTransform(var a : TAABB; const m : TMatrix);
procedure AABBCorners(out c : array of TVec3; const box : TAABB);
procedure AABBCenter(out c : TVec3; const box : TAABB);
procedure CalcBSphere(out s : TSphere; const b : TAABB);

function PackNormal(const normal : TVec3) : Longword;
procedure UnpackNormal(var n : TVec3; normal : Longword);

procedure FrustumFromMatrix(out frustum : array of TPlane; const vp : TMatrix);
function AABBVisible(const frustum : array of TPlane; const box : TAABB) : Boolean;

procedure Identity(var m : TMatrix);
procedure Translate(var m : TMatrix; const v : TVec3); overload;
procedure Translate(var m : TMatrix; x, y, z : Single); overload;
procedure Scale(var m : TMatrix; const v : TVec3); overload;
procedure Scale(var m : TMatrix; x, y, z : Single); overload;
procedure Mul44(var a : TMatrix; const b : TMatrix);
procedure Invert43(var mat : TMatrix); overload;
procedure Invert43(out mat : TMatrix; const m : TMatrix); overload;
procedure RotateAxis(out m : TMatrix; const axis : TVec3; angle : Single); overload;
procedure RotateAxis(out m : TMatrix; ax, ay, az, angle : Single); overload;

procedure PerspectiveLH(out m : TMatrix; fov, aspect, znear, zfar : Single);
procedure LookAtLH(out m : TMatrix; const eye, center, up : TVec3); overload;
procedure LookAtLH(out m : TMatrix; eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz : Single); overload;

procedure Decompose(const m : TMatrix; out translate, rot, scale : TVec3);

implementation
uses Math;

procedure Normalize(var v : TVec3);
var
  len : Single;
begin
  len := Sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
  v.x := v.x / len;
  v.y := v.y / len;
  v.z := v.z / len;
end;

procedure Normalize(var v : TVec2);
var
  len : Single;
begin
  len := Sqrt(v.x*v.x + v.y*v.y);
  v.x := v.x / len;
  v.y := v.y / len;
end;

function Dot(const v1 : TVec3; const v2 : TVec3) : Single;
begin
  Dot := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

function Dot(const v1 : TVec2; const v2 : TVec2) : Single;
begin
  Dot := v1.x*v2.x + v1.y*v2.y;
end;

procedure Cross(out c : TVec3; const v1, v2 : TVec3);
begin
  c.x := v1.y * v2.z - v1.z * v2.y;
  c.y := v1.z * v2.x - v1.x * v2.z;
  c.z := v1.x * v2.y - v1.y * v2.x;
end;

procedure Transform(var v : TVec3; const m : TMatrix);
var
  x, y, z, w : Single;
begin
  x := v.x;
  y := v.y;
  z := v.z;
  w := m[1,4] + m[2,4] + m[3,4] + m[4,4];

  v.x := x*m[1,1] + y*m[2,1] + z*m[3,1] + m[4,1];
  v.y := x*m[1,2] + y*m[2,2] + z*m[3,2] + m[4,2];
  v.z := x*m[1,3] + y*m[2,3] + z*m[3,3] + m[4,3];

  v.x := v.x / w;
  v.y := v.y / w;
  v.z := v.z / w;
end;

procedure Transform33(var v : TVec3; const m : TMatrix);
var
  x, y, z : Single;
begin
  x := v.x;
  y := v.y;
  z := v.z;

  v.x := x*m[1,1] + y*m[2,1] + z*m[3,1];
  v.y := x*m[1,2] + y*m[2,2] + z*m[3,2];
  v.z := x*m[1,3] + y*m[2,3] + z*m[3,3];
end;

procedure AABBMerge(var a : TAABB; const b : TAABB);
begin
  if a.min.x > b.min.x then
    a.min.x := b.min.x;
  if a.min.y > b.min.y then
    a.min.y := b.min.y;
  if a.min.z > b.min.z then
    a.min.z := b.min.z;
  if a.max.x < b.max.x then
    a.max.x := b.max.x;
  if a.max.y < b.max.y then
    a.max.y := b.max.y;
  if a.max.z < b.max.z then
    a.max.z := b.max.z;
end;

procedure AABBMergePoint(var a : TAABB; const p : TVec3);
begin
  if p.x < a.min.x then
    a.min.x := p.x;
  if p.y < a.min.y then
    a.min.y := p.y;
  if p.z < a.min.z then
    a.min.z := p.z;
  if p.x > a.max.x then
    a.max.x := p.x;
  if p.y > a.max.y then
    a.max.y := p.y;
  if p.z > a.max.z then
    a.max.z := p.z;
end;

procedure AABBTransform(var a : TAABB; const m : TMatrix);
var
  I : Integer;
  c : array[0..7] of TVec3;
begin
  AABBCorners(c, a);

	Transform(c[0], m);
	a.min := c[0];
	a.max := c[0];

  for I := 1 to 7 do
  begin
    Transform(c[I], m);
    AABBMergePoint(a, c[I]);
  end;
end;

procedure AABBCorners(out c : array of TVec3; const box : TAABB);
begin
  c[0] := box.min;

  c[1].x := box.min.x;
  c[1].y := box.min.y;
  c[1].z := box.max.z;

  c[2].x := box.max.x;
  c[2].y := box.min.y;
  c[2].z := box.max.z;

  c[3].x := box.max.x;
  c[3].y := box.min.y;
  c[3].z := box.min.z;

  c[4].x := box.min.x;
  c[4].y := box.max.y;
  c[4].z := box.min.z;

  c[5].x := box.min.x;
  c[5].y := box.max.y;
  c[5].z := box.max.z;

  c[6] := box.max;

  c[7].x := box.max.x;
  c[7].y := box.max.y;
  c[7].z := box.min.z;
end;

procedure AABBCenter(out c : TVec3; const box : TAABB);
begin
	c.x := box.min.x + ((box.max.x-box.min.x) / 2);
	c.y := box.min.y + ((box.max.y-box.min.y) / 2);
	c.z := box.min.z + ((box.max.z-box.min.z) / 2);
end;

procedure CalcBSphere(out s : TSphere; const b : TAABB);
var
  half : TVec3;
begin
  half.x := (b.max.x-b.min.x)/2;
  half.y := (b.max.y-b.min.y)/2;
  half.z := (b.max.z-b.min.z)/2;
  s.center.x := b.min.x+half.x;
  s.center.y := b.min.y+half.y;
  s.center.z := b.min.z+half.z;
  s.radius := Sqrt(half.x*half.x+half.y*half.y+half.z*half.z);
end;

function PackNormal(const normal : TVec3) : Longword;
var
  nx, ny, nz : Byte;
begin
  nx := Trunc(((normal.x+1)/2)*255);
  ny := Trunc(((normal.y+1)/2)*255);
  nz := Trunc(((normal.z+1)/2)*255);

  Result := (nx shl 16) or (ny shl 8) or nz;
end;

procedure UnpackNormal(var n : TVec3; normal : Longword);
begin
  n.x := (((normal shr 16) and $FF) / 255 * 2) - 1;
  n.y := (((normal shr 8) and $FF) / 255 * 2) - 1;
  n.z := ((normal and $FF) / 255 * 2) - 1;
end;

procedure FrustumFromMatrix(out frustum : array of TPlane; const vp : TMatrix);
begin
	frustum[0].x := vp[1,4] + vp[1,1];
	frustum[0].y := vp[2,4] + vp[2,1];
	frustum[0].z := vp[3,4] + vp[3,1];
	frustum[0].d := vp[4,4] + vp[4,1];

	frustum[1].x := vp[1,4] - vp[1,1];
	frustum[1].y := vp[2,4] - vp[2,1];
	frustum[1].z := vp[3,4] - vp[3,1];
	frustum[1].d := vp[4,4] - vp[4,1];

	frustum[2].x := vp[1,4] - vp[1,2];
	frustum[2].y := vp[2,4] - vp[2,2];
	frustum[2].z := vp[3,4] - vp[3,2];
	frustum[2].d := vp[4,4] - vp[4,2];

	frustum[3].x := vp[1,4] + vp[1,2];
	frustum[3].y := vp[2,4] + vp[2,2];
	frustum[3].z := vp[3,4] + vp[3,2];
	frustum[3].d := vp[4,4] + vp[4,2];

	frustum[4].x := vp[1,4] - vp[1,3];
	frustum[4].y := vp[2,4] - vp[2,3];
	frustum[4].z := vp[3,4] - vp[3,3];
	frustum[4].d := vp[4,4] - vp[4,3];

	frustum[5].x := vp[1,4] + vp[1,3];
	frustum[5].y := vp[2,4] + vp[2,3];
	frustum[5].z := vp[3,4] + vp[3,3];
	frustum[5].d := vp[4,4] + vp[4,3];
end;

{
function AABBVisible(const frustum : array of TPlane; const box : TAABB) : Boolean;
var
  I : Integer;
  c : array[0..7] of TVec3;
  dist : Single;
begin
  AABBCorners(c, box);

  Result := True;

  for I := 0 to 5 do
  begin
    dist := c[0].x * frustum[I].x + c[0].y * frustum[I].y + c[0].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[1].x * frustum[I].x + c[1].y * frustum[I].y + c[1].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[2].x * frustum[I].x + c[2].y * frustum[I].y + c[2].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[3].x * frustum[I].x + c[3].y * frustum[I].y + c[3].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[4].x * frustum[I].x + c[4].y * frustum[I].y + c[4].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[5].x * frustum[I].x + c[5].y * frustum[I].y + c[5].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[6].x * frustum[I].x + c[6].y * frustum[I].y + c[6].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;
    dist := c[7].x * frustum[I].x + c[7].y * frustum[I].y + c[7].z * frustum[I].z + frustum[I].d;
    if dist > 0 then
      Continue;

    Result := False;
    Break;
  end;

end;
}
function AABBVisible(const frustum : array of TPlane; const box : TAABB) : Boolean;
var
	I : Integer;
	n, p : TVec3;
begin
	Result := True;
	
	for I := 0 to 5 do
	begin
		if frustum[I].x >= 0.0 then
		begin
			p.x := box.max.x;
			//n.x := box.min.x;
		end else
		begin
			p.x := box.min.x;
			//n.x := box.max.x;
		end;
		if frustum[I].y >= 0.0 then
		begin
			p.y := box.max.y;
			//n.y := box.min.y;
		end else
		begin
			p.y := box.min.y;
			//n.y := box.max.y;
		end;
		if frustum[I].z >= 0.0 then
		begin
			p.z := box.max.z;
			//n.z := box.min.z;
		end else
		begin
			p.z := box.min.z;
			//n.z := box.max.z;
		end;
		
		if (p.x*frustum[I].x + p.y*frustum[I].y + p.z*frustum[I].z + frustum[I].d) < 0.0 then
		begin
			Result := False;
			Exit;
		end;
	end;
	
end;

procedure Identity(var m : TMatrix);
begin
	m[1,1] := 1.0; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := 1.0; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := 1.0; m[3,4] := 0.0;
	m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0;
end;

procedure Translate(var m : TMatrix; x, y, z : Single); overload;
begin
	m[1,1] := 1.0; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := 1.0; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := 1.0; m[3,4] := 0.0;
	m[4,1] := x; m[4,2] := y; m[4,3] := z; m[4,4] := 1.0;
end;

procedure Translate(var m : TMatrix; const v : TVec3); overload;
begin
	m[1,1] := 1.0; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := 1.0; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := 1.0; m[3,4] := 0.0;
	m[4,1] := v.x; m[4,2] := v.y; m[4,3] := v.z; m[4,4] := 1.0;
end;

procedure Scale(var m : TMatrix; x, y, z : Single); overload;
begin
	m[1,1] := x  ; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := y  ; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := z  ; m[3,4] := 0.0;
	m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0;
end;

procedure Scale(var m : TMatrix; const v : TVec3); overload;
begin
	m[1,1] := v.x; m[1,2] := 0.0; m[1,3] := 0.0; m[1,4] := 0.0;
	m[2,1] := 0.0; m[2,2] := v.y; m[2,3] := 0.0; m[2,4] := 0.0;
	m[3,1] := 0.0; m[3,2] := 0.0; m[3,3] := v.z; m[3,4] := 0.0;
	m[4,1] := 0.0; m[4,2] := 0.0; m[4,3] := 0.0; m[4,4] := 1.0;
end;

procedure Mul44(var a : TMatrix; const b : TMatrix);
var
	m : TMatrix;
begin
	m := a;
	a[1,1] := m[1,1] * b[1,1] + m[2,1] * b[1,2] + m[3,1] * b[1,3] + m[4,1] * b[1,4];
	a[1,2] := m[1,2] * b[1,1] + m[2,2] * b[1,2] + m[3,2] * b[1,3] + m[4,2] * b[1,4];
	a[1,3] := m[1,3] * b[1,1] + m[2,3] * b[1,2] + m[3,3] * b[1,3] + m[4,3] * b[1,4];
	a[1,4] := m[1,4] * b[1,1] + m[2,4] * b[1,2] + m[3,4] * b[1,3] + m[4,4] * b[1,4];
	
	a[2,1] := m[1,1] * b[2,1] + m[2,1] * b[2,2] + m[3,1] * b[2,3] + m[4,1] * b[2,4];
	a[2,2] := m[1,2] * b[2,1] + m[2,2] * b[2,2] + m[3,2] * b[2,3] + m[4,2] * b[2,4];
	a[2,3] := m[1,3] * b[2,1] + m[2,3] * b[2,2] + m[3,3] * b[2,3] + m[4,3] * b[2,4];
	a[2,4] := m[1,4] * b[2,1] + m[2,4] * b[2,2] + m[3,4] * b[2,3] + m[4,4] * b[2,4];
	
	a[3,1] := m[1,1] * b[3,1] + m[2,1] * b[3,2] + m[3,1] * b[3,3] + m[4,1] * b[3,4];
	a[3,2] := m[1,2] * b[3,1] + m[2,2] * b[3,2] + m[3,2] * b[3,3] + m[4,2] * b[3,4];
	a[3,3] := m[1,3] * b[3,1] + m[2,3] * b[3,2] + m[3,3] * b[3,3] + m[4,3] * b[3,4];
	a[3,4] := m[1,4] * b[3,1] + m[2,4] * b[3,2] + m[3,4] * b[3,3] + m[4,4] * b[3,4];
	
	a[4,1] := m[1,1] * b[4,1] + m[2,1] * b[4,2] + m[3,1] * b[4,3] + m[4,1] * b[4,4];
	a[4,2] := m[1,2] * b[4,1] + m[2,2] * b[4,2] + m[3,2] * b[4,3] + m[4,2] * b[4,4];
	a[4,3] := m[1,3] * b[4,1] + m[2,3] * b[4,2] + m[3,3] * b[4,3] + m[4,3] * b[4,4];
	a[4,4] := m[1,4] * b[4,1] + m[2,4] * b[4,2] + m[3,4] * b[4,3] + m[4,4] * b[4,4];
end;

procedure Invert43(var mat : TMatrix); overload;
var
	m : TMatrix;
begin
	m := mat;
	Invert43(mat, m);
end;

procedure Invert43(out mat : TMatrix; const m : TMatrix); overload;
var
	det, det_inv : Single;
begin

	det := 
		m[1,1] * (m[2,2] * m[3,3] - m[2,3] * m[3,2]) - 
		m[1,2] * (m[2,1] * m[3,3] - m[2,3] * m[3,1]) +
		m[1,3] * (m[2,1] * m[3,2] - m[2,2] * m[3,1]);
	det_inv := 1.0 / det;
	
	mat[1,1] :=  det_inv * (m[2,2] * m[3,3] - m[2,3] * m[3,2]);
	mat[1,2] := -det_inv * (m[1,2] * m[3,3] - m[1,3] * m[3,2]);
	mat[1,3] :=  det_inv * (m[1,2] * m[2,3] - m[1,3] * m[2,2]);
	mat[1,4] := 0.0;
	
	mat[2,1] := -det_inv * (m[2,1] * m[3,3] - m[2,3] * m[3,1]);
	mat[2,2] :=  det_inv * (m[1,1] * m[3,3] - m[1,3] * m[3,1]);
	mat[2,3] := -det_inv * (m[1,1] * m[2,3] - m[1,3] * m[2,1]);
	mat[2,4] := 0.0;
	
	mat[3,1] :=  det_inv * (m[2,1] * m[3,2] - m[2,2] * m[3,1]);
	mat[3,2] := -det_inv * (m[1,1] * m[3,2] - m[1,2] * m[3,1]);
	mat[3,3] :=  det_inv * (m[1,1] * m[2,2] - m[1,2] * m[2,1]);
	mat[3,4] := 0.0;
	
	mat[4,1] := -(m[4,1] * mat[1,1] + m[4,2] * mat[2,1] + m[4,3] * mat[3,1]);
	mat[4,2] := -(m[4,1] * mat[1,2] + m[4,2] * mat[2,2] + m[4,3] * mat[3,2]);
	mat[4,3] := -(m[4,1] * mat[1,3] + m[4,2] * mat[2,3] + m[4,3] * mat[3,3]);
	mat[4,4] := 1.0;
end;

procedure RotateAxis(out m : TMatrix; const axis : TVec3; angle : Single); overload;
var
	Cosine : Single;
	Sine : Single;
begin
	Cosine	:= Cos(angle);
	Sine		:= Sin(angle);
	m[1][1] := axis.x * axis.x + ( 1 - axis.x * axis.x) * Cosine;
	m[1][2] := axis.x * axis.y * ( 1 - Cosine ) + axis.z * Sine;
	m[1][3] := axis.x * axis.z * ( 1 - Cosine ) - axis.y * Sine;
	m[1][4] := 0;
	m[2][1] := axis.x * axis.y * ( 1 - Cosine ) - axis.z * Sine;
	m[2][2] := axis.y * axis.y + ( 1 - axis.y * axis.y) * Cosine;
	m[2][3] := axis.y * axis.z * ( 1 - Cosine ) + axis.x * Sine;
	m[2][4] := 0;
	m[3][1] := axis.x * axis.z * ( 1 - Cosine ) + axis.y * Sine;
	m[3][2] := axis.y * axis.z * ( 1 - Cosine ) - axis.x * Sine;
	m[3][3] := axis.z * axis.z + ( 1 - axis.z * axis.z) * Cosine;
	m[3][4] := 0; m[4][1] := 0; m[4][2] := 0;
	m[4][3] := 0; m[4][4] := 1;
end;

procedure RotateAxis(out m : TMatrix; ax, ay, az, angle : Single); overload;
var
	axis : TVec3;
begin
	axis.x := ax;
	axis.y := ay;
	axis.z := az;
	RotateAxis(m, axis, angle);
end;

procedure PerspectiveLH(out m : TMatrix; fov, aspect, znear, zfar : Single);
var
	t : Single;
begin
	t := tan(fov/2.0);
	
	m[1,1] := 1.0 / (aspect * t); 
	m[1,2] := 0.0; 
	m[1,3] := 0.0; 
	m[1,4] := 0.0;
	
	m[2,1] := 0.0; 
	m[2,2] := 1.0 / t; 
	m[2,3] := 0.0; 
	m[2,4] := 0.0;
	
	m[3,1] := 0.0; 
	m[3,2] := 0.0; 
	m[3,3] := zfar / (zfar - znear); 
	m[3,4] := 1.0;
	
	m[4,1] := 0.0; 
	m[4,2] := 0.0; 
	m[4,3] := (zfar * znear) / (znear - zfar); 
	m[4,4] := 0.0;
end;

procedure LookAtLH(out m : TMatrix; const eye, center, up : TVec3); overload;
var
	right : TVec3;
	upn : TVec3;
	dir : TVec3;
begin	
	dir.x := center.x - eye.x;
	dir.y := center.y - eye.y;
	dir.z := center.z - eye.z;
	Normalize(dir);
	
	Cross(right, up, dir);
	Normalize(right);
	
	Cross(upn, dir, right);
	Normalize(upn);
	
	m[1,1] := right.x; m[1,2] := upn.x; m[1,3] := dir.x; m[1,4] := 0.0;
	m[2,1] := right.y; m[2,2] := upn.y; m[2,3] := dir.y; m[2,4] := 0.0;
	m[3,1] := right.z; m[3,2] := upn.z; m[3,3] := dir.z; m[3,4] := 0.0;
	m[4,1] := -Dot(right, eye); m[4,2] := -Dot(upn, eye); m[4,3] := -Dot(dir, eye); m[4,4] := 1.0;
end;

procedure LookAtLH(out m : TMatrix; eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz : Single); overload;
var
	eye, center, up : TVec3;
begin
	eye.x := eyex;
	eye.y := eyey;
	eye.z := eyez;
	center.x := centerx;
	center.y := centery;
	center.z := centerz;
	up.x := upx;
	up.y := upy;
	up.z := upz;
	LookAtLH(m, eye, center, up);
end;

////////////////////////////////////////////////////

//
//	IC	void	getHPB	(T& h, T& p, T& b) const
//	{
//        T cy = _sqrt(j.y*j.y + i.y*i.y);
//        if (cy > 16.0f*type_epsilon(T)) {
//            h = (T) -atan2(k.x, k.z);
//            p = (T) -atan2(-k.y, cy);
//            b = (T) -atan2(i.y, j.y);
//        } else {
//            h = (T) -atan2(-i.z, i.x);
//            p = (T) -atan2(-k.y, cy);
//            b = 0;
//        }
//    }


procedure GetHPB(const m : TMatrix; out h, p, b : Single);
var
	cy : Single;
begin
	cy := Sqrt(m[1,2]*m[1,2] + m[2,2]*m[2,2]);
	if cy > 0.00001 then
	begin
		h := -arctan2(m[3,1], m[3,3]);
		p := -arctan2(-m[3,2], cy);
		b := -arctan2(m[1,2], m[2,2]);
	end else
	begin
		h := -arctan2(-m[1,3], m[1,1]);
		p := -arctan2(-m[3,2], cy);
		b := 0.0;
	end;
end;

procedure GetScale(const m : TMatrix; out x, y, z : Single);
begin
	x := Sqrt(m[1,1]*m[1,1] + m[1,2]*m[1,2] + m[1,3]*m[1,3]);
	y := Sqrt(m[2,1]*m[2,1] + m[2,2]*m[2,2] + m[2,3]*m[2,3]);
	z := Sqrt(m[3,1]*m[3,1] + m[3,2]*m[3,2] + m[3,3]*m[3,3]);
end;

procedure Decompose(const m : TMatrix; out translate, rot, scale : TVec3);
begin
	GetHPB(m, rot.y, rot.x, rot.z);
	GetScale(m, scale.x, scale.y, scale.z);
	translate.x := m[4,1];
	translate.y := m[4,2];
	translate.z := m[4,3];
end;

end.
