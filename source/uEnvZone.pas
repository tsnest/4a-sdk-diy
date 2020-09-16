unit uEnvZone;

interface
uses vmath, PhysX, Konfig;

type
	TEnvZone = class
		data : TSection;
		param_tris : TFloatArrayValue;
		
		ph_scene : TPHScene; // TODO use global variable?
		ph : TPHActor;
		ph_shape : TPHShape;
		
		FMatrix : TMatrix;
		FSelected : Boolean;
		
		constructor Create(owner : TPHScene; data : TSection);
		destructor Destroy; override;
		
		procedure GetBBox(out b : TAABB);
		
		protected
		procedure UpdatePhysics;
		procedure SetMatrix(const m : TMatrix);
		
		public
		property Matrix : TMatrix read FMatrix write SetMatrix;
		property Selected : Boolean read FSelected write FSelected;
	end;
	
	TEnvZoneArray = array of TEnvZone;

implementation
uses Iup, PHGroups, sysutils;

constructor TEnvZone.Create(owner : TPHScene; data : TSection);
var
	Tris : TFloatArrayValue;
	I : Longint;
	
	v : array[1..6] of TVec3;
	fwd, rt, up, c : TVec3;
	
	dim : TVec3;
	desc : Pointer;
begin
	inherited Create;
	
	ph_scene := owner;
	
	self.data := data;
	param_tris := data.GetParam('tris', 'fp32_array') as TFloatArrayValue;
	
	if (param_tris = nil) then
		raise Exception.Create('Invalid env zone description!');
		
	if Length(param_tris.data) <> 18 then
	begin
		IupMessageError(IupGetHandle('MAINDIALOG'), 
		'Invalid env zone description! Length(param_tris.data) <> 18');
	end;
		
	// Calc matrix
	Tris := param_tris;
	
	v[1].x := Tris.data[0]; v[1].y := Tris.data[1]; v[1].z := Tris.data[2];
	v[2].x := Tris.data[3]; v[2].y := Tris.data[4]; v[2].z := Tris.data[5];
	v[3].x := Tris.data[6]; v[3].y := Tris.data[7]; v[3].z := Tris.data[8];
	v[4].x := Tris.data[9]; v[4].y := Tris.data[10]; v[4].z := Tris.data[11];
	v[5].x := Tris.data[12]; v[5].y := Tris.data[13]; v[5].z := Tris.data[14];
	v[6].x := Tris.data[15]; v[6].y := Tris.data[16]; v[6].z := Tris.data[17];
	
	fwd.x := v[2].x - v[1].x;
	fwd.y := v[2].y - v[1].y;	
	fwd.z := v[2].z - v[1].z;
	//Normalize(fwd);
	
	rt.x := v[3].x - v[2].x;
	rt.y := v[3].y - v[2].y;	
	rt.z := v[3].z - v[2].z;
	//Normalize(rt);
	
	Cross(up, fwd, rt);
	Normalize(up);
	
	c := v[1];
	for I := 2 to 6 do
	begin
		c.x := c.x + v[I].x;
		c.y := c.y + v[I].y;
		c.z := c.z + v[I].z;
	end;
	
	c.x := c.x / 6;
	c.y := c.y / 6;
	c.z := c.z / 6;
	
	FMatrix[1,1] := rt.x; FMatrix[1,2] := rt.y; FMatrix[1,3] := rt.z; FMatrix[1,4] := 0.0;
	FMatrix[2,1] := up.x; FMatrix[2,2] := up.y; FMatrix[2,3] := up.z; FMatrix[2,4] := 0.0;
	FMatrix[3,1] := fwd.x; FMatrix[3,2] := fwd.y; FMatrix[3,3] := fwd.z; FMatrix[3,4] := 0.0;
	FMatrix[4,1] := c.x; FMatrix[4,2] := c.y; FMatrix[4,3] := c.z; FMatrix[4,4] := 1.0;
	
	dim.x := 0.5; dim.y := 0.5; dim.z := 0.5;
	desc := PHShapeBox(@dim, nil);
	
	ph := PHCreateActor(ph_scene, 1, 1, @desc, nil);
	ph_shape := PHGetShape(ph, 0);
	
	PHSetUserdata(ph, self);
	PHSetGroup(ph_shape, PH_GROUP_ENV_ZONE);
	
	UpdatePhysics;
end;

destructor TEnvZone.Destroy;
begin
	data.Free;
	PHRemoveActor(ph_scene, ph);	
	inherited;
end;

procedure TEnvZone.GetBBox(out b : TAABB);
var
	I, npoints : Longint;
	p : TVec3;
begin
	npoints := Length(param_tris.data) div 3;
	
	b.min.x := param_tris.data[0];
	b.min.y := param_tris.data[1];
	b.min.z := param_tris.data[2];
	
	b.max := b.min;
	
	for I := 1 to npoints-1 do
	begin
		p.x := param_tris.data[I*3  ];
		p.y := param_tris.data[I*3+1];
		p.z := param_tris.data[I*3+2];
		AABBMergePoint(b, p);
	end;
end;

procedure TEnvZone.UpdatePhysics;
var
	ph_matrix : TMatrix;
	scale : TVec3;
begin
	ph_matrix := FMatrix;
	scale.x := Sqrt(ph_matrix[1,1]*ph_matrix[1,1] + ph_matrix[1,2]*ph_matrix[1,2] + ph_matrix[1,3]*ph_matrix[1,3]);
	ph_matrix[1,1] := ph_matrix[1,1] / scale.x; 
	ph_matrix[1,2] := ph_matrix[1,2] / scale.x;
	ph_matrix[1,3] := ph_matrix[1,3] / scale.x;
	scale.y := Sqrt(ph_matrix[2,1]*ph_matrix[2,1] + ph_matrix[2,2]*ph_matrix[2,2] + ph_matrix[2,3]*ph_matrix[2,3]);
	ph_matrix[2,1] := ph_matrix[2,1] / scale.y; 
	ph_matrix[2,2] := ph_matrix[2,2] / scale.y;
	ph_matrix[2,3] := ph_matrix[2,3] / scale.y;
	scale.z := Sqrt(ph_matrix[3,1]*ph_matrix[3,1] + ph_matrix[3,2]*ph_matrix[3,2] + ph_matrix[3,3]*ph_matrix[3,3]);
	ph_matrix[3,1] := ph_matrix[3,1] / scale.z; 
	ph_matrix[3,2] := ph_matrix[3,2] / scale.z;
	ph_matrix[3,3] := ph_matrix[3,3] / scale.z;
	
	scale.x := scale.x / 2;
	scale.y := scale.y / 2;
	scale.z := scale.z / 2;
	PHSetBoxDim(ph_shape, @scale);
	
	PHSetGlobalPoseM44(ph, @ph_matrix);
end;

procedure TEnvZone.SetMatrix(const m : TMatrix);
var
	p : TVec3;
begin
	FMatrix := m;
	UpdatePhysics;
		
	p.x := -0.5; p.y := 0.0; p.z := -0.5;
	Transform(p, FMatrix);
	param_tris.data[0] := p.x;
	param_tris.data[1] := p.y;
	param_tris.data[2] := p.z;
	
	p.x := -0.5; p.y := 0.0; p.z :=  0.5;
	Transform(p, FMatrix);
	param_tris.data[3] := p.x;
	param_tris.data[4] := p.y;
	param_tris.data[5] := p.z;
	
	p.x :=  0.5; p.y := 0.0; p.z :=  0.5;
	Transform(p, FMatrix);
	param_tris.data[6] := p.x;
	param_tris.data[7] := p.y;
	param_tris.data[8] := p.z;
	
	p.x := -0.5; p.y := 0.0; p.z := -0.5;
	Transform(p, FMatrix);
	param_tris.data[9] := p.x;
	param_tris.data[10] := p.y;
	param_tris.data[11] := p.z;
	
	p.x :=  0.5; p.y := 0.0; p.z := -0.5;
	Transform(p, FMatrix);
	param_tris.data[12] := p.x;
	param_tris.data[13] := p.y;
	param_tris.data[14] := p.z;
	
	p.x :=  0.5; p.y := 0.0; p.z :=  0.5;
	Transform(p, FMatrix);
	param_tris.data[15] := p.x;
	param_tris.data[16] := p.y;
	param_tris.data[17] := p.z;
end;

end.