unit Manipulator;

interface
uses vmath, PhysX;

type
	TManipulator = class
		FMatrix : TMatrix;
		FDiff		: TMatrix;
		phscene : TPHScene;
		phactor : TPHActor;
		shapes : array[1..3] of TPHShape;
		offset : TVec3;
		start_x, start_y, lineWidth : Longint;
		active : TPHShape;
		world : Boolean;

		constructor Create(scene : TPHScene; isworld : Boolean; line_width : Longint);
		destructor Destroy; override;

		procedure Draw; virtual;

		function Activate(x, y : Longint) : Boolean;
		procedure Deactivate;
		function	IsActive : Boolean;

		procedure Update(x, y : Longint);

		protected
		procedure SetMatrix(const m : TMatrix);

		procedure OnActivate; virtual; abstract;
		procedure OnDeactivate; virtual; abstract;
		procedure OnUpdate(x, y : Longint); virtual; abstract;

		public
		property Matrix : TMatrix read FMatrix write SetMatrix;
		property Diff : TMatrix read FDiff;
	end;

	TMoveManipulator = class(TManipulator)
		local_offset : TVec3;
		offset : TVec3;

		procedure Draw; override;

		protected
		procedure OnActivate; override;
		procedure OnDeactivate; override;
		procedure OnUpdate(x, y : Longint); override;
	end;

	TRotateManipulator = class(TManipulator)
		angle : Single;
		axis : TVec3;

		procedure Draw; override;

		protected
		procedure OnActivate; override;
		procedure OnDeactivate; override;
		procedure OnUpdate(x, y : Longint); override;
	end;
	
	TScaleManipulator = class(TManipulator)
		uniform : Boolean;
		scale : Single;
	
		constructor Create(scene : TPHScene; isuniform : Boolean; line_width : LongInt);
	
		procedure Draw; override;
		
		protected
		procedure OnActivate; override;
		procedure OnDeactivate; override;
		procedure OnUpdate(x, y : Longint); override;
	end;

implementation
uses common, GL, GLU, PHGroups;

function RaycastManipulator(scene : TPHScene; const p, dir : TVec3; dist : Single; out shape : Pointer) : TManipulator;
var
	group : Longword;
	actor : TPHActor;
	sel : TObject;
begin
	group := PH_GROUP_MANIPULATOR_MASK;
	shape := PHRaycastClosestShape(scene, @p, @dir, dist, nil, nil, nil, group);
	if shape <> nil then
	begin
		actor := PHGetActor(shape);
		sel := TObject(PHGetUserdata(actor));

		if sel is TManipulator then
			Result := TManipulator(sel)
		else
			Result := nil;
	end;
end;

constructor TManipulator.Create(scene : TPHScene; isworld : Boolean; line_width : Longint);
var
	d, o : TVec3;
	desc : array[1..3] of Pointer;
begin
	inherited Create;

	phscene := scene;
	world := isworld;
	lineWidth := line_width;

	Identity(FMatrix);
	Identity(FDiff);

	d.x := 0.4; d.y := 0.05; d.z := 0.05;
	o.x := 0.5; o.y := 0.0; o.z := 0.0;
	desc[1] := PHShapeBox(@d, @o);

	d.x := 0.05; d.y := 0.4; d.z := 0.05;
	o.x := 0.0; o.y := 0.5; o.z := 0.0;
	desc[2] := PHShapeBox(@d, @o);

	d.x := 0.05; d.y := 0.05; d.z := 0.4;
	o.x := 0.0; o.y := 0.0; o.z := 0.5;
	desc[3] := PHShapeBox(@d, @o);

	phactor := PHCreateActor(phscene, 1, 0, nil, nil); 
	if phactor = nil then
		WriteLn('phactor = nil');
																				 
	shapes[1] := PHAddShape(phactor, desc[1]);
	shapes[2] := PHAddShape(phactor, desc[2]);
	shapes[3] := PHAddShape(phactor, desc[3]);

	PHSetGroup(shapes[1], PH_GROUP_MANIPULATOR);
	PHSetGroup(shapes[2], PH_GROUP_MANIPULATOR);
	PHSetGroup(shapes[3], PH_GROUP_MANIPULATOR);

	PHSetUserData(phactor, self);
end;

destructor TManipulator.Destroy;
begin
	PHRemoveActor(phscene, phactor);

	inherited;
end;

procedure TManipulator.Draw;
var
	c : TVec3;
	d : TVec3;
begin
	glLineWidth(lineWidth);

	glBegin(GL_LINES);

	c.x := FMatrix[4,1]; c.y := FMatrix[4,2]; c.z := FMatrix[4,3];

	if active = shapes[1] then
		glColor3f(1,1,0)
	else
		glColor3f(1,0,0);

	d.x := FMatrix[1,1]; d.y := FMatrix[1,2]; d.z := FMatrix[1,3];
	Normalize(d);

	glVertex3fv(@c);
	glVertex3f(d.x+c.x, d.y+c.y, d.z+c.z);

	if active = shapes[2] then
		glColor3f(1,1,0)
	else
		glColor3f(0,1,0);
		
	d.x := FMatrix[2,1]; d.y := FMatrix[2,2]; d.z := FMatrix[2,3];
	Normalize(d);

	glVertex3fv(@c);
	glVertex3f(d.x+c.x, d.y+c.y, d.z+c.z);

	if active = shapes[3] then
		glColor3f(1,1,0)
	else
		glColor3f(0,0,1);
		
	d.x := FMatrix[3,1]; d.y := FMatrix[3,2]; d.z := FMatrix[3,3];
	Normalize(d);

	glVertex3fv(@c);
	glVertex3f(d.x+c.x, d.y+c.y, d.z+c.z);

	glEnd;
	
	glLineWidth(1.0);
end;

function TManipulator.Activate(x: Integer; y: Integer) : Boolean;
var
	shape : Pointer;
	p1, p2 : array[1..3] of GLdouble;
	p, dir : TVec3;
const
	RAYCAST_DIST = 500.0;
begin
	// in OpenGL origin of window coordinates is a left-bottom corner. That's why viewport[3]-y
	gluUnProject(x, viewport[3]-y, 0.1, @modelview_d, @proj_d, @viewport, @p1[1], @p1[2], @p1[3]);
	gluUnProject(x, viewport[3]-y, 0.9, @modelview_d, @proj_d, @viewport, @p2[1], @p2[2], @p2[3]);

	p.x := p1[1];
	p.y := p1[2];
	p.z := p1[3];

	dir.x := p2[1]-p1[1];
	dir.y := p2[2]-p1[2];
	dir.z := p2[3]-p1[3];
	Normalize(dir);

	if RaycastManipulator(phscene, p, dir, RAYCAST_DIST, shape) = self then
	begin
		start_x := x;
		start_y := y;
		active := shape;
	
		OnActivate;
		Activate := True
	end else
		Activate := False
end;

procedure TManipulator.Deactivate;
begin
	OnDeactivate;
	active := nil;
end;

function TManipulator.IsActive : Boolean;
begin
	IsActive := active <> nil;
end;

procedure TManipulator.Update(x, y : Longint);
begin
	OnUpdate(x, y);
end;

procedure TManipulator.SetMatrix(const m: TMatrix);
var
	ph_matrix : TMatrix;
	scale : TVec3;
begin
	if world then
		Translate(FMatrix, m[4,1], m[4,2], m[4,3])
	else
		FMatrix := m;
		
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
		
	PHSetGlobalPoseM44(phactor, @ph_matrix);
end;

procedure TMoveManipulator.Draw;
begin
	inherited;

	if active <> nil then
	begin
		glEnable(GL_LINE_STIPPLE);
		glLineStipple(1, $F0F0);

		glBegin(GL_LINES);

		glColor3f(1,0.2,0.2);
		glVertex3f(FMatrix[4,1]+offset.x, FMatrix[4,2]+offset.y, FMatrix[4,3]+offset.z);
		glVertex3f(FMatrix[4,1]+FMatrix[1,1]+offset.x, FMatrix[4,2]+FMatrix[1,2]+offset.y, FMatrix[4,3]+FMatrix[1,3]+offset.z);

		glColor3f(0.2,1,0.2);
		glVertex3f(FMatrix[4,1]+offset.x, FMatrix[4,2]+offset.y, FMatrix[4,3]+offset.z);
		glVertex3f(FMatrix[4,1]+FMatrix[2,1]+offset.x, FMatrix[4,2]+FMatrix[2,2]+offset.y, FMatrix[4,3]+FMatrix[2,3]+offset.z);

		glColor3f(0.2,0.2,1);
		glVertex3f(FMatrix[4,1]+offset.x, FMatrix[4,2]+offset.y, FMatrix[4,3]+offset.z);
		glVertex3f(FMatrix[4,1]+FMatrix[3,1]+offset.x, FMatrix[4,2]+FMatrix[3,2]+offset.y, FMatrix[4,3]+FMatrix[3,3]+offset.z);

		glEnd;

		glDisable(GL_LINE_STIPPLE);
	end;
end;

procedure TMoveManipulator.OnActivate;
begin
	offset.x := 0.0;
	offset.y := 0.0;
	offset.z := 0.0;
	
	local_offset.x := 0.0;
	local_offset.y := 0.0;
	local_offset.z := 0.0;
end;

procedure TMoveManipulator.OnDeactivate;
var
	new_m: TMatrix;
begin
	new_m := Matrix;
	new_m[4,1] := new_m[4,1] + offset.x;
	new_m[4,2] := new_m[4,2] + offset.y;
	new_m[4,3] := new_m[4,3] + offset.z;
	Matrix := new_m;
	
	Translate(FDiff, offset);
end;

procedure TMoveManipulator.OnUpdate(x, y : Longint);
var
	diff_x, diff_y : Single;
	dtx, dty : Single;
	scale : Single;
	axis : TVec3;
	local_axis : TVec3;
begin
	diff_x := (x - start_x) / 100;
	diff_y := (y - start_y) / 100;

	//writeln((x - start_x), ' ', (y - start_y));

	if active = shapes[1] then
	begin
		axis.x := FMatrix[1,1]; axis.y := FMatrix[1,2]; axis.z := FMatrix[1,3];
		local_axis.x := 1.0; local_axis.y := 0.0; local_axis.z := 0.0;
	end else
	if active = shapes[2] then
	begin
		axis.x := FMatrix[2,1]; axis.y := FMatrix[2,2]; axis.z := FMatrix[2,3];
		local_axis.x := 0.0; local_axis.y := 1.0; local_axis.z := 0.0;
	end else
	if active = shapes[3] then
	begin
		axis.x := FMatrix[3,1]; axis.y := FMatrix[3,2]; axis.z := FMatrix[3,3];
		local_axis.x := 0.0; local_axis.y := 0.0; local_axis.z := 1.0;
	end else
		Writeln('???');
		
	Normalize(axis);

	dtx := axis.x*modelview[1,1] + axis.y*modelview[1,2] + axis.z*modelview[1,3];
	dty := axis.x*modelview[2,1] + axis.y*modelview[2,2] + axis.z*modelview[2,3];

	scale := (diff_x*dtx + diff_y*dty);
	offset.x := axis.x*scale;
	offset.y := axis.y*scale;
	offset.z := axis.z*scale;
	local_offset.x := local_axis.x*scale;
	local_offset.y := local_axis.y*scale;
	local_offset.z := local_axis.z*scale;
end;

procedure TRotateManipulator.Draw;
begin
	inherited;

	if active <> nil then
	begin
		glEnable(GL_LINE_STIPPLE);
		glLineStipple(1, $F0F0);

		glPushMatrix;
		glTranslatef(FMatrix[4,1], FMatrix[4,2], FMatrix[4,3]);
		glRotatef(angle, axis.x, axis.y, axis.z);

		glBegin(GL_LINES);

		glColor3f(1,0.2,0.2);
		glVertex3f(0, 0, 0);
		glVertex3f(FMatrix[1,1], FMatrix[1,2], FMatrix[1,3]);


		glColor3f(0.2,1,0.2);
		glVertex3f(0, 0, 0);
		glVertex3f(FMatrix[2,1], FMatrix[2,2], FMatrix[2,3]);

		glColor3f(0.2,0.2,1);
		glVertex3f(0, 0, 0);
		glVertex3f(FMatrix[3,1], FMatrix[3,2], FMatrix[3,3]);

		glEnd;

		glPopMatrix;

		glDisable(GL_LINE_STIPPLE);
	end;
end;

procedure TRotateManipulator.OnActivate;
begin
	angle := 0.0;

	if active = shapes[1] then
	begin
		axis.x := FMatrix[1,1]; axis.y := FMatrix[1,2]; axis.z := FMatrix[1,3];
	end;
	if active = shapes[2] then
	begin
		axis.x := FMatrix[2,1]; axis.y := FMatrix[2,2]; axis.z := FMatrix[2,3];
	end;
	if active = shapes[3] then
	begin
		axis.x := FMatrix[3,1]; axis.y := FMatrix[3,2]; axis.z := FMatrix[3,3];
	end;
	
	Normalize(axis);
end;

procedure TRotateManipulator.OnDeactivate;
var
	rot, m : TMatrix;
begin
	RotateAxis(rot, axis, angle*(PI/180));
	
	FDiff := rot;

	if not world then
	begin
		// multiply difference by current rotation
		m[1,1] := rot[1,1]*FMatrix[1,1] + rot[2,1]*FMatrix[1,2] + rot[3,1]*FMatrix[1,3];
		m[1,2] := rot[1,2]*FMatrix[1,1] + rot[2,2]*FMatrix[1,2] + rot[3,2]*FMatrix[1,3];
		m[1,3] := rot[1,3]*FMatrix[1,1] + rot[2,3]*FMatrix[1,2] + rot[3,3]*FMatrix[1,3];
	
		m[2,1] := rot[1,1]*FMatrix[2,1] + rot[2,1]*FMatrix[2,2] + rot[3,1]*FMatrix[2,3];
		m[2,2] := rot[1,2]*FMatrix[2,1] + rot[2,2]*FMatrix[2,2] + rot[3,2]*FMatrix[2,3];
		m[2,3] := rot[1,3]*FMatrix[2,1] + rot[2,3]*FMatrix[2,2] + rot[3,3]*FMatrix[2,3];
	
		m[3,1] := rot[1,1]*FMatrix[3,1] + rot[2,1]*FMatrix[3,2] + rot[3,1]*FMatrix[3,3];
		m[3,2] := rot[1,2]*FMatrix[3,1] + rot[2,2]*FMatrix[3,2] + rot[3,2]*FMatrix[3,3];
		m[3,3] := rot[1,3]*FMatrix[3,1] + rot[2,3]*FMatrix[3,2] + rot[3,3]*FMatrix[3,3];
	
		// copy translation & etc
		m[1,4] := FMatrix[1,4];
		m[2,4] := FMatrix[2,4];
		m[3,4] := FMatrix[3,4];
		m[4,1] := FMatrix[4,1]; m[4,2] := FMatrix[4,2]; m[4,3] := FMatrix[4,3]; m[4,4] := FMatrix[4,4];
	
		Matrix := m;
	end;
end;

procedure TRotateManipulator.OnUpdate(x, y : Longint);
var
	diff_x, diff_y : Single;
	dtx, dty : Single;
begin
	diff_x := (x - start_x) / 10;
	diff_y := (y - start_y) / 10;

	//writeln((x - start_x), ' ', (y - start_y));

	dtx := axis.x*modelview[1,1] + axis.y*modelview[1,2] + axis.z*modelview[1,3];
	dty := axis.x*modelview[2,1] + axis.y*modelview[2,2] + axis.z*modelview[2,3];

	angle := (diff_x*dty + diff_y*dtx)
end;

constructor TScaleManipulator.Create(scene : TPHScene; isuniform : Boolean; line_width : Longint);
begin
	Inherited Create(scene, False, line_width);
	uniform := isuniform;
	lineWidth := line_width;
end;

procedure TScaleManipulator.Draw;
begin
	inherited;

	if active <> nil then
	begin
		glEnable(GL_LINE_STIPPLE);
		glLineStipple(1, $F0F0);
		
		glBegin(GL_LINES);

		if uniform or (active = shapes[1]) then
		begin
			glColor3f(1,0.2,0.2);
			glVertex3f(FMatrix[4,1], FMatrix[4,2], FMatrix[4,3]);
			glVertex3f(FMatrix[4,1]+FMatrix[1,1]*scale, FMatrix[4,2]+FMatrix[1,2]*scale, FMatrix[4,3]+FMatrix[1,3]*scale);
		end;
		if uniform or (active = shapes[2]) then
		begin
			glColor3f(0.2,1,0.2);
			glVertex3f(FMatrix[4,1], FMatrix[4,2], FMatrix[4,3]);
			glVertex3f(FMatrix[4,1]+FMatrix[2,1]*scale, FMatrix[4,2]+FMatrix[2,2]*scale, FMatrix[4,3]+FMatrix[2,3]*scale);
		end;
		if uniform or (active = shapes[3]) then
		begin
			glColor3f(0.2,0.2,1);
			glVertex3f(FMatrix[4,1], FMatrix[4,2], FMatrix[4,3]);
			glVertex3f(FMatrix[4,1]+FMatrix[3,1]*scale, FMatrix[4,2]+FMatrix[3,2]*scale, FMatrix[4,3]+FMatrix[3,3]*scale);
		end;

		glEnd;

		glDisable(GL_LINE_STIPPLE);
	end;
end;

procedure TScaleManipulator.OnActivate;
begin
	scale := 1.0;
end;

procedure TScaleManipulator.OnDeactivate;
var
	m : TMatrix;
begin
	Identity(m);
	
	if uniform then
	begin
		m[1,1] := scale;
		m[2,2] := scale;
		m[3,3] := scale;
	end else
	begin
		if active = shapes[1] then
			m[1,1] := scale;
		if active = shapes[2] then
			m[2,2] := scale;
		if active = shapes[3] then
			m[3,3] := scale;
	end;
	
	FDiff := m;
	Mul44(FMatrix, m);
end;

procedure TScaleManipulator.OnUpdate(x, y : Longint);
var
	diff_x, diff_y : Single;
	dtx, dty : Single;
	amount : Single;
	axis : TVec3;
begin
	diff_x := (x - start_x) / 100;
	diff_y := (y - start_y) / 100;

	//writeln((x - start_x), ' ', (y - start_y));

	if active = shapes[1] then
	begin
		axis.x := FMatrix[1,1]; axis.y := FMatrix[1,2]; axis.z := FMatrix[1,3];
	end else
	if active = shapes[2] then
	begin
		axis.x := FMatrix[2,1]; axis.y := FMatrix[2,2]; axis.z := FMatrix[2,3];
	end else
	if active = shapes[3] then
	begin
		axis.x := FMatrix[3,1]; axis.y := FMatrix[3,2]; axis.z := FMatrix[3,3];
	end else
		Writeln('???');
		
	Normalize(axis);

	dtx := axis.x*modelview[1,1] + axis.y*modelview[1,2] + axis.z*modelview[1,3];
	dty := axis.x*modelview[2,1] + axis.y*modelview[2,2] + axis.z*modelview[2,3];

	amount := (diff_x*dtx + diff_y*dty) / 10;
	scale := scale + amount;
	
	if scale < 0.0 then
		scale := 0.0;
end;

end.