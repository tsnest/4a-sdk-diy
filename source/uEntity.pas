unit uEntity;

interface
uses common, fouramdl, skeleton, vmath, PhysX, Konfig;

type
	TEntity = class
		data : TSection;
		param_id : TIntegerValue;
		param_pid : TIntegerValue;
		param_matrix : TFloatArrayValue;

		classname, FName, visual_name : String;
		FMatrix : TMatrix;
		bbox : TAABB;
		
		ph_scene : TPHScene;
		ph : TPHActor;
		ph_shape : TPHShape;

		model : TResModel;
		mtlset : Longint;
		
		FSelected : Boolean;
		FVisible : Boolean;
		
		// служебное поле для рендера, хранит квадрат расстояния до камеры
		// вроде-бы не должно здесь быть, но с другой стороны тут его разместить удобно
		distance_sqr : Single;

		constructor Create(owner : TPHScene; data : TSection);
		destructor Destroy; override;

		procedure Draw(blended, distort : Boolean);
		procedure DrawFlag;
		procedure DrawShapes;
		
		function GetSkeleton : T4ASkeleton;
		function GetBoneTransform(const bone_name : String; out transform : TMatrix) : Boolean;
		
		procedure UpdateShapes;

		protected
		procedure UpdatePhysics;
		procedure UpdateBBox;
		
		function GetID : Word;
		function GetParentID : Word;
		
		procedure SetParentID(pid : Word);
		procedure SetMatrix(const m : TMatrix);
		procedure SetName(const newname : String);
		procedure SetVisual(const v : String);
		
		function GetAttachBone : String;
		procedure SetAttachBone(const bone : String);
		
		function GetAttachOffset : TMatrix;
		procedure SetAttachOffset(const offset : TMatrix);
		
		procedure SetVisible(v : Boolean);

		public
		property ID : Word read GetID;
		property ParentID : Word read GetParentID write SetParentID;
		property Matrix : TMatrix read FMatrix write SetMatrix;
		property Name : String read FName write SetName;
		property VisualName : String read visual_name write SetVisual;
		
		property AttachBone : String read GetAttachBone write SetAttachBone;
		property AttachOffset : TMatrix read GetAttachOffset write SetAttachOffset;
		
		property Selected : Boolean read FSelected write FSelected;
		property Visible : Boolean read FVisible write SetVisible;
	end;
	
	TEntityArray = array of TEntity;

implementation
uses GL, GLU, GLExt, PHGroups, sysutils, classes;

constructor TEntity.Create(owner : TPHScene; data : TSection);
var
	param_name : TStringValue;
	v : TSimpleValue;
	m : TFloatArrayValue;

	vn : String;
begin
	inherited Create;

	ph_scene := owner;

	self.data := data;

	param_id := data.GetParam('id', 'u16') as TIntegerValue;
	param_pid := data.GetParam('parent_id', 'u16') as TIntegerValue;
	param_name := data.GetParam('name', 'stringz') as TStringValue;
	param_matrix := data.GetParam('', 'pose, matrix') as TFloatArrayValue;

	if (param_id = nil) or (param_pid = nil) or (param_name = nil) or (param_matrix = nil) then
		raise Exception.Create('Invalid entity description');

	FName := param_name.str;
	classname := data.GetStrDef('class', '');
	
	if Length(param_matrix.data) = 16 then
		param_matrix.GetMatrix44(FMatrix)
	else
		param_matrix.GetMatrix43(FMatrix);

	v := data.GetParam('visual', 'stringz');
	if v <> nil then
	begin
		vn := (v as TStringValue).str;
		VisualName := vn;
	end else
		VisualName := '';
		
	FSelected := False;
	FVisible := True;
end;

destructor TEntity.Destroy;
begin
	if ph <> nil then
		PHRemoveActor(ph_scene, ph);

	if model <> nil then
		FreeModel(model);

	inherited Destroy;
end;

procedure TEntity.Draw(blended, distort : Boolean);
begin
	glPushMatrix;
	glMultMatrixf(@FMatrix);

	if Assigned(model) then
	begin
		if (model.maler_lod0 <> nil) and (distance_sqr > 30*30) then
			model.maler_lod0.Draw(mtlset, self.selected, blended, distort)
		else if (model.maler_lod1 <> nil) and (distance_sqr > 10*10) then
			model.maler_lod1.Draw(mtlset, self.selected, blended, distort)
		else if model.maler <> nil then
			model.maler.Draw(mtlset, self.selected, blended, distort);
	end;

	glPopMatrix;
end;

procedure TEntity.DrawFlag;
var
	I : Longint;
	light_bone : String;
	ltype : Byte;
	angle : Single;
	range : Single;
begin
	glPushMatrix;
	glMultMatrixf(@FMatrix);

	if selected then
		common.DrawFlag(fclYellow)
	else
		common.DrawFlag(fclWhite);
{		
	if selected and (classname = 'o_hlamp') then
	begin
		ltype := data.GetInt('ltype', 'u8');
		angle := data.GetFloat('spot_cone_angle', 'angle, fp32');
		range := data.GetFloat('range');
		
		if ltype in [4,5,6,7,8,9] then // spot or quad
		begin
			glBegin(GL_LINES);
			for I := 1 to 25 do
			begin
				glVertex3f(0, 0, 0);
				glVertex3f(Sin(angle/2) * Cos(I/25*PI*2) * range, Cos(angle/2) * range, Sin(angle/2) * -Sin(I/25*PI*2) * range);
			end;
			glEnd;
		end;
		
		if ltype in [7,8,9] then // quad
		begin		
			glBegin(GL_LINES);

			glVertex3f(0, 0, 0);
			glVertex3f(Sin(angle/2) * range, Cos(angle/2) * range, Sin(angle/2) * range);
			
			glVertex3f(0, 0, 0);
			glVertex3f(-Sin(angle/2) * range, Cos(angle/2) * range, Sin(angle/2) * range);
			
			glVertex3f(0, 0, 0);
			glVertex3f(Sin(angle/2) * range, Cos(angle/2) * range, -Sin(angle/2) * range);
			
			glVertex3f(0, 0, 0);
			glVertex3f(-Sin(angle/2) * range, Cos(angle/2) * range, -Sin(angle/2) * range);
			
			//
			glVertex3f(Sin(angle/2) * range, Cos(angle/2) * range, Sin(angle/2) * range);
			glVertex3f(-Sin(angle/2) * range, Cos(angle/2) * range, Sin(angle/2) * range);
			
			//
			glVertex3f(Sin(angle/2) * range, Cos(angle/2) * range, -Sin(angle/2) * range);
			glVertex3f(-Sin(angle/2) * range, Cos(angle/2) * range, -Sin(angle/2) * range);
			
			//
			glVertex3f(Sin(angle/2) * range, Cos(angle/2) * range, -Sin(angle/2) * range);
			glVertex3f(Sin(angle/2) * range, Cos(angle/2) * range, Sin(angle/2) * range);
			
			//
			glVertex3f(-Sin(angle/2) * range, Cos(angle/2) * range, -Sin(angle/2) * range);
			glVertex3f(-Sin(angle/2) * range, Cos(angle/2) * range, Sin(angle/2) * range);

			glEnd;
		end;
	end;
}
	glPopMatrix;
end;

procedure TEntity.DrawShapes;
var
	I : Longint;
	shapes, shape : TSection;
	shape_type : Longword;

	procedure DrawBox(box : TSection);
	var
		v : TFloatArrayValue;
		h_size : TFloatArrayValue;
		
		m : TMatrix;
		x, y, z : Single;
	begin
		v := box.GetParam('', 'pose, matrix') as TFloatArrayValue;
		if Length(v.data) = 16 then
		begin
			v.GetMatrix44(m);
		end else
		begin
			v.GetMatrix43(m);
		end;
	
		h_size := box.GetParam('h_size', 'vec3f') as TFloatArrayValue;
		x := h_size.data[0];
		y := h_size.data[1];
		z := h_size.data[2];

		glPushMatrix;
		glMultMatrixf(@m);

		glBegin(GL_QUADS);
		glVertex3f(-x,	y,	z);
		glVertex3f( x,	y,	z);
		glVertex3f( x, -y,	z);
		glVertex3f(-x, -y,	z);

		glVertex3f( x, -y, -z);
		glVertex3f( x,	y, -z);
		glVertex3f(-x,	y, -z);
		glVertex3f(-x, -y, -z);

		glVertex3f(-x, -y,	z);
		glVertex3f(-x, -y, -z);
		glVertex3f(-x,	y, -z);
		glVertex3f(-x,	y,	z);

		glVertex3f( x,	y,	z);
		glVertex3f( x,	y, -z);
		glVertex3f( x, -y, -z);
		glVertex3f( x, -y,	z);

		glVertex3f(-x,	y, -z);
		glVertex3f( x,	y, -z);
		glVertex3f( x,	y,	z);
		glVertex3f(-x,	y,	z);

		glVertex3f(-x, -y, -z);
		glVertex3f(-x, -y,	z);
		glVertex3f( x, -y,	z);
		glVertex3f( x, -y, -z);
		glEnd;

		glPopMatrix;
	end;

	procedure DrawSphere(sphere : TSection);
	var
		radius : TSingleValue;
		center : TFloatArrayValue;
		q : PGLUquadric;
	begin
		radius := sphere.GetParam('radius', 'fp32') as TSingleValue;
		center := sphere.GetParam('center', 'vec3f') as TFloatArrayValue;

		glPushMatrix;
		glTranslatef(center.data[0], center.data[1], center.data[2]);

		q := gluNewQuadric;
		gluSphere(q, radius.num, 10, 10);
		gluDeleteQuadric(q);

		glPopMatrix;
	end;
begin
	shapes := data.GetSect('shapes', False);
	if shapes <> nil then
	begin
		glDisable(GL_CULL_FACE);

		if selected then
			glColor4f(0.20, 0.85, 0.75, 0.4)
		else
			glColor4f(0.20, 0.85, 0.75, 0.2);

		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

		glPushMatrix;
		glMultMatrixf(@FMatrix);

		for I := 0 to shapes.items.Count - 1 do
		begin
			if TObject(shapes.items[I]) is TSection then
			begin
				shape := TObject(shapes.items[I]) as TSection;
				shape_type := shape.GetInt('type', 'u32');
				case shape_type of
					1: DrawBox(shape.GetSect('box'));
					0: DrawSphere(shape.GetSect('sphere'));
					else
						WriteLn('shape type = ', shape_type, ' object ', Name);
				end;
				
				
				if selected then
				begin
					RenderWireframe;
					
					glEnable(GL_FRAGMENT_PROGRAM_ARB);
					glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, prog[FP_SELECTED]);
					glDisable(GL_BLEND);
					
					case shape_type of
						1: DrawBox(shape.GetSect('box'));
						0: DrawSphere(shape.GetSect('sphere'));
						else
							WriteLn('shape type = ', shape_type, ' object ', Name);
					end;
					
					glDisable(GL_FRAGMENT_PROGRAM_ARB);
					glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, 0);
					glEnable(GL_BLEND);
					
					RenderDefault;
				end;
			end;
		end;

		glPopMatrix;

		glColor4f(1, 1, 1, 1);
		glDisable(GL_BLEND);

		glEnable(GL_CULL_FACE);
	end;
end;

function TEntity.GetSkeleton : T4ASkeleton;
begin
	if (model <> nil) and (model.model is T4AModelSkeleton) then
		Result := T4AModelSkeleton(model.model).skeleton
	else
		Result := nil;
end;

function TEntity.GetBoneTransform(const bone_name : String; out transform : TMatrix) : Boolean;
var
	mat, bone_mat : TMatrix;
	skeleton : T4ASkeleton;
begin
	Result := False;
	
	skeleton := GetSkeleton;
	if (skeleton <> nil) and skeleton.GetTransform(bone_name, bone_mat) then
	begin
		mat := FMatrix;
		Mul44(mat, bone_mat);
		transform := mat;
		
		Result := True;
	end;
end;

procedure TEntity.UpdateShapes;
var
	I : Longint;
	remove : TList;
	
	shapes, shape, box, sphere : TSection;
	shape_type : Longword;
	
	v : TFloatArrayValue;
	f : TFloatArrayValue;
	mat : TMatrix;
	h_size, center : TVec3;
	
	sss, ddd : Pointer;
begin
	if ph = nil then
		Exit;
		
	remove := TList.Create;
	
	for I := 0 to PHGetShapeCount(ph)-1 do
		if PHGetGroup(PHGetShape(ph, I)) = PH_GROUP_SHAPE then
			remove.Add(PHGetShape(ph, I));
			
	for I := 0 to remove.Count-1 do
		PHRemoveShape(remove[I]);
		
	remove.Free;
	
	// Shapes of restrictor
	
	shapes := data.GetSect('shapes', False);
	if shapes <> nil then
	begin
		for I := 0 to shapes.ParamCount - 1 do
		begin
			if shapes.GetParam(I) is TSection then
			begin
				shape := TSection(shapes.GetParam(I));
				shape_type := shape.GetInt('type', 'u32');
				case shape_type of
					1: begin
						box := shape.GetSect('box');
						
						v := box.GetParam('', 'pose, matrix') as TFloatArrayValue;
						if Length(v.data) = 16 then
							v.GetMatrix44(mat)
						else
							v.GetMatrix43(mat);
					
						f := box.GetParam('h_size', 'vec3f') as TFloatArrayValue;
						h_size.x := f.data[0];
						h_size.y := f.data[1];
						h_size.z := f.data[2];
						
						ddd := PHShapeBoxM44(@h_size, @mat);
						sss := PHAddShape(ph, ddd);
						
						PHSetGroup(sss, PH_GROUP_SHAPE);
						PHSetShapeUserdata(sss, Pointer(shape));
					end;
					0: begin
						sphere := shape.GetSect('sphere');
						
						f := sphere.GetParam('center', 'vec3f') as TFloatArrayValue;
						center.x := f.data[0];
						center.y := f.data[1];
						center.z := f.data[2];
						
						ddd := PHShapeSphere(sphere.GetFloat('radius'), @center);
						sss := PHAddShape(ph, ddd);
						
						PHSetGroup(sss, PH_GROUP_SHAPE);
						PHSetShapeUserdata(sss, Pointer(shape));
					end;
					else
						WriteLn('shape type = ', shape_type, ' object ', Name);
				end;
			end;
		end;
	end;
end;

procedure TEntity.UpdatePhysics;
var
	dim, offset : TVec3;
	
	descs : array of Pointer;
	I, J : Longint;
	
	ph_matrix : TMatrix;
	scale : TVec3;
begin
	if ph <> nil then
		PHRemoveActor(ph_scene, ph);
	ph := nil;
	
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
	
	// Triangle mesh
	
	if (model <> nil) and (model.ph <> nil) and (model.ph.Count > 0) then
	begin
		SetLength(descs, model.ph.Count);
		
		J := 0;
		for I := 0 to model.ph.Count - 1 do
			if model.ph[I] <> nil then
			begin
				descs[J] := PHShapeTrimesh(model.ph[I], @scale);
				Inc(J);
			end;

		ph := PHCreateActor(ph_scene, 1, J, @descs[0], @ph_matrix);
	end;
	
	// Default box if we hasn't model
	
	if ph = nil then
	begin
		dim.x := 0.05; dim.y := 0.25; dim.z := 0.125; // scale ?
		offset.x := 0; offset.y := 0.25; offset.z := -0.125;

		SetLength(descs, 1);
		descs[0] := PHShapeBox(@dim, @offset);
		ph := PHCreateActor(ph_scene, 1, Length(descs), @descs[0], @ph_matrix);
		
		if ph = nil then
		begin
			//IupMessage('error', 'phaddbox');
			WriteLn('PHCreateActor failed, entity ''' + Name + '''');
		end;
	end;

	PHSetUserdata(ph, self);	
	
	UpdateShapes;
end;

procedure TEntity.UpdateBBox;
begin
	if Assigned(model) then
	begin
		bbox := model.model.bbox;
	end else
	begin
		bbox.min.x := -0.25;
		bbox.min.y := 0.0;
		bbox.min.z := -0.25;

		bbox.max.x := 0.25;
		bbox.max.y := 0.5;
		bbox.max.z := 0.25;
	end;
	AABBTransform(bbox, FMatrix);
end;

function TEntity.GetID : Word;
begin
	GetID := param_id.num;
end;

function TEntity.GetParentID : Word;
begin
	GetParentID := param_pid.num;
end;

procedure TEntity.SetParentID(pid : Word);
begin
	param_pid.num := pid;
end;

procedure TEntity.SetMatrix(const m : TMatrix);
var
	ph_matrix : TMatrix;
	scale : TVec3;
	
	I : Longint;
	shape : TPHShape;
begin
	FMatrix := m;
	UpdateBBox;
	
	if ph <> nil then
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
		
		for I := 0 to PHGetShapeCount(ph)-1 do
		begin
			shape := PHGetShape(ph, I);
			if PHIsTrimeshShape(shape) <> 0 then
				PHSetMeshScale(shape, @scale);
		end;
		
		PHSetGlobalPoseM44(ph, @ph_matrix);
	end;

	if Length(param_matrix.data) = 16 then
	begin
		param_matrix.SetMatrix44(FMatrix);
	end else
	begin
		param_matrix.SetMatrix43(FMatrix);	
	end;
end;

procedure TEntity.SetName(const newname : String);
var
	param_name : TStringValue;
begin
	FName := newname;

	data.name := FName;
	param_name := data.GetParam('name', 'stringz') as TStringValue;
	param_name.str := FName;
end;

procedure TEntity.SetVisual(const v : String);
var
	at : Integer;
	model_name, preset : String;
begin
	if model <> nil then
		FreeModel(model);
	model := nil;
	
	visual_name := v;
	
	at := Pos('@', v);
	if at <> 0 then
	begin
		model_name := Copy(v, 1, at-1);
		preset := Copy(v, at+1, Length(v)-at);
	end else
	begin
		model_name := v;
		preset := '';
	end;
	
	if model_name <> '' then
	begin
		model := GetModel(model_name);

		if (model <> nil) and (preset <> '') then
			mtlset := model.GetMaterialSet(preset)
		else
			mtlset := -1;
	end;

	UpdatePhysics;
	UpdateBBox;
end;

function TEntity.GetAttachBone : String;
var
	s : TStringValue;
begin
	s := data.GetParam('att_bone_id', 'stringz') as TStringValue;
	GetAttachBone := s.str;
end;

procedure TEntity.SetAttachBone(const bone : String);
var
	s : TStringValue;
begin
	s := data.GetParam('att_bone_id', 'stringz') as TStringValue;
	s.str := bone;
end;

function TEntity.GetAttachOffset : TMatrix;
var
	att_offset : TFloatArrayValue;
	att_mat : TMatrix;
begin
	att_offset := data.GetParam('att_offset', 'pose, matrix') as TFloatArrayValue;
	if att_offset <> nil then
	begin
		att_offset.GetMatrix44(att_mat);
	end else
	begin
		att_offset := data.GetParam('att_offset', 'pose, matrix_43T') as TFloatArrayValue;
		att_offset.GetMatrix43T(att_mat);
	end;
	
	GetAttachOffset := att_mat;
end;

procedure TEntity.SetAttachOffset(const offset : TMatrix);
var
	att_offset : TFloatArrayValue;
begin
	att_offset := data.GetParam('att_offset', 'pose, matrix') as TFloatArrayValue;
	if att_offset <> nil then
	begin
		att_offset.SetMatrix44(offset);
	end else
	begin
		att_offset := data.GetParam('att_offset', 'pose, matrix_43T') as TFloatArrayValue;
		att_offset.SetMatrix43T(offset);
	end;
end;

procedure TEntity.SetVisible(v : Boolean);
begin
	if v <> FVisible then
	begin
		if v then begin
			UpdatePhysics;
		end else begin
			if ph <> nil then
				PHRemoveActor(ph_scene, ph);
			ph := nil;
		end;
	
		FVisible := v;
	end;
end;

end.

end.