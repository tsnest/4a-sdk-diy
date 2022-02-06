unit uLEOptions;

interface

type
  TManipulatorMode = (mmNone, mmMove, mmRotate, mmScale);
	TManipulatorAxis = (maObject, maWorld, maGroup);
	
var
	m_mode : TManipulatorMode = mmNone;
	m_move_axis : TManipulatorAxis = maObject;
	m_rotate_axis : TManipulatorAxis = maObject;
	m_uniform_scale : Boolean = False;
	lineWidth : Longint = 1;
	
var
	camera_move_sens      : Single = 0.2;
	camera_rotate_sens    : Single = 1.0;
	camera_fly_speed      : Single = 1.0;
	camera_fly_speed_fast : Single = 5.0;
	
	props_exclude_vss_ver_6 : Boolean = False;
	props_two_column        : Boolean = False;
	cull_distance           : Boolean = True;
	instancing              : Boolean = True;

implementation
uses common, Inifiles, uLevelUndo;

var
	F : TIniFile;
	undo_limit : Longint;
	
initialization
	
F := TIniFile.Create('editor_data\level_editor.ini');

camera_move_sens      := F.ReadFloat('camera', 'move_sens', 0.2);
camera_rotate_sens    := F.ReadFloat('camera', 'rotate_sens', 1.0);
camera_fly_speed      := F.ReadFloat('camera', 'fly_speed', 1.0);
camera_fly_speed_fast := F.ReadFloat('camera', 'fly_speed_fast', 5.0);

lineWidth             := F.ReadInteger('manipulator', 'line_width', 1);
m_move_axis           := TManipulatorAxis( F.ReadInteger('manipulator', 'move_axis', Integer(maObject)) );
m_rotate_axis         := TManipulatorAxis( F.ReadInteger('manipulator', 'rotate_axis', Integer(maObject)) );
m_uniform_scale       := F.ReadBool('manipulator', 'uniform_scale', False);

bkg_color.x           := F.ReadFloat('clear_color', 'r', bkg_color.x);
bkg_color.y           := F.ReadFloat('clear_color', 'g', bkg_color.y);
bkg_color.z           := F.ReadFloat('clear_color', 'b', bkg_color.z);
bkg_color.w           := F.ReadFloat('clear_color', 'a', bkg_color.w);

undo_limit            := F.ReadInteger('history', 'undo_limit', 64);
SetUndoLimit          (undo_limit);

props_exclude_vss_ver_6 := F.ReadBool('properties', 'exclude_vss_ver_6', False);
props_two_column        := F.ReadBool('properties', 'two_column', False);

cull_distance           := F.ReadBool('rendering', 'cull_distance', True);

F.Free;

if not (m_move_axis in [maObject,maWorld]) then
	m_move_axis := maObject;
if not (m_rotate_axis in [maObject,maWorld,maGroup]) then
	m_rotate_axis := maObject;

finalization

F := TIniFile.Create('editor_data\level_editor.ini');

F.WriteFloat('camera', 'move_sens', camera_move_sens);
F.WriteFloat('camera', 'rotate_sens', camera_rotate_sens);
F.WriteFloat('camera', 'fly_speed', camera_fly_speed);
F.WriteFloat('camera', 'fly_speed_fast', camera_fly_speed_fast);

F.WriteInteger('manipulator', 'line_width', lineWidth);
F.WriteInteger('manipulator', 'move_axis', Integer(m_move_axis));
F.WriteInteger('manipulator', 'rotate_axis', Integer(m_rotate_axis));
F.WriteBool('manipulator', 'uniform_scale', m_uniform_scale);

F.WriteFloat('clear_color', 'r', bkg_color.x);
F.WriteFloat('clear_color', 'g', bkg_color.y);
F.WriteFloat('clear_color', 'b', bkg_color.z);
F.WriteFloat('clear_color', 'a', bkg_color.w);

F.WriteBool('rendering', 'cull_distance', cull_distance);

F.Free;

end.