#include <IGizmo.h>

#define C_EXPORT extern "C" __declspec(dllexport)

C_EXPORT IGizmo *IGizmo_CreateMoveGizmo()
{
	return CreateMoveGizmo();
}

C_EXPORT IGizmo *IGizmo_CreateRotateGizmo()
{
	return CreateRotateGizmo();
}

C_EXPORT IGizmo *IGizmo_CreateScaleGizmo()
{
	return CreateScaleGizmo();
}

C_EXPORT void IGizmo_SetEditMatrix(IGizmo *_this, float *matrix)
{
	_this->SetEditMatrix(matrix);
}

C_EXPORT void IGizmo_SetCameraMatrix(IGizmo *_this, const float *model, const float *proj)
{
	_this->SetCameraMatrix(model, proj);
}

C_EXPORT void IGizmo_SetScreenDimension(IGizmo *_this, int x, int y)
{
	_this->SetScreenDimension(x, y);
}

C_EXPORT int IGizmo_OnMouseDown(IGizmo *_this, int x, int y)
{
	return _this->OnMouseDown(x, y);
}

C_EXPORT void IGizmo_OnMouseMove(IGizmo *_this, int x, int y)
{
	_this->OnMouseMove(x, y);
}

C_EXPORT void IGizmo_OnMouseUp(IGizmo *_this, int x, int y)
{
	_this->OnMouseUp(x, y);
}

C_EXPORT void IGizmo_SetLocation(IGizmo *_this, int loc)
{
	_this->SetLocation(IGizmo::LOCATION(loc));
}

C_EXPORT void IGizmo_SetAxisMask(IGizmo *_this, int mask)
{
	_this->SetAxisMask(mask);
}

C_EXPORT void IGizmo_Draw(IGizmo *_this)
{
	_this->Draw();
}
