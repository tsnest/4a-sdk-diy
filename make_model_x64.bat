ppcrossx64 -Sd -FE. -Fusource\main -FUobj\model source\main\model.pas
if [%ERRORLEVEL%]==[0] goto :EOF 
pause