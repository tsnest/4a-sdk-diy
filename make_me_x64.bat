set UNIT_PATHS= ^
-Fusource ^
-Fusource\main ^
-Fusource\AssimpDelphi ^
-Fusource\model_editor

ppcrossx64 %UNIT_PATHS% -FE. -FUobj\me -vw -gw -Mdelphi source\model_editor\model_Editor.pas
if not [%ERRORLEVEL%]==[0] pause