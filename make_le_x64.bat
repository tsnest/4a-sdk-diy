set UNIT_PATHS= ^
-Fusource ^
-Fusource\main ^
-Fusource\AssimpDelphi

ppcrossx64 -FE. -FUobj\le %UNIT_PATHS% -O2 -vw -gw -Mdelphi source\level_Editor.pas
if not [%ERRORLEVEL%]==[0] pause