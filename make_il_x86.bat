set UNIT_PATHS= ^
-Fusource ^
-Fusource\main ^
-Fusource\AssimpDelphi 

ppc386 -FE. -FUobj\il %UNIT_PATHS% -O2 -vw -gw -Mdelphi source\image_library.pas
if not [%ERRORLEVEL%]==[0] pause