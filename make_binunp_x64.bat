ppcrossx64 -gw -vw -Sd -FE. -Fusource\main -FUobj\binunp source\main\binunp.pas
if [%ERRORLEVEL%]==[0] goto :EOF 
pause