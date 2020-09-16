ppcrossx64 -Sd -FE. -Fusource\main -FUobj\split source\main\split.pas
if [%ERRORLEVEL%]==[0] goto :EOF 
pause