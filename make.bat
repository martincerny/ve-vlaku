elm-make src\Main.js --output bin\main.js

if not exist bin\img mkdir bin\img 
xcopy img bin\img /s /e

copy index-dist.html bin\index.html