elm-make src\Main.elm --output bin\main.js

if not exist bin\img mkdir bin\img 
xcopy img bin\img /s /e /y

copy *.css bin
copy *.js bin
copy index-dist.html bin\index.html