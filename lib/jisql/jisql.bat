@echo off

SET mypath=%~dp0

java -cp %mypath%jisql.jar;%mypath%jopt-simple.jar;%mypath%javacsv.jar;%mypath%%5 com.xigole.util.sql.Jisql -driver %6 -user %1 -password %2 -cstring jdbc:sqlserver://%3;DataBaseName=%4 -c SENDQUERY