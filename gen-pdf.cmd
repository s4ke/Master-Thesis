@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION
SET outfile=out/pdf/out.pdf
SET mddir=src/md
SET mdfiles=
FOR %%f IN (%mddir%/*.md) DO (
    SET mdfiles=!mdfiles! %mddir%\%%~f
)
ECHO Building from !mdfiles!
pandoc -f markdown+tex_math_dollars src/res/options.yaml !mdfiles! -o %outfile% --template src/res/gdv-pandoc-template.latex --table-of-contents --filter pandoc-eqnos --filter pandoc-fignos --filter pandoc-tablenos --filter pandoc-citeproc --filter filter/haskell
"C:\Program Files\SumatraPDF\SumatraPDF.exe" "%~dp0\%outfile%"
ECHO Build complete, opening file.