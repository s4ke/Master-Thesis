@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION
SET customTeXFile=out/tex/finalCustom.tex
SET outfile=out/pdf/customOut.pdf
SET outjobname=out/pdf/customOut

latexmk -pdf -jobname=%outjobname% %customTeXFile%

ECHO Build complete, opening file.
start "C:\Program Files\SumatraPDF\SumatraPDF.exe" "%outfile%"