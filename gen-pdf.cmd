@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION
SET postlhs2TeX=out/tex/postlhs2TeX.tex
SET pandoccompiled=out/tex/pandoccompiled.tex
SET outfile=out/pdf/out.pdf
SET outjobname=out/pdf/out
SET mddir=src/md
SET mdfiles=

FOR %%f IN (%mddir%/*.md) DO (
    SET mdfiles=!mdfiles! %mddir%\%%~f
)
ECHO Building from !mdfiles!

pandoc -f markdown+tex_math_dollars src/res/options.yaml !mdfiles! -o %pandoccompiled% --template src/res/gdv-pandoc-template.latex --table-of-contents --filter pandoc-eqnos --filter pandoc-fignos --filter pandoc-tablenos --filter pandoc-citeproc --filter haskell-filter
REM pandoc -f markdown+tex_math_dollars src/res/options.yaml !mdfiles! -o %pandoccompiled% --template src/res/gdv-pandoc-template.latex --table-of-contents --natbib --bibliography=src/res/references.bib --filter pandoc-eqnos --filter pandoc-fignos --filter pandoc-tablenos --filter haskell-filter

REM welcome to the bad bad world of hacky shell scripts
sed -i 's/@/@@/g' %pandoccompiled%
lhs2TeX -i custom.fmt --poly %pandoccompiled% > %postlhs2TeX%
REM magic number 266 == length of preamble generated by lhs2TeX
sed -i -e 1,266d %postlhs2TeX%
sed -i 's/ATSIGN/@/g' %postlhs2TeX%

latexmk -pdf -jobname=%outjobname% %postlhs2TeX%

ECHO Build complete, opening file.
start "C:\Program Files\SumatraPDF\SumatraPDF.exe" "%outfile%"