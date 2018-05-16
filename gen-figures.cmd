@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION
SET texdir=src\tex\figures
SET texout=src\tex\texout
SET destdir=src\img
ECHO Generating figures in %texdir%/*.tex...
FOR %%f IN (%texdir%/*.tex) DO (
    SET tex=%texdir%\%%~nf
    ECHO Generating !tex! from %texdir%
    pdflatex -output-directory=%texout% -interaction=batchmode !tex! > NUL
    SET pdf=%texout%\%%~nf.pdf
    SET dest=%destdir%\%%~nf.pdf
    copy !pdf! !dest! > NUL
)
ECHO Figure generation complete.