@ECHO OFF
ECHO Build started...
CALL gen-figures.cmd
ECHO Starting markdown to pdf conversion...
CALL gen-pdf.cmd