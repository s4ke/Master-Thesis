@ECHO OFF
ECHO Build started...
stack ghc filter/haskell.hs
CALL gen-figures.cmd
ECHO Starting markdown to pdf conversion...
CALL gen-pdf.cmd
rm -r tex2pdf.*