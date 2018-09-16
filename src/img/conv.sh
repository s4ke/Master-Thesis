#!/bin/bash
for pdfile in *.pdf ; do
  /usr/bin/convert -verbose -density 500 -resize '800' "${pdfile}" "${pdfile%.*}".png
done
