#!/bin/bash
echo "%include custom.fmt" > in.tmp
lhs2TeX -i custom.fmt --poly in.tmp > compare.tmp
cat escapeInput.tmp >> in.tmp
lhs2TeX -i custom.fmt --poly in.tmp > actual.tmp
comm -23 --nocheck-order actual.tmp compare.tmp
# magic number 274 == length of compare.tmp
# sed -i -e 1,274d actual.tmp