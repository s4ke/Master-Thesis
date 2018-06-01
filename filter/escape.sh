#!/bin/bash
echo "%include lhs2TeX.fmt" > in.tmp
lhs2TeX --poly in.tmp > compare.tmp
cat escapeInput.tmp >> in.tmp
lhs2TeX --poly in.tmp > actual.tmp
comm -23 --nocheck-order actual.tmp compare.tmp