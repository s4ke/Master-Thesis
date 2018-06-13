#!/bin/bash
echo "%include custom.fmt" > in.tmp
lhs2TeX -i custom.fmt --poly in.tmp > $1
rm in.tmp