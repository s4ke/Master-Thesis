#!/bin/bash
echo "%include polycode.fmt" > in.tmp
lhs2TeX --poly in.tmp > $1
rm in.tmp