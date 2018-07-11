import os
import fileinput
from subprocess import Popen, PIPE
import uuid
import threading
from io import StringIO


def lhs2TeX(input, num_lines_header):

    p = Popen(
        ['lhs2TeX', '-i', 'custom.fmt', '--poly'],
        stdin = PIPE,
        stdout = PIPE,
        stderr = PIPE,
        universal_newlines=True)

    res = ""
    for line in input:
        if str(line).lower().startswith("myshittyendmarker"):
            break

        p.stdin.write(line + "\n\n%EOF\n")
        p.stdin.flush()

    lineCnt = 0
    for line in p.stdout.readlines():
        if(lineCnt > num_lines_header):
            print(line)

        lineCnt += 1




    p.stdin.close()  # no more input
    assert not p.stdout.read()  # should be empty
    p.stdout.close()
    p.kill()

lhs2TeX(["\\begin{code}\ntoast\n\\end{code}", "MyShittyEndMarker"], 10)
