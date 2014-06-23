#!/usr/bin/env python

import os
import sys
import glob
import tempfile
import subprocess

if sys.version_info[0] >= 3:
    from io import BytesIO
else:
    from cStringIO import StringIO as BytesIO

TEST_DIR = os.path.dirname(__file__)
EXE = os.path.join(TEST_DIR, '..', 'main.byte')


successes = []
fails = []
for (caml, coq) in zip(glob.glob(os.path.join(TEST_DIR, '*.ml')),
                       glob.glob(os.path.join(TEST_DIR, '*.v'))):
    sys.stdout.write('%s... ' % caml)
    sys.stdout.flush()
    with open(caml) as caml_fd:
        try:
            output = subprocess.check_output([EXE], stdin=caml_fd)
        except subprocess.CalledProcessError:
            fails.append(caml)
            continue
    with tempfile.TemporaryFile() as coq_of_caml_fd:
        coq_of_caml_fd.write(output)
        coq_of_caml_fd.seek(0)
        if subprocess.call(['diff', '-', coq], stdin=coq_of_caml_fd):
            fails.append(caml)
        else:
            successes.append(caml)
            print('ok.')

print('Result:')
print('\t%d successes' % len(successes))
print('\t%d fails:' % len(fails))
for fail in fails:
    print('\t\t%s' % fail)
