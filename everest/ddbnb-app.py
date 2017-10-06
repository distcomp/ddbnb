import sys
import os
import subprocess
import json
from zipfile import ZipFile

name = sys.argv[1]
assert(name)

solver = sys.argv[2]

test = int(sys.argv[3])

if test:
    batchPath = '../batch_solve.py'
else:
    batchPath = 'batch_solve.py'

command = ['python2.7', batchPath, '-d', '-s', solver,
           '-o', name, '-i', 'inputs.txt', '-ss', '-l']

with open('parameters.json', 'r') as f:
    params = json.load(f)
for p in params:
    n = os.path.basename(p)
    assert(os.path.isfile(n))
    command.append('-pf')
    command.append(n)

os.mkdir('inputs')

inputs = []
with open('subproblems.json', 'r') as f:
    subproblems = json.load(f)
for p in subproblems:
    n = os.path.basename(p)
    assert(os.path.isfile(n))
    inputs.append(n)

if os.path.isfile('subproblems.zip'):
    with ZipFile('subproblems.zip', 'r') as z:
        for x in z.namelist():
            if not x.endswith('.nl') or '/' in x or '\\' in x:
                continue
            n = os.path.join('inputs', x)
            with open(n, 'w') as f:
                f.write(z.read(x))
            inputs.append(n)

with open('inputs.txt', 'w') as f:
    for i in inputs:
        f.write('%s\n' % i)

if len(params) > 1 and len(inputs) == 1:
    command.append('-sm')
    command.append('1')

if not test:
    token = os.environ.get('EVEREST_TASK_TOKEN', '')
    assert(token)
    with open('token.txt', 'w') as f:
        f.write(token)
    command.append('-t')
    command.append('token.txt')

print command, inputs

subprocess.check_call(command)

def makeName(suffix):
    return os.path.join('debug', name + suffix)

os.rename(name + '.sol', 'solution.sol')
os.rename(name + '.log', 'everest.log')
os.rename(os.path.join('debug', name + '-results.zip'), 'results.zip')

with open(name + '-status.txt', 'r') as f:
    lines = f.read().split('\n')
with open('objective.txt', 'w') as f:
    f.write('%s' % lines[0])
with open('status.txt', 'w') as f:
    f.write('%s' % lines[1])

