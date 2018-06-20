import sys
import os
import subprocess
import json
from zipfile import ZipFile
from contextlib import closing

name = sys.argv[1]
assert(name)

solver = sys.argv[2]
incumbent = sys.argv[3]
test = int(sys.argv[4])

if test:
    batchPath = '../batch_solve.py'
else:
    batchPath = 'batch_solve.py'

command = ['python2.7', batchPath, '-d', '-s', solver,
           '-o', name, '-i', 'inputs.txt', '-ss', '-l', '-ii', incumbent]

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
    with closing(ZipFile('subproblems.zip', 'r')) as z:
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

if len(params) > 1:
    command.append('-sm')
    command.append('1')

resources = []
if os.path.isfile('resources.json'):
    with open('resources.json', 'r') as f:
        resources = json.load(f)

if resources:
    command.append('-r')
    command += resources

if not test:
    token = os.environ.get('EVEREST_TASK_TOKEN', '')
    assert(token)
    with open('token.txt', 'w') as f:
        f.write(token)
    command.append('-t')
    command.append('token.txt')

print command, inputs

retCode = 0
try:
    subprocess.check_call(command)
except subprocess.CalledProcessError:
    print 'batch_solve.py returned nonzero error code'
    retCode = 1

#os.rename(name + '.sol', 'solution.sol')
if os.path.isfile(name + '.log'):
    os.rename(name + '.log', 'everest.log')
if os.path.isfile(os.path.join('debug', name + '-results.zip')):
    os.rename(os.path.join('debug', name + '-results.zip'), 'results.zip')
if os.path.isfile(name + '-solutions.zip'):
    os.rename(name + '-solutions.zip', 'solutions.zip')
if os.path.isfile(name + '-solutions.json'):
    os.rename(name + '-solutions.json', 'solutions.json')
if os.path.isfile(name + '-tasks.json'):
    os.rename(name + '-tasks.json', 'tasks.json')
if os.path.isfile(name + '-incumbent-no-sol.txt'):
    os.rename(name + '-incumbent-no-sol.txt', 'incumbent-no-sol.txt')

if os.path.isfile(name + '-status.txt'):
    with open(name + '-status.txt', 'r') as f:
        lines = f.read().split('\n')
    with open('objective.txt', 'w') as f:
        f.write('%s' % lines[0])
    with open('status.txt', 'w') as f:
        f.write('%s' % lines[1])

exit(retCode)
