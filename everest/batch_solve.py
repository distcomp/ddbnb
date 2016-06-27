#!/usr/bin/env python2.7
import os
import sys
import argparse
from zipfile import ZipFile
import shutil
import tempfile

import everest

def makeParser():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-d', '--debug', action='store_true')
    parser.add_argument('-t', '--token', default='~/.everest_token',
                        help='token obtainable with everest.py')
    parser.add_argument('-r', '--resources', nargs='+', default=[],
                        help='everest resources to be used')
    parser.add_argument('-s', '--solver', default='scip', choices=['scip', 'cbc'],
                        help='solver to use')
    parser.add_argument('-i', '--input', type=argparse.FileType('rb'),
                        help='file with a list of input NL-files')
    parser.add_argument('-o', '--out-prefix', default='out', help='output prefix')
    parser.add_argument('file', nargs='*', default=[], help='input NL-files')
    return parser

def main0():
    tmpDir = tempfile.mkdtemp()
    try:
        main(tmpDir)
    finally:
        shutil.rmtree(tmpDir)

def main(tmpDir):
    parser = makeParser()
    args = parser.parse_args()

    with open(os.path.expanduser(args.token)) as f:
        token = f.read().strip()

    stubs = []
    if not args.input is None:
        stubs.extend(args.input.read().split())
    stubs.extend(args.file)

    if not stubs:
        print 'No problem stubs specified'
        sys.exit(1)

    # if not args.resources:
    #     print 'No resources specified'
    #     sys.exit(1)

    if args.debug:
        tmpDir = 'debug'
        try:
            os.mkdir(tmpDir)
        except OSError:
            pass

    def makeName(suffix):
        return os.path.join(tmpDir, 'out' + suffix)

    with ZipFile(makeName('.zip'), 'w') as z:
        z.write('run-task.sh')
        z.write('port_proxy.py')
        z.write('task.py')
        for i, stub in enumerate(stubs):
            z.write(stub, 'stub%d.nl' % i)

    with open(makeName('.plan'), 'wb') as f:
        f.write('parameter n from 0 to %d step 1\n' % (len(stubs) - 1))
        f.write('input_files run-task.sh task.py port_proxy.py stub${n}.nl\n')
        f.write('command bash run-task.sh %s_port stub${n}.nl\n' % args.solver)
        f.write('output_files stub${n}.sol stderr stdout\n')

    session = everest.Session('dcbc', 'https://everest.distcomp.org',
                              token=token)
    try:
        psweep = everest.App('57430c9c2b00004f3492590d', session)

        job = psweep.run({
            "plan": open(makeName('.plan'), 'rb'),
            "files": open(makeName('.zip'), 'rb')
        }, args.resources)

        try:
            result = job.result()
        except everest.JobException:
            result = session.getJobStatus(job.id)
            if 'result' in result:
                print 'Job failed, result downloaded'
                session.getFile(result['result']['results'], makeName('-results.zip'))
            else:
                print 'Job failed, no result available'
            sys.exit(1)

        session.getFile(result['results'], makeName('-results.zip'))

        with ZipFile(makeName('-results.zip'), 'r') as z:
            solutions = []
            for x in z.namelist():
                if not 'stderr' in x:
                    continue
                err = z.read(x)
                result = [l for l in err.split('\n') if 'sendResult' in l]
                assert(len(result) == 1)
                result = result[0].split()
                spl = x.split('/')
                n = int(spl[0])
                spl[-1] = 'stub%d.sol' % n
                solutions.append((float(result[-2].rstrip(',')), result[-1],
                                  '/'.join(spl)))
            best = min(solutions)
            print 'Best solution %f (%s) for %s saved to %s' % \
                (best[0], best[1], best[2], args.out_prefix + '.sol')
            with open(args.out_prefix + '.sol', 'wb') as f:
                f.write(z.read(best[2]))
    finally:
        session.close()

if __name__ == "__main__":
    main0()
