#!/usr/bin/env python2.7
import os
import sys
import argparse
from zipfile import ZipFile, ZIP_DEFLATED
import shutil
import tempfile
from collections import defaultdict, OrderedDict
import json

import requests
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
    parser.add_argument('-p', '--parameters', default=[], nargs='+',
                        help='solver parameters as k=v pairs')
    parser.add_argument('-pf', '--parameters-file', action='append', default=[],
                        help='files with solver parameters. Overrides -p params')
    parser.add_argument('-l', '--get-log', action='store_true',
                        help='download job log')
    parser.add_argument('-ss', '--save-status', action='store_true',
                        help='save solution status and objective value')
    parser.add_argument('-sm', '--stop-mode', type=int, default='0',
                        help='0 - run until all tasks finish, 1 - run until any task finish, return best solution as result')
    parser.add_argument('-ii', '--initial-incumbent', type=float, default='1e23',
                        help='start task with initial incumbent')
    parser.add_argument('-i', '--input', type=argparse.FileType('rb'),
                        help='file with a list of input NL-files')
    parser.add_argument('-o', '--out-prefix', default='out', help='output prefix')
    parser.add_argument('-ur', '--use-results', help='Skip parameter sweep run by using already computed results')
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
    d = os.path.dirname(__file__)

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
        return os.path.join(tmpDir, args.out_prefix + suffix)

    paramsFiles = args.parameters_file
    if not paramsFiles:
        for p in args.parameters:
            assert(not ' ' in p)
        with open(makeName('params.txt'), 'w') as f:
            f.write('\n'.join(args.parameters))
        paramsFiles.append(makeName('params.txt'))

    stubNames = OrderedDict()
    with ZipFile(makeName('.zip'), 'w', ZIP_DEFLATED) as z:
        z.write(os.path.join(d, 'run-task.sh'), 'run-task.sh')
        z.write(os.path.join(d, 'port_proxy.py'), 'port_proxy.py')
        z.write(os.path.join(d, 'task.py'), 'task.py')
        for i, stub in enumerate(stubs):
            stubNames['stub%d' % i] = os.path.basename(stub)
            z.write(stub, 'stub%d.nl' % i)
        for i, params in enumerate(paramsFiles):
            z.write(params, 'params%d.txt' % i)

    with open(makeName('.plan'), 'wb') as f:
        f.write('parameter n from 0 to %d step 1\n' % (len(stubs) - 1))
        f.write('parameter p from 0 to %d step 1\n' % (len(paramsFiles) - 1))
        f.write('input_files run-task.sh task.py port_proxy.py stub${n}.nl params${p}.txt\n')
        f.write('command bash run-task.sh %s_port stub${n}.nl %d params${p}.txt %g\n' % (
            args.solver, args.stop_mode, args.initial_incumbent))
        f.write('output_files stub${n}.sol stderr stdout.tgz\n')

    if not args.use_results is None:
        saveResults(args.use_results, stubNames, args)
        return

    session = everest.Session('dcbc - ' + args.out_prefix, 'https://everest.distcomp.org',
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
                saveResults(makeName('-results.zip'), stubNames, args)
                if args.get_log:
                    print "Downloading job's log..."
                    session.getJobLog(job.id, args.out_prefix + '.log')
            else:
                print 'Job failed, no result available'
            sys.exit(1)
        except KeyboardInterrupt:
            print 'Cancelling the job...'
            try:
                job.cancel()
            except requests.exceptions.HTTPError as e:
                sys.stderr.write('Response from the server: %s\n' % e.response.content)
                sys.stderr.write('Headers of the request: %s\n' % e.request.headers)
                raise
            try:
                result = job.result()
            except everest.JobException as e:
                print e
            return

        session.getFile(result['results'], makeName('-results.zip'))
        saveResults(makeName('-results.zip'), stubNames, args)
        if args.get_log:
            print "Downloading job's log..."
            session.getJobLog(job.id, args.out_prefix + '.log')
    finally:
        session.close()

def saveResults(jobResults, stubNames, args):
    jobs = defaultdict(dict)
    with ZipFile(jobResults, 'r') as z:
        for x in z.namelist():
            if 'stub' in x:
                jobId = x.split('/')[0]
                stubId = os.path.splitext(x.split('/')[-1])[0]
                solution = z.read(x)
                if solution:
                    jobs[jobId]['sol'] = solution
                    jobs[jobId]['stub'] = stubNames[stubId]
                continue
            if not 'stderr' in x:
                continue
            err = z.read(x)
            hasResult = False
            incumbents = [float(l.split()[-1]) for l in err.split('\n')
                          if 'sendIncumbent' in l]
            result = [l for l in err.split('\n') if 'sendResult' in l]
            solHeader = [l for l in err.split('\n') if 'solutionHeader' in l]
            assert(len(result) <= 1)
            hasResult = len(result) == 1
            jobId = x.split('/')[0]
            if hasResult:
                result = result[0].split()
                status = result[-1]
                if solHeader:
                    hdr = solHeader[0].split(':')[1].strip()
                    hdr = hdr.split(',')[0].split()
                    if 'CBC' in solHeader[0]:
                        status = ' '.join(hdr[2:])
                    else:
                        status = ' '.join(hdr)
                jobs[jobId]['val'] = float(result[-2].rstrip(','))
                jobs[jobId]['status'] = status
            elif len(incumbents) >= 1:
                jobs[jobId]['val'] = min(incumbents)
                jobs[jobId]['status'] = 'failed'

    if not jobs:
        print 'No incumbents or solutions in job results'
        return

    solutions = defaultdict(list)
    for k, v in jobs.iteritems():
        if 'sol' in v:
            solutions[v['stub']].append(v)

    infos = []
    with ZipFile(args.out_prefix + '-solutions.zip', 'w') as z:
        for stubId, stubName in stubNames.iteritems():
            info = {'stub' : stubName, 'has_solution' : False,
                    'taskNum' : int(stubId.strip('stub'))}
            if not stubName in solutions:
                infos.append(info)
                continue
            best = min(solutions[stubName], key=lambda v: v['val'])
            info['incumbent'] = best['val']
            info['status'] = best['status']
            info['has_solution'] = True
            infos.append(info)
            outName = stubName.replace('.nl', '.sol')
            print 'Saving solution %s (%s) for task %d with incumbent %f' % (
                outName, best['status'], info['taskNum'], best['val'])
            z.writestr(outName, best['sol'])

    withSol = [i for i in infos if i['has_solution']]
    if not withSol:
        print 'No solutions in job results'
        return
    best = min(withSol, key=lambda v: v['incumbent'])
    print 'Best incumbent %f found for %s' % (best['incumbent'], best['stub'])

    if args.save_status:
        with open(args.out_prefix + '-status.txt', 'w') as f:
            f.write('%g\n%s\n' % (best['incumbent'], best['status']))
        with open(args.out_prefix + '-solutions.json', 'w') as f:
            json.dump(infos, f, indent=4)

if __name__ == "__main__":
    main0()
