#!/usr/bin/env python2.7
import os
import sys
import argparse
from zipfile import ZipFile, ZIP_DEFLATED
import shutil
import tempfile
from collections import defaultdict, OrderedDict
import json
import calendar
import time
import string

import requests
import everest

def makeParser():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-d', '--debug', action='store_true')
    parser.add_argument('-t', '--token', default='~/.everest_token',
                        help='token obtainable with everest.py')
    parser.add_argument('-r', '--resources', nargs='+', default=[],
                        help='everest resources to be used')
    parser.add_argument('-s', '--solver', default='scip', choices=['scip', 'cbc', 'scip_bundle'],
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

    inputFiles = ['run-task.sh', 'task.py', 'port_proxy.py']
    if args.solver == 'scip_bundle':
        inputFiles.append('scip_port')
        solver = './scip_port'
    else:
        solver = '%s_port' % args.solver

    stubNames = OrderedDict()
    with ZipFile(makeName('.zip'), 'w', ZIP_DEFLATED) as z:
        for f in inputFiles:
            z.write(os.path.join(d, f), f)
        for i, stub in enumerate(stubs):
            stubNames['stub%d' % i] = os.path.basename(stub)
            z.write(stub, 'stub%d.nl' % i)
        for i, params in enumerate(paramsFiles):
            z.write(params, 'params%d.txt' % i)

    with open(makeName('.plan'), 'wb') as f:
        f.write('parameter n from 0 to %d step 1\n' % (len(stubs) - 1))
        f.write('parameter p from 0 to %d step 1\n' % (len(paramsFiles) - 1))
        f.write('input_files stub${n}.nl params${p}.txt %s\n' % ' '.join(inputFiles))
        f.write('command bash run-task.sh %s stub${n}.nl %d params${p}.txt %g\n' % (
            solver, args.stop_mode, args.initial_incumbent))
        f.write('output_files stub${n}.sol stderr stdout.tgz\n')

    if not args.use_results is None:
        tasksRes = saveResults(args.use_results, stubNames, args)
        if args.get_log:
            parseJobLog(args.out_prefix + '.log', tasksRes, args)
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
        tasksRes = saveResults(makeName('-results.zip'), stubNames, args)
        if args.get_log:
            print "Downloading job's log..."
            session.getJobLog(job.id, args.out_prefix + '.log')
            parseJobLog(args.out_prefix + '.log', tasksRes, args)
    finally:
        session.close()

def parseJobLog(logFile, tasksRes, args):
    tasksRaw = sorted(parseFile(logFile)['tasks'].items())
    taskTimes = defaultdict(list)
    for taskId, task in tasksRaw:
        jobId, taskNum, restartNum = tuple(taskId.split('-'))
        taskTimes[taskNum].append({'start_time' : task['start'],
                                   'stop_time' : task.get('end'),
                                   'resource_id' : task.get('resourceId'),
                                   'has_solution' : False,
                                   'status' : 'failed',
                                   'task_id' : taskId})
    tasks = OrderedDict()
    for taskNum, task in sorted(tasksRes.items()):
        for i, t in enumerate(taskTimes[taskNum]):
            if 'stub' in task:
                t['stub'] = task['stub']
            tasks[t['task_id']] = t
            del t['task_id']
        taskTimes[taskNum][-1].update(task)
    if args.save_status:
        with open(args.out_prefix + '-tasks.json', 'w') as f:
            json.dump(tasks, f, indent=4)

def tsStrToSeconds(s):
    return calendar.timegm(time.strptime(s[:s.find('.')], '%Y-%m-%d %H:%M:%S'))

def parseFile(fileName):
    result = {'tasks' : {}}
    firstLine = True
    with open(fileName, 'r') as f:
        for l in f.readlines():
            timestamp = tsStrToSeconds(' '.join(l.split()[:2]))
            if firstLine:
                firstLine = False
                result['jobSubmitTS'] = timestamp

            if not 'Received' in l or not 'TASK_STATE' in l:
                continue
            start = string.find(l, '["T')
            msg = json.loads(l[start:])
            status = msg[2]

            if status == 'RUNNING':
                result['taskStartTS'] = timestamp
                result['tasks'][msg[1]] = {'start' : timestamp}
            elif status == 'COMPLETED':
                result['taskCompletedTS'] = timestamp
                result['tasks'][msg[1]]['end'] = timestamp

            if not status in ['DONE', 'FAILED']:
                continue

            result['tasks'][msg[1]]['status'] = status
            result['tasks'][msg[1]].update(msg[3])
            result.update(msg[3])
            result['status'] = status
            result['networkTime'] = result['stageInTime'] + result['stageOutTime']
            for s in map(lambda s: s.strip(), l.split()):
                if s[0] == '[' and '/' in s:
                    result['resourceId'] = s.strip('[]').split('/')[0]
                    result['tasks'][msg[1]]['resourceId'] = s.strip('[]').split('/')[0]
                    break

    result['jobDoneTS'] = timestamp
    result['jobSeconds'] = result['jobDoneTS'] - result['jobSubmitTS']
    return result

def saveResults(jobResults, stubNames, args):
    OTHER_FIELDS = ['hostname', 'solver_exitcode']
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
            jobId = x.split('/')[0]
            err = z.read(x).split('\n')
            hasResult = False
            incumbents = [float(l.split()[-1]) for l in err
                          if 'sendIncumbent' in l]
            result = [l for l in err if 'sendResult' in l]
            solHeader = [l for l in err if 'solutionHeader' in l]
            for otherField in OTHER_FIELDS:
                lines = [l for l in err if otherField in l]
                if not lines:
                    continue
                value = lines[0].split()[-1]
                try:
                    value = int(value)
                except ValueError:
                    pass
                jobs[jobId][otherField] = value
            assert(len(result) <= 1)
            hasResult = len(result) == 1
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
            v['has_solution'] = True
        else:
            v['has_solution'] = False

    infos = []
    with ZipFile(args.out_prefix + '-solutions.zip', 'w') as z:
        for stubId, stubName in stubNames.iteritems():
            info = {
                'stub' : stubName,
                'has_solution' : False,
                'taskNum' : int(stubId.strip('stub')),
                'solver' : args.solver
            }
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

    incumbentNoSol = min([v['val'] for v in jobs.values() if 'val' in v])
    if args.save_status:
        with open(args.out_prefix + '-incumbent-no-sol.txt', 'w') as f:
            f.write('%g' % incumbentNoSol)

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
        for k, v in jobs.iteritems():
            if 'sol' in v:
                del v['sol']
        with open(args.out_prefix + '-tasks.json', 'w') as f:
            json.dump(jobs, f, indent=4)
    return jobs

if __name__ == "__main__":
    main0()
