import os
import time
import struct
import signal

def readExact(fd, l):
    bufs = []
    while l > 0:
        try:
            buf = os.read(fd, l)
        except KeyboardInterrupt:
            print 'port_proxy: KeyboardInterrupt received'
            continue
        if not buf:
            break
        l -= len(buf)
        bufs.append(buf)
    return ''.join(bufs)

def sendIncumbent((_, fd, cpid), value, seq=None):
    if seq is None:
        msg = struct.pack('>HBd', 9, 1, value)
    else:
        msg = struct.pack('>HBdH', 11, 5, value, seq)
    wr = os.write(fd, msg)
    #assert(wr == len(msg))

def stopSolver((_, fd, cpid)):
    os.kill(cpid, signal.SIGINT)

def startSolver(args, fork=True):
    if not fork:
        print 'Starting solver', args[0], args
        os.execvp(args[0], args)
        return
    solver2proxyRead, solver2proxyWrite = os.pipe()
    proxy2solverRead, proxy2solverWrite = os.pipe()
    old = signal.signal(signal.SIGINT, signal.SIG_IGN)
    cpid = os.fork()
    if not cpid:
        os.close(solver2proxyRead)
        os.close(proxy2solverWrite)
        os.dup2(solver2proxyWrite, 4)
        os.dup2(proxy2solverRead, 3)
        print 'Starting solver', args[0], args
        os.execvp(args[0], args)
    signal.signal(signal.SIGINT, old)
    os.close(solver2proxyWrite)
    os.close(proxy2solverRead)
    return (solver2proxyRead, proxy2solverWrite, cpid)

def readFromSolver((solver2proxyRead, _, cpid)):
    buf = readExact(solver2proxyRead, 3)
    if not buf:
        p, exitcode = os.waitpid(cpid, 0)
        return 'closed', exitcode
    bodyLen, msgType = struct.unpack('>HB', buf)
    buf = readExact(solver2proxyRead, bodyLen-1)
    if msgType == 4:
        incumbent, seqNumber = struct.unpack('>dH', buf)
        return 'incumbent-seq', incumbent, seqNumber
    elif msgType == 3:
        incumbent, = struct.unpack('>d', buf)
        if 0:
            with open('outsol-1.sol', 'w') as f:
                f.write(str(incumbent)*100)
            return 'incumbent-seq', incumbent, 1
        return 'incumbent', incumbent
    elif msgType == 2:
        statusLen = bodyLen - 9
        incumbent, status = struct.unpack('>d%ds' % statusLen, buf)
        return 'result', incumbent, status

def main():
    fork = os.environ.get("OMPI_COMM_WORLD_RANK", "0") == "0"
    solver = startSolver(['./bin/parascip', 'parascip.set', '../../BalanceTestDyn_000.cip', '-q', '-ddbnb'],
                         fork)
                          #'bin/cbc_port', '../test/BalanceTestDyn.nl',
                          #'-p'])#, '-q', '-o', 'test.log'])
    sendIncumbent(solver, 3.91, 0)

    while True:
        try:
            got = readFromSolver(solver)
        except KeyboardInterrupt:
            print 'Got SIGINT, shutting down gracefully'
            continue
        print got
        if got[0] == 'closed':
            break

if __name__ == "__main__":
    main()
