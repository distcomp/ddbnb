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

def sendIncumbent((_, fd, cpid), value):
    msg = struct.pack('>HBd', 9, 1, value)
    wr = os.write(fd, msg)
    #assert(wr == len(msg))

def stopSolver((_, fd, cpid)):
    os.kill(cpid, signal.SIGINT)

def startSolver(args):
    solver2proxyRead, solver2proxyWrite = os.pipe()
    proxy2solverRead, proxy2solverWrite = os.pipe()
    cpid = os.fork()
    if not cpid:
        os.close(solver2proxyRead)
        os.close(proxy2solverWrite)
        os.dup2(solver2proxyWrite, 4)
        os.dup2(proxy2solverRead, 3)
        print 'Starting solver', args[0], args
        os.execvp(args[0], args)
    os.close(solver2proxyWrite)
    os.close(proxy2solverRead)
    return (solver2proxyRead, proxy2solverWrite, cpid)

def readFromSolver((solver2proxyRead, _, cpid)):
    buf = readExact(solver2proxyRead, 3)
    if not buf:
        return 'closed',
    bodyLen, msgType = struct.unpack('>HB', buf)
    buf = readExact(solver2proxyRead, bodyLen-1)
    if msgType == 3:
        incumbent, = struct.unpack('>d', buf)
        return 'incumbent', incumbent
    elif msgType == 2:
        statusLen = bodyLen - 9
        incumbent, status = struct.unpack('>d%ds' % statusLen, buf)
        return 'result', incumbent, status

def main():
    solver = startSolver(['../c_src/cbc_port', '../test/BalanceTestDyn.nl',
                          '-p'])#, '-q', '-o', 'test.log'])

    sendIncumbent(solver, 4.0)

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
