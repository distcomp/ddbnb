import os
import time
import struct

def readExact(fd, l):
    bufs = []
    while l > 0:
        buf = os.read(fd, l)
        if not buf:
            break
        l -= len(buf)
        bufs.append(buf)
    return ''.join(bufs)

def sendIncumbent((_, fd), value):
    msg = struct.pack('>HBd', 9, 1, value)
    wr = os.write(fd, msg)
    #assert(wr == len(msg))

def startSolver(args):
    solver2proxyRead, solver2proxyWrite = os.pipe()
    proxy2solverRead, proxy2solverWrite = os.pipe()
    cpid = os.fork()
    if not cpid:
        os.close(solver2proxyRead)
        os.close(proxy2solverWrite)
        os.dup2(solver2proxyWrite, 4)
        os.dup2(proxy2solverRead, 3)
        os.execvp(args[0], args)
    os.close(solver2proxyWrite)
    os.close(proxy2solverRead)
    return (solver2proxyRead, proxy2solverWrite)

def readFromSolver((solver2proxyRead, _)):
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
        got = readFromSolver(solver)
        print got
        if got[0] == 'closed':
            break

if __name__ == "__main__":
    main()
