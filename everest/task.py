import sys
import os
import socket
import struct
import random
import threading
import time

import port_proxy

def recvExact(sock, l):
    bufs = []
    while l > 0:
        try:
            buf = sock.recv(l)
        except socket.timeout:
            if not bufs:
                raise
        if not buf:
            break
        l -= len(buf)
        bufs.append(buf)
    return ''.join(bufs)

class Task:

    def __init__(self):
        port = int(os.environ['EVEREST_AGENT_PORT'])
        task_id = os.environ['EVEREST_AGENT_TASK_ID']

        # connect to agent
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect(('localhost', port))
        print 'Connected to agent, setting message mode'
        self.sock.sendall(struct.pack('>Ib', len(task_id) + 1, 0))
        self.sock.sendall(task_id)
        self.sock.settimeout(0.5)
        self.running = True

    def run(self):
        solver = sys.argv[1]
        stub = sys.argv[2]
        args = [solver, stub, '-p']
        # time.sleep(random.uniform(1, 10))

        # get current record
        msg = "VAR_GET record"
        self.send_message(msg)
        resp = self.receive_message()
        cur_record = resp.split()[2]
        if cur_record != "NULL":
            args.append('-b')
            args.append(cur_record)

        self.solver = port_proxy.startSolver(args)

        # receive record updates in a separate thread
        receiver = threading.Thread(target=self.receive_records)
        receiver.start()

        while self.running:
            solverMsg = port_proxy.readFromSolver(self.solver)
            if solverMsg[0] in ['incumbent', 'result']:
                print "Found new record: %f" % solverMsg[1]
                msg = "VAR_SET_MD record %f" % solverMsg[1]
                self.send_message(msg)
            elif solverMsg[0] == 'closed':
                self.running = False
                self.sock.shutdown(socket.SHUT_WR)
                receiver.join()
                print 'Finished'

    def send_message(self, msg):
        self.sock.sendall(struct.pack('>I', len(msg)))
        self.sock.sendall(msg)

    def receive_message(self):
        header = recvExact(self.sock, 4)
        if not header:
            return ''
        size, = struct.unpack('>I', header)
        msg = recvExact(self.sock, size)
        print "Received message: %s" % msg
        return msg

    def receive_records(self):
        while self.running:
            try:
                msg = self.receive_message()
                if not msg:
                    self.running = False
                    break
                if msg.startswith('VAR_VALUE record'):
                    record = float(msg.split()[2])
                    port_proxy.sendIncumbent(self.solver, record)
                    print "Updated record: %f" % record
            except socket.timeout:
                pass
            except:
                self.running = False
                raise

    def shutdown(self):
        self.running = False
        self.sock.close()

def main():
    task = Task()
    try:
        task.run()
    finally:
        task.shutdown()


if __name__ == "__main__":
    main()
