import os
import socket
import subprocess
import struct

PORT = 9997

def main():
    os.environ['EVEREST_AGENT_PORT'] = str(PORT)
    os.environ['EVEREST_AGENT_TASK_ID'] = '000'

    # connect to agent
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(('localhost', PORT))
    sock.listen(1)

    p = subprocess.Popen(['python', 'task.py', 'cbc_port', '../test/BalanceTestDyn.nl'],
                         preexec_fn=os.setsid,
                         stdin=subprocess.PIPE)

    conn, addr = sock.accept()
    print 'Accepted, sending'

    msg = 'VAR_VALUE record NULL'
    conn.sendall(struct.pack('>I', len(msg)))
    conn.sendall(msg)

    print 'Wait result:', os.wait()

if __name__ == "__main__":
    main()
