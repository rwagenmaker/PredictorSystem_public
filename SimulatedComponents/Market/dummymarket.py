import socket
import sys
import pickle
from OffloadingPredictor.Extra.printColors import bcolors

dummyvolunteers = {'testserver': {'port': 6969, 'score': 1, 'price': 2},
                   'dummy1': {'port': 10005, 'score': 1, 'price': 4},
                   'dummy2': {'port': 10006, 'score': 1, 'price': 6},
                   'dummy3': {'port': 10007, 'score': 1, 'price': 8}}

dummyusers = {'myuserID': {'credits': 10.0}}  # list with the info

userinfo = {'credits': 10.0}

PORTMARKET = 11111
RECV_BUFFER = 4096  # Advisable to keep it as an exponent of 2

# Create a TCP/IP socket
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

# Bind the socket to the port
server_address = ('0.0.0.0', PORTMARKET)
print >> sys.stderr, 'starting dummy market up on %s port %s' % server_address
server_socket.bind(server_address)

# Listen for incoming connections
server_socket.listen(1)

try:
    print bcolors.RED + '\nWaiting for requests' + bcolors.RESET

    while True:
        c, address = server_socket.accept()  # Establish connection with client.
        print 'Got connection from', address, '\n'
        c.send('Connection Received\n')

        L = c.recv(RECV_BUFFER)
        messageToAnalyse = pickle.loads(L)

        if messageToAnalyse[0] == "GETVOLUNTEERS":  # Then it is a new request that must be analysed
            volunteersToSend = pickle.dumps(dummyvolunteers)
            c.sendall(volunteersToSend)
        elif messageToAnalyse[0] == "GETUSER":
            idUserToSend = messageToAnalyse[1]
            userToSend = pickle.dumps(dummyusers[idUserToSend])
            c.sendall(userToSend)
        elif messageToAnalyse[0] == "INIT":
            idUserToSend = messageToAnalyse[1]
            initInfoToSend = pickle.dumps([dummyvolunteers, dummyusers[idUserToSend]])
            c.sendall(initInfoToSend)

        print bcolors.RED + '\nRequest Handled. Waiting for next connection' + bcolors.RESET

except KeyboardInterrupt:
    print >> pickle.sys.stderr, 'Offloading Predictor Interrupted: "Keyboard Interrupt"'
except:
    server_socket.close()
    print "\nServer was killed\n"
    print "Unexpected error:", sys.exc_info()[0]

server_socket.close()
