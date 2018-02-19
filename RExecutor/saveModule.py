import socket, select, pickle, fcntl, struct, string, sys

RECV_BUFFER = 4096  # Advisable to keep it as an exponent of 2
PORT = 10000  # Reserve a port for your service.

global args
global functCommand
global decision
global s
global execTime


try:

    newMessage = [2, execTime]
    s.sendall(pickle.dumps(newMessage))  # CONNECT WITH THE OFFLOADING PREDICTOR

    print "enviou tempo de execucao final"


    s.close
except:
    print 'Unable to connect'
    decision = False
    s.close()  # Close the socket when done
