import socket, select, pickle, fcntl, struct, string, sys

RECV_BUFFER = 4096  # Advisable to keep it as an exponent of 2
PORT = 10000  # Reserve a port for your service.

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

host = socket.gethostname()  # Get local machine name


# global args
# global functCommand
global userInfo
global decision

# =========================================================================
# =========================================================================

try:
    server_address = (host, PORT)
    s.connect(server_address)

    data = s.recv(1024)
    if data:
        print 'Connected!'
        print "Message Received: " + data

        newRequest = [1, "New Request"]  # , functCommand, args]
        s.sendall(pickle.dumps(newRequest))  # CONNECT WITH THE OFFLOADING PREDICTOR

        L = s.recv(RECV_BUFFER)
        message = pickle.loads(L)
        userInfo = message[1]
        volunteersInfo = message[2]

        # for volunteer in volunteersInfo.keys():
        #    networkinfo = volunteersInfo[volunteer]
        #    #print(networkinfo)
        #    showinfo = volunteer + ":   BW: {0}(MB/s) ; Average Latency: {1}".format(networkinfo["availableBW"],
        #                                                                             networkinfo["latency"])
        #    print(showinfo)


        decision = False
        # print decision
    else:
        print 'Unable to connect. Executing Locally'
except:
    print 'Unable to connect'
    print "Unexpected error:", sys.exc_info()[0]
    # print sys.exc_info()
    decision = False
    s.close()  # Close the socket when done
