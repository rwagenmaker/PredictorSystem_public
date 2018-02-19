# =====================================================================================================================
# =====================================================================================================================
# ====================================    OFFLOADING PREDICTOR COMPONENT   ============================================
#
#   > Starts by initiating the "Bandwidth Estimator Thread", which then starts the BWlistener.
#   > Then initiates the "Market Information Thread", which communicates with the Market and initializes
# all the system information.
#   ------> The components' initialization is done before any other task, so that when there is as analysis request the
# necessary information is readily available
#   > After starting the initialization threads, becomes listening for user requests. Every time the server receives
# a request, then starts a "Complexity Estimator" thread, where the request is handled.
#
# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================
import pickle
import socket

import sys

from OffloadingPredictor.threads import *
from Extra.printColors import *

RECV_BUFFER = 4096  # Advisable to keep it as an exponent of 2
PORT = 10000
PORT_DECIDER_LISTENER = 11112  # Reserve port for the communication between the market information and the decider
requestID = 0


# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================

def askVolunteerInformation():
    # return : dictionary with each volunteer paired with its correspondent info
    #   dict { string IPaddress : List info [... , ... , ...] }

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    host = socket.gethostname()  # Get local machine name
    server_address = (host, PORT_DECIDER_LISTENER)
    try:
        s.connect(server_address)
        data = s.recv(RECV_BUFFER)
    except:
        printX('Unable to connect with the market information listener ')
        return [False]

    if data:
        printMarketInfo('Connected! New VolunteersGroup Requested')

        newRequest = ["GETALLINFO"]
        s.sendall(pickle.dumps(newRequest))  # CONNECT WITH THE MARKET INFORMATION'S LISTENER

        L = s.recv(RECV_BUFFER)
        allInfo = pickle.loads(L)
    else:
        printX('Unable to connect with the market information listener ')
        # return [False]

    return allInfo


def deliverInformation(decision, userInfo, volunteersInfo, usersocket):
    if decision:
        printX("<< The execution should be sent to the volunteer with the ID: " + bcolors.BOLD + str("XPTO")
               + bcolors.RESET + " >>")
    else:
        printX("<< The execution should be done locally >>")

    usersocket.sendall(pickle.dumps([decision, userInfo, volunteersInfo]))


# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================


threadBandwidthEstimator = startBW()  # Starts the bw listener (thread definition is on threads.py)

# Initializes the MarketInformation module and all the information necessary for the decider to base upon
threadInit_MarketInformation = initMarketInfo()
initInfo = threadInit_MarketInformation.join()
# Start the market information module (thread definition is on threads.py)
threadMarketInformation = startMarketInfo(initInfo)

# Create a TCP/IP socket
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

# Bind the socket to the port
server_address = ('0.0.0.0', PORT)
print >> sys.stderr, 'starting up on %s port %s' % server_address
server_socket.bind(server_address)

# Listen for incoming connections
server_socket.listen(1)

try:
    print(bcolors.OKGREEN +
          '\n##########################################################################################################'
          '\n-------------------------------------- OFFLOADING PREDICTOR READY ----------------------------------------'
          '\n##########################################################################################################'
          + bcolors.OKGREEN)

    while True:
        c, address = server_socket.accept()  # Establish connection with client.
        print 'Got connection from', address, '\n'
        c.send('Connection Received\n')

        L = c.recv(RECV_BUFFER)
        messageToAnalyse = pickle.loads(L)

        # if messageToAnalyse[0] == 1:  # Then it is a new request that must be analysed
        #    functCommand = messageToAnalyse[2]
        #    args = messageToAnalyse[3]

        #            requestID += 1
        #    startEstimatorThread(functCommand, args, c)


        allInfo = askVolunteerInformation()
        userInfo = allInfo['userinfo']
        volunteersInfo = allInfo['volunteers']

        # printX(userInfo)
        # print(userInfo)

        deliverInformation(False, userInfo, volunteersInfo, c)
        print bcolors.RED + '\nRequest Handled. Waiting for next connection' + bcolors.RESET


except KeyboardInterrupt:
    print >> pickle.sys.stderr, 'Offloading Predictor Interrupted: "Keyboard Interrupt"'
except:
    server_socket.close()
    print "\nServer was killed\n"
    print "Unexpected error:", sys.exc_info()[0]

server_socket.close()
