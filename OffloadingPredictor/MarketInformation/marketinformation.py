# =====================================================================================================================
# =====================================================================================================================
# ======================================    MARKET INFORMATION COMPONENT   ============================================
#
#   ------> The Bandwidth Scout is inserted in the volunteer update cycle. This way there is only one copy of the
# current volunteers in memory (in the market information component). It also allows for the information to be
# centralized when the "decider" component requests this information
#   ------> The update is force asked by this component and not by the external entity. It is easier to control when we
# ask an information update than to control when the market considers that there are relevant changes in the volunteers
#   >
#
# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================

from OffloadingPredictor.threads import *
from OffloadingPredictor.BandwidthEstimator.bwEstimator import *
import threading, Queue
import socket
from OffloadingPredictor.Extra.printColors import *
import pickle

RECV_BUFFER = 4096  # Advisable to keep it as an exponent of 2
PORTMARKET = 11111  # Reserved port for the communication between the market and the Market Information
PORT_DECIDER_LISTENER = 11112  # Reserve port for the communication between the market information and the decider


# {'userinfo' : {'credits': 10, 'anyinfo': stuffs} ,
# 'volunteerinfo' : { dummyIP1 :{'port': 10001, 'availableBW': 1978.71446609 , 'latency': 0.012},
#                     dummyIP2 :{'port': 10002, 'availableBW': 1910.31098366 , 'latency': 0.013}  }
# global infoGlobal
# infoGlobal = {'userinfo': {}, 'volunteers': {}}


# =====================================================================================================================
# =========    Definition of the Market Information Listener Thread   ===========
class m_i_ListenerThread(threading.Thread):
    def __init__(self, name):
        threading.Thread.__init__(self)
        self.name = name

    def run(self):
        printX("On")

        # Create a TCP/IP socket
        listener_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        listener_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

        # Bind the socket to the port
        server_address = ('0.0.0.0', PORT_DECIDER_LISTENER)
        listener_socket.bind(server_address)

        # Listen for incoming connections
        listener_socket.listen(1)

        try:
            print bcolors.RED + '\nWaiting for requests from the decider' + bcolors.RESET

            while True:
                c, address = listener_socket.accept()  # Establish connection with client.
                c.send('Connection Received from decider\n')

                L = c.recv(RECV_BUFFER)
                messageToAnalyse = pickle.loads(L)

                if messageToAnalyse[0] == "GETALLINFO":  # Then it is a new request that must be analysed
                    global infoGlobal
                    # printMarketInfo("infoGlobal: " + str(infoGlobal))
                    allInfoToSend = pickle.dumps(infoGlobal)

                    c.sendall(allInfoToSend)


        except KeyboardInterrupt:
            print >> pickle.sys.stderr, 'Market Information`s Listener Interrupted: "Keyboard Interrupt"'
        except:
            listener_socket.close()
            print "\nServer was killed\n"
            print "Unexpected error:", sys.exc_info()[0]

        listener_socket.close()

        printX("Off")

    def exit(self):
        printX("Broke up1 " + self.name)


def startM_I_Listener():
    threadMI_Listener = m_i_ListenerThread("MI_Listener_Thread")
    threadMI_Listener.start()

    return threadMI_Listener


# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================

# Handles the communication part of the request operation.
def marketRequest(requestType):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    host = socket.gethostname()  # Get local machine name
    server_address = (host, PORTMARKET)
    try:
        s.connect(server_address)
        data = s.recv(RECV_BUFFER)
    except:
        printMarketInfo('Unable to connect with the market ')
        return [False]

    if data:

        if requestType is "GETVOLUNTEERS":
            returnInfo = getVolunteers(s)
            return returnInfo
        elif requestType is "GETUSER":
            returnInfo = getMyUser(s)
            return returnInfo
        elif requestType is "INIT":
            returnInfo = getInitInfo(s)
            return returnInfo
    else:
        return [False]


# =======================================================================
# ============  Definition of the getVolunteers() module  ==============
#
#   >> Makes the communication with the Market (external component), and requests for the most updated version of the
# information on the volunteers. Communicates through sockets.
#   Input: - String requestType  : String indicating whether the function is being called as a initialization
#                               procedure, or whether is being called as an update procedure
#
def getVolunteers(marketSocket):
    printMarketInfo('Connected! New VolunteersGroup Requested')

    newRequest = ["GETVOLUNTEERS"]
    marketSocket.sendall(pickle.dumps(newRequest))  # CONNECT WITH THE DUMMY MARKET

    L = marketSocket.recv(RECV_BUFFER)
    volunteers = pickle.loads(L)

    return [True, volunteers]


# =======================================================================
# ============  Definition of the getUserInfo module  ==============
#
#   >>
#   Input: -
#
def getMyUser(marketSocket):
    printMarketInfo('Connected! User Information Requested')

    newRequest = ["GETUSER", "myuserID"]
    marketSocket.sendall(pickle.dumps(newRequest))  # CONNECT WITH THE DUMMY MARKET

    #  decision = offloadPredictor(functCommand, args)
    L = marketSocket.recv(RECV_BUFFER)
    myuser = pickle.loads(L)

    return [True, myuser]


def getInitInfo(marketSocket):
    printMarketInfo('Connected! Initiation Request Sent')

    newRequest = ["INIT", "myuserID"]
    marketSocket.sendall(pickle.dumps(newRequest))  # CONNECT WITH THE DUMMY MARKET

    #  decision = offloadPredictor(functCommand, args)
    L = marketSocket.recv(RECV_BUFFER)
    initInfo = pickle.loads(L)

    return [True, initInfo[0], initInfo[1]]


def updateVolunteers(volunteers):
    newVolunteers = marketRequest("GETVOLUNTEERS")
    if newVolunteers[0]:
        # print newVolunteers[1]
        return newVolunteers[1]
    else:
        return volunteers


def initMarketInfo():
    initInformation = marketRequest("INIT")

    while not initInformation[0]:  # while there are no new volunteers
        printMarketInfo("WAITING FOR THE INITIALIZATION PROCESS TO SUCCEED")
        time.sleep(10)  # waits 5 seconds until trying to connect again with the market
        initInformation = marketRequest("INIT")

    volunteers = initInformation[1]
    userInfo = initInformation[2]
    printMarketInfo("My user information: " + str(userInfo))

    #  initiates the first process of probing the volunteers to gather the bandwidth information.
    bwinformation = startbandwidthScout(volunteers)
    printBWinfo(bwinformation)

    return [volunteers, userInfo, bwinformation]


# =======================================================================
# =============  Definition of the printBWinfo() module  ================
#
#   >> Prints the information received
#   Input: - Dictionary bwinformation  : key   --> volunteer node identifier (most likely the IP address)
#                                        value --> List[ Available Bandwidth ; Latency ]
#
def printBWinfo(bwinformation):
    for volunteer in bwinformation.keys():
        networkinfo = bwinformation[volunteer]
        showinfo = volunteer + ":   BW: {0}(MB/s) ; Average Latency: {1}".format(
            networkinfo[0], networkinfo[1])

        printAvailableBW(showinfo)


# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================

def initMarketInformation():
    # startM_I_Listener()

    initInfo = initMarketInfo()
    volunteers = initInfo[0]
    userInfo = initInfo[1]
    bwinformation = initInfo[2]

    global infoGlobal
    infoGlobal = {'userinfo': userInfo, 'volunteers': volunteers}

    allvolunteers = infoGlobal['volunteers']

    for volunteerID in allvolunteers.keys():
        volunteerInfo = allvolunteers[volunteerID]
        volunteerInfo['availableBW'] = bwinformation[volunteerID][0]
        volunteerInfo['latency'] = bwinformation[volunteerID][1]

        infoGlobal['volunteers'][volunteerID] = volunteerInfo

    return [volunteers, infoGlobal]


def mainMarketInformation(volunteers, totalInfo):
    startM_I_Listener()

    global infoGlobal
    infoGlobal = totalInfo

    # UPDATE OPERATION
    while True:
        print ""
        time.sleep(10)

        volunteers = updateVolunteers(volunteers)

        bwinformation = startbandwidthScout(volunteers)
        # printMarketInfo(str(bwinformation))
        allvolunteers = infoGlobal['volunteers']

        for volunteerID in allvolunteers.keys():
            volunteerInfo = allvolunteers[volunteerID]
            volunteerInfo['availableBW'] = bwinformation[volunteerID][0]
            volunteerInfo['latency'] = bwinformation[volunteerID][1]

            infoGlobal['volunteers'][volunteerID] = volunteerInfo

        printBWinfo(bwinformation)
