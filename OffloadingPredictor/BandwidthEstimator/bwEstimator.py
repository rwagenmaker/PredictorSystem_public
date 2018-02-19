# =====================================================================================================================
# =====================================================================================================================
# ====================================    BANDWIDTH ESTIMATOR COMPONENT   ============================================
#
#   >
#   ------> The iperf listener had to be started in a different process, because otherwise it would show a segmentation
#  fault. For that reason the script is started through the command line, using the "subprocess" module.
#
# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================

import os
import pickle
import re
import socket
import subprocess
import threading
import time
import sqlite3 as lite
from multiprocessing import Queue
from OffloadingPredictor.Extra.printColors import *

PORT = 10000
RECV_BUFFER = 4096  # Advisable to keep it as an exponent of 2
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
host = socket.gethostname()

queue = Queue()
bwServerQueue = Queue()

dir_path = os.path.dirname(os.path.realpath(__file__))


# =======================================================================
# =========  Definition of the calculateAvailableBW() module  ===========
#
#   >> Gets the last estimated value for the host "volunteerID"
# Input: - String volunteerID     : Identifier of the volunteer node
#
def getLastBW(volunteerID):
    queryVolunteerID = "%" + volunteerID + "%"

    con = lite.connect('bwinformation.db')

    cur = con.cursor()
    cur.execute(
        'CREATE TABLE IF NOT EXISTS '
        'BWEstimates(VolunteerID TEXT, '
        'BandwidthEstimate REAL, '
        'SamplesNumber INTEGER)')
    cur.execute("SELECT * FROM BWEstimates WHERE VolunteerID like '" + volunteerID + "'")

    col_names = [cn[0] for cn in cur.description]

    row = cur.fetchone()

    # print(
    #    "%-10s %-8s %s" % (
    #        col_names[0], col_names[1], col_names[2]))

    # for row in rows:
    #    print("%-10s %-15s %-25s" % row)

    # print volunteerID + ":" + str(row) + " /// " + str(row[1])
    return row


# =======================================================================
# ========  Definition of the saveBWEstimate() module thread  ===========
#
#   >> Saves all the available information about the whole estimate process and about the execution itself.
#   Input: - String volunteerID     : Name of the function that is being analysed
#          - Float bwestimate       : The quick estimate value of how much the execution is going to take
#          - int samplesNumber       : the number of samples that were already used to reach an estimate value
#
def saveBWEstimate(volunteerID, bwestimate, newSamplesNumber):

    con = lite.connect('bwinformation.db')  # Name of the DB file where the information is stored

    with con:
        cur = con.cursor()
        cur.execute(
            'CREATE TABLE IF NOT EXISTS '
            'BWEstimates(VolunteerID TEXT, '
            'BandwidthEstimate REAL, '
            'SamplesNumber INTEGER)')

        cur.execute("SELECT count(*) FROM BWEstimates WHERE VolunteerID = ?", (volunteerID,))
        data = cur.fetchone()[0]

        if data == 0:  # if there is no volunteer named volunteerID)
            cur.execute('INSERT INTO '
                        'BWEstimates VALUES (?,?,?)', (volunteerID, bwestimate, 1))

        else:  # else volunteer volunteerID is present in the database
            cur.execute('UPDATE BWEstimates '
                        'SET BandwidthEstimate = ?, SamplesNumber = ? '
                        'WHERE VolunteerID = ?', (bwestimate, newSamplesNumber, volunteerID))

        con.commit()


def calculateNewBandwidth(oldBandwidth, samplesNumber, newProbeBandwidth):
    return newProbeBandwidth


# =======================================================================
# =============  Definition of the Bandwidth Scout thread  ==============
#
#   >> Starts the bwestimatorScout script in a new subprocess, which represents an Iperf client, which communicates with
#  the Iperf server listening at the "volunteerHostId", in the port "port". After this subprocess concludes, calculates
#  the latency with this specific volunteer.
#   Input: - String volunteerHostId  : Identifier of the volunteer (most likely to be the host IP address)
#          - String port             : The port where the volunteer node (Iperf server) is listening
#
class bandwidthScoutThread(threading.Thread):
    def __init__(self, name, volunteerHostId, port):  # , resultsDict):
        threading.Thread.__init__(self)
        self.name = volunteerHostId
        self.result = 0
        self.volunteerHostId = volunteerHostId
        self.port = port

    def run(self):

        bwinfo = getLastBW(self.volunteerHostId)

        # ----------------------------------------------
        args = [self.volunteerHostId, self.port]
        returnValue = subprocess.check_output(
            ['python', os.path.expanduser(dir_path + '/bwestimatorScout.py')] + args)

        reconstructedReturnValue = pickle.loads(returnValue)

        # this "0.0.0.0" has to be changed to a variable passed when starting the thread
        averageLatency = calculateLatency("0.0.0.0")
        # ----------------------------------------------

        if not reconstructedReturnValue.error:

            if bwinfo is not None:  # if there was already a previous value for the BW of this volunteer
                newAvailableBandwidth = calculateNewBandwidth(bwinfo[1], bwinfo[2],
                                                              reconstructedReturnValue.received_MB_s)
                self.result = [newAvailableBandwidth, averageLatency]
                saveBWEstimate(self.volunteerHostId, newAvailableBandwidth, bwinfo[2] + 1)
            else:
                newAvailableBandwidth = reconstructedReturnValue.received_MB_s
                self.result = [reconstructedReturnValue.received_MB_s, averageLatency]
                saveBWEstimate(self.volunteerHostId, newAvailableBandwidth, 1)

        else:
            self.result = [str(reconstructedReturnValue.error), averageLatency]

    def exit(self):
        printX("Broke up1 " + self.name)

    def join(self):
        threading.Thread.join(self)
        return self.result


# =======================================================================
# ===========  Definition of the calculateLatency() module  =============
#
#   >> Launches a ping request from the command line to the IP address of the host ("hostIP"). Starts a new subprocess
# so it may be called through the cmd
#   Input: - String hostId  : The IP address of the volunteer host
#
def calculateLatency(hostIP):
    command = 'ping -q -c 3 ' + hostIP
    cmd = subprocess.Popen(command.split(' '), stdout=subprocess.PIPE)
    output = cmd.communicate()[0]
    match = re.search('(\d+\.\d+)\/(\d+\.\d+)\/(\d+\.\d+)\/(\d+\.\d+)\s+ms', output)

    if not (match is None):
        return "%0.3f" % float(match.group(1))
    else:
        return "Failure"


# =======================================================================
# ============  Definition of the startBWlistener() module  =============
#
#   >> Starts a new process where it has an iperf server listening for incoming bandwidth calculation requests
#   Input: None
#
def startBWlistener():
    p = subprocess.Popen(['python', os.path.expanduser(dir_path + '/bwestimatorListener.py')])
    bwestimatorServerPID = p.pid

    printBWlistener("Listener Started")


# =======================================================================
# =========  Definition of the startbandwidthScout() module  ============
#
#   >> Starts the scouts for every volunteer received from the market information. Each scout measures the BW available
# and the latency; Each scout is a different process running an iperf client
#   Input: - Dictionary remotevolunteers  : key   --> volunteer node identifier (most likely the IP address)
#                                           value --> list with all the info about the respective volunteer
#
def startbandwidthScout(remotevolunteers):
    try:
        printAvailableBW("Bandwidth Scout Initiated")

        resultsDict = {}


        threadpool = []
        for volunteerIP in remotevolunteers.keys():
            volunteerInfo = remotevolunteers[volunteerIP]
            threadScout = bandwidthScoutThread("volunteer_" + volunteerIP,
                                               volunteerIP,
                                               str(volunteerInfo['port']))
            threadScout.start()
            threadpool.append(threadScout)

        for thread in threadpool:
            threadResult = thread.join()
            resultsDict[thread.getName()] = threadResult

        return resultsDict

    except KeyboardInterrupt:
        print >> pickle.sys.stderr, 'Bandwidth Scout Interrupted: "Keyboard Interrupt"'


# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================

def startBandwidthEstimator():
    # print('module name:', __name__)
    # print('parent process:', os.getppid())
    # print('process id:', os.getpid())

    startBWlistener()
