from OffloadingPredictor.BandwidthEstimator.bwEstimator import *
from OffloadingPredictor.MarketInformation.marketinformation import *

# ################################################################################################
# ################################################################################################

# =========    Definition of the Bandwidth Estimator Thread   ===========
class bwEstimatorThread(threading.Thread):
    def __init__(self, name):
        threading.Thread.__init__(self)
        self.name = name
        self.bwsocket = 0

    def run(self):
        printX("On")

        self.bwsocket = startBandwidthEstimator()

        printX("Off")

    def exit(self):
        printX("Broke up1 " + self.name)

    def join(self):
        threading.Thread.join(self)
        return self.bwsocket


# =========    Definition of the Market Information Thread   ===========
class marketInformationThread(threading.Thread):
    def __init__(self, name, initInfo):
        threading.Thread.__init__(self)
        self.name = name
        self.initInfo = initInfo

    def run(self):
        printX("On")

        mainMarketInformation(self.initInfo[0], self.initInfo[1])
        printX("Off")

    def exit(self):
        printX("Broke up1 " + self.name)


# =========    Definition of the Initialization Market Information Thread   ===========
class init_M_I_Thread(threading.Thread):
    def __init__(self, name):
        threading.Thread.__init__(self)
        self.name = name
        self.initInfo = 0

    def run(self):
        printX("On")

        self.initInfo = initMarketInformation()
        printX("Off")

    def exit(self):
        printX("Broke up1 " + self.name)

    def join(self):
        threading.Thread.join(self)
        return self.initInfo


# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================


# ============= Starts a new Bandwidth Estimator Thread ===========
def startBW():
    # print " create Complexity Estimator Thread"

    threadBandwidthEstimator = bwEstimatorThread("BW_Estimator_Module_Thread")
    threadBandwidthEstimator.start()

    return threadBandwidthEstimator


# ============= Starts a new Bandwidth Estimator Thread ===========
def startMarketInfo(initInfo):
    # print " create Complexity Estimator Thread"

    threadMarketInfo = marketInformationThread("Market_Information_Module_Thread", initInfo)
    threadMarketInfo.start()

    return threadMarketInfo


# ============= Starts a new Bandwidth Estimator Thread ===========
def initMarketInfo():
    # print " create Complexity Estimator Thread"

    threadInitMarketInfo = init_M_I_Thread("Init_M_I_Thread")
    threadInitMarketInfo.start()

    return threadInitMarketInfo

# =====================================================================================================================
# =====================================================================================================================
# =====================================================================================================================
