import threading

__author__ = 'rwagenmaker'


class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    CYAN = '\033[96m'
    BLACK = '\033[90m'
    MAGENTA = '\033[95m'
    OKGREEN = '\033[92m'
    GREY = '\033[90m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    RESET = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def printX(toprint):

    threadname = threading.Thread.getName(threading.current_thread())

    if "Complexity_Estimator_Thread" in threadname:
        print(bcolors.YELLOW + bcolors.BOLD + "\n\t" + threadname + "> " + bcolors.RESET + toprint)
    elif "Quick_Analysis_Thread" in threadname or "Deep_Analysis_Thread" in threadname:
        print(bcolors.YELLOW + bcolors.BOLD + "\n\t\t\t" + threadname + "> " + bcolors.RESET + toprint)
    else:
        print(bcolors.YELLOW + bcolors.BOLD + "\n\t\t\t\t\t" + threadname + "> " + bcolors.RESET + toprint)


def printDB(toprint):
    #if isDBaccess:
    print(bcolors.OKGREEN + bcolors.BOLD + "DB ACCESS> " + bcolors.RESET + bcolors.OKGREEN + toprint + bcolors.RESET)

    #else:

def printBWlistener(toprint):
    print(bcolors.GREY  + bcolors.BOLD + "BWlistener> " + bcolors.RESET + bcolors.GREY + toprint + bcolors.RESET)

def printAvailableBW(toprint):
    print(bcolors.OKBLUE + bcolors.BOLD + "BWscout> " + bcolors.RESET + bcolors.OKBLUE + toprint + bcolors.RESET)

def printMarketInfo(toprint):
    print(bcolors.MAGENTA + bcolors.BOLD + "MarketInfo> " + bcolors.RESET + bcolors.MAGENTA + toprint + bcolors.RESET)
