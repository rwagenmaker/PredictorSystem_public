import pickle
import sys
from OffloadingPredictor.Extra.printColors import *
import iperf3

# print('module name:', __name__)
# print('parent process:', os.getppid())
# print('process id:', os.getpid())

try:

    server = iperf3.Server()
    server.bind_address = '0.0.0.0'

    #if sys.argv.__len__() > 1:
    #    server.port = sys.argv[2]
    #    print sys.argv[1]

    server.port = 70000
    # server.verbose = False


    printBWlistener('Running server: {0}:{1}'.format(server.bind_address, server.port))

    while True:
        result = server.run()

        if result.error:
            printBWlistener(str(result.error))
        else:
            printBWlistener("...")
            # print('')
            # print('Test results from {0}:{1}'.format(result.remote_host,
            #                                         result.remote_port))
            # print('  started at         {0}'.format(result.time))
            # print('  bytes received     {0}'.format(result.received_bytes))

            # print('Average transmitted received in all sorts of networky formats:')
            # print('  bits per second      (bps)   {0}'.format(result.received_bps))
            # print('  Kilobits per second  (kbps)  {0}'.format(result.received_kbps))
            # print('  Megabits per second  (Mbps)  {0}'.format(result.received_Mbps))
            # print('  KiloBytes per second (kB/s)  {0}'.format(result.received_kB_s))
            # print('  MegaBytes per second (MB/s)  {0}'.format(result.received_MB_s))
            # print('')

except KeyboardInterrupt:
    print >> pickle.sys.stderr, 'Bandwidth Estimator Server Interrupted: "Keyboard Interrupt"'
except:

    print "\nServer was killed\n"
    print "Unexpected error:", sys.exc_info()[0]

# client = iperf3.Client()
# client.duration = 1
# client.server_hostname = '127.0.0.1'
# client.port = 5201
# client.run()


# ==================================================================
# ==================================================================
