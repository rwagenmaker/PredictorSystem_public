import ast
import pickle
import sys

import iperf3
#try:
client = iperf3.Client()
client.duration = 1
client.server_hostname = '0.0.0.0'

if sys.argv.__len__() > 1:
    #print sys.argv

    client.port = sys.argv[2]

#print sys.argv


result = client.run()

# client.bind_address = '10.0.0.1'



# print('Connecting to {0}:{1}'.format(client.server_hostname, client.port))

print pickle.dumps(result)



#except:
#    print "\nServer was killed\n"
#    print "Unexpected error:", sys.exc_info()[0]

    # print('')
    # print('Test completed:')
    # print('  started at         {0}'.format(result.time))
    # print('  bytes transmitted  {0}'.format(result.sent_bytes))
    # print('  retransmits        {0}'.format(result.retransmits))
    # print('  avg cpu load       {0}%\n'.format(result.local_cpu_total))

    # print('Average transmitted data in all sorts of networky formats:')
    # print('  bits per second      (bps)   {0}'.format(result.sent_bps))
    # print('  Kilobits per second  (kbps)  {0}'.format(result.sent_kbps))
    # print('  Megabits per second  (Mbps)  {0}'.format(result.sent_Mbps))
    # print('  KiloBytes per second (kB/s)  {0}'.format(result.sent_kB_s))
    # print('  MegaBytes per second (MB/s)  {0}'.format(result.sent_MB_s))
