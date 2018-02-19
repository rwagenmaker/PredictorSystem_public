# =======================================================================
# ===================     Dummy Volunteers   ============================
# =======================================================================
import os
import subprocess

import pickle

import time

dir_path = os.path.dirname(os.path.realpath(__file__))
dummyvolunteers = {'dummy1': 10005, 'dummy2': 10006, 'dummy3': 10007}

for volunteer in dummyvolunteers.keys():
    # print dummyvolunteers[volunteer]
    args = [volunteer, str(dummyvolunteers[volunteer])]
    p = subprocess.Popen(['python', os.path.expanduser(dir_path + '/bwestimatorDummyVolunteers.py')] + args)
    # result = subprocess.check_output(['python', os.path.expanduser(dir_path + '/bwestimatorDummyVolunteers.py')] + args)
    # print result
    print(volunteer, ": ", p.pid)
    # reconstructedReturnValue = pickle.loads(returnValue)

try:
    while(True):
        time.sleep(60)

except KeyboardInterrupt:
    print >> pickle.sys.stderr, 'Dummy Volunteers Interrupted: "Keyboard Interrupt"'
