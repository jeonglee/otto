#! /usr/bin/env python2
import subprocess as sp
import shutil as sh
import os
import errno

rpath = os.path.realpath

# http://stackoverflow.com/questions/600268/mkdir-p-functionality-in-python
def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc:  # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise

def run_client(i):
    mkdir_p("client{}".format(i))
    log = open("client{}.log".format(i), mode="w+")
    p = sp.Popen([rpath("otto_client.byte"), "-debug", "true"],
                 cwd="client{}".format(i),
                 stdin=log, stdout=log)

    return (p, log)

def run_server():
    log = open("server.log", mode="w+")
    p = sp.Popen([rpath("otto_server.byte"), "-debug", "true"],
                 cwd="testdir", stdin=log, stdout=log)
    return (p, log)

import sys

num_clients = int(sys.argv[1])

proc_list = []

for i in xrange(num_clients):
    proc_list.append(run_client(i))

proc_list.append(run_server())

for elem in proc_list:
    p,l = elem
    p.wait()
    l.close()
