**Requirements**

Otto supports Ubuntu and has been verified on:
* Kubuntu 16.4
* Kubuntu 15.10
* 3110 VM

Otto DOES NOT support:
* Mac
* Windows

OPAM:
* zmq=4.0-7
* base64=2.1.2
* yojson=1.3.2
* ounit=2.0.0

Ubuntu:
* libzmq3-dev
* curl=7.51.0
* pstree=version on vm


**Execution Manual**

Run ‘./otto_server.byte -help’ to view instructions on server
Run ‘./otto_client.byte -help’ to view instructions on client


Tests and student assignments must be formatted as follows:
  - Common directory: contains testfiles and Makefile used to test assignments
  - Tests directory: contains directories containing each students' assignment
                     for example, if student submissions are organized by netid ./tests
                                                                                 |__ vb239
                                                                                 |__ jl964
                                                                                 ...
  - Commands file: contains a single command executing tests (‘make test’ for example)


**Example**

On the server:
> make
> cd _build
> ./otto_server.byte -port 5555 -tests "../testdir/tests" -common "../testdir/common" -test-timeout 15 -client-timeout 45 -commands "../testdir/commands"


On the client(s):
> path/to/otto_client.byte

NOTE: otto_client downloads files into its current directory so it is advisable to execute otto_client from an empty directory
