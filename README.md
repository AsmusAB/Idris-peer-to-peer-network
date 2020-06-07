# Idris Peer To Peer Network

## Using the system
The system is very simple, when you startup a Peer you will be able to chose an ip and connect to another ip, or spawn your own network. To do this you need all of the files and also to have build the .ibc files correctly.
Luckily we have made a ***makefile*** for you to do all of this.
#### Step 1
Clone the repository with the following command: \
`git clone https://github.com/AsmusAB/Idris-peer-to-peer-network.git`

#### Step 2
Run `make build`

#### Step 3
Run `make install`

#### Step 4
Run either `make run` or just use the output file called `Peer` that was created by the make install

## Testing the System
Testing the system there are 3 test, that are trying to accomplish the same thing, but one test connects all peers to the first peer, second test connects all peers in a chain, and last test connects them randomly to eachother.\
The tests are semi-automatic so you will have to do a small bit of manual work. To run a test make sure you have **cloned** the repo and run `make build` before you proceed, then follow it with the following steps:
#### Step 1
Run `idris PeerTest.idr -p contrib -p effects`

#### Step 2
When the idris repl opens, use the `:exec` command with one of the following tests:
1. `peerTest1` connection to the first peer
2. `peerTest2` chain connection
3. `peerTest3` random connections

An example of a command could be `:exec peerTest1`

#### Step 3
After having run the tests, and selected an ip with the following instructions of when the test begins. You now have to take a copy of all of the lines that contain `123:Hello` and copy them into a wordcount or something alike.

## Contact
You can contact us by making an **issue** on this repo, or by contacting one of the following mail addresses.\
Frederik Vigen : [frederikvigen@gmail.com](mailto:frederikvigen@gmail.com)\
Asmus Batram : [asmusbatram@gmail.com](mailto:asmusbatram@gmail.com)\
Christian Weis : [christian.weis.8270@gmail.com](mailto:christian.weis.8270@gmail.com)