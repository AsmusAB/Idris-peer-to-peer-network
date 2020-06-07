module PeerTest

import public Peer
import System
import Channel
import Util
import Effects
import Effect.Random
import Effect.StdIO

firstPeerTest : (chans : PeerChannels) -> (sock : Socket) ->
    (ip : SocketAddress) -> (port : Int) ->
    PeerCmd () Setup Setup
firstPeerTest chans sock ip port = 
    do PutStrLn $ "Socket is listening for connections on: " ++ (show ip) ++ ":" ++ (show port)
       ListenForNewClients sock chans
       BroadcastToClients chans


connectingPeerTest : (chans : PeerChannels) -> (mySock : Socket) ->
                 (myIp : SocketAddress) -> (myPort : Int) ->
                 (newIp : SocketAddress) -> (newPort : Int) ->
                 PeerCmd () NotReady Running
connectingPeerTest chans mySock myIp myPort newIp newPort =
  do Right newSock <- CreateSocket | Left err => Error (show err)
     ok <- Connect newSock newIp newPort
     if ok == 0 then
      do Push (connChan chans) newSock
         WaitForRelayedMessages newSock (relayed chans)
         PutStrLn $ "Socket is listening for connections on: " ++ (show myIp) ++ ":" ++ (show myPort)
         RecMessages newSock newIp chans
         ListenForNewClients mySock chans
         BroadcastToClients chans
         -- THIS IS NOT AN ERROR BUT A FORCE THROUGH STATES FOR TESTING BEHAVIOUR
         Error $ ""
     else Error $ "Something went wrong : error code " ++ (show ok) 


startConnectingPeersChain : (nrOfPeers : Nat) -> (ip : SocketAddress)
                        -> (newPort : Int) -> IO ()
startConnectingPeersChain Z ip newPort = pure ()
startConnectingPeersChain (S n) ip newPort = do connChan <- makeChan (the (List Socket) [])
                                                msgQueue <- makeChan (the (List Message) [])
                                                relayed <- makeChan (the (List Message) [])
                                                chans <- pure (MkPeerChans msgQueue relayed connChan) 
                                                Right sock <- socket AF_INET Stream 0 
                                                    | Left err => putStr (show err) 
                                                port <- pure (3500 + (cast n))
                                                ok <- bind sock (Just ip) port
                                                listeningCode <- listen sock
                                                putStrLn $ "Creating peer nr: " ++ (show (n + 1))
                                                run (connectingPeerTest chans sock ip port ip newPort)
                                                startConnectingPeersChain n ip port

startConnectingPeersToFirstPeer : (nrOfPeers : Nat) -> (ip : SocketAddress)
                                -> (newPort : Int) -> IO ()
startConnectingPeersToFirstPeer Z ip newPort = pure ()
startConnectingPeersToFirstPeer (S n) ip newPort = do connChan <- makeChan (the (List Socket) [])
                                                      msgQueue <- makeChan (the (List Message) [])
                                                      relayed <- makeChan (the (List Message) [])
                                                      chans <- pure (MkPeerChans msgQueue relayed connChan) 
                                                      Right sock <- socket AF_INET Stream 0 
                                                        | Left err => putStr (show err) 
                                                      port <- pure (3500 + (cast n))
                                                      ok <- bind sock (Just ip) port
                                                      listeningCode <- listen sock
                                                      putStrLn $ "Creating peer nr: " ++ (show (n + 1))
                                                      run (connectingPeerTest chans sock ip port ip newPort)
                                                      startConnectingPeersToFirstPeer n ip port


startconnectingPeersRandom : (nrOfPeers : Nat) -> (ip : SocketAddress)
                            -> (newPort : Int) -> IO ()
startconnectingPeersRandom Z ip newPort = pure ()
startconnectingPeersRandom (S n) ip newPort = do connChan <- makeChan (the (List Socket) [])
                                                 msgQueue <- makeChan (the (List Message) [])
                                                 relayed <- makeChan (the (List Message) [])
                                                 chans <- pure (MkPeerChans msgQueue relayed connChan) 
                                                 Right sock <- socket AF_INET Stream 0 
                                                    | Left err => putStr (show err) 
                                                 rndProg <- pure (rndInt (cast n) 100)
                                                 rndNr <- Effects.run rndProg
                                                 port <- pure (3500 + (fromInteger rndNr))
                                                 ok <- bind sock (Just ip) port
                                                 listeningCode <- listen sock
                                                 putStrLn $ "Creating peer nr: " ++ (show (n + 1))
                                                 run (connectingPeerTest chans sock ip port ip newPort)
                                                 startconnectingPeersRandom n ip port

peerTest1 : IO ()
peerTest1 = do Right sock <- socket AF_INET Stream 0 
                | Left err => putStr (show err) 
               connChan <- makeChan (the (List Socket) [])
               msgQueue <- makeChan (the (List Message) [])
               relayed <- makeChan (the (List Message) [])
               chans <- pure (MkPeerChans msgQueue relayed connChan)
               putStrLn "Select what ip you want to use, by inputting the number in the parentheses:"
               ip <- selectIp
               port <- pure 3500
               ok <- bind sock (Just ip) port
               listeningCode <- listen sock
               run (firstPeerTest chans sock ip port)
               putStrLn "Created first peer"
               startConnectingPeersToFirstPeer 100 ip port
               addSingleMsg "hello" msgQueue "123"
               usleep 1000000
               pure ()

peerTest2 : IO ()
peerTest2 = do Right sock <- socket AF_INET Stream 0 
                | Left err => putStr (show err) 
               connChan <- makeChan (the (List Socket) [])
               msgQueue <- makeChan (the (List Message) [])
               relayed <- makeChan (the (List Message) [])
               chans <- pure (MkPeerChans msgQueue relayed connChan)
               putStrLn "Select what ip you want to use, by inputting the number in the parentheses:"
               ip <- selectIp
               seed <- time
               port <- pure 3500
               ok <- bind sock (Just ip) port
               listeningCode <- listen sock
               run (firstPeerTest chans sock ip port)
               putStrLn "Created first peer"
               startConnectingPeersChain 100 ip port
               addSingleMsg "hello" msgQueue "123"
               usleep 1000000
               pure ()

peerTest3 : IO ()
peerTest3 = do Right sock <- socket AF_INET Stream 0 
                | Left err => putStr (show err) 
               connChan <- makeChan (the (List Socket) [])
               msgQueue <- makeChan (the (List Message) [])
               relayed <- makeChan (the (List Message) [])
               chans <- pure (MkPeerChans msgQueue relayed connChan)
               putStrLn "Select what ip you want to use, by inputting the number in the parentheses:"
               ip <- selectIp
               seed <- time
               port <- pure 3500
               ok <- bind sock (Just ip) port
               listeningCode <- listen sock
               run (firstPeerTest chans sock ip port)
               putStrLn "Created first peer"
               startconnectingPeersRandom 100 ip port
               addSingleMsg "hello" msgQueue "123"
               usleep 1000000
               pure ()