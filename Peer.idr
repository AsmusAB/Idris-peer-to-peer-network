module Peer

import Util
import System.Concurrency.Channels
import Data.Primitives.Views
import System
import Data.String
import Data.List
import Language.JSON
import public Network.Socket
import Channel

%access public export
%include C "c/ip.h"
%link C "c/ip.o"


record Message where
       constructor MkMsg
       msgId : String
       content : String

record PeerChannels where
       constructor MkPeerChans
       msgQueue : Channel (List Message)
       relayed : Channel (List Message)
       connChan : Channel (List Socket)

data PeerState = Joining
               | Setup
               | Running
               | Ready
               | NotReady

data PeerCmd : (resultType : Type) -> (stateIn : PeerState) ->
     (stateOut : PeerState) -> Type where
     WaitForRelayedMessages : (sock : Socket) -> (relayed : Channel (List Message)) -> PeerCmd () Joining Setup
     ListenForNewClients : (sock : Socket) -> (chans : PeerChannels) -> PeerCmd () Setup Setup
     BroadcastToClients : (chans : PeerChannels) -> PeerCmd () Setup Setup
     RecMessages : (sock : Socket) -> (ip : SocketAddress) -> (chans : PeerChannels) -> PeerCmd () Setup Setup
     ReplStart : (chans : PeerChannels) -> (ip : SocketAddress) -> (port : Int) -> PeerCmd () Setup Running
     Connect : (sock : Socket) -> (ip : SocketAddress) -> (port : Int) -> PeerCmd Int Ready Joining
     CreateSocket : PeerCmd (Either SocketError Socket) NotReady Ready
    
     Error : (str : String) -> PeerCmd () state Running
     Push : (chan : Channel (List ty)) -> (val : ty) -> PeerCmd () state state
     GetLine : PeerCmd String state state
     PutStrLn : (str : String) -> PeerCmd () state state 
     PureCmd : (res : ty) -> PeerCmd ty state state
     (>>=) : PeerCmd a state1 state2 -> (a -> PeerCmd b state2 state3) ->
             PeerCmd b state1 state3

msgToJson : Message -> JSON
msgToJson msg = JObject [("id", JString (msgId msg)), ("content", JString (content msg))]

jsonToMsg : JSON -> Maybe Message
jsonToMsg (JObject [("id", JString msgId), ("content", JString msg)]) = Just (MkMsg msgId msg)
jsonToMsg _ = Nothing

msgListToJson : List Message -> JSON
msgListToJson msgList = JArray (map (\msg => msgToJson msg) msgList)

jsonToMsgList : JSON -> Maybe (List (Maybe Message))
jsonToMsgList (JArray msgs) = Just (map (\msg => jsonToMsg msg) msgs)
jsonToMsgList _ = Nothing

recAll : (sock : Socket) -> IO (Either SocketError String)
recAll sock = recvRec sock [] 64
  where
    partial
    recvRec : Socket -> List String -> ByteLength -> IO (Either SocketError String)
    recvRec sock acc n = do res <- recv sock n
                            case res of
                              Left c => pure (Left c)
                              Right (str, res) => let n' = min (n * 2) 65536 in
                                                  if res < n then pure (Right $ concat $ reverse $ str :: acc)
                                                  else recvRec sock (str :: acc) n'
                                                     

parseIpAndPort : String -> IO (Maybe(SocketAddress, Port))
parseIpAndPort str = 
  let (addrStr, portStr) = span (/= ':') str in
  case parseIPv4 addrStr of
        (IPv4Addr p1 p2 p3 p4) => 
        case parseInteger $ substr 1 (length portStr) portStr of
          Nothing => pure Nothing
          (Just port) => pure $ Just ((IPv4Addr p1 p2 p3 p4), toIntNat (fromIntegerNat port))
        _ => pure Nothing


push : (chan : Channel (List ty)) -> (elem : ty) ->
       ChannelCmd (ResultType (List ty)) (List ty) Unlocked Unlocked
push chan elem = do res <- ReadCmd chan
                    case res of
                        (Correct state) => do WriteCmd chan (elem :: state); PureCmd res
                        _ => do ReleaseLockCmd chan; PureCmd res

pop : (chan : Channel (List ty)) -> ChannelCmd (ResultType (Maybe ty)) (List ty) Unlocked Unlocked
pop chan = 
  do res <- ReadCmd chan
     case res of
           (Correct state) => 
             case state of
               [] => do WriteCmd chan []; PureCmd (Correct Nothing)
               (x :: xs) => do WriteCmd chan xs; PureCmd (Correct (Just x))
           (Fatal x) => do ReleaseLockCmd chan; PureCmd (Fatal x)
           (Warning x) => do ReleaseLockCmd chan; PureCmd (Warning x)

peek : (chan : Channel (List ty)) -> ChannelCmd (ResultType (Maybe ty)) (List ty) Unlocked Unlocked
peek chan = 
  do res <- ReadOnlyCmd chan
     case res of
           (Correct state) => 
             case state of
               [] => PureCmd (Correct Nothing)
               (x :: xs) => PureCmd (Correct (Just x))
           (Fatal x) => PureCmd (Fatal x)
           (Warning x) => PureCmd (Warning x)

releaseLock : (chan : (Channel (List ty))) ->
              ChannelCmd Bool (List ty) Locked Unlocked
releaseLock chan = ReleaseLockCmd chan

lockChannel : (chan : Channel (List ty)) ->
              ChannelCmd () (List ty) Unlocked Locked
lockChannel chan = do ReadCmd chan
                      PureCmd ()

getListOfChannel : (chan : Channel (List ty)) ->
                   ChannelCmd (ResultType (List ty)) (List ty) Unlocked Unlocked
getListOfChannel chan = ReadOnlyCmd chan

writeToChan : (chan : Channel (List ty)) -> (newList : List ty) -> ChannelCmd () (List ty) Locked Unlocked
writeToChan chan newList = do WriteCmd chan newList
                              PureCmd ()

readFromChan : (chan : Channel (List ty)) -> ChannelCmd (ResultType (List ty)) (List ty) Unlocked Locked
readFromChan chan = ReadCmd chan

getIP : Int -> (IO String)
getIP = foreign FFI_C "getipc" (Int -> (IO String))

parseSingleIp : String -> Maybe SocketAddress
parseSingleIp input = let parsedIp = parseIPv4 input in
                      case parsedIp of
                        (IPv4Addr p1 p2 p3 p4) => Just (IPv4Addr p1 p2 p3 p4)
                        _ => Nothing

getAllIps : Int -> IO (List String)
getAllIps startingInt = do ip <- getIP startingInt
                           if ip == "END" 
                            then pure []
                            else do rest <- getAllIps (startingInt + 1)
                                    pure (ip :: rest)               

printIpOptions : List (SocketAddress, Nat) -> IO ()
printIpOptions [] = pure ()
printIpOptions ((ip, nr) :: rest) = do putStrLn $ (show ip) ++ "(" ++ (show nr) ++ ")"
                                       printIpOptions rest

removeNothings : List (Maybe ty) -> List ty
removeNothings [] = []
removeNothings (Nothing :: rest) = removeNothings rest
removeNothings (Just elem :: rest) = (elem :: removeNothings rest)

selectIp : IO (SocketAddress)
selectIp = do ips <- getAllIps 0
              parsedIps <- pure (removeNothings (map parseSingleIp ips))
              zippedIps <- pure (zip parsedIps [0..(length parsedIps)])
              printIpOptions zippedIps
              input <- getLine
              case parseInteger input of
                Just option => do Just addr <- pure (index' (fromIntegerNat option) parsedIps) | Nothing => selectIp
                                  pure addr
                Nothing => selectIp

recMessages : (sock : Socket) -> (ip : SocketAddress) -> 
              (chans : PeerChannels) -> IO ()
recMessages sock ip chans = 
  do Right jsonString <- recAll sock
      | Left err => do if err == 0 then putStrLn $ (show ip) ++ " has lost connection" else putStrLn $ "Recmessages err: " ++ (show err)
     Just json <- pure (parse jsonString) 
      | Nothing => putStrLn "The message does not have json structure"
     Just msg <- pure (jsonToMsg json) 
      | Nothing => putStrLn "Structure of json was incorrect"
     ok <- run (readFromChan (relayed chans))
     case ok of
           (Fatal err) => 
             do putStrLn err
           (Warning err) => 
             do putStrLn ("Error in recMessages, error is: " ++ err); recMessages sock ip chans
           (Correct seenmsgs) => 
             case isElem (msgId msg) (map (\x => msgId x) seenmsgs) of
               (Yes prf) => 
                 do run (push (msgQueue chans) msg)
                    run (releaseLock (relayed chans))
                    recMessages sock ip chans
               (No contra) => 
                 do run (push (msgQueue chans) msg)
                    putStrLn $ (msgId msg) ++ ": " ++ (content msg)
                    run (releaseLock (relayed chans))
                    recMessages sock ip chans

listenForClients : (listeningSock : Socket) -> 
                (chans : PeerChannels) -> IO ()
listenForClients listeningSock chans = 
  do Right (newSock, newIp) <- accept listeningSock 
      | Left err => do putStrLn $ "ListenForClients error: " ++ (show err)
     putStrLn $ "Socket accepted connection from: " ++ (show newIp)
     run (push (connChan chans) newSock)
     run (lockChannel (msgQueue chans))
     seenMsgs <- run (getListOfChannel (relayed chans))
     case seenMsgs of
      (Fatal x) => do putStrLn $ "listenForClients dead: " ++ x
      (Warning x) => 
        do putStrLn ("Failed to get seenmsgs list from channel, reason : " ++ x)
           listenForClients listeningSock chans
      (Correct seenMsgs) => 
        do Right ok <- send newSock ((format 1 (msgListToJson seenMsgs))) 
           | Left err => do putStrLn $ "Could not send seenMsgs, error: " ++ (show err)
           Right str <- recAll newSock | Left err => do putStrLn $ "Failed to recieve the ACK: " ++ (show err)
           if str == "ACK" then
            do spawn (recMessages newSock newIp chans)
               run (releaseLock (msgQueue chans))
               listenForClients listeningSock chans
           else putStrLn $ "Did not recieve ACK but some other msg" ++ str 

broadcastInner : (msg : Message) -> (relayed : List Message) ->
                 (connections : List Socket) -> IO ()
broadcastInner msg relayed [] = pure ()
broadcastInner msg relayed (current :: rest) = 
  case isElem (msgId msg) (map (\x => msgId x) relayed) of
            (Yes prf) => pure ()
            (No contra) => 
              do Right ok <- send current (format 1 (msgToJson msg)) 
                  | Left err => if not (err == 32) then putStrLn $ "Could not send message: " ++ (show err) else broadcastInner msg relayed rest
                 broadcastInner msg relayed rest

broadcast : (msg : Message) -> (connChan : Channel (List Socket)) -> 
  (relayed : Channel (List Message)) -> IO ()
broadcast msg connChan relayed = 
  do connections <- run (readFromChan connChan)
     case connections of 
      (Fatal x) => do putStrLn $ "Broadcast dead: " ++ x
                      run (releaseLock connChan)
                      pure ()
      (Warning x) => do run (releaseLock connChan)
                        broadcast msg connChan relayed
                        pure ()
      (Correct connections) => 
        do ok <- run (readFromChan relayed)
           case ok of 
              (Fatal x) => do putStrLn $ "Broadcast dead: " ++ x
                              run (releaseLock relayed)
                              run (releaseLock connChan)
                              pure ()
              (Warning x) => do run (releaseLock relayed)
                                run (releaseLock connChan)
                                broadcast msg connChan relayed
                                pure ()
              (Correct relayedmsgs) => do broadcastInner msg relayedmsgs connections
                                          run (releaseLock connChan)
                                          run (writeToChan relayed (msg :: relayedmsgs))
                                          pure () 

broadcastToClients : (chans : PeerChannels) -> IO ()
broadcastToClients chans = 
  do msg <- run (pop (msgQueue chans))
     case msg of
       (Fatal err) => do putStrLn $ "BroadCastToClients Dead: " ++ err
       (Warning err) => 
         do putStrLn ("BroadcastToClientsErr " ++ err)
            broadcastToClients chans
       (Correct maybeMsg) => 
         case maybeMsg of
           Nothing => 
             do broadcastToClients chans
           (Just msg) => 
             do broadcast msg (connChan chans) (relayed chans)
                broadcastToClients chans

addSingleMsg : (input : String) -> (msgQueue : Channel (List Message)) -> (msgId : String) -> IO ()
addSingleMsg input msgQueue msgId = do pushedOk <- run (push msgQueue (MkMsg msgId input))
                                       pure ()

generateMsgId : (ip : SocketAddress) -> (port : Int) -> (msgId : Int) -> IO String
generateMsgId ip port msgId = pure $ (show ip) ++ ":" ++ (show port) ++ "-" ++ (show msgId)

repl : (chans : PeerChannels) -> (oldId : Int) -> 
  (ip: SocketAddress) -> (port: Int) -> IO ()  
repl chans oldId ip port = 
  do input <- getLine
     newId <- pure (oldId + 1)
     msgId <- generateMsgId ip port newId
     spawn (addSingleMsg input (msgQueue chans) msgId)
     repl chans newId ip port

addMessagesToChan : (msgs : List (Maybe Message)) -> (relayed : Channel (List Message)) -> IO ()
addMessagesToChan [] relayed = pure ()
addMessagesToChan (Nothing :: rest) relayed = pure ()
addMessagesToChan (Just msg :: rest) relayed = 
  do run (push relayed msg)
     addMessagesToChan rest relayed
     putStrLn $ (msgId msg) ++ ": " ++ (content msg)
     

waitForSeenMsgs : (sock : Socket) -> (relayed : Channel (List Message)) -> IO ()
waitForSeenMsgs sock relayed = do Right jsonString <- recAll sock | Left err => putStrLn (show err)
                                  Just json <- pure (parse jsonString) 
                                    | Nothing => putStrLn "The message does not have json structure"
                                  Just msgs <- pure (jsonToMsgList json)
                                    | Nothing => putStrLn "Structure of json was incorrect"
                                  addMessagesToChan msgs relayed
                                  Right ok <- send sock "ACK" | Left err => do putStrLn $ "Could not send ACK " ++ (show err)
                                  pure ()
                

firstPeer : (chans : PeerChannels) -> (sock : Socket) ->
            (ip : SocketAddress) -> (port : Int) ->
            PeerCmd () Setup Running
firstPeer chans sock ip port = 
  do PutStrLn $ "Socket is listening for connections on: " ++ (show ip) ++ ":" ++ (show port)
     ListenForNewClients sock chans
     BroadcastToClients chans
     ReplStart chans ip port

connectingPeer : (chans : PeerChannels) -> (mySock : Socket) ->
                 (myIp : SocketAddress) -> (myPort : Int) ->
                 (newIp : SocketAddress) -> (newPort : Int) ->
                 PeerCmd () NotReady Running
connectingPeer chans mySock myIp myPort newIp newPort =
  do Right newSock <- CreateSocket | Left err => Error (show err)
     ok <- Connect newSock newIp newPort
     if ok == 0 then 
      do Push (connChan chans) newSock
         WaitForRelayedMessages newSock (relayed chans)
         PutStrLn $ "Socket is listening for connections on: " ++ (show myIp) ++ ":" ++ (show myPort)
         RecMessages newSock newIp chans 
         ListenForNewClients mySock chans 
         BroadcastToClients chans
         ReplStart chans myIp myPort
     else Error $ "Something went wrong : error code " ++ (show ok) 

run : PeerCmd res inState outState -> IO res
run (WaitForRelayedMessages sock relayed) = waitForSeenMsgs sock relayed
run (ListenForNewClients sock chans) = 
  do spawn (listenForClients sock chans )
     pure ()
run (BroadcastToClients chans) =
  do spawn (broadcastToClients chans)
     pure ()
run (RecMessages newSock ip chans) = 
  do spawn (recMessages newSock ip chans)
     pure ()
run (ReplStart chans ip port) = repl chans 0 ip port
run (Connect newSock newIp newPort) = connect newSock newIp newPort
run (CreateSocket) = socket AF_INET Stream 0

run (Error str) = putStrLn str
run (Push chan val) = 
  do run (push chan val)
     pure ()
run (GetLine) = getLine
run (PutStrLn str) = putStrLn str
run (PureCmd res) = pure res
run (x >>= f) = do x' <- run x
                   run (f x')

peerStart : IO ()
peerStart =  
  do Right sock <- socket AF_INET Stream 0 
      | Left err => putStr (show err)
     putStrLn "Socket created"
     connChan <- makeChan (the (List Socket) [])
     msgQueue <- makeChan (the (List Message) [])
     relayed <- makeChan (the (List Message) [])
     chans <- pure (MkPeerChans msgQueue relayed connChan)
     putStrLn "Select what ip you want to use, by inputting the number in the parentheses:"
     ip <- selectIp
     seed <- time
     port <- pure (randPort (fromInteger seed))
     ok <- bind sock (Just ip) port
     listeningCode <- listen sock
     if ok == 0 then 
       do putStrLn "Press ENTER to start a new network, or input an ip to connect to an existing network"
          ipAndPort <- getLine
          if ipAndPort == "" then
            do run (firstPeer chans sock ip port)
          else
            do Just (newIp, newPort) <- parseIpAndPort ipAndPort 
                | Nothing => putStrLn "Failed to parse ip or port"
               run (connectingPeer chans sock ip port newIp newPort)
     else if ok == 13 then
       putStrLn $ "Something went wrong, port is probably already in use. Try again"
     else putStrLn $ "Something went wrong, error code: " ++ (show ok)

main : IO ()
main = peerStart