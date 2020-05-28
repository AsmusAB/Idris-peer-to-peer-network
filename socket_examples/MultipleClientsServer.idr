module MultipleClientsServer

import System.Concurrency.Channels
import public Network.Socket

recMessages : (sock : Socket) -> (ip : SocketAddress) -> IO ()
recMessages sock ip = do Right (str, res) <- recv sock 1024  | Left err => putStrLn (show err)
                         putStrLn $ (show ip) ++ " send: " ++ str
                         putStrLn $ "Sending back: " ++ str
                         Right ok <- send sock $ str | Left err => putStrLn (show err)
                         recMessages sock ip

listenForClients : (sock : Socket) -> IO ()
listenForClients sock = do putStrLn $ "Listening for clients...."
                           Right (newSock, newIp) <- accept sock | Left err => putStrLn (show err)
                           putStrLn $ "Socket accepted connection from: " ++ (show newIp)
                           spawn (recMessages newSock newIp)
                           listenForClients sock


serverbound : (sock : Socket) -> (ok : Int) -> (ip : SocketAddress) -> (port : Port) -> IO ()
serverbound sock ok ip port = do putStrLn $ "Socket is bound to: " ++ (show ip) ++ ":" ++ (show port)
                                 listeningCode <- listen sock
                                 listenForClients sock

servererr : (sock : Socket) -> (ok : Int) -> IO ()
servererr sock ok = putStrLn $ "Server failed" ++ (show ok)

startServer : IO ()
startServer = do Right sock <- socket AF_INET Stream 0 | Left err => putStr (show err)
                 putStrLn "Socket created"
                 ip <- pure (IPv4Addr 192 168 87 189)
                 port <- pure (the (Port) 9420)
                 ok <- bind sock (Just ip) port
                 if ok == 0 then (serverbound sock ok ip port)
                            else (servererr sock ok)