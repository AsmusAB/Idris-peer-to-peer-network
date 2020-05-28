module SingleEchoServer

import public Network.Socket


serverbound : (sock : Socket) -> (ok : Int) -> (ip : SocketAddress) -> (port : Port) -> IO ()
serverbound sock ok ip port = 
  do putStrLn $ "Socket is bound to: " ++ (show ip) ++ ":" ++ (show port)
     listeningCode <- listen sock
     putStrLn $ "Socket is listening to: " ++ (show ip) ++ ":" ++ (show port)
     Right (newSock, newIp) <- accept sock | Left err => putStrLn (show err)
     putStrLn $ "Socket accepted connection from: " ++ (show newIp)
     putStrLn "Waiting for message..."
     Right (str, res) <- recv newSock 1024  | Left err => putStrLn (show err)
     putStrLn $ "Client send: " ++ str
     putStrLn $ "Sending back: Hello, " ++ str
     Right ok <- send newSock $ "Hello, " ++ str | Left err => putStrLn (show err)
     close newSock

servererr : (sock : Socket) -> (ok : Int) -> IO ()
servererr sock ok = putStrLn $ "Server failed" ++ (show ok)

startServer : IO ()
startServer = 
  do Right sock <- socket AF_INET Stream 0 | Left err => putStr (show err)
     putStrLn "Socket created"
     ip <- pure (IPv4Addr 192 168 87 189)
     port <- pure (the (Port) 9420)
     ok <- bind sock (Just ip) port
     if ok == 0 then (serverbound sock ok ip port)
     else (servererr sock ok)
                 
