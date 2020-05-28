import Channel
import System.Concurrency.Channels

otherThreadProg : Channel (List String) -> IO ()
otherThreadProg chan = do Just state <- read chan | Nothing => putStrLn "Something went wrong 3"
                          write chan ("1234" :: state)
                          Just updatedState <- readOnly chan | Nothing => putStrLn "Something went wrong2"
                          putStrLn $ "Other thread : " ++ (show updatedState)                        

main : IO ()
main = do chan <- makeChan
          spawn (otherThreadProg chan)
          Just state <- read chan | Nothing => putStrLn "Something went wrong"
        --   input <- getLine
          write chan ("123" :: state)
          Just updatedState <- readOnly chan | Nothing => putStrLn "Something went wrong2"
          putStrLn $ "Main thread : " ++ (show updatedState)