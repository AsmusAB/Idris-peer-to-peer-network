module Channel

import Data.List.Views
import System.Concurrency.Process
import public Network.Socket

%access public export

data Request msg = WriteLock
                 | Read
                 | ReleaseLock
                 | Pure msg

data ResultType msg = Fatal   String
                    | Warning String
                    | Correct msg

data Channel msg = MkChan (ProcID (Request msg))

data ChannelState = Unlocked | Locked

data ChannelCmd : (resultType : Type) -> (sharedResTyp : Type) -> (stateIn : ChannelState) ->
                  (stateOut : ChannelState) -> Type where
     ReadOnlyCmd : (chan : Channel typ) -> ChannelCmd (ResultType typ) typ Unlocked Unlocked
     ReadCmd : (chan : Channel typ) -> ChannelCmd (ResultType typ) typ Unlocked Locked
     WriteCmd : (chan : Channel typ) -> (newRes : typ) -> ChannelCmd Bool typ Locked Unlocked
     ReleaseLockCmd : (chan : Channel typ) -> ChannelCmd Bool typ Locked Unlocked

     PureCmd : (res : ty) -> ChannelCmd ty typ state state
     (>>=) : ChannelCmd a typ state1 state2 -> (a -> ChannelCmd b typ state2 state3) ->
             ChannelCmd b typ state1 state3

mutual
  write_inner : (req : Request msgType) -> (state : msgType) ->
                (procId : ProcID (Request msgType)) -> Process (Request msgType) ()
  write_inner req state procId = 
    do send procId (Pure state)
       Just newReq <- recvFrom procId 
        | Nothing => channelLoop state
       case newReq of
            (Pure newState) => channelLoop newState
            (ReleaseLock) => channelLoop state
            _ =>  channelLoop state

  channelLoop : (state : msgType) -> Process (Request msgType) ()
  channelLoop state = do (procId, req) <- recvWithSender
                         case req of
                               WriteLock => write_inner req state procId
                               ReleaseLock => channelLoop state
                               Read => do send procId (Pure state)
                                          channelLoop state
                
makeChan : (init : ty) -> IO (Channel (ty))
makeChan init = do procId <- run (create (channelLoop init))
                   pure (MkChan procId)



read_inner : Channel msg -> (req : Request msg) -> IO (ResultType msg)
read_inner (MkChan procId) req = 
    do ok <- run (send procId req)
       Just newReq <- run (recvFrom procId) 
        | Nothing => pure (Fatal "Channel thread died")
       case newReq of
            (Pure val) => pure (Correct val)
            _ => pure (Warning "Did not recieve correct formatted state from channel thread")

read : Channel msg -> IO (ResultType msg)
read chan = read_inner chan WriteLock

readOnly : Channel msg -> IO (ResultType msg)
readOnly chan = read_inner chan Read

write : Channel msg -> msg -> IO Bool
write (MkChan procId) msg = run (send procId (Pure msg))

run : ChannelCmd res typ inState outState -> IO res
run (ReadOnlyCmd chan) = readOnly chan
run (ReadCmd chan) = read chan
run (WriteCmd chan newRes) = write chan newRes
run (ReleaseLockCmd (MkChan procId)) = run (send procId ReleaseLock)
run (PureCmd res) = pure res
run (x >>= f) = do x' <- run x
                   run (f x')
