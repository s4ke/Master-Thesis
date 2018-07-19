# Experiment: Cloud Haskell Backend

Cloud Computing has become more and more prevalent in recent years. Servers are
replaced more and more with virtualized servers positioned all around the globe.
These virtualized servers can easily be brought up when required and shut down
when not in use. This trend in computing has also been embraced by the Haskell
community and therefore, libraries such as Cloud Haskell were born. Cloud Haskell
is described on the projects website^[see \url{http://haskell-distributed.github.io/}] as:

> Cloud Haskell: Erlang-style concurrent and distributed programming in Haskell.
The Cloud Haskell Platform consists of a generic network transport API,
libraries for sending static closures to remote nodes, a rich API for
distributed programming and a set of platform libraries modelled after
Erlang's Open Telecom Platform.

> Generic network transport backends have been developed for TCP and in-memory
messaging, and several other implementations are available including a
transport for Windows Azure.[...]
    
It is basically a set of APIs and libraries for communication between networks
of nodes and easy use in a cloud environment. With it programmers can
write fully-featured Haskell based cloud solutions.

While users can already write concurrent applications with the help of Cloud Haskell
using some of its libraries or even with the bare communication API, it seems like
a good idea to write parallel programs in a less involved way. In the following section we
will therefore explore the possibility of a Cloud Haskell based backend for the
`ArrowParallel` interface given in this thesis after a short introduction to the
API. Note that for easier testing, we only work with a local-net Cloud Haskell
backend. The results, however, hold true for other architectures as well. 

## The Cloud Haskell API

## Program harness

~~~~{.haskell}
-- our config type. for now, this only hosts the current state
-- of the backend
type Conf = State

data State = State {
  workers :: MVar [NodeId],
  shutdown :: MVar Bool,
  started :: MVar Bool,
  localNode :: LocalNode,
  serializeBufferSize :: Int
}

-- default buffer size used by trySerialize
defaultBufSize :: Int
defaultBufSize = 10 * 2^20 -- 10 MB

defaultInitConf :: LocalNode -> IO Conf
defaultInitConf = initialConf defaultBufSize

initialConf :: Int -> LocalNode -> IO Conf
initialConf serializeBufferSize localNode = do
  workersMVar <- newMVar []
  shutdownMVar <- newMVar False
  startedMVar <- newMVar False
  return State {
    workers = workersMVar,
    shutdown = shutdownMVar,
    started = startedMVar,
    localNode = localNode,
    serializeBufferSize = serializeBufferSize
  }
~~~~

~~~~{.haskell}
data BackendType = Master | Slave
type Host = String
type Port = String

startBackend :: RemoteTable -> BackendType -> Host -> Port -> IO Conf
startBackend remoteTable Master host port = do
	backend <- initializeBackend host port remoteTable

	localNode <- newLocalNode backend

	conf <- defaultInitConf localNode
	putMVar ownLocalConfMVar conf

	-- fork away the master node
	forkIO $ startMaster backend (master conf backend)

	-- wait for startup
	waitForStartup conf
	return conf
startBackend remoteTable Slave host port = do
	backend <- initializeBackend host port remoteTable

	localNode <- newLocalNode backend

	conf <- defaultInitConf localNode
	putMVar ownLocalConfMVar conf

	startSlave backend
	return conf
~~~~

~~~~{.haskell}
-- the code for the master node. automatically discovers all slaves and 
-- adds/removes them from the Conf object
master :: Conf -> Backend -> [NodeId] -> Process ()
master conf backend slaves = do
    forever $ do
      shutdown <- liftIO $ readMVar $ shutdown conf
      if shutdown
        then do
          terminateAllSlaves backend
          die "terminated"
        else do
          slaveProcesses <- findSlaves backend
          let slaveNodes = map processNodeId slaveProcesses
          liftIO $ do
              modifyMVar_ (workers conf) (\_ -> return slaveNodes)
              if (length slaveNodes) > 0 then
                modifyMVar_ (started conf) (\_ -> return True)
              else
                return ()
~~~~

~~~~{.haskell}
waitUntil condition = fix $ \loop -> do
  cond <- condition
  if cond
    then return ()
    else threadDelay 100 >> loop

hasSlaveNode :: Conf -> IO Bool
hasSlaveNode conf = readMVar (started conf)

-- wait for the (started Conf) == true (i.e. the master node has found slaves)
waitForStartup :: Conf -> IO ()
waitForStartup conf = waitUntil (hasSlaveNode conf)
~~~~

## Serializing arbitrary data

## Parallel Evaluation with Cloud Haskell

~~~~{.haskell}
-- Wrapper for the packman type Serialized
newtype Thunk a = Thunk { fromThunk :: Serialized a } deriving (Typeable)

toThunk a = Thunk { fromThunk = a }

instance (Typeable a) => Binary (Thunk a) where
  put = Data.Binary.put . fromThunk
  get = do
    (ser :: Serialized a) <- Data.Binary.get
    return $ Thunk { fromThunk = ser }
~~~~

~~~~{.haskell}
-- forces a single value
forceSingle :: (Evaluatable a) => NodeId -> MVar a -> a -> Process ()
forceSingle node out a = do
  -- create the Channel that we use to send the 
  -- Sender of the input from the slave node from
  (inputSenderSender, inputSenderReceiver) <- newChan

  -- create the channel to receive the output from
  (outputSender, outputReceiver) <- newChan

  -- spawn the actual evaluation task on the given node
  -- and pass the two sender objects we created above
  spawn node (evalTask (inputSenderSender, outputSender))

  -- wait for the slave to send the input sender
  inputSender <- receiveChan inputSenderReceiver

  thunkA <- liftIO $ trySerialize a

  -- send the input to the slave
  sendChan inputSender $ toThunk thunkA

  -- wait for the result from the slave
  forcedA <- receiveChan outputReceiver

  -- put the output back into the passed MVar
  liftIO $ putMVar out forcedA
~~~~

~~~~{.haskell}
-- base evaluation task for easy instance declaration
evalTaskBase :: (Binary a, Typeable a, NFData a) => 
  (SendPort (SendPort (Thunk a)), SendPort a) -> Process ()
evalTaskBase (inputPipe, output) = do
  (sendMaster, rec) <- newChan

  -- send the master the SendPort, that we
  -- want to listen the other end on for the input
  sendChan inputPipe sendMaster

  -- receive the actual input
  thunkA <- receiveChan rec

  -- and deserialize
  a <- liftIO $ deserialize $ fromThunk thunkA

  -- force the input and send it back to master
  sendChan output (rnf a `seq` a)
~~~~

~~~~{.haskell}
-- evaluates a single value inside the Par monad
evalSingle :: Evaluatable a => Conf -> NodeId -> a -> Par a
evalSingle conf node a = do
  mvar <- newEmptyMVar
  let computation = forkProcess (localNode conf) $ forceSingle node mvar a
  return $ Comp { computation = computation >> return (), result = takeMVar mvar }
~~~~

~~~~{.haskell}
-- evaluates multiple values inside the Par monad
evalParallel :: Evaluatable a => Conf -> [a] -> Par [a]
evalParallel conf as = do
  workers <- readMVar $ workers conf

  -- shuffle the list of workers, so we don't end up spawning
  -- all tasks in the same order everytime
  shuffledWorkers <- randomShuffle workers

  -- complete the work assignment node to task (NodeId, a)
  let workAssignment = zipWith (,) (cycle shuffledWorkers) as

  -- build the parallel computation with sequence
  comps <- sequence $ map (uncurry $ evalSingle conf) workAssignment

  return $ sequenceComp comps
~~~~

~~~~{.haskell}
class (Binary a, Typeable a, NFData a) => Evaluatable a where
  evalTask :: (SendPort (SendPort (Thunk a)), SendPort a) -> Closure (Process ())
~~~~

## `Future`s

## Circular skeletons and issues with Laziness
