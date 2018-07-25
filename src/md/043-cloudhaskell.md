# Experiment: Cloud Haskell Backend

Cloud Computing has become more and more prevalent in recent years. Servers are
replaced with virtualized servers positioned all around the globe.
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
API. For easier testing, we only work with a local-net Cloud Haskell
backend in this thesis. The results, however, are transferable to other architectures as well.

## Node discovery and program harness

In cloud services it is common that the architecture of the running network
changes more often than in ordinary computing clusters where the participating
nodes are usually known at startup. In the SimpleLocalNet Cloud Haskell
backend we are using for this experiment, this is reflected in the fact that
there already exists a pre-implemented master-slave structure.
The master node - the node that starts the computation is considered the master
node here - has to keep track of all the available slave nodes. The slave
nodes wait for tasks and handle them as required.

### The `State` data-structure

The data-structure containing all relevant information about
the state of the computation network and the computation in general 
we will use, `State`, is defined as

~~~~{.haskell}
data State = State {
  workers :: MVar [NodeId],
  shutdown :: MVar Bool,
  started :: MVar Bool,
  localNode :: LocalNode,
  serializeBufferSize :: Int
}
~~~~

Notice that `workers :: MVar [NodeId]`, `shutdown :: MVar Bool` and `started :: MVar ()`
are all low level mutable locations instead of regular fields.
This is because we pass this `State` around between functions,
but want it to be constantly be updated with new information.
These modifiable variables can be created empty with `newEmptyMVar :: IO (MVar a)` or
already with contents with `newMVar :: a -> IO (MVar a)`. They can be read with 
`readMVar :: MVar a -> IO a` or emptied with `takeMVar :: MVar a -> IO a`.
Values can be placed inside with `putMVar :: MVar a -> a -> IO ()`.
`MVar`s are threadsafe and all reading operations block until some content is placed in
them. We will see them used in other places of this backend as well.

`workers :: MVar [NodeId]` holds information about all available slave nodes,
`shutdown :: MVar Bool` determines whether the backend is to be shut down,
`started :: MVar ()` returns a signalling `()` if the backend has properly started when accessed
with `readMVar`.
`localNode :: LocalNode` and `serializeBufferSize :: Int` store information about
all Cloud Haskell internals for the master node and the buffer size for serialization (we will
discuss the system itself separately), respectively.

Note that as we will use the `State` type as the `conf` parameter in the `ArrowParallel` instance,
we use the type synonym `type Conf = State` in the following code sections.
Furthermore, an initial config can be created with the function `initialConf :: Int -> LocalNode -> IO Conf`
where the resulting config contains a `serializeBufferSize` as specified by the first
parameter and the `LocalNode` specified by the second parameter. Additionally, 
the list of workers `workers :: MVar [NodeId]` is initialized with an empty list,
`shutdown :: MVar Bool` is set to `False` and `started :: MVar ()` is created
as an empty `MVar` so that it can be populated with the signalling `()` when
the startup is finished.

~~~~{.haskell}
initialConf :: Int -> LocalNode -> IO Conf
initialConf serializeBufferSize localNode = do
  workersMVar <- newMVar []
  shutdownMVar <- newMVar False
  startedMVar <- newEmptyMVar
  return State {
    workers = workersMVar,
    shutdown = shutdownMVar,
    started = startedMVar,
    localNode = localNode,
    serializeBufferSize = serializeBufferSize
  }
~~~~

A utility function `defaultInitConf` using a default serialization buffer size
of 10MB is also defined as:

~~~~{.haskell}
defaultBufSize :: Int
defaultBufSize = 10 * 2^20 -- 10 MB

defaultInitConf :: LocalNode -> IO Conf
defaultInitConf = initialConf defaultBufSize
~~~~

### Starting Slave nodes

With the `State`/`Conf` data structure we can then implement a node-discovery scheme 
that works as follows: For slave nodes, we can just use the basic 
utilities for a slave backend in the SimpleLocalNet library. The code
to start a master node for the `Slave` backend is therefore:

~~~~{.haskell}
type Host = String
type Port = String

initializeSlave :: RemoteTable -> Host -> Port -> IO ()
initializeSlave remoteTable host port = do
  backend <- initializeBackend host port remoteTable
  startSlave backend
~~~~

We start a slave node by initializing the Cloud Haskell backend with a
given `host`, `port` and `remoteTable` via `initializeBackend :: String -> String -> RemoteTable`
and then delegating the logic completely to the library
function `startSlave :: Backend -> IO ()` which does not return unless the slave
is shutdown manually from the master node. The `RemoteTable` contains all  
serialization information about static values required by Cloud Haskell. We will 
later see how we can automatically generate such a table.

### Starting Master nodes

For master nodes, we have to do things a bit different. The actual
`startMaster :: Backend -> Process -> IO ()` supplied by SimpleLocalNet
is meant to start a computation represented by a `Process` monad and then return.
In our use-case we want to be able to spawn functions outside of the `Process` monad
however. We use the `Process` passed into this startup function only for slave-node
discovery and management:

~~~~{.haskell}
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
          redirectLogsHere backend slaveProcesses
          let slaveNodes = map processNodeId slaveProcesses
          liftIO $ do
              modifyMVar_ (workers conf) (\_ -> return slaveNodes)
              isEmpty <- isEmptyMVar $ started conf
              if (isEmpty && length slaveNodes > 0) then
                  putMVar (started conf) ()
              else
                return ()
~~~~

Basically, it continuously updates
the list of slaves inside the configuration by first querying for all slave processes with
`findSlaves backend` and redirecting the log output to the master node with
`redirectLogsHere backend slaveProcesses` to then finally update `workers :: MVar [NodeId]`
inside the configuration. Additionally, as soon as one slave is found, `started :: MVar ()`
is supplied with the signalling `()` so that any thread waiting for node discovery can start 
its actual computation. Notice that while we could add an additional sleep here
to not generate too much network noise in this function, we leave it out here for
the sake of brevity. All this is embedded in a checks whether a shutdown is requested
with `liftIO $ readMVar $ shutdown conf` and does the  necessary cleanup if instructed to do so -
terminating all slaves with `terminateAllSlaves backend` and shutting itself down with `die "terminated"` -
otherwise continuing with the updating process.
With this `master` function, we define our initialization function 
`initializeMaster :: RemoteTable -> Host -> Port -> IO Conf`:

~~~~{.haskell}
initializeMaster :: RemoteTable -> Host -> Port -> IO Conf
initializeMaster remoteTable host port = do
	backend <- initializeBackend host port remoteTable

	localNode <- newLocalNode backend

	conf <- defaultInitConf localNode

	forkIO $ startMaster backend (master conf backend)

	waitForStartup conf
	return conf
~~~~

Similar to the slave backend, we again initialize the backend via
`initializeBackend :: String -> String -> RemoteTable`, but also
create a new local node that is used to start computations outside of the 
initialization logic. With this node we then create a default initial config
via `defaultInitConf :: LocalNode -> Conf` which we can then pass into the 
discovery function with `startMaster backend (master conf backend)`.
We have to fork this `IO` action away with `forkIO`, because the `IO` action
will run forever as long as the 
program has not be manually shutdown via the corresponding variable in the `State`.
Finally, we wait for the startup to finish via `waitForStartup :: Conf -> IO ()` to
return a `IO Conf` action containing the initial config/state. Here, `waitForStartup` 
can simply be defined as

~~~~{.haskell}
waitForStartup :: Conf -> IO ()
waitForStartup conf = readMVar (started conf)
~~~~

because of the blocking behaviour of empty `MVar`s.

### Startup harness

If we put all this logic together we can then easily write a startup harness:

~~~~{.haskell}
myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ["master", host, port] -> do
      conf <- initializeMaster myRemoteTable host port
      
      -- read and print the list of available workers
      readMVar (workers conf) >>= print

      -- TODO: parallel computation here
      
    ["slave", host, port] -> do
      initializeSlave myRemoteTable host port
      print "slave shutdown."
~~~~

In order to launch a program using this harness, we have
to start slave nodes for each cpu core with commands like
\enquote{<executable> slave 127.0.0.1 8000} where the last parameter determines the
port the slave will listen to and wait for requests on. Similarly a single master node can be started with
\enquote{<executable> master 127.0.0.1 7999} where, once again, the last parameter
determines the communication port.

This example also shows how a `RemoteTable` is obtained so that it
can be used inside `main :: IO ()`.
Note, that the definition of `Main.__remoteTable :: RemoteTable -> RemoteTable`
used in `myRemoteTable :: RemoteTable` is an automatically, Template-Haskell
^[Template-Haskell is a code generator for Haskell written in Haskell,
and can be enabled with a language pragma `{-# LANGUAGE TemplateHaskell #-}` at the 
top of the source file] generated
function that builds a `RemoteTable` from the `initRemoteTable` from Cloud Haskell 
containing all relevant static declarations. In Cloud Haskell, we
can for example generate such a declaration for some function `f :: Int -> Int`,
with a call to `remotable` inside a Template-Haskell splice as `$(remotable [ `'`f ])`.

As can be seen from this, any function passed to `remotable` must have a top-level
declaration. Furthermore, we must also add any function manually. This is usually
okay for basic applications where the user usually knows which
functions/values need to be serialized statically at compile time,
but not in our use case as we want to be able
to send arbitrary functions/`Arrow`s to remote nodes to be evaluated.
In Section \ref{sec:parEvalCloudHaskell} we will see how to resolve this problem.

## Parallel Evaluation with Cloud Haskell

\label{sec:parEvalCloudHaskell}

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
