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

### Startup harness and `RemoteTable`

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
In Section \ref{sec:parEvalCloudHaskell} we will see how we will resolve this problem.

## Parallel Evaluation with Cloud Haskell

\label{sec:parEvalCloudHaskell}

As already mentioned earlier, in Cloud Haskell we can not send arbitrary
functions or Arrows to the slave nodes. Thankfully, there is an alternative:
The serialization mechanism, that Eden uses internally
has been made available separately in a package called \enquote{packman}
^[see \url{https://hackage.haskell.org/package/packman}].
This mechanism allows values to be serialized
in the exact evaluation state they are currently in.

We can use this to our advantage. Instead of sending inputs and functions
to the slave nodes and sending the result back (which does not work with the current Cloud Haskell
API), we can instead apply the function, serialize this unevaluated thunk,
send it to the evaluating slave, and send the fully evaluated value back.

### Communication between nodes

#### Serialized data type 

The packman package comes with a serialization function
`trySerializeWith :: a -> Int -> IO (Serialized a)` (the second parameter is the buffer size)
and a deserialization function `deserialize :: Serialized a -> IO a`. Here,
`Serialized a` is the type containing the serialized value of `a`.

We can then define a wrapper type `Thunk a` around `Serialized a` as

~~~~{.haskell}
-- Wrapper for the packman type Serialized
newtype Thunk a = Thunk { fromThunk :: Serialized a } deriving (Typeable)

toThunk a = Thunk { fromThunk = a }
~~~~

Additionally, we require a `Binary` for our wrapper as well.

~~~~{.haskell}
instance (Typeable a) => Binary (Thunk a) where
  put = Data.Binary.put . fromThunk
  get = do
    (ser :: Serialized a) <- Data.Binary.get
return $ Thunk { fromThunk = ser }
~~~~

This instance is required so that we can send values of type `Thunk a`
to Cloud Haskell nodes.

#### Sending and Receiving data

\label{sec:sendRecCloud}

In order to send and receive data between nodes, Cloud Haskell uses typed channels.
A typed channel consists of a `SendPort a` and a `ReceivePort a`.
We can create a new typed cannel with the help of
`newChan :: Serializable a => Process (SendPort a, ReceivePort a)`:

~~~~{.haskell}
myProc :: Process ()
myProc = do
    (sendPort, receivePort) <- newChan
    
    -- do stuff
~~~~

Data can be sent with the help of `sendChan :: Serializable a => SendPort a -> a -> Process ()`:

~~~~{.haskell}
sendTen :: SendPort Int -> Process ()
sendTen sendPort = sendChan sendPort 10
~~~~

Values are received in a blocking manner with `receiveChan :: Serializable a => ReceivePort a -> Process a`:

~~~~{.haskell}
receiveVal :: ReceivePort Int -> Process Int
receiveVal receivePort = receivechan receivePort
~~~~

Note that only `SendPort a` is serializable here.
So in order to have a two way communication where process A
sends some input to process B and awaits its result like we require
in our use case,
we have to first receive a `SendPort a` in A via some 
`ReceivePort (SendPort a))` of some channel
`(SendPort (SendPort a), ReceivePort (SendPort a))`.
This `SendPort a` is sent by B and belongs to the channel `(SendPort a, ReceivePort a)`
where B expects its input to come through the `ReceivePort a`.
Additionally, we also require a channel `(SendPort b, ReceivePort b)` on which
B sends its result through the `SendPort b` and A awaits its result
on the `ReceivePort b`. This idea is executed in the following code example.
Process A looks like

~~~~{.haskell}
procA :: ReceivePort (SendPort a) -> ReceivePort b -> Process ()
procA aSenderReceiver bReceiver = do
    aSender <- receiveChan aSenderReceiver
    
    let someA = ...
    sendChan aSender someA
    
    someB <- receiveChan bReceiver
    ...
    
    return ()
~~~~

while process B is schematically defined as

~~~~{.haskell}    
procB :: SendPort (SendPort a) -> SendPort b -> Process ()
procB aSenderSender bSender = do
    (aSender, aReceiver) <- newChan
    
    sendChan aSenderSender aSender
    
    someA <- receiveChan aReceiver
    
    let someB = useAToMakeB someA
    
    sendChan bSender someB
~~~~

### Evaluation on slave nodes

Having discussed the communication scheme and serialization mechanism we want to use,
we can now go into detail how the evaluation on slave nodes works.

#### Master node

The following function `forceSingle :: NodeId -> MVar a -> a -> Process ()`
is used to evaluate a single value `a`. It returns a monadic action
 `Process ()` that evaluates a value of type `a` on the node with the given
 `NodeId` and stores the evaluated result in the given `MVar a`.
It starts by creating the necessary communication channels 
on the master side (the A side from Section \ref{sec:sendRecCloud}).
It then spawns the actual evaluation task (process B from Section \ref{sec:sendRecCloud})

~~~~{.haskell}
evalTask :: (SendPort (SendPort (Thunk a)), SendPort a) -> Process ()
~~~~

with the necessary `SendPort`s for input communication (`SendPort (SendPort (Thunk a))`)
and result communication (`SendPort a`^[here the type is `a` instead of
some potentially other `b` because we only evaluate some `a`]) on the given
node via
 
~~~~{.haskell}
spawn node (evalTask (inputSenderSender, outputSender))
~~~~

where `spawn` is of type

~~~~{.haskell}
spawn :: NodeId -> Closure (Process ()) -> Process ProcessId
~~~~

Then, like process A in Section \ref{sec:sendRecCloud},
`forceSingle` waits for the input `SendPort a` of the evaluation task with `receiveChan inputSenderReceiver`.
It then sends the serialized version of the `a` to be evaluated, `serialized <- liftIO $ trySerialize a`
over that `SendPort` with `sendChan inputSender $ toThunk serialized` to the evaluating
slave node. Then, it awaits the
result of the evaluation with `forcedA <- receiveChan outputReceiver`
to finally put it inside the passed `MVar a` with `liftIO $ putMVar out forcedA`.

~~~~{.haskell}
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

  serialized <- liftIO $ trySerialize a

  -- send the input to the slave
  sendChan inputSender $ toThunk serialized

  -- wait for the result from the slave
  forcedA <- receiveChan outputReceiver

  -- put the output back into the passed MVar
  liftIO $ putMVar out forcedA
~~~~

#### Slave node

In the definition of `forceSingle` we use a function

~~~~{.haskell}
evalTask :: (SendPort (SendPort (Thunk a)), SendPort a) -> Closure (Process ())
~~~~

As indicated by the `Evaluatable a` in the type signature,
this function is hosted on a `Evaluatable a` type class:

~~~~{.haskell}
class (Binary a, Typeable a, NFData a) => Evaluatable a where
    evalTask :: (SendPort (SendPort (Thunk a)), SendPort a) -> Closure (Process ())
~~~~

This is an abstraction required because of the way Cloud Haskell does serialization.
We can not write a single definition `evalTask` and expect it to work even though
it would be a valid definition. This is because for Cloud Haskell to be able to create
the required serialization code, at least in our tests, we require a
fixed type like for example for `Int`s:
`evalTaskInt :: (SendPort (SendPort (Thunk Int)), SendPort Int) -> Closure (Process ())`
If we then make this function remotable with `$(remotable [`'`evalTaskInt])`, we can write
a valid Cloud Haskell compatible instance `Evaluatable Int` simply as

~~~~{.haskell}
instance Evaluatable Int where
    evalTask = evalTaskInt
~~~~

These instances and evaluation tasks can however easily be generated with the Template Haskell code generator 
in Fig. \ref{fig:evalGen} from the Appendix via calls to the following three
Template Haskell functions:

~~~~{.haskell}
$(mkEvalTasks [''Int])
$(mkRemotables [''Int])
$(mkEvaluatables [''Int])
~~~~ 

This is possible because `evalTaskInt` just like any other function on types that
have instances for `Binary a`, `Typeable a`, and `NFData a` can be just delegated to
`evalTaskBase`, which we behaves as follows: Starting off, it creates the channel that
it wants to receive its input from with `(sendMaster, rec) <- newChan`. Then it
sends the `SendPort (Thunk a)` of this channel back to the master process via 
`sendChan inputPipe sendMaster` to then receive its actual input on the
`ReceivePort (Thunk a)` end with
`thunkA <- receiveChan rec`. It then deserializes this thunk with
`a <- liftIO $ deserialize $ fromThunk thunkA` and sends
the fully evaluated result back with `sendChan output (seq (rnf a) a)`.
Its complete definition is

~~~~{.haskell}
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
  sendChan output (seq (rnf a) a)
~~~~

### Parallel Evaluation on Slave nodes

Since we have discussed how to evaluate a value on slave nodes via
`forceSingle :: (Evaluatable a) => NodeId -> MVar a -> a -> Process ()`, we can
now use this to build up the internal API we require in order to fit the
`ArrowParallel` type class. For this we start by defining an abstraction of a
computation as

~~~~{.haskell}
data Computation a = Comp {
  computation :: IO (),
  result :: IO a
}
~~~~

where `computation :: IO ()` is the `IO ()` action that is required to be evaluated
so that we can get a result from `result :: IO a`.

Next is the definition of
`evalSingle :: Evaluatable => Conf -> NodeId -> a -> IO (Computation a)`.
Its resulting `IO` action starts by creating an empty `MVar a` with
`mvar <- newEmptyMVar`. Then it creates an `IO` action that forks
away the evaluation process of `forceSingle` 
on the single passed value `a` by means of `forkProcess :: LocalNode -> Process () -> IO ProcessId`
on the the master node with `forkProcess (localNode conf) $ forceSingle node mvar a`.
The action concludes by returning a `Computation a` encapsulating
the evaluation `IO ()` action and the result communication action
`takeMVar mvar :: IO a`:

~~~~{.haskell}
evalSingle :: Evaluatable a => Conf -> NodeId -> a -> IO (Computation a)
evalSingle conf node a = do
  mvar <- newEmptyMVar
  let computation = forkProcess (localNode conf) $ forceSingle node mvar a
  return $ Comp { computation = computation >> return (), result = takeMVar mvar }
~~~~

With this we can then easily define a function
`evalParallel :: Evaluatable a => Conf -> [a] -> IO (Computation [a])`
that builds an `IO` action containing a parallel `Computation [a]`
from an input list `[a]`. This IO action starts by retrieving the current list of
workers with `workers <- readMVar $ workers conf`. It then continues
by shuffling this list of workers with `shuffledWorkers <- randomShuffle workers`
^[`randomShuffle :: [a] -> IO [a]` from \url{https://wiki.haskell.org/Random_shuffle}]
to ensure at least some level of equal work distribution between multiple calls
to `evalParallel`. It then assigns the input values `a` to their 
corresponding workers to finally build the list of parallel computations `[Computation a]`
with `comps <- sequence $ map (uncurry $ evalSingle conf) workAssignment`. 
It concludes by turning this list `[Computation a]` into a computation of a list
`Computation [a]` with `return $ sequenceComp comps`.

~~~~{.haskell}
evalParallel :: Evaluatable a => Conf -> [a] -> IO (Computation [a])
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

Here, the definition of `sequenceComp :: [Computation a] -> Computation [a]` is

~~~~{.haskell}
sequenceComp :: [Computation a] -> Computation [a]
sequenceComp comps = Comp { computation = newComp, result = newRes } 
  where newComp = sequence_ $ map computation comps
        newRes = sequence $ map result comps
~~~~

## Circular skeletons and issues with Laziness
