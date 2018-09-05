# Experiment: Cloud Haskell Backend

\label{sec:cloudHaskellExperiment}

Cloud Computing has become more and more prevalent in the recent years. Servers are
replaced with virtual ones positioned all around the globe.
These can easily be brought up when required and shut down
when they are not in use. This trend in computing has also been embraced by the Haskell
community, and therefore libraries such as Cloud Haskell were born. Cloud Haskell
is described on the project's website^[See \url{http://haskell-distributed.github.io/}.] as:

>Cloud Haskell: Erlang-style concurrent and distributed programming in Haskell.
The Cloud Haskell Platform consists of a generic network transport API,
libraries for sending static closures to remote nodes, a rich API for
distributed programming and a set of platform libraries modelled after
Erlang's Open Telecom Platform.
>
>Generic network transport backends have been developed for TCP and in-memory
messaging, and several other implementations are available including a
transport for Windows Azure.[...]
    
It is basically a set of APIs and libraries for communication between networks
of nodes in a cloud environment. With it, programmers can
write fully-featured, Haskell based cloud solutions targeting a wide range
of architectures.

While users can already write concurrent applications with the help of Cloud Haskell
using some of its libraries or even with the bare communication API, it seems like
a good idea to enable writing parallel programs requiring less involvement from the user.
This way, they can focus on parallel algorithms instead of manual communication.
In the following chapter, we will therefore explore the possibility of a Cloud Haskell
based backend for the PArrows DSL, while
explaining all the necessary
parts of Cloud Haskell's API. For easier testing and as this
is only meant as a proof of concept, we only work with a local-net Cloud Haskell
backend in this thesis. The results of this experiment, however, are transferable
to other architectures as well when building upon the results presented here.
^[With the help of virtual private networks one could even use this local-net variant.]

The following is structured as follows.
We start by explaining how to discover nodes with a master-slave
structure while also defining a program startup harness that
can be used with this scheme in Chapter \ref{sec:nodeDiscAndHarness}.
Then, we explain how parallel evaluation
of arbitrary data is possible with Cloud Haskell in Chapter \ref{sec:parEvalCloudHaskell}
and also discuss how we can implement the PArrows DSL with this
knowledge in Chapter \ref{sec:CloudHaskellArrowParallel}.

## Node discovery and program harness

\label{sec:nodeDiscAndHarness}

In cloud services, changes of the architecture in the running network
are more common than in ordinary computing clusters where the participating
nodes are usually known at startup. In the SimpleLocalNet^[See \url{http://hackage.haskell.org/package/distributed-process-simplelocalnet}.]
Cloud Haskell variant we are using for this experiment, this is reflected in the fact that
there already exists a pre-implemented master-slave structure.
The master node -- the node that starts the computation is considered the master
node here -- has to keep track of all the available slave nodes. The slave
nodes wait for tasks and handle them as required.

We will now first go into detail on the data-structure
(Chapter \ref{sec:cloudhaskellstate})
we use in order to handle
this information to then explain how to start slave (Chapter \ref{sec:cloudhaskellslaves})
and master nodes (Chapter \ref{sec:cloudhaskellmasters}). We also
explain how to create a startup harness
(Chapter \ref{sec:cloudhaskellstartupharness}) to wrap all of this. 

### The `State` data-structure

\label{sec:cloudhaskellstate}

The data-structure containing all relevant information about
the state of the network as well as the computation in general,
`State`, is defined as

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
are all low level mutable variables instead of regular fields.
This is because we pass this `State` around between functions,
but want to update it with new information on-the-fly.
These modifiable variables can be created empty with `newEmptyMVar :: IO (MVar a)` or
already with contents with `newMVar :: a -> IO (MVar a)`. They can be read with 
`readMVar :: MVar a -> IO a` or emptied with `takeMVar :: MVar a -> IO a`.
Values can be placed inside with `putMVar :: MVar a -> a -> IO ()`.
`MVar`s are thread-safe and all reading operations block until some content is placed in
them. We will see them used in other places of this backend as well.

In the `State` type,
`workers :: MVar [NodeId]` holds information about all available slave nodes,
`shutdown :: MVar Bool` determines whether the backend is to be shut down,
`started :: MVar ()` returns a signalling `()` if the backend has properly started when accessed
with `readMVar`.
`localNode :: LocalNode` and `serializeBufferSize :: Int` store information about
all Cloud Haskell internals for the master node and the buffer size for serialization (we will
discuss serialization system itself separately), respectively.

Note that as we will later use the `State` type as the `conf` parameter in the `ArrowParallel` instance,
we use the type synonym `type Conf = State` in the following code chapters.
Furthermore, an initial config can be created with the function `initialConf :: Int -> LocalNode -> IO Conf`
where the resulting config contains a `serializeBufferSize` as specified by the first
parameter and the `LocalNode` specified by the second parameter. Additionally, 
the list of workers, `workers :: MVar [NodeId]`, is initialized with an empty list,
`shutdown :: MVar Bool` is set to `False` and `started :: MVar ()` is created
as an empty `MVar` so that it can be populated with the signalling `()` when
the startup is finished. The complete code for `initialConf` is the following:

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

### Starting slave nodes

\label{sec:cloudhaskellslaves}

With the `State`/`Conf` data structure, we can now implement our node-discovery scheme.

For the slave nodes, we can just use the basic 
utilities for a slave backend in the SimpleLocalNet library. The code
to start a node for the `Slave` backend is then:

~~~~{.haskell}
type Host = String
type Port = String

initializeSlave :: RemoteTable -> Host -> Port -> IO ()
initializeSlave remoteTable host port = do
  backend <- initializeBackend host port remoteTable
  startSlave backend
~~~~

We here start by initializing the Cloud Haskell backend with a
given `host`, `port` and `remoteTable` via `initializeBackend :: String -> String -> RemoteTable`
to then delegate the logic completely to the library
function `startSlave :: Backend -> IO ()` which does not return unless the slave
is shutdown manually from the master node. The `RemoteTable` contains all
serialization information about static values required by Cloud Haskell. We will 
later see how we can automatically generate such a table.

### Starting master nodes

\label{sec:cloudhaskellmasters}

For master nodes, the implementation of the node-discovery scheme
is a bit more involved. The actual
`startMaster :: Backend -> Process -> IO ()` supplied by SimpleLocalNet
is meant to start a computation represented by a `Process` Monad and then return.
In our use-case, we want to be able to spawn functions outside of the `Process` Monad,
though. We therefore use the following function to build a process `Process ()`
which will be passed into `startMaster` only for the sake of slave-node
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

Basically, this continuously updates
the list of slaves inside the configuration by first querying for all slave processes with
`findSlaves backend` and redirecting the log output to the master node with
`redirectLogsHere backend slaveProcesses`, to then finally update `workers :: MVar [NodeId]`
inside the configuration. Additionally, as soon as one slave is found, `started :: MVar ()`
is supplied with the signalling `()`, so that any thread waiting for node discovery can start 
its actual computation.^[Notice that while we could add an additional sleep here
to not generate too much network noise in this function, we leave it out for
the sake of simplicity.] All of this is embedded in a check whether a shutdown is requested
with `liftIO $ readMVar $ shutdown conf`. If instructed to do so, the program 
does the necessary cleanup --
terminating all slaves with `terminateAllSlaves backend` and shutting itself down with `die "terminated"` -
otherwise continuing with the updating process.

With this `master` function, we can now define our initialization function 
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

Similar to the slave code, we initialize the Cloud Haskell backend via
`initializeBackend :: String -> String -> RemoteTable`, but also
create a new local node that is used to start computations outside of the 
initialization logic. With this node we can create a default initial config
via `defaultInitConf :: LocalNode -> Conf` which is then passed into the 
discovery function with `startMaster backend (master conf backend)`.
We have to fork this `IO` action away with `forkIO`, because the `IO` action
will run forever as long as the 
program has not been manually shutdown via the corresponding variable in the `State`.
Finally, we wait for the startup to finish via `waitForStartup :: Conf -> IO ()` to
end with returning a `IO Conf` action containing the initial config/state.
`waitForStartup` can simply be defined as

~~~~{.haskell}
waitForStartup :: Conf -> IO ()
waitForStartup conf = readMVar (started conf)
~~~~

because of the blocking behaviour of empty `MVar`s and the fact that we are signalling
the startup with a simple dummy value `()`, as described earlier.

### Startup harness

\label{sec:cloudhaskellstartupharness}

If we put together all of the startup logic we have discussed until now,
we can easily write a
startup harness in which we simply delegate to the proper initialization
code depending on the command line arguments:

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
port the slave will listen to and wait for requests on. Similarly, a single master node can be started with
\enquote{<executable> master 127.0.0.1 7999} where, once again, the last parameter
determines the communication port.

This example also shows how a `RemoteTable` is obtained so that it
can be used inside `main :: IO ()`.
Note, that the definition of `Main.__remoteTable :: RemoteTable -> RemoteTable`
used in `myRemoteTable :: RemoteTable` is a
function automatically generated by Template-Haskell
^[Template-Haskell is a code generator for Haskell written in Haskell,
and can be enabled with a language pragma `{-# LANGUAGE TemplateHaskell #-}` at the 
top of the source file.] building a `RemoteTable` on top of the `initRemoteTable`
supplied by Cloud Haskell by adding all relevant static declarations of
the program.
In Cloud Haskell, we
can for example generate such a declaration for some function `f :: Int -> Int`,
with a call to `remotable` inside a Template-Haskell splice as
`$(remotable [ `'`f ])`.

As can be seen from this, any function passed to `remotable` must have a top-level
declaration. Furthermore, we must also add any function manually. This is usually
okay for basic applications where the user knows which
functions/values need to be serialized statically at compile time,
but not in our use case, as we want to be able
to evaluate arbitrary functions/Arrows on remote nodes.
In Chapter \ref{sec:parEvalCloudHaskell}, we will see how to resolve this problem.

## Parallel evaluation with Cloud Haskell

\label{sec:parEvalCloudHaskell}

As we have seen in the previous chapter, we can not send arbitrary
functions or Arrows to the slave nodes. Thankfully, there is an alternative:
Eden's serialization mechanism
has been made available separately in a package called \enquote{packman}
^[See \url{https://hackage.haskell.org/package/packman}.].
This allows for values to be serialized
in the exact evaluation state they are currently in.

We can use this to our advantage. Instead of sending inputs and functions/Arrows
to the slave nodes and sending the result back (which does not work with the current Cloud Haskell
API), we can instead apply the function, serialize this unevaluated thunk,
send it to the evaluating slave, and communicate the fully evaluated value back
to the master.

With this idea in mind, we will now explain how to achieve parallel evaluation
of Arrows with Cloud Haskell. We start by explaining the communication basics
in Chapter \ref{sec:cloudhaskellCommBetweenNodes}.
Next, we describe how to achieve evaluation of single values on slave nodes in
Chapter \ref{sec:cloudhaskellEvaluationOnSlaves}. Finally, we use these results
to implement a parallel evaluation scheme in Chapter \ref{sec:cloudhaskellParallelEvaluation}.

### Communication basics

We will now go over the communication basics we require in the later parts
of this chapter. This includes a quick introduction
on how we actually send the data in Cloud Haskell and also a quick definition
of our serialized data wrapper
we use to send unevaluated data between nodes.

\label{sec:cloudhaskellCommBetweenNodes}

#### Sending and receiving data

\label{sec:sendRecCloud}

Cloud Haskell uses typed channels to send and receive data between nodes.
A typed channel consists of a `SendPort a` and a `ReceivePort a`.
We can create a new typed cannel with the help of
`newChan :: Serializable a => Process (SendPort a, ReceivePort a)`:

~~~~{.haskell}
myProc :: Process ()
myProc = do
    (sendPort, receivePort) <- newChan
    
    -- do stuff
~~~~

Data can be sent with `sendChan :: Serializable a => SendPort a -> a -> Process ()`:

~~~~{.haskell}
sendTen :: SendPort Int -> Process ()
sendTen sendPort = sendChan sendPort 10
~~~~

Values are received in a blocking manner with `receiveChan :: Serializable a => ReceivePort a -> Process a`:

~~~~{.haskell}
receiveVal :: ReceivePort Int -> Process Int
receiveVal receivePort = receiveChan receivePort
~~~~

One thing to keep in mind is that only `SendPort` are serializable.
So in order to have a two way communication where the master
sends some input to the slave and awaits its result, like we require
in our use case,
we have to first receive a `SendPort a` in the master via the 
`ReceivePort (SendPort a))` of some channel
`(SendPort (SendPort a), ReceivePort (SendPort a))`.
This `SendPort a` is sent the slave and belongs to the channel `(SendPort a, ReceivePort a)`
where it expects its input to come through the corresponding `ReceivePort a`.
Additionally, we also require a channel `(SendPort b, ReceivePort b)` on which
the slave sends its result through the `SendPort b` and the master awaits its result
on the `ReceivePort b`. We depict this process with schematically in Figure \ref{fig:cloudHaskellGeneralComm}.

![Required communication scheme for our Cloud Haskell backend. Actions corresponding to specific channels are marked with their respective colour.](src/img/CloudHaskellCommunication.pdf){#fig:cloudHaskellGeneralComm}

This idea is executed in the following code example.
The master looks like

~~~~{.haskell}
master :: ReceivePort (SendPort a) -> ReceivePort b -> Process ()
master aSenderReceiver bReceiver = do
    aSender <- receiveChan aSenderReceiver
    
    let someA = ...
    sendChan aSender someA
    
    someB <- receiveChan bReceiver
    ...
    
    return ()
~~~~

while the slave is schematically defined as

~~~~{.haskell}    
slave :: SendPort (SendPort a) -> SendPort b -> Process ()
slave aSenderSender bSender = do
    (aSender, aReceiver) <- newChan
    
    sendChan aSenderSender aSender
    
    someA <- receiveChan aReceiver
    
    let someB = useAToMakeB someA
    
    sendChan bSender someB
~~~~

#### Serialized data type 

The packman package comes with a serialization function
`trySerializeWith :: a -> Int -> IO (Serialized a)` (the second parameter is the buffer size)
and a deserialization function `deserialize :: Serialized a -> IO a`. Here,
`Serialized a` is the type containing the serialized value of `a`.

In order to have a clean slate in terms
of type class instances, we define a wrapper type `Thunk a` around `Serialized a` as

~~~~{.haskell}
-- Wrapper for the packman type Serialized
newtype Thunk a = Thunk { fromThunk :: Serialized a }
    deriving (Typeable)

toThunk a = Thunk { fromThunk = a }
~~~~

Additionally, we require a `Binary` for our wrapper in order
to be able to send it with Cloud Haskell. This only delegates to
the implementation of the actual `Serialized` we wrap:

~~~~{.haskell}
instance (Typeable a) => Binary (Thunk a) where
  put = Data.Binary.put . fromThunk
  get = do
    (ser :: Serialized a) <- Data.Binary.get
return $ Thunk { fromThunk = ser }
~~~~

### Evaluation of values on slave nodes

\label{sec:cloudhaskellEvaluationOnSlaves}

Having discussed the general communication scheme and serialization mechanism we want to use,
we can explain how the evaluation of values on slave nodes works with Cloud Haskell, next.
We give the master node's code
for evaluation of a single value on a slave node and also
the slave nodes' code.

#### Master node

\label{sec:cloudhaskellparEvalMasterNode}

On the master node, the function `forceSingle :: NodeId -> MVar a -> a -> Process ()`
is used to evaluate a single value `a`. It returns a monadic action
 `Process ()` that evaluates a value of type `a` on the node with the given
 `NodeId` and stores the evaluated result in the given `MVar a`.

Unlike the master from Chapter \ref{sec:sendRecCloud},
it starts by creating the top level communication channels

~~~~{.haskell}
(SendPort a, ReceivePort a)
~~~~

and

~~~~{.haskell}
(SendPort (SendPort (Thunk a)), ReceivePort (SendPort (Thunk a)))
~~~~ 
 
as we have to spawn the slave node from the master. This is different from
the schematic depiction we have seen earlier where the two main
channels were created outside of both functions.
The communication scheme is the same, nevertheless.
Furthermore, we intentionally do not have some `(SendPort b, ReceivePort b)`
as we only evaluate some `a` and not transform it.
Then, the master spawns the actual evaluation task
(the slave from Chapter \ref{sec:sendRecCloud})

~~~~{.haskell}
evalTask ::
    (SendPort (SendPort (Thunk a)), SendPort a) -> Process ()
~~~~

with the necessary `SendPort`s for input communication (`SendPort (SendPort (Thunk a))`)
and result communication (`SendPort a`^[Again, the type is `a` instead of
some potentially other `b` because we only evaluate some `a`.]) on the given
node via
 
~~~~{.haskell}
spawn node (evalTask (inputSenderSender, outputSender))
~~~~

where `spawn` is of type

~~~~{.haskell}
spawn :: NodeId -> Closure (Process ()) -> Process ProcessId
~~~~

Next, like the master from Chapter \ref{sec:sendRecCloud},
`forceSingle` waits for the input `SendPort a` of the evaluation task with `receiveChan inputSenderReceiver`.
After that, it sends the not yet evaluated, serialized version of `a`, `serialized <- liftIO $ trySerialize a`
over that `SendPort` with `sendChan inputSender $ toThunk serialized` to the evaluating
slave node. Finally, it awaits the
result of the evaluation with `forcedA <- receiveChan outputReceiver`
and puts it inside the passed `MVar a` with `liftIO $ putMVar out forcedA`.
The complete definition is:

~~~~{.haskell}
forceSingle :: (Evaluatable a) => NodeId -> MVar a -> a -> Process ()
forceSingle node out a = do
  -- create the Channel that we use to send the 
  -- sender of the input from the slave node from
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

\label{sec:cloudhaskellparEvalSlaveNode}

In the definition of `forceSingle`, we use a function

~~~~{.haskell}
evalTask ::
    (SendPort (SendPort (Thunk a)), SendPort a) -> Closure (Process ())
~~~~

As indicated by the `Evaluatable a` in the type signature,
this function is hosted on a `Evaluatable a` type class:

~~~~{.haskell}
class (Binary a, Typeable a, NFData a) => Evaluatable a where
    evalTask :: (SendPort (SendPort (Thunk a)), SendPort a) ->
        Closure (Process ())
~~~~

This abstraction is required because of the way Cloud Haskell does serialization.
We can not write a single definition `evalTask` and expect it to work even though
it would be a valid definition. This is because for Cloud Haskell to be able to create
the required serialization code, at least in our tests, we require a
fixed type, e.g. for `Int`s:

~~~~{.haskell}
evalTaskInt ::
    (SendPort (SendPort (Thunk Int)), SendPort Int) ->Closure (Process ())
~~~~

This function can be made remotable with `$(remotable [`'`evalTaskInt])`.
We can now write
a valid Cloud Haskell compatible instance `Evaluatable Int` simply as

~~~~{.haskell}
instance Evaluatable Int where
    evalTask = evalTaskInt
~~~~

We do not have to write these manually, though, as
as they can easily be generated with the Template Haskell code generator 
in Figure \ref{fig:evalGen} from the Appendix via calls to the following three
Template Haskell functions:

~~~~{.haskell}
$(mkEvalTasks [''Int])
$(mkRemotables [''Int])
$(mkEvaluatables [''Int])
~~~~ 

This is possible because `evalTaskInt`, just like any other function on types that
have instances for `Binary a`, `Typeable a`, and `NFData a`, can be just delegated to
`evalTaskBase`, which behaves as follows: It starts by creating the channel that
it wants to receive its input from with `(sendMaster, rec) <- newChan`. Next, it
sends the `SendPort (Thunk a)` of this channel back to the master process via 
`sendChan inputPipe sendMaster` to then receive its actual input on the
`ReceivePort (Thunk a)` end with
`thunkA <- receiveChan rec`. It finally deserializes this thunk with
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

### Parallel evaluation scheme

\label{sec:cloudhaskellParallelEvaluation}

Since we now know how to evaluate a value on slave nodes via
`forceSingle :: (Evaluatable a) => NodeId -> MVar a -> a -> Process ()`, we can
use this to build up an internal parallel evaluation scheme.
For this, we start by defining an abstraction of a
computation as

~~~~{.haskell}
data Computation a = Comp {
  computation :: IO (),
  result :: IO a
}
~~~~

where `computation :: IO ()` is the `IO ()` action that has to be evaluated
in order to get a result from `result :: IO a`.

Next is the definition of
`evalSingle :: Evaluatable => Conf -> NodeId -> a -> IO (Computation a)`.
Its resulting `IO` action starts by creating an empty `MVar a` with
`mvar <- newEmptyMVar`. Then, it creates an `IO` action that forks
away the evaluation process of `forceSingle` 
on the single passed value `a` by means of
`forkProcess :: LocalNode -> Process () -> IO ProcessId`
on the the master node with

~~~~{.haskell}
forkProcess (localNode conf) $ forceSingle node mvar a
~~~~

The action concludes by returning a `Computation a` encapsulating
the evaluation `IO ()` and the result communication action
`takeMVar mvar :: IO a`:

~~~~{.haskell}
evalSingle :: Evaluatable a => Conf -> NodeId -> a -> IO (Computation a)
evalSingle conf node a = do
  mvar <- newEmptyMVar
  let comp = forkProcess (localNode conf) $ forceSingle node mvar a
  return $ Comp { 
        computation = comp >> return ()
        result = takeMVar mvar
    }
~~~~

With this, we can easily define a function
`evalParallel :: Evaluatable a => Conf -> [a] -> IO (Computation [a])`
that builds an `IO` action containing a parallel `Computation [a]`
from an input list `[a]`. This `IO` action starts by retrieving the current list of
workers with `workers <- readMVar $ workers conf`. It continues
by shuffling this list of workers with `shuffledWorkers <- randomShuffle workers`
^[`randomShuffle :: [a] -> IO [a]` is from \url{https://wiki.haskell.org/Random_shuffle}.]
to ensure at least some level of equal work distribution between multiple calls
to `evalParallel`. Then, the input values `[a]` are assigned to their 
corresponding workers to finally build the list of parallel computations `[Computation a]`
with `comps <- sequence $ map (uncurry $ evalSingle conf) workAssignment`. 
The action concludes by turning this list `[Computation a]` into a computation of a list
`Computation [a]` with `return $ sequenceComp comps`.

~~~~{.haskell}
evalParallel :: Evaluatable a => Conf -> [a] -> IO (Computation [a])
evalParallel conf as = do
  workers <- readMVar $ workers conf

  -- shuffle the list of workers, so we don not end up spawning
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

Now, in order to start the actual computation from a blueprint in `Computation a` 
and get the result back as a pure value `a`, we have to use the function
`runComputation :: IO (Computation a) -> a` defined as follows:

Internally, it uses an `IO a` action that 
starts by unwrapping `Computation a` from the input `IO (Computation a)`
with `comp <- x` in order to launch the actual evaluation with `computation comp`.
The `IO` action concludes by returning the result with `result comp`. Now,
in order to turn the `IO a` action into `a`, we have to use
`unsafePerformIO :: IO a -> a` which allows us to unwrap the pure values
\enquote{contained} in `IO` actions.
Generally, the use of this function is discouraged, because it can introduce 
severe bugs if not handled with utmost care.
Here, its use is necessary and absolutely fine, though, since we only
do evaluation inside the `IO` Monad and if this were to fail,
the computation would be wrong anyways. Also in order to force the
compiler to not inline the result as we do not want to spawn the
computation multiple times, we protect the definition of `runComputation`
with a `NOINLINE` pragma:

~~~~{.haskell}
{-# NOINLINE runComputation #-}
runComputation :: IO (Computation a) -> a
runComputation x = unsafePerformIO $ do
  comp <- x
  computation comp
  result comp
~~~~

## Implementing the PArrows API

\label{sec:CloudHaskellArrowParallel}

Finally, we describe in this chapter how to implement the PArrows
API with Cloud Haskell and evaluate our results.

We start by
explaining how to implement `ArrowParallel` in Chapter \ref{sec:CloudHaskellArrowParallelInstance}.
Then, we discuss the limits of
the current code and explain why we can not yet give a proper instance for `ArrowLoopParallel` or a proper
`Future` implementation in Chapter \ref{sec:CloudHaskellArrowParallelLimits}. Finally, we
lay out a possible solution to this which could be implemented in the future in Chapter
\ref{sec:CloudHaskellArrowParallelLimitsMitigation}.

### `ArrowParallel` instance 

\label{sec:CloudHaskellArrowParallelInstance}

We will now give an experimental implementation of the `ArrowParallel` type class 
with Cloud Haskell. Obviously, as already mentioned earlier, here 
the additional conf parameter is the `State/Conf` type.

We implement `parEvalN` for our `ArrowParallel arr a b Conf` instance
as follows: We start off by forcing the input
`[a]` into normal form with `arr force`, as during testing this was 
found necessary because
a not fully evaluated value `a` can still have attached things like a file
handle which may be not serializable. Then, the parallel Arrow goes on to
feed the now fully forced input list `[a]`
into the evaluation Arrow obtained by applying `evalN :: [arr a b] -> arr [a] [b]`
to the list of Arrows to be parallelized `[arr a b]` with `evalN fs`. This results
in a not yet evaluated list of results `[b]` which is prepared to be forked away with
`arr (evalParallel conf) :: arr [a] (Computation [b])`. The resulting computation
blueprint is finally executed with `arr runComputation :: arr (Computation [b]) [b]`.

~~~~{.haskell}
instance (NFData a, Evaluatable b, ArrowChoice arr) =>
   ArrowParallel arr a b Conf where
    parEvalN conf fs = 
        arr force >>>
        evalN fs >>>
        arr (evalParallel conf) >>>
        arr runComputation
~~~~

### Limits of the current implementation

\label{sec:CloudHaskellArrowParallelLimits}

Similar to the GpH and `Par` Monad backends, our experimental Cloud Haskell backend suffers
from the problem that
it does not work in conjunction with the looping skeletons `pipe`, `ring` and `torus`.
All testing programs would refuse to compute anything
and hang indefinitely.
While this is no big problem for the shared-memory backends where we could just 
implement a workaround with the help of an `ArrowLoopParallel` instance

~~~~ {.haskell}
instance (ArrowChoice arr, ArrowParallel arr a b Conf) =>
	ArrowLoopParallel arr a b Conf where
    loopParEvalN _ = evalN
    postLoopParEvalN = parEvalN
~~~~

a similar solution would not be feasible here because we are
in a distributed-memory setting.
The topology skeletons would become meaningless as all benefits of
using a sophisticated distributed evaluation scheme would be lost.

Since it would not make sense to have a `Future` instance without
proper support for skeletons that could make use of it, we also do 
not give an implementation for a `CloudFuture` in this thesis.

### Possible mitigation of the limits

\label{sec:CloudHaskellArrowParallelLimitsMitigation}

While investigating the problem with the looping skeletons, we noticed
a difference in behaviour between Eden and all other backends including
our experimental Cloud Haskell backend: Eden streams lists of data `[a]`
instead of sending the complete list as one big serialized chunk. Another difference is
that tuples of data `(a, b, ...)` are sent in parallel on $n$ threads
for a tuple of $n$ entries.
When investigating the `torus` or `ring` skeletons we ported from Eden,
we notice how these two specialities are important. For example, in the `ring` skeleton
we build up the resulting Arrow so that it calculates the result in multiple
rounds:

~~~~{.haskell}
ring conf f =
    loop (second (rightRotate >>> lazy) >>>
        -- convert the current input into a form we can process in this round
        arr (uncurry zip) >>>
        -- here, we evaluate the current round
        loopParEvalN conf
            (repeat (second (get conf) >>> f >>> second (put conf))) >>>
        -- put the current result back into the original input form
        arr unzip) >>>
    postLoopParEvalN conf (repeat (arr id))
~~~~

Here, anything other than the exact same behaviour as Eden will result in a dead-lock
when using `loopParEvalN = parEvalN`.
We therefore believe that it is crucial for a proper Cloud Haskell backend to have the same
streaming behaviour as Eden does. While we are confident that this is definitely possible
to achieve with Cloud Haskell, as early experiments on this suggest, we have to date
not been able to achieve proper streaming behaviour, though.
We stopped further development here,
as this would have bursted the scope of this thesis.

Nevertheless, the most promising idea to implement
the correct behaviour is to use a more sophisticated mechanism to stream data
back to the master node by using pipes along the lines of

~~~~{.haskell}
type PipeIn a = SendPort (SendPort (Maybe (SendPort (Maybe a))))
type PipeOut a = ReceivePort (SendPort (Maybe (SendPort (Maybe a))))
~~~~

where `PipeIn a` would be the port where the evaluating process on the slave node
sends its result through to the corresponding `PipeOut a` on the master node.
Note that we here encode a \enquote{stream} of some `a` with `SendPort (Maybe a)`:
For types with singular values, we just request one value. And on types like e.g.
a list `[a]` we expect multiple singleton lists `Just [a]`,
on the `SendPort` and the end of the input with `Nothing`.
For other multi-valued types,
this would work similar even if some hacks would be required.

Then, in order to communicate the result from the slave node, we would
first send `SendPort (Maybe (SendPort (Maybe a)))` on which the slave-node
would want to receive the stream of `SendPort (Maybe a)`. This stream
of `SendPort`s is required instead of a singular `SendPort`
because of types that have to be sent by multiple threads
like e.g. tuples. Via these `SendPort (Maybe a)`s the slave can then finally
communicate the stream of evaluated results back to the master node.
A corresponding communication scheme doing the
necessary opposite tasks would obviously be required on the master node.

During testing, as already mentioned, we were not successful in making this idea work
with the looping skeletons.^[It did however still work with non-looping skeletons.]
We still believe that this path is worth exploring further in the future, though.
