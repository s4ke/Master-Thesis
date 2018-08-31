## Experimental Cloud Haskell backend code

\label{sec:appendixCloudHaskell}

Finally, we include the Template Haskell based code
generator to make the experimental Cloud Haskell backend easier
to use and a version of the main Sudoku benchmark program as an example.

The code generator can be found in Figure \ref{fig:evalGen}. Here, if we enclose
this in a Haskell module, the functions `mkEvalTasks` (to generate the `evalTask`s for the
specific types), `mkRemotables` (to mark the evaluation tasks as remotable in Cloud Haskell)
and `mkEvaluatables` (to create the `Evaluatable` instance) are the ones exposed to the user. 

~~~~{#fig:evalGen
    .haskell
    .figure
    caption="The Template Haskell code generator for the Cloud Haskell backend."
    options=th}

nested :: Type -> Type -> Type
nested a b = a `AppT` (ParensT b)

tuple2 :: Type -> Type -> Type
tuple2 a b = (TupleT 2 `AppT` a) `AppT` b

fn :: Type -> Type -> Type
fn a b = (ArrowT `AppT` a) `AppT` b

nameToFnName :: Name -> Name
nameToFnName (Name (OccName str) _) = mkName $ ("__" ++ str ++ "_evalTaskImpl")

evalTaskFn :: Name -> Name -> Q [Dec]
evalTaskFn typeName fnName = do
	let sendPort = ConT $ mkName "SendPort"
	    thunk = ConT $ mkName "Thunk"
	    process = ConT $ mkName "Process"
	    firstTup = (sendPort `nested` (sendPort `nested` (thunk `nested` (ConT typeName))))
	    secondTup = sendPort `nested` (ConT typeName)
	    procNil = process `AppT` (TupleT 0)
	return [
			SigD fnName ((firstTup `tuple2` secondTup) `fn` procNil),
			FunD fnName [Clause [] (NormalB (VarE $ mkName "evalTaskBase")) []]
		]

evaluatableInstance :: Name -> Name -> Q [Dec]
evaluatableInstance typeName fnName = do
	let evaluatable = ConT $ mkName "Evaluatable"
	closure <- mkClosure fnName
	return [
			InstanceD (Nothing) [] (evaluatable `nested` ConT typeName) [
				FunD (mkName "evalTask") [Clause [] (NormalB closure) []]
			]
		]

mkEvalTasks :: [Name] -> Q [Dec]
mkEvalTasks names = do
	let fnNames = map nameToFnName names
  	(mapM (uncurry evalTaskFn) (zipWith (,) names fnNames)) >>= (return . concat)

mkRemotables :: [Name] -> Q [Dec]
mkRemotables names = do
	let fnNames = map nameToFnName names
	remotable fnNames

mkEvaluatables :: [Name] -> Q [Dec]
mkEvaluatables names = do
	let fnNames = map nameToFnName names
  	(mapM (uncurry evaluatableInstance) (zipWith (,) names fnNames)) >>= (return . concat)
~~~~

The Template Haskell version of the main Sudoku benchmark program can be found in
Figure \ref{fig:sudokuCloudHaskell}^[For the full code, see the GitHub repository at
\url{https://github.com/s4ke/Parrows/blob/e1ab76018448d9d4ca3ed48ef1f0c5be26ae34ab/CloudHaskell/testing/Test.hs}].
We have to write type aliases for `Maybe Grid` (`MaybeGrid`)
and `[Maybe Grid]` (`MaybeGridList`). We can then use these to generate the code
required to to evaluate these types in the Cloud Haskell backend with. In the
`main` program we have two cases: a) the program is started in master mode and starts
the computation, b) the program is started in slave mode and waits for computation
requests.
In order to launch this program and have speedup as well, we have
to start slave nodes for each cpu core with commands like
\enquote{<executable> slave 127.0.0.1 8000} where the last parameter determines the
port the slave will listen to and wait for requests on. Similarly a single master node can be started with
\enquote{<executable> master 127.0.0.1 7999} where, once again, the last parameter
determines the communication port.

~~~~{#fig:sudokuCloudHaskell
    .haskell
    .figure
    caption="The Template Haskell version of the Sudoku benchmark program."
    options=th}
type MaybeGrid = Maybe Grid
type MaybeGridList = [Maybe Grid]

-- remotable declaration for all eval tasks
$(mkEvalTasks [''MaybeGrid, ''MaybeGridList])
$(mkRemotables [''MaybeGrid, ''MaybeGridList])
$(mkEvaluatables [''MaybeGrid, ''MaybeGridList])

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      conf <- startBackend myRemoteTable Master host port
      readMVar (workers conf) >>= print

      grids <- fmap lines $ readFile "sudoku.txt"

      print (length (filter isJust (farm conf 4 solve grids)))
    ["slave", host, port] -> do
      startBackend myRemoteTable Slave host port
      print "slave shutdown."
~~~~

