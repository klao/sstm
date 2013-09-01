STM with blocking detection
===========================

How do you test a program with several interdependent threads, which
communicate with each other via various `STM` primitives?  How do you
know when to feed it the next bit of input when you testing it with
emulated input?  You wait till all the threads are blocked waiting for
each other or input.

We achieve this by extending `STM` with the ability to report when
it's "blocked". First, we wrap `STM` transactions into "signalled
`STM`", or `SSTM`:

``` haskell
data SSTM a = SSTM { sTest :: STM Bool, sOp :: STM a }
```

The `sTest` should always be a non-blocking no-side-effects
transaction that returns True if-and-only-if sOp would block.

`SSTM` is `Applicative` and `Alternative` like `STM`, but it's not a
monad. It _could_ be, but we actually have a better solution, we have
a general way of turning any `STM` value into an `SSTM` value. The
naive idea almost works: you could just try the transaction and return
False if it goes through, and return True in an `orElse` branch. But,
the transaction could have side-effects... So, we just throw an
exception and `catchSTM` it to unwind all side-effects:


``` haskell
toSignalled :: STM a -> SSTM a
toSignalled op = SSTM test op
  where
    test = ((op >> throwSTM SSTMTranslation)
            `catchSTM` (\SSTMTranslation -> return False)) <|> return True
```


Next, instead of just running an `SSTM` with atomically, we record
the test part of it in variable specific to the current thread and
then atomically do the transaction and "unrecord" the test:

``` haskell
newtype BlockVar = BV (TVar (STM Bool))

signalledOnBlockVar :: BlockVar -> SSTM a -> IO a
signalledOnBlockVar (BV tvar) (SSTM test op) = do
  atomically $ writeTVar tvar test
  atomically $ op <* writeTVar tvar (return False)

isBlocked :: BlockVar -> STM Bool
isBlocked (BV tvar) = join $ readTVar tvar
```

Thus, `isBlocked` on a given `BlockVar` tells us exactly when is the
corresponding thread blocked. And we can wait for all of the essential
thread to be blocked at the same time.

Finally, to avoid common programming mistakes we wrap all essential
threads' `IO` computations in a special monad (called `Signalled`),
which take care of dealing with `BlockVar` and provides a convenient
`signalled` combinator—the equivalent of `atomically` for `STM`.

``` haskell
newtype BlockRegistry = BR (TVar [BlockVar])

runSignalled :: BlockRegistry -> Signalled a -> IO a

signalled :: SSTM a -> Signalled a

waitAllBlocked :: BlockRegistry -> STM ()
waitAllBlocked (BR registry) = mapM_ waitBlocked =<< readTVar registry
```

* * *


Example: naive shortest paths algorithm
=======================================

Learning about the shortest paths algorithms you visualize the
Bellman–Ford algorithm as nodes getting updates on their incoming
edges with their new distances, updating their current distance and
announcing it on outgoing edges if the new distance is smaller than
the previous ones. And edges just add their length to every "message"
passing on them. Can we implement this naive algorithm?

We'll need the `SSTM` mechanism for this, if every node and edge is a
separate thread and they only coordinate locally, they won't know when
to stop waiting for updates. Then one way how we can be sure that we
have the final distances is by ensuring that every node and edge is
blocked waiting for input.

But, otherwise the implementation is straightforward. Here's the
some interesting parts:

``` haskell
data Node = Node { _nIncoming :: TChan Length
                 , _nOutgoing :: TChan Length
                 , _nDistance :: TVar Length
                 }
makeLenses ''Node

currentDistance_ :: Node -> STM Length
currentDistance_ n = readTVar $ n ^. nDistance

newNode :: BlockRegistry -> IO Node
newNode registry = do
  inCh <- newTChanIO
  outCh <- newBroadcastTChanIO
  dist <- newTVarIO infiniteDist
  _ <- async $ runSignalled registry $ forever $ do
    d <- signalled $ readTChanSig inCh
    current <- signalled $ readTVarSig dist
    when (d < current) $ do signalled $ writeTVarSig dist d
                            signalled $ writeTChanSig outCh d
  return $ Node inCh outCh dist
```

So, the `Node` is just a pair of `TChan`s: on one it receives
incoming messages and broadcasts its new state on the other. Forever.

The `Edge` is very similar:

``` haskell
data Edge = Edge { _eIncoming :: TChan Length
                 , _eOutgoing :: TChan Length
                 , _eLength :: Length
                 }

newEdge :: Node -> Node -> Length -> BlockRegistry -> IO Edge
newEdge na nb len registry = do
  inCh <- atomically $ dupTChan $ na ^. nOutgoing
  let outCh = nb ^. nIncoming
  _ <- async $ runSignalled registry $ forever $ do
    d <- signalled $ readTChanSig inCh
    signalled $ writeTChanSig outCh (d+len)
  return $ Edge inCh outCh len
```

And the main program is trivial. We construct the graph, initializing
all nodes to have infinite distance. We change the distance of
starting node(s) to zero, triggering an avalanche of concurrent
updates. Wait for everything to settle down. Read out the current
distance of the nodes we are interested in:

``` haskell
main = do
  [fileName] <- getArgs
  g <- readGraph fileName
  decreaseDistance g 1 0   -- source node: #1
  waitSettled g
  distDest <- currentDistance g 3  -- destination node: #3
  print distDest
```

Discussion
----------

There are several reasons why computing shortest paths in a graph is
not practical this way. Keep in mind that this is just an
illustration. But, as an illustration for doing things in many
threads that coordinate only locally I think it's kind of neat.

*   First of all, make no mistake, this is __not__ the Bellman–Ford
    algorithm. With adversarial concurrency we can get exponential
    running time. Though, if the scheduling is fair, we get back the
    `O(n*e)` estimate.

*   Doing parallel computations with this granularity (we just add two
    numbers and switch to another thread) is very inefficient.

*   What we do here is basically a parallel computation, for example
    we arrive at a deterministically final state. But it looks
    inherently hard to turn this into an _actual_ parallel computation
    (with `Par` monad or something like that).

Conclusions
-----------

Our original reason for creating `SSTM` was to deterministically test
a concurrent program with many threads and complex data flow. (We only
need to mock out the threads that do the I/O and we know exactly when
to "feed" the other threads vs. when to expect results from them: when
they all blocked waiting for us.)  And for this `SSTM` works
exceptionally well. But maybe there is an even better way to do
that. And conversely, what other things is `SSTM` useful for?..
