{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.SSTM
import Control.Lens
import Control.Monad
import Data.Map.Strict (Map,(!))
import Data.Monoid
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

type Length = Double

infiniteDist :: Length
infiniteDist = read "Infinity"

--------------------------------------------------------------------------------
-- Node

data Node = Node { _nIncoming :: TChan Length
                 , _nOutgoing :: TChan Length
                 , _nDistance :: TVar Length
                 }
makeLenses ''Node

currentDistance_ :: Node -> STM Length
currentDistance_ n = readTVar $ n ^. nDistance

decreaseDistance_ :: Node -> Length -> IO ()
decreaseDistance_ (Node _ outgoing dist) d = do
  current <- atomically $ readTVar dist
  when (d > current) $ error "Increasing distance"
  atomically $ writeTVar dist d
  atomically $ writeTChan outgoing d

newNode :: BlockRegistry -> IO Node
newNode registry = do
  inCh <- newTChanIO
  outCh <- atomically $ newBroadcastTChan -- Bug in STM, see: http://ghc.haskell.org/trac/ghc/ticket/7986
  dist <- newTVarIO infiniteDist
  _ <- async $ runSignalled registry $ forever $ do
    d <- signalled $ readTChanSig inCh
    current <- signalled $ readTVarSig dist
    when (d < current) $ do signalled $ writeTVarSig dist d
                            signalled $ writeTChanSig outCh d
  return $ Node inCh outCh dist

--------------------------------------------------------------------------------
-- Edge

data Edge = Edge { _eIncoming :: TChan Length
                 , _eOutgoing :: TChan Length
                 , _eLength :: Length
                 }
--makeLenses ''Edge

newEdge :: Node -> Node -> Length -> BlockRegistry -> IO Edge
newEdge na nb len registry = do
  inCh <- atomically $ dupTChan $ na ^. nOutgoing
  let outCh = nb ^. nIncoming
  _ <- async $ runSignalled registry $ forever $ do
    -- TODO(klao): add a way to change edge lengths too!
    d <- signalled $ readTChanSig inCh
    signalled $ writeTChanSig outCh (d+len)
  return $ Edge inCh outCh len

--------------------------------------------------------------------------------
-- Graph

data Graph = Graph { _nodes :: Map Int Node
                   , _edges :: Map (Int,Int) [Edge]
                   , _blockRegistry :: BlockRegistry
                   }
makeLenses ''Graph

emptyGraph :: IO Graph
emptyGraph = do
  registry <- newBlockRegistryIO
  return $ Graph M.empty M.empty registry

addNode :: Int -> Graph -> IO Graph
addNode i g = do
  n <- newNode $ g ^. blockRegistry
  return $ g & nodes.at i .~ Just n

addEdge :: Int -> Int -> Length -> Graph -> IO Graph
addEdge i j len g@(Graph ns _ registry) = do
  when (not $ M.member i ns && M.member j ns) $ error "Nodes not in the graph"
  e <- newEdge (ns!i) (ns!j) len registry
  return $ g & edges.at (i,j) %~ mappend (Just [e])

decreaseDistance :: Graph -> Int -> Length -> IO ()
decreaseDistance g n len = maybe (error "No such node") (decreaseDistance_ ?? len) $ g ^. nodes.at n

currentDistance :: Graph -> Int -> IO Length
currentDistance g n = atomically $ maybe (error "No such node") currentDistance_ $ g ^. nodes.at n

waitSettled :: Graph -> IO ()
waitSettled g = atomically $ waitAllBlocked $ g ^. blockRegistry


--------------------------------------------------------------------------------

-- exampleGraph :: IO Graph
-- exampleGraph = do
--   emptyGraph
--     >>= addNode 1
--     >>= addNode 2
--     >>= addNode 3
--     >>= addEdge 1 2 2
--     >>= addEdge 2 3 (-1)
--     >>= addEdge 1 3 1.5

readGraph :: FilePath -> IO Graph
readGraph file = do
  (h:ls) <- fmap lines $ readFile file
  emptyGraph
    >>= addNodes h
    >>= addEdges ls
    where
      addNodes header graph = do
        let numNodes = read header
        print $ "Number of nodes: " ++ show numNodes
        foldM (\g i -> addNode i g) graph [1..numNodes]

      addEdges ls graph =
        foldM (\g l -> let (a,b,len) = readEdge l in addEdge a b len g) graph ls

readEdge :: String -> (Int, Int, Length)
readEdge s = (a,b,l)
  where
    [(a, r1)] = reads s
    [(b, r2)] = reads r1
    [(l, "")] = reads r2

--------------------------------------------------------------------------------

main :: IO ()
main = do
  [fileName] <- getArgs
  g <- readGraph fileName
  decreaseDistance g 1 0
  waitSettled g
  distDest <- currentDistance g 3
  print distDest
