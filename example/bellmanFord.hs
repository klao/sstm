import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs)

type Length = Double

infiniteDist :: Length
infiniteDist = read "Infinity"

type Graph = (Int, [(Int,Int,Length)])

readGraph :: FilePath -> IO Graph
readGraph file = do
  (h:ls) <- fmap lines $ readFile file
  return $ (read h, map readEdge ls)

readEdge :: String -> (Int, Int, Length)
readEdge s = (a-1,b-1,l)
  where
    [(a, r1)] = reads s
    [(b, r2)] = reads r1
    [(l, "")] = reads r2

bellmanFord :: Graph -> Int -> Length
bellmanFord (n, edges) = (result VU.!)
  where
    ini = VU.singleton 0 VU.++ VU.replicate (n-1) infiniteDist
    result = go n ini
    go k v | k < 0 = error "Negative cycle"
           | otherwise  = if v' == v then v else go (k-1) v'
      where
        v' = VU.accum min v [ (j,d) | (i,j,l) <- edges, let d = v VU.! i + l ]

main :: IO ()
main = do
  [fileName] <- getArgs
  g <- readGraph fileName
  print $ bellmanFord g 2
