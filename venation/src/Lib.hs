module Lib
  (
    initiateSources,
    Source(..),
    Branch(..),
    size,
    reach,
    plantTree,
    foldTree,
    start,
    State,
    Tree(..),
    step,
    calculateGrowth,
    closestBranches,
    branchToLine
  ) where

import Linear.Metric
import Data.Tree

import Genart

type Vec = V2 Double

newtype Source = Source (V2 Double) deriving Show
instance Draw Source where
  draw (Source (V2 x y)) = draw (circle (x .& y) 0.2)

type Props = (V2 Double, V2 Double)
type Branch = Tree Props
type State = ([Branch], [Source])

maxDist = 20
len = 1 :: Double
minDist = len

initiateSources :: Int -> Generate [Source]
initiateSources n = do
    randxs <- replicateM n ((0 :: Double) <=> (100 :: Double)) 
    randys <- replicateM n ((0 :: Double) <=> (100 :: Double))
    pure [Source (V2 x y) | (x, y) <- zip randxs randys]

plantTree :: Double -> Double -> Double -> Double -> Branch
plantTree x y dx dy  = Node (V2 x y, V2 dx dy) []

start :: Int -> Double -> Double -> Double -> Double -> Generate State
start n x y dx dy = do
  let root = plantTree x y dx dy
  sources <- initiateSources n 
  pure ([root], sources)

size :: Branch -> Int
size = foldTree (\_ sizeChildren -> 1 + sum sizeChildren)

spawnChild :: Branch -> Branch
spawnChild (Node (pos, dir) children) = Node (pos + len *^ dir, dir) []

distTo :: Branch -> Source -> Double
distTo (Node (posB, _) _) (Source posS) = distance posB posS

inRange :: Branch -> [Source] -> Bool
inRange branch sources = or [branch `distTo` source <= maxDist | source <- sources]

reach :: State -> State
reach (branches, sources) = let top = head branches in
  if top `inRange` sources
    then (branches, sources)
    else reach (spawnChild top : branches, sources)

check :: Source -> Branch -> V2 Double
check s@(Source posS) n@(Node (posB, _) _) =
  let dVec = posS - posB 
      d = norm dVec 
      dir = normalize dVec in
    if d < maxDist then
      dir
    else
      V2 0 0

tCheck :: Source -> Branch -> (V2 Double, Branch)
tCheck s b = (check s b, b)

tupledDistTo :: Branch -> Source -> (Double, Branch)
tupledDistTo branch source = (branch `distTo` source, branch)

closestTo :: [Branch] -> Source -> Branch
branches `closestTo` source = snd $ foldl (\(d1, b1) (d2, b2) -> if d1 < d2 then (d1, b1) else (d2, b2)) (100000000, head branches) (map (`tupledDistTo` source) branches)

closestBranches :: State -> [Branch]
closestBranches (branches, sources) = map (branches `closestTo`) sources

makeDirList :: [Branch] -> [(Source, Branch)] -> [V2 Double]
makeDirList branches closests
  = map
      (\ branch ->
         sum
           [if branch == c then check s c else V2 0 0 |
            (s, c) <- closests])
      branches

calculateGrowth :: State -> [V2 Double]
-- calculateGrowth (branches, sources) = foldr (zipWith (+)) (repeat 0) [map (check source) branches | source <- sources]

calculateGrowth state@(branches, sources) = let closests = closestBranches state in
  makeDirList branches (zip sources closests)
  -- [if b == changed then   | b <- branches, (dir, changed) <- [tCheck s b | (b, s) <- zip closests sources]]

removeReached :: State -> [Source]
removeReached (branches, sources) = [s | s <- sources, minimum (map (`distTo` s) branches) > minDist]

growBranch :: V2 Double -> Branch -> Generate [Branch]
growBranch dir n@(Node (posB, dirB) children) 
 | dir == V2 0 0 = return [n]
 | otherwise = do
   r <- randomVec (0, 0.2) (0, 0.2)
   let new = Node (posB, normalize (dirB + dir + r)) children
   return [n, spawnChild new]

normalizeDirs :: [Branch] -> [Branch]
normalizeDirs = map (\(Node (pos, dir) children) -> Node (pos, normalize dir) children)

growBranches :: [V2 Double] -> [Branch] -> Generate [Branch]
growBranches dirs branches = do
  bss <- sequence [growBranch d b | (d, b) <- zip dirs branches]
  return $ concat bss

step :: State -> Generate State
step state@(branches, sources) = do
  let dirs = calculateGrowth state
  nextBranches <- growBranches dirs branches
  return (nextBranches, removeReached state)

branchToLine :: Branch -> Line
branchToLine (Node (pos@(V2 x y), V2 dx dy) _) =
  Line (x .& y) (x + dx * len .& y + dy * len) 
