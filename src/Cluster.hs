module Cluster (Cluster,
                emptyCluster,
                createCluster,
                blocksInCluster,
                isClusterEmpty,
                doClustersClash,
                isClusterWithinArea,
                moveClusterInDirection,
                joinClusters,
                rotateCluster,
                centerClusterAtLocation) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data

data Cluster = Cluster { blocks :: Map Location Coloring }

createCluster :: [Block] -> Cluster
createCluster blocks = Cluster $ Map.fromList [(location, coloring) | (Block location coloring) <- blocks]

emptyCluster :: Cluster
emptyCluster = Cluster Map.empty

blocksInCluster :: Cluster -> [Block]
blocksInCluster = (map $ uncurry Block) . Map.toList . blocks

isClusterEmpty :: Cluster -> Bool
isClusterEmpty = Map.null . blocks

doClustersClash :: Cluster -> Cluster -> Bool
doClustersClash (Cluster a) (Cluster b) = not $ Map.null $ Map.intersection a b

isClusterWithinArea :: Area -> Cluster -> Bool
isClusterWithinArea area = (all $ isLocationWithinArea area) . Map.keys . blocks

moveClusterInDirection :: Direction -> Cluster -> Cluster
moveClusterInDirection direction = Cluster . (Map.mapKeys $ moveLocationInDirection direction) . blocks

joinClusters :: Cluster -> Cluster -> Cluster
joinClusters (Cluster a) (Cluster b) = Cluster $ Map.union a b

rotateCluster :: Cluster -> Cluster
rotateCluster cluster@(Cluster blocks) =
    let tranformation = \(Location x y) -> Location (y) (- x)
        rotatedCluster = Cluster $ Map.mapKeys tranformation blocks in
        centerClusterAtLocation (centerLocationOfCluster cluster) rotatedCluster

centerClusterAtLocation :: Location -> Cluster -> Cluster
centerClusterAtLocation (Location x0 y0) cluster@(Cluster blocks) =
    let (Location x1 y1) = centerLocationOfCluster cluster
        dx = x1 - x0
        dy = y1 - y0
        transformation = \(Location x y) -> Location (x - dx) (y - dy) in
        Cluster $ Map.mapKeys transformation blocks

centerLocationOfCluster :: Cluster -> Location
centerLocationOfCluster (Cluster blocks) =
    let (xs, ys) = unzip $ map (\(Location x y) -> (x, y)) (Map.keys blocks)
        average ns = (div (sum ns) (length ns)) in
    Location (average xs) (average ys)