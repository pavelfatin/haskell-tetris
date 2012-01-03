module Scene (Scene,
              createEmptyScene,
              areaOfScene,
              statsInScene,
              blocksInScene,
              isPiecePresentInScene,
              isValidScene,
              lowerPieceInScene,
              movePieceLeftInScene,
              movePieceRightInScene,
              dropPieceInScene,
              rotatePieceInScene,
              addPieceToScene) where

import Data.Function
import Data.List
import Data.Ord

import Cluster
import Data
import Piece
import Stats

data Scene = Scene { area :: Area, piece :: Cluster, field :: Cluster, stats :: Stats }

createEmptyScene :: Area -> Scene
createEmptyScene area = Scene area emptyCluster emptyCluster initialStats

movePieceLeftInScene :: Scene -> Scene
movePieceLeftInScene = tryToUpdatePieceInScene $ moveClusterInDirection DirectionLeft

movePieceRightInScene :: Scene -> Scene
movePieceRightInScene = tryToUpdatePieceInScene $ moveClusterInDirection DirectionRight

dropPieceInScene :: Scene -> Scene
dropPieceInScene scene = case (updatePieceInScene $ moveClusterInDirection DirectionDown) scene of
                            (Just nextScene) -> dropPieceInScene nextScene
                            Nothing -> scene

rotatePieceInScene :: Scene -> Scene
rotatePieceInScene = tryToUpdatePieceInScene rotateCluster

areaOfScene :: Scene -> Area
areaOfScene = area

statsInScene :: Scene -> Stats
statsInScene = stats

blocksInScene :: Scene -> [Block]
blocksInScene scene = blocksInCluster $ joinClusters (piece scene) (field scene)

isPiecePresentInScene :: Scene -> Bool
isPiecePresentInScene = not . isClusterEmpty . piece

lowerPieceInScene :: Scene -> Scene
lowerPieceInScene scene =
    case updatePieceInScene (moveClusterInDirection DirectionDown) scene of
      Just nextScene -> nextScene
      Nothing -> let newField = joinClusters (field scene) (piece scene)
                     nextScene = scene { piece = emptyCluster, field = newField } in
                 clearFullRowsInScene nextScene

addPieceToScene :: Piece -> Scene -> Scene
addPieceToScene blocks scene =
    let initialLocation = (Location (div (width $ areaOfScene scene) 2) 0)
        newPiece = centerClusterAtLocation initialLocation (createCluster blocks) in
    scene { piece = newPiece }

tryToUpdatePieceInScene :: (Cluster -> Cluster) -> Scene -> Scene
tryToUpdatePieceInScene f scene = maybe scene id (updatePieceInScene f scene)

updatePieceInScene :: (Cluster -> Cluster) -> Scene -> Maybe Scene
updatePieceInScene f scene =
    let newPiece = f $ piece scene
        newScene = scene { piece = newPiece } in
    if isValidScene newScene then Just newScene else Nothing

isValidScene :: Scene -> Bool
isValidScene (Scene area piece field _) =
    (isClusterWithinArea area piece) && (not (doClustersClash piece field))

clearFullRowsInScene :: Scene -> Scene
clearFullRowsInScene scene =
    let (Area w h) = areaOfScene scene
        fieldBlocks = blocksInCluster $ field scene
        allRows = groupBy (on (==) blockLine) (sortBy (comparing blockLine) fieldBlocks)
        partialRows = filter ((< w) . length) allRows
        cleanBlocks = concatMap id (map (uncurry withLine) (zip [pred h, (h - 2)..0] (reverse partialRows)))
        linesCount = (length allRows) - (length partialRows) in
    scene { field = createCluster cleanBlocks, stats = addLinesToStats linesCount (statsInScene scene) }
    where
        blockLine = (\(Block (Location _ line) _) -> line)
        withLine line = map (\(Block (Location x _) coloring) -> Block (Location x line) coloring)