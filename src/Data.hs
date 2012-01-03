module Data where

data Direction = DirectionUp | DirectionDown | DirectionLeft | DirectionRight

data Location = Location { x :: Int, y :: Int } deriving (Eq, Ord)

data Area = Area { width :: Int, height :: Int }

data Coloring = Coloring { red :: Float, green :: Float, blue :: Float }

data Block = Block { location :: Location, coloring :: Coloring }

moveLocationInDirection :: Direction -> Location -> Location
moveLocationInDirection direction (Location x y) =
    case direction of
        DirectionUp -> Location x (pred y)
        DirectionDown -> Location x (succ y)
        DirectionLeft -> Location (pred x) y
        DirectionRight -> Location (succ x) y

isLocationWithinArea :: Area -> Location -> Bool
isLocationWithinArea (Area w h) (Location x y) = x >= 0 && x < w && y < h