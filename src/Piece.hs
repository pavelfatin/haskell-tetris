module Piece (Piece, pieces) where

import Data

type Piece = [Block]

pieces :: [Piece]
pieces = [[Block location coloring | location <- locations] | (locations, coloring) <- shapes]

shapes :: [([Location], Coloring)]
shapes = [([Location 0 0, Location 1 0, Location 2 0, Location 3 0], Coloring 0.7 0.4 0.4),
          ([Location 0 0, Location 1 0, Location 2 0, Location 1 1], Coloring 0.4 0.7 0.4),
          ([Location 0 0, Location 1 0, Location 0 1, Location 1 1], Coloring 0.4 0.4 0.7),
          ([Location 0 0, Location 0 1, Location 1 1, Location 1 2], Coloring 0.7 0.4 0.7),
          ([Location 1 0, Location 1 1, Location 0 1, Location 0 2], Coloring 0.4 0.7 0.7),
          ([Location 0 0, Location 1 0, Location 2 0, Location 2 1], Coloring 0.7 0.7 0.4),
          ([Location 0 1, Location 1 1, Location 2 1, Location 2 0], Coloring 0.7 0.7 0.7)]