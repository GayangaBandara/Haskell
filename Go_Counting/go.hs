module Go
  ( Owner(..)
  , territoryFor
  , territories
  ) where

import qualified Data.Set as S

-- We’ll represent a coordinate as (row, column), 1-based
type Coord = (Int, Int)

-- Who owns a territory
data Owner = Black | White | None
  deriving (Eq, Show)

-- Return all territories on the board:
-- For each group of connected empty intersections, figure out
-- which player’s stones border it.
territories :: [String] -> [(Owner, [Coord])]
territories board = goAll empties S.empty
  where
    h = length board                         -- number of rows
    w = if null board then 0 else length (head board)  -- number of columns

    -- List of all empty positions
    empties = [p | r <- [1..h], c <- [1..w], let p=(r,c), cell p==' ']

    cell (r,c) = board !! (r-1) !! (c-1)     -- get character at coordinate

    -- Go through all empty cells, skip ones already visited,
    -- flood-fill each group and determine its owner.
    goAll [] _ = []
    goAll (p:ps) vis
      | p `S.member` vis = goAll ps vis
      | otherwise =
          let (owner, grp) = flood p
              vis' = vis `S.union` S.fromList grp
          in (owner, grp) : goAll ps vis'

    -- Flood-fill from one empty cell
    flood start = explore S.empty S.empty [start]
      where
        explore empties owners [] = (ownerFrom owners, S.toList empties)
        explore empties owners (q:qs)
          | q `S.member` empties = explore empties owners qs
          | otherwise =
              let ns = neighbors q
                  (newEmpty, newOwners) = foldr classify ([], owners) ns
              in explore (S.insert q empties) newOwners (qs ++ newEmpty)

        -- For each neighbour, collect more empty cells or bordering stone colours
        classify n (es, os)
          | inBounds n && cell n == ' ' = (n:es, os)
          | inBounds n && cell n == 'B' = (es, S.insert Black os)
          | inBounds n && cell n == 'W' = (es, S.insert White os)
          | otherwise = (es, os)

        inBounds (r,c) = r>=1 && r<=h && c>=1 && c<=w
        neighbors (r,c) = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]

    -- If only one colour touches the empty area, that’s the owner.
    ownerFrom os
      | os == S.singleton Black = Black
      | os == S.singleton White = White
      | otherwise               = None

-- Get the territory for a single coordinate, or Nothing if it’s not empty.
territoryFor :: [String] -> Coord -> Maybe (Owner, [Coord])
territoryFor board p
  | not (inBounds p) = Nothing
  | cell p /= ' '    = Nothing
  | otherwise        = Just (flood p)
  where
    h = length board
    w = if null board then 0 else length (head board)

    cell (r,c) = board !! (r-1) !! (c-1)
    inBounds (r,c) = r>=1 && r<=h && c>=1 && c<=w

    flood start = explore S.empty S.empty [start]
      where
        explore empties owners [] = (ownerFrom owners, S.toList empties)
        explore empties owners (q:qs)
          | q `S.member` empties = explore empties owners qs
          | otherwise =
              let ns = neighbors q
                  (newEmpty, newOwners) = foldr classify ([], owners) ns
              in explore (S.insert q empties) newOwners (qs ++ newEmpty)

        classify n (es, os)
          | inBounds n && cell n == ' ' = (n:es, os)
          | inBounds n && cell n == 'B' = (es, S.insert Black os)
          | inBounds n && cell n == 'W' = (es, S.insert White os)
          | otherwise = (es, os)

        neighbors (r,c) = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]

    ownerFrom os
      | os == S.singleton Black = Black
      | os == S.singleton White = White
      | otherwise               = None
