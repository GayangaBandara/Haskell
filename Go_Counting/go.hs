module Go
  ( Owner(..)
  , territoryFor
  , territories
  ) where

import qualified Data.Set as S
import qualified Data.List as L

-- We’ll represent a coordinate as (row, column), 1-based
type Coord = (Int, Int)

-- Who owns a territory
data Owner = Black | White | None
  deriving (Eq, Show, Ord)   -- Added Ord so we can use S.insert without error

-- Return all territories on the board:
-- For each group of connected empty intersections, figure out
-- which player’s stones border it.
territories :: [String] -> [(Owner, [Coord])]
territories board = goAll empties S.empty
  where
    h = length board
    w = case L.uncons board of
          Nothing       -> 0
          Just (row, _) -> length row

    -- List of all empty positions
    empties = [p | r <- [1..h], c <- [1..w], let p=(r,c), cell p==' ']

    cell (r,c) = board !! (r-1) !! (c-1)

    goAll [] _ = []
    goAll (p:ps) vis
      | p `S.member` vis = goAll ps vis
      | otherwise =
          let (owner, grp) = flood p
              vis' = vis `S.union` S.fromList grp
          in (owner, grp) : goAll ps vis'

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

        inBounds (r,c) = r>=1 && r<=h && c>=1 && c<=w
        neighbors (r,c) = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]

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
    w = case L.uncons board of
          Nothing       -> 0
          Just (row, _) -> length row

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
