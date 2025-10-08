module Go
  ( Owner(..)
  , territoryFor
  , territories
  ) where

import qualified Data.Set as S

-- Coordinate type (row, column)
type Coord = (Int, Int)

-- Owner of a territory
data Owner = Black | White | None
  deriving (Eq, Ord, Show)   -- ✅ Added Ord

-- Return all territories on the board
territories :: [String] -> [(Owner, [Coord])]
territories board = goAll empties S.empty
  where
    h = length board
    w = if null board then 0 else length (head board)

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
