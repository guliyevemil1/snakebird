module Level1 where

import qualified Data.Set as S
import Snakebird

sb = [[ (0,4), (-1,4) ]]
f = S.fromList $ [ (1,6), (-3,6) ]
o = S.fromList $ [ (-2, 4), (-2, 3), (-1, 3), (0, 3), (1, 3), (0, 6), (2, 6), (2, 7), (-3, 7) ]
gs = GameState 0 sb o S.empty f (0, 9) Nothing
solution = solve gs
