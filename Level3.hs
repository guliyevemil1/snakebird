module Level3 where

import qualified Data.Set as S
import Snakebird

sb = [[ (0,6), (-1,6), (-1,5) ]]
f = S.fromList $ [ (0,5), (5,5) ]
sp = S.fromList $ [ (1,4), (1,3) ]
g = (3,7)
o = S.fromList $ [ (-1,2), (0,2), (1,2), (2,3), (3,3), (4,3), (5,3), (6,3) ]
gs = GameState 0 sb o sp f g Nothing
solution = solve gs

main = print $ showSolution solution
