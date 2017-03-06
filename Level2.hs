module Level1 where

import qualified Data.Set as S
import Snakebird

sb = [ (0,4), (1,4), (1,5) ]
f = S.fromList $ [ (-4,5), (3,4) ]
o = S.fromList $ [ (1,3), (0,3), (-1,3), (-2,3), (-3,3), (-4,2), (-5,2), (-6,3), (-6,4), (-6,5), (-5,6), (-4,6), (-3,5)] 
g = (0,7)
gs = GameState sb o S.empty f g Nothing
solution = solve gs

main = print $ showSolution solution
