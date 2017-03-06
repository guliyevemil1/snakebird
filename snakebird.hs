import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

type MyInt = Int
data Point = Point MyInt MyInt deriving (Show, Eq, Ord)

down = Point 0 (-1)
up = Point 0 1
left = Point (-1) 0
right = Point 1 0

instance Num Point where
    (Point x y) + (Point x' y') = Point (x+x') (y+y')
    _ * _ = undefined
    abs = undefined
    fromInteger n = Point (fromIntegral n) 0
    negate (Point x y) = Point (-x) (-y)
    signum = undefined

type PointSet = S.Set Point

type Snakebird = [Point]

data GameState = GameState {
    snakebird :: Snakebird,
    obstacles :: PointSet,
    fruits :: PointSet,
    goal :: Point
} deriving (Show, Eq, Ord)

side :: Point -> Snakebird -> [Point]
side p s = filter (`elem` s) $ map (+p) s

bottom = side down

isEmpty :: GameState -> Point -> Bool
isEmpty gs p@(Point x y) = and [y >= 0, not $ S.member p $ obstacles gs, not $ p `elem` (snakebird gs)]

allEmpty :: GameState -> [Point] -> Bool
allEmpty gs = null . filter (not . isEmpty gs)

moveSnakebird :: Point -> GameState -> GameState
moveSnakebird p gs = if allEmpty gs (side p (snakebird gs))
    then moveSnakebird p $ gs { snakebird = map (+p) (snakebird gs) }
    else gs

fall = moveSnakebird down

moveHead :: GameState -> Point -> [GameState]
moveHead gs p = do
    guard $ isEmpty gs p
    let hasFruit = S.member p (fruits gs)
    let newFruits = (if not hasFruit then id else S.delete p) (fruits gs)
    let newSnakebird = (if hasFruit then id else init) (p : snakebird gs)
    return $ gs { fruits = newFruits, snakebird = newSnakebird }

move :: GameState -> [GameState]
move gs = do
    dir <- [right, up, left, down]
    moveHead gs dir

main = do
    print gs
    mapM_ print (move gs)
    where
        gs = GameState [Point 0 0, Point (-1) 0] S.empty (S.singleton (Point 1 0)) (Point 1 2)
