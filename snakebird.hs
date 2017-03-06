{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Lens

type MyInt = Int
data Point = Point MyInt MyInt deriving (Show, Eq, Ord)
type Direction = Point

down = Point 0 (-1)
up = Point 0 1
left = Point (-1) 0
right = Point 1 0

instance Num Point where
    (Point x y) + (Point x' y') = Point (x+x') (y+y')
    fromInteger n = Point (fromIntegral n) 0
    negate (Point x y) = Point (-x) (-y)
    signum = undefined
    _ * _ = undefined
    abs = undefined

type PointSet = S.Set Point

data GameState = GameState {
        _snakebird :: [Point],
        _obstacles :: PointSet,
        _fruits :: PointSet,
        _goal :: Point
    } deriving (Show, Eq, Ord)

makeLenses ''GameState

type Snakebird a = StateT GameState [] a

side :: Point -> [Point] -> [Point]
side p ps = filter (`elem` ps) $ map (+p) ps

bottom = side down

isEmpty :: Point -> Snakebird Bool
isEmpty p@(Point x y) = do 
    guard $ y >= 0 
    hasNoObstacles <- not . S.member p <$> use obstacles
    guard hasNoObstacles
    doesNotContainSb <- not . (p `elem`) <$> use snakebird
    guard doesNotContainSb
    return True

allEmpty :: [Point] -> Snakebird Bool
allEmpty ps = and <$> mapM isEmpty ps 

moveSnakebird :: Point -> Snakebird ()
moveSnakebird p = do
    pointsAreEmpty <- use snakebird >>= allEmpty . side p
    guard pointsAreEmpty
    snakebird %= map (+p)
    moveSnakebird p

fall = moveSnakebird down

moveHead :: Point -> Snakebird ()
moveHead p = do
    canMove <- isEmpty p
    guard $ canMove
    hasFruit <- S.member p <$> use fruits
    fruits %= (if not hasFruit then id else S.delete p)
    snakebird %= (if hasFruit then id else init) . (p:)

move :: Snakebird ()
move = do
    dir <- lift [right, up, left, down]
    p <- ((+dir) . head) <$> use snakebird
    moveHead p

main = do
    print $ gs
    putStrLn "Running once"
    mapM_ print $ runStateT move gs
    putStrLn "Running twice"
    mapM_ print $ runStateT (move >> move) gs
    where
        gs = GameState [Point 0 0, Point (-1) 0] S.empty (S.singleton (Point 1 0)) (Point 1 2)
