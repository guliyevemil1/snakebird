{-# LANGUAGE TemplateHaskell #-}

module Snakebird where

import qualified Data.Set as S
import Data.Function (on)
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Lens

import Utils

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
        _spikes :: PointSet,
        _fruits :: PointSet,
        _goal :: Point,
        _previous :: Maybe GameState
    } deriving (Show)

instance Eq GameState where
    a == b = compare a b == EQ

instance Ord GameState where
    compare a b = mconcat $ map (\f -> f a b) [
        compare `on` _snakebird,
        compare `on` _fruits]

makeLenses ''GameState

snakebirdHead = head <$> use snakebird

type Snakebird a = StateT GameState [] a

side :: Direction -> [Point] -> [Point]
side d ps = filter (not . (`elem` ps)) $ map (+d) ps

bottom = side down

isEmpty :: Point -> Snakebird Bool
isEmpty p@(Point _ y) = do
    isObstacle <- S.member p <$> use obstacles
    isPartOfSb <- (p `elem`) <$> use snakebird
    return $ and [y >= 0, not isObstacle, not isPartOfSb]

allEmpty :: [Point] -> Snakebird Bool
allEmpty ps = and <$> mapM isEmpty ps 

checkForSpikes :: Snakebird Bool
checkForSpikes = do
    sb <- use snakebird
    sp <- use spikes
    return $ null $ filter (`S.member` sp) sb

moveSnakebird :: Direction -> Snakebird ()
moveSnakebird d = do
    canMove <- use snakebird >>= allEmpty . side d
    if not canMove
    then return ()
    else do
        snakebird %= map (+d)
        guardM checkForSpikes
        moveSnakebird d

fall = moveSnakebird down

isSolved :: Snakebird Bool
isSolved = do
    fruitCount <- S.size <$> use fruits
    sbh <- snakebirdHead
    g <- use goal
    return (fruitCount == 0 && sbh == g)

moveHead :: Direction -> Snakebird Bool
moveHead dir = do
    p <- ((+dir) . head) <$> use snakebird
    guardM $ isEmpty p
    gs <- get
    previous .= Just gs
    hasFruit <- S.member p <$> use fruits
    fruits %= if hasFruit then S.delete p else id
    snakebird %= (if hasFruit then id else init) . (p:)
    fall
    isSolved

move :: Snakebird Bool
move = do
    dir <- lift [right, up, left, down]
    moveHead dir

solve'' :: S.Set GameState -> [GameState] -> [(Bool, GameState)]
solve'' s gs = gs >>= filter (not . (`S.member` s) . snd) . runStateT move

solve' :: S.Set GameState -> [GameState] -> GameState
solve' s gs = case filter fst res of
    [] -> solve' s' gs'
    ((_,x):_) -> x
    where
        res = solve'' s gs
        gs' = map snd res
        s' = foldr S.insert s gs'

solve :: GameState -> GameState
solve gs = solve' S.empty [gs]

main = do
    print $ gs
    print $ solve gs
    where
        gs = GameState [Point 0 0, Point (-1) 0, Point (-2) 0] S.empty S.empty (S.singleton (Point 1 0)) (Point 1 2) Nothing
