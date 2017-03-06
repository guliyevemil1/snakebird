{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Snakebird where

import qualified Data.Set as S
import Data.Function (on)
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Lens

import Utils

type MyInt = Int
type Point = (MyInt, MyInt)
type Direction = Point

down = (0, -1)
up = (0, 1)
left = (-1, 0)
right = (1, 0)

instance Num Point where
    (x, y) + (x', y') = (x+x', y+y')
    fromInteger n = (fromIntegral n, 0)
    negate (x, y) = (-x, -y)
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
        _previous :: Maybe (Direction, GameState)
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
isEmpty p@(_, y) = do
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
    previous .= Just (dir, gs)
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

toList' :: GameState -> [Direction]
toList' gs = case _previous gs of
    Nothing -> []
    Just (d, gs') -> d : toList' gs'

toList = reverse . toList'

showSolution = map showDir . toList
    where
        showDir (1, 0) = "left"
        showDir (-1, 0) = "right"
        showDir (0, 1) = "up"
        showDir (0, -1) = "down"
        showDir x = show x
