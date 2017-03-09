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

data Action = Move Direction | Switch

instance Show Action where
    show (Move (1, 0)) = "right"
    show (Move (-1, 0)) = "left"
    show (Move (0, 1)) = "up"
    show (Move (0, -1)) = "down"
    show Switch = "switch"

data GameState = GameState {
        _active :: Int,
        _snakebirds :: [[Point]],
        _obstacles :: PointSet,
        _spikes :: PointSet,
        _fruits :: PointSet,
        _goal :: Point,
        _previous :: Maybe (Action, GameState)
    } deriving (Show)

instance Eq GameState where
    a == b = compare a b == EQ

instance Ord GameState where
    compare a b = mconcat $ map (\f -> f a b) [
        compare `on` _active,
        compare `on` _snakebirds,
        compare `on` _fruits]

makeLenses ''GameState

currentSnakebird = do
    i <- use active
    use $ snakebirds . ix i

snakebirdHead = head <$> currentSnakebird

type Snakebird a = StateT GameState [] a

side :: Direction -> [Point] -> [Point]
side d ps = filter (not . (`elem` ps)) $ map (+d) ps

bottom = side down

isEmpty :: Point -> Snakebird Bool
isEmpty p@(_, y) = do
    isFruit <- S.member p <$> use fruits
    isObstacle <- S.member p <$> use obstacles
    return $ and [y >= 0, not isFruit, not isObstacle]

allEmpty :: [Point] -> Snakebird Bool
allEmpty ps = and <$> mapM isEmpty ps 

checkForSpikes :: Snakebird Bool
checkForSpikes = do
    sb <- currentSnakebird
    sp <- use spikes
    return $ null $ filter (`S.member` sp) sb

moveSnakebird :: Direction -> Snakebird ()
moveSnakebird d = do
    guardM checkForSpikes
    canMove <- currentSnakebird >>= allEmpty . side d
    when canMove $ do
        i <- use active
        snakebirds . ix i %= map (+d)
        moveSnakebird d

fall = moveSnakebird down

isSolved :: Snakebird Bool
isSolved = do
    fruitCount <- S.size <$> use fruits
    sbh <- snakebirdHead
    g <- use goal
    return (fruitCount == 0 && sbh == g)

canMoveHeadTo :: Point -> Snakebird Bool
canMoveHeadTo p@(_, y) = do
    isObstacle <- S.member p <$> use obstacles
    isPartOfSb <- (p `elem`) <$> currentSnakebird
    return $ and [y >= 0, not isObstacle, not isPartOfSb]

doAction :: Action -> Snakebird Bool
doAction (Move dir) = do
    p <- ((+dir) . head) <$> currentSnakebird
    guardM $ canMoveHeadTo p
    gs <- get
    previous .= Just (Move dir, gs)
    hasFruit <- S.member p <$> use fruits
    fruits %= if hasFruit then S.delete p else id
    i <- use active
    snakebirds . ix i %= (if hasFruit then id else init) . (p:)
    fall
    isSolved
doAction Switch = do
    sbCount <- length <$> use snakebirds
    active %= (`rem` sbCount) . succ
    return False

actions = Switch : map Move [right, up, left, down]

move :: Snakebird Bool
move = lift actions >>= doAction

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

toList' :: GameState -> [Action]
toList' gs = case _previous gs of
    Nothing -> []
    Just (d, gs') -> d : toList' gs'

toList = reverse . toList'

solve :: GameState -> [Action]
solve gs = toList (solve' S.empty [gs])
