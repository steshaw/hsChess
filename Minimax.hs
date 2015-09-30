module Minimax where

import Board
import Evaluator
import Moves

data GameTree = GameTree {state::State, gameTree::[GameTree]}

-- maximal depth of the tree
depth::Int
depth = 3

prettyGameTree::GameTree->String
prettyGameTree = prettyGameTree2 0
   where prettyGameTree2 x (GameTree z bs) = prettyBoardIndent (10*x) (snd z) ++ 
                                               ' ':show (evalState z) ++ 
                                               concatMap (prettyGameTree2 (x+1)) bs

evalState::State->Int
evalState = evalBoard . snd

genGameTree::Int->State->GameTree
genGameTree 0 z = GameTree z []
genGameTree maxdepth z | finalState z = GameTree z []
                       | otherwise = GameTree z (map (genGameTree (maxdepth-1)) (nextStates z))

-- minmax algorithm, computes value of best outcome
play::GameTree->Int
play (GameTree p []) = evalState p
play (GameTree (White,_) xs) = maximum (map play xs)
play (GameTree (Black,_) xs) = minimum (map play xs)

doMove::State->State
doMove z = case (genGameTree depth z) of
                  GameTree p [] -> p
                  GameTree (f, _) xs -> snd (findBest f (comp f) (map (\x->(play x, state x)) xs))
    where comp White = (>)
          comp Black = (<)

findBest :: PieceColor -> (Int -> Int -> Bool) -> [(Int, State)] -> (Int, State)
findBest _ _ [x] = x
findBest f cmp ((x1,y1):xs) | winningState f y1 = (x1,y1)
                            | otherwise = let (x2, y2) = findBest f cmp xs in
                                             if cmp x1 x2 then (x1,y1) else (x2,y2)

gameComp::State->[Either State String]
gameComp st | sw > threshold  = [Right "White wins!"] 
            | sw < -threshold = [Right "Black wins!"]
            | otherwise = (Left st):gameComp (doMove st) 
   where sw = evalState st 

finalState::State->Bool
finalState st = sw > threshold || sw < -threshold
   where sw = evalState st

winningState::PieceColor->State->Bool
winningState White st = evalState st > threshold
winningState Black st = evalState st < -threshold


printPosition::Either State String->String
printPosition (Left zust) = '\n':prettyBoard (snd zust)
printPosition (Right s) = '\n':s

