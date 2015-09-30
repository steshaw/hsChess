module Evaluator where

import Board

-- The value of the king has to be infinity
-- the threshold has to be infinity minus
-- the maximal material value of one player
infinity = 1000::Int
threshold = 900::Int

valuePiece::PieceType->Int
valuePiece Pawn = 1
valuePiece Rook = 5
valuePiece Knight = 3
valuePiece Bishop = 3
valuePiece Queen = 9
valuePiece King = infinity

-- aggregated value of material of both players
boardAnalysis::Board->(Int,Int)
boardAnalysis = foldl addValue (0,0) . concat 
   where addValue points Nothing = points
         addValue (pw,pb) (Just (Piece a f)) | f == Black = (pw, pb + valuePiece a)
                                             | otherwise = (pw + valuePiece a, pb)

evalBoard::Board->Int
evalBoard b = let (p1,p2) = boardAnalysis b in p1-p2

