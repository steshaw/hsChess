module Moves where

import Board
import Utils

-- PieceColor indicates whose turn it is
type State = (PieceColor, Board)

straight, diagonal::[Pos]
straight = [(0,1),(0,-1),(1,0),(-1,0)]
diagonal = [(1,1),(-1,-1),(1,-1),(-1,1)]

-- rough definition of moves to be refined later
moves::PieceType->[Pos]
moves Rook = straight
moves Bishop = diagonal
moves Knight = [(1,2),(2,1),(-1,2),(2,-1),(-2,1),(1,-2),(-1,-2),(-2,-1)]
moves King = straight ++ diagonal
moves Queen = straight ++ diagonal
moves Pawn = []

-- direction of play
direction::PieceColor->Int
direction White = -1
direction Black = 1

-- move generator, simple pawn, no castling
genMoves::Board->Pos->[Board]
genMoves b pos = case getSquare b pos of
                   Nothing -> [] 
                   Just p -> map (flip (movePos pos) b) $ genPieceMoves b pos p 

genPieceMoves::Board->Pos->Piece->[Pos]
genPieceMoves b pos (Piece Knight f) = [coord|v<-moves Knight, let coord = addPair pos v, notColor b f coord]
genPieceMoves b pos (Piece King f) = [coord|v<-moves King, let coord = addPair pos v, notColor b f coord]
genPieceMoves b pos (Piece Pawn f) = (filter (empty b) [addPair pos (direction f, 0)]) ++
                                     (filter (oppositePiece b f) (map (addPair pos) [(direction f, 1),(direction f, -1)]))
genPieceMoves b pos (Piece x f) = concatMap (iterateDirection 1 pos b f) (moves x)

notColor b f p      = inside p && not (hasColor f (getSquare b p))
empty b p           = inside p && Nothing == (getSquare b p)
oppositePiece b f p = inside p && hasColor (oppositeColor f) (getSquare b p) 

-- gets moving vectors for rooks, queens and bishops, the first paramater pos is the 
-- position of the piece, the second the direction to iterate at
iterateDirection::Int->Pos->Board->PieceColor->Pos->[Pos]
iterateDirection n pos b f r | outside aimsAt = []
                             | otherwise = case getSquare b aimsAt of
                                             Nothing -> aimsAt:iterateDirection (n+1) pos b f r
                                             Just (Piece _ f2) -> if f==f2 then [] else [aimsAt]
   where aimsAt = addPair (multPair n r) pos

nextStates::State->[State]
nextStates (f, b) = [(oppositeColor f, b')|pos<-colorPos f b, b'<-genMoves b pos]
