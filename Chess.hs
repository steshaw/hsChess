module Chess where
import Board
import Minimax
import Moves
import Game


mateBoard1 = (White, 
              [[Nothing, Nothing, Nothing, Just (Piece Queen Black), Nothing, Nothing, Just (Piece King Black), Nothing],
               [Nothing, Nothing, Nothing, Nothing, Just (Piece Bishop Black), Nothing, Just (Piece Pawn Black), Nothing],
               [Nothing, Nothing, Nothing, Nothing, Just (Piece Pawn Black), Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Just (Piece Pawn Black), Just (Piece Pawn White), Nothing, Nothing, Just (Piece Queen White)],
               [Nothing, Nothing, Nothing, Just (Piece Pawn White), Nothing, Just (Piece Rook Black), Nothing, Nothing],
               [Nothing, Nothing, Just (Piece Pawn White), Nothing, Nothing, Nothing, Nothing, Just (Piece Bishop White)],
               [Nothing, Nothing, Just (Piece Pawn White), Nothing, Nothing, Just (Piece Pawn White), Nothing, Just (Piece Pawn White)],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Piece King White), Nothing]])

mateBoard2 = (Black,
              [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Just (Piece Rook White), Nothing, Nothing, Nothing, Just (Piece King Black), Nothing, Nothing],
               [Just (Piece Rook White), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing, Nothing, Nothing, Just (Piece King White), Nothing, Nothing]])


-- Falkbeer Countergambit
exampleOpening = [move "e2" "e4",
                  move "e7" "e5",
                  move "f2" "f4",
                  move "d7" "d5"]

outputExampleOpening = putStr $ prettyBoard $ playGame exampleOpening
exampleGame = putStr $ concatMap (("\n"++) . prettyBoard . snd ) $ take 10 $ iterate doMove (White,initialBoard)
exampleMateGame1 = putStr $ concatMap printPosition $ gameComp mateBoard1
exampleMateGame2 = putStr $ concatMap printPosition $ gameComp mateBoard2

