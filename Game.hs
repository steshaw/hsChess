module Game where
import Utils
import Board

type Move = Board->Board
type Game = [Move]

playGame::Game->Board
playGame = applyAll initialBoard