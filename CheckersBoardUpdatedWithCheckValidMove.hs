module CheckersBoard (
    Board
  , ModifiedBoard
  , BSquare
  , initialBoard
  , getPiece
  , deletePiece
  , getModifiedBoard
  , toListFromBoard
) where

import Data.List (intercalate)
import Data.Vector (Vector,toList,fromList,(!),(//))
import qualified Data.Vector as V

-- Player Pieces represention
-- The types of player on the checkers board - Red and Black
data Player = Red | Black deriving (Eq)

-- Type of Pieces on the board
data Piece = Single | King deriving (Eq)

-- Player Piece will be comprised of player and type.
data PlayerPiece = PlayerPiece Player Piece deriving (Eq)

-- Prints the player -> R or B
instance Show Player where
  show Red  = "R"
  show Black  = "B"
 
-- Prints the type -> s or k
instance Show Piece where
  show King   = "k"
  show Single = "s"
  
-- Combine Player and Type to form the player piece -> Rs
instance Show PlayerPiece where
  show (PlayerPiece player piece) = (show player) ++ (show piece)


-- Coordinates are 0 based starts from (0,0)
type Position = (Int,Int)


-- Board representation
data Board = Board (Vector(Vector BSquare)) deriving (Eq)

-- Return checkValidMove result or error
type ModifiedBoard  = Either String (Board, BSquare)

-- A square of the chess board may contain a piece, or it may not.
type BSquare = Maybe PlayerPiece

-- Display Board
instance Show Board where
  show board = (unlines ([yCoordInd] ++ (prettyBorderLine : rowString)))
    where
      boardList = toListFromBoard board
      rowString = zipWith displayRow ([0..7]) $ boardList
      showSquare Nothing = "  "
      showSquare (Just x) = show x
      prettyBorderLine = "  " ++ (replicate 41 '-' )
      displayRow :: Integer -> [BSquare] -> String
      displayRow i sq       = (intercalate " | " $ (show i) : (map showSquare sq) ) ++ " |" ++ "\n" ++ prettyBorderLine
      yCoordInd         = (intercalate " |  " $ " " : (map (:[]) ['0'..'7']))  ++ " |"
        

-- Initial Board with start values
initialBoard :: Board
initialBoard = Board $ fromList $ map fromList $ concat [
  [ redPieceFirstLine, redEmptyFirstLine, redPieceFirstLine]
  , (replicate 2 emptyLine)
  , [ blackEmptyFirstLine , blackPieceFirstLine, blackEmptyFirstLine]
  ]
  where
    redEmptyFirstLine   = emptyFirstLine Red
    redPieceFirstLine    =  pieceFirstLine Red
    blackPieceFirstLine   = pieceFirstLine Black
    blackEmptyFirstLine    = emptyFirstLine Black
    emptyLine        = replicate 8 Nothing
    pieceFirstLine player = concat [replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing] 
    emptyFirstLine player = concat [replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single, replicate 1 Nothing, replicate 1 $ Just $ PlayerPiece player Single]
    
     

-- Query a piece from Board at a Position eg. getPiece initialBoard (0,0) -> will return Just Rs
getPiece :: Board -> Position -> BSquare
getPiece board pos = getBSquareValue board $ pos

-- deletePiece a piece from the Board and get the new Board as a result
deletePiece :: Board -> Position -> Board
deletePiece (Board board) pos = let
  (xPos, yPos) = pos
  in Board $ board // [ (xPos,((board ! xPos) // [(yPos, Nothing)]))]


-- getModifiedBoard - returns the new board if the move is Valid.
getModifiedBoard ::
  Board
  -> Position --  Start position
  -> Position --  End position
  -> ModifiedBoard
getModifiedBoard board start end = let
  startC = start
  endC   = end
  in
   checkValidMove board startC endC (getBSquareValue board startC) (getBSquareValue board endC)

toListFromBoard :: Board -> [[BSquare]]        
toListFromBoard boardList = toList $ V.map toList boardList                            
 
-- checkValidMove -- TODO: Add more constraints to the possible moves
checkValidMoveComputer :: Board -> Position -> Position -> BSquare -> BSquare -> Bool
checkValidMoveComputer _ _ _ Nothing _                     = False
checkValidMoveComputer board startC endC (Just p1) (Just p2)
	|p1 == p2 = False 
	|otherwise = if ((jump_test board startC endC) == Left True) then True else False 
checkValidMoveComputer board startC endC (Just p1) Nothing 
	|(fst endC < 0) || (snd endC < 0) || (fst endC > 7) || (snd endC > 7) = False
	|(fst endC - fst startC) == 0 || (snd endC - snd startC) == 0 = False
	|(abs (fst endC - fst startC)) /= (abs (snd endC - snd startC)) = False
	|(abs (fst endC - fst startC)) > 2 = False
	|(abs (fst endC - fst startC)) == 2 &&  (getPiece board (((fst startC) + (fst endC)) `div` 2, ((snd startC) + (snd endC)) `div` 2)) == Just (PlayerPiece Black Single) = True
	|otherwise = True
 
-- internal function to check if the move is valie
-- checkValidMove -- TODO: Add more constraints to the possible moves
checkValidMove :: Board -> Position -> Position -> BSquare -> BSquare -> ModifiedBoard
checkValidMove _ _ _ Nothing _                     = Left "No piece at source to move"
checkValidMove board startC endC (Just p1) (Just p2) = Left "Invalid Move. Cant replace existing piece!!"
checkValidMove board startC endC (Just p1) Nothing 
	|(fst endC < 0) || (snd endC < 0) || (fst endC > 7) || (snd endC > 7) = Left "Move out of Board!"
	|(fst endC - fst startC) == 0 || (snd endC - snd startC) == 0 = Left "Cannot move horizontally or vertically"
	|(abs (fst endC - fst startC)) /= (abs (snd endC - snd startC)) = Left "Invalid Move"
	|(abs (fst endC - fst startC)) > 2 = Left "Invalid Move"
	|(abs (fst endC - fst startC)) == 2 &&  (getPiece board (((fst startC) + (fst endC)) `div` 2, ((snd startC) + (snd endC)) `div` 2)) == Just (PlayerPiece Red Single) = Right ((modifiedBoard (deletePiece board (((fst startC) + (fst endC)) `div` 2, ((snd startC) + (snd endC)) `div` 2)) startC endC p1),Nothing)
	|otherwise = Right ((modifiedBoard board startC endC p1),Nothing)
  
-- internal function to create the modified board if the move is valid                      
modifiedBoard :: Board -> Position -> Position -> PlayerPiece -> Board
modifiedBoard (Board board) (startX,startY) (endX,endY) piece = Board $
  if startX == endX
  then board // [ (startX,( (board ! startX) // [(startY,Nothing),(endY,(Just piece))]))]
  else board // [
    (startX,(board ! startX) // [(startY,Nothing)])
    , (endX,(board ! endX) // [(endY,(Just piece))])
    ]

	
--Possible moves by all the piece. It is a 2D array

possible_moves :: Board -> [(Position, Position)]
possible_moves board = map (whereCanItMove board) [(x,y) | x<-[0..7], y<-[0..7]]  
	
--Determines where the piece can move	
whereCanItMove ::Board -> Position -> (Position,Position)
whereCanItMove board startC = if (getPiece board startC) == (Just PlayerPiece Black Single)
								then (((fst startC) + 1, (snd startC)-1), ((fst startC) + 1, (snd startC)+1))
								else if (getPiece board startC) == (Just PlayerPiece Red Single) then (((fst startC) - 1, (snd startC)-1), ((fst startC) - 1, (snd startC)+1))
								else ((0,0),(0,0))

-- Store the good moves. possibleMovesList we can get it from the possible_moves function								
validMoves :: Board -> [(Position,Position)] -> [((Position,Position), (Position,Position))]
validMoves board possibleMovesList = map (goodMoves board) (zip [(x,y) | x<-[0..7], y<-[0..7]] possibleMovesList)

--Select the moves which are valid. 
goodMoves::Board -> (Position, (Position,Position)) -> ((Position, Position),(Position,Position))
goodMoves board movesForPos 
	|checkValidMoveComputer board startC endC1 (Just (getPiece board startC)) (Just (getPiece board endC1)) = if checkValidMoveComputer startC endC2 then ((startC, endC1),(startC, endC2)) else ((startC, endC1),(0,0))
	|otherwise = ((0,0),(0,0))
	where 
	startC = fst movesForPos
	endC1 = fst (snd movesForPos)
	endC2 = snd (snd movesForPos)
		  

		  
--Test when to jump and if jump is possible
jump_test :: Board -> Position -> Position -> Either Bool Position
jump_test board startC endC
    |(getPiece board endC == Just (PlayerPiece Black Single))= Left False
    |(snd endC) - (snd startC)>0 = if getPiece board ((fst startC-2),(snd startC+2))== Nothing then Right ((fst startC)-2,(snd startC)+2) 
																										else Left False 
	|(snd endC) - (snd startC)<0 = if getPiece board ((fst startC-2),(snd startC-2))== Nothing then Right ((fst startC)-2,(snd startC)-2)
																											else Left False
	|otherwise = Left False
	
	
getBSquareValue :: Board -> Position -> BSquare
getBSquareValue (Board b) (xPosition, yPositionInd) = b ! xPosition ! yPositionInd
	