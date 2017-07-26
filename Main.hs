import CheckersBoard(Game, Position, getInitialGame, getModifiedGame, validateMove, validatePlayer, checkGameOver, getWinner)
import Data.Data


playCheckers :: Game -> IO()
playCheckers game = do
  if not (checkGameOver game) 
    then continueGame
    else endGame where
  continueGame = do
    putStrLn $ show game
    putStrLn $ "Enter the start Position (row,col). Press q to quit: "
    startPosStr <- getLine
    if startPosStr == "q"
      then putStrLn "Quitting game !!" 
      else do
        putStrLn $ "Enter the end Position (row,col). Press q to quit: "
        endPosStr <- getLine
        if endPosStr == "q"
          then putStrLn "Quitting game !!" 
          else if validatePlayer game (getPosition startPosStr)
            then do
              if validateMove game (getPosition startPosStr) (getPosition endPosStr)
                then do
                putStrLn $ "Valid Move. Applying move"
                playCheckers $ getModifiedGame game (getPosition startPosStr) (getPosition endPosStr) 
                else do
                  putStrLn $ "Invalid move. Try Again !!"
                  playCheckers $ game
            else do
              putStrLn $ "Invalid Move. Select your piece to play. Try Again !!"
              playCheckers $ game

  endGame = do 
    putStrLn $ show (getWinner game) ++ " has won !!"
    putStrLn $ "The game is over. Final game state: \n" ++ show game
    


getPosition :: String -> Position
getPosition strPosition = read strPosition :: Position

main :: IO ()
main = do
  putStrLn "Checkers starting"
  playCheckers getInitialGame

