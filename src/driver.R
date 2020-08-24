# Driver

source("model.r")

simulUTTT <- function(theseed){
# To reproduce experiment
set.seed(theseed)
  
# Start game
masterBoard <- array(0, dim = c(3,3,3,3))
statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
winner <- 0  # Initially a tie

# First move
player <- 1
# io method
# move <- as.numeric(unlist(strsplit(readline("Enter a move: "), ",")));
# random method
move <- sample(c(1,2,3), size = 4, replace = T)
masterBoard[move[1], move[2], move[3], move[4]] <- player
forcedMove <- c(move[3], move[4])
player <- player %% 2 + 1

# Debug
# cat("--------------------------\n")
# printMasterBoard(masterBoard)

# The game continue normally
while (T) {
  validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
  
  if (length(validMoves) == 0)
    break
  
  # Some smart method to determine a move instead
  # io method
  # move <- as.numeric(unlist(strsplit(readline("Enter a move: "), ",")));
  # random move method
  move <- sample(validMoves, size = 1)
  move <- move[[1]]
  
  # Play  the move
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[3], move[4])
  
  # Check for win
  if(hasWonBoard(masterBoard[move[1], move[2],,], player))
    statusBoard[move[1], move[2]] <- player  # Won subBoard
  if(hasWonBoard(statusBoard, player)){
    winner <- player # Won game
    break 
  }
  
  player <- player %% 2 + 1  # Change player
  
  # Debug
  # cat("**************************\n")
  # printMasterBoard(masterBoard)
  # cat("**************************\n")
  # printBoard(statusBoard)
}
# Debug
# cat("**************************\n")
# printMasterBoard(masterBoard)
# cat("**************************\n")
# printBoard(statusBoard)



# if(winner == 0){
  # cat("Draw\n")
# } else if(winner == 1){
    # cat("Player 1 wins\n")
# } else{
    # cat("Player 2 wins\n")
# }

return(winner)

}




