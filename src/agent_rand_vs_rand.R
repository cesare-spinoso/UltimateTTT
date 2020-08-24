# this agent plays randomly against another random agent
simulUTTT <- function(theseed){
  # To reproduce experiment
  set.seed(theseed)
  
  # Start game
  masterBoard <- array(0, dim = c(3,3,3,3))
  statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
  winner <- 0  # Initially a tie
  
  # First move
  player <- 1
  
  # random method
  move <- sample(c(1,2,3), size = 4, replace = T)
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[3], move[4])
  player <- player %% 2 + 1
  
  # The game continue normally
  while (T) {
    validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
    
    if (length(validMoves) == 0)
      break
    
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
  }
  return(winner)
}
# simulate for playing first and second

# playing as player 1 - randomly

# run and find win ratio
# playing as player 1 - with QLearning technique

wins <- 0 
draws <- 0
losses <- 0
nepis <- 10000

winRatio <- vector("numeric",length = nepis)
drawRatio <- vector("numeric",length = nepis)
lossRatio <- vector("numeric",length = nepis)

for(i in 1:nepis){
  result <- simulUTTT(i)[[1]]
  
  if (result == 0)
    draws <- draws + 1
  else if(result == 1)
    wins <- wins + 1
  else
    losses <- losses + 1
  
  winRatio[i] <- wins/i
  drawRatio[i] <- draws/i
  lossRatio[i] <- losses/i
}

# win ratio of about 40.5%
plot(x=1:nepis,winRatio,ylab= "Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(drawRatio,col="blue")
lines(lossRatio,col="red")
legend("topright",legend=c("Win Ratio","Draw Ratio","Loss Ratio"),
       col=c("darkgreen","blue","red"),lty=1)

# print the win, loss and draw rations
cat("The win ratio given a random policy was ", winRatio[nepis],"\n")
cat("The draw ratio given a random policy was ", drawRatio[nepis],"\n")
cat("The loss ratio given a random policy was ", lossRatio[nepis],"\n")


