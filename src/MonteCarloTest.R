# convert board to a vector for the nnet
boardToVector <- function(board){
  # take elments in board an lists them from left to right, top to bottom as a vector
  vec <- c()
  for(i in 1:3){
    for(j in 1:3){
      vec <- c(vec, board[i,1,j,], board[i,2,j,], board[i,3,j,])
    }
  }
  return(vec)
}

# reward function
reward <- function(winner, playingAs){
  # returns a reward of -1,0,1 based on the winner + who you are playing as
  if(winner == 0)
    return(0) # a draw always has reward 0
  else if(winner == 1 && playingAs == 1)
    return(1)
  else if(winner == 1 && playingAs == 2)
    return(-1)
  else if(winner == 2 && playingAs == 1)
    return(-1)
  else
    return(1)
}

# Encode into a neural network input form

stateToInLayer <- function(state){
  # takes a state vector (sequence of 0,1 and 2's) and returns vector of bits of 3*81 long
  # for player 1, player 2 and unoccupied (i.e  0)
  stateLayer <- vector("numeric", 3*81)
  seqlen <- 81
  
  for(i in 1:seqlen){
    if(state[i] == 0){
      stateLayer[i+2*seqlen] <- 1
    }
    else if(state[i] == 1){
      stateLayer[i] <- 1
    }
    else{
      stateLayer[i+seqlen] <- 1
    }
  }
  
  return(stateLayer)
}
# simulate with states
simulUTTT <- function(theseed){
  # To reproduce experiment
  set.seed(theseed)
  
  # Start game
  masterBoard <- array(0, dim = c(3,3,3,3))
  statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
  winner <- 0  # Initially a tie
  
  # track the states 
  boardStates <- matrix(0, ncol = 81)
  
  # First move
  player <- 1
  
  # random method
  move <- sample(c(1,2,3), size = 4, replace = T)
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[3], move[4])
  player <- player %% 2 + 1
  
  # add state to tracker
  boardStates <- rbind(boardStates, boardToVector(masterBoard))
  
  # The game continue normally
  while (T) {
    validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
    
    if (length(validMoves) == 0)
      break
    
    move <- sample(validMoves, size = 1)
    move <- move[[1]]
    
    # Play  the move
    masterBoard[move[1], move[2], move[3], move[4]] <- player
    forcedMove <- c(move[3], move[4])
    
    # Update tracked states
    boardStates <- rbind(boardStates, boardToVector(masterBoard))
    
    # Check for win
    if(hasWonBoard(masterBoard[move[1], move[2],,], player))
      statusBoard[move[1], move[2]] <- player  # Won subBoard
    if(hasWonBoard(statusBoard, player)){
      winner <- player # Won game
      break 
    }
    
    player <- player %% 2 + 1  # Change player
  }
  
  return(list(winner,boardStates))
}
# create a dataset that contains runs of random plays (states) + of the winner (rewards)
nepis <- 10000
RewardState1 <- rep(NULL, nepis)

# playing as player 1
for(i in 1:nepis){
  run <- simulUTTT(i)
  rewardR <- reward(run[[1]],playingAs = 1)
  RewardState1[[i]] <- list(rewardR,run[[2]])
}

# state-value function of all the visited states 
# 9 for the board you are in - 9 for the status board?
stataValue <- array(0, dim = c())



