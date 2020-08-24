# convert board to a vector for the state vectors
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

suboardToVector <- function(subBoard){
  vec <- c()
  for(i in 1:3){
    for(j in 1:3){
      vec <- c(vec,subBoard[i,j])
    }
  }
  return(vec)
}

actionToVecMapping <- function(actionNumerical){
  return(switch(actionNumerical,
                c(1,1),
                c(1,2),
                c(1,3),
                c(2,1),
                c(2,2),
                c(2,3),
                c(3,1),
                c(3,2),
                c(3,3)
  )
  )
}

vecToActionMapping <- function(actionVec){
  if(actionVec[1] == 1){
    if(actionVec[2] == 1)
      return(1)
    else if(actionVec[2] == 2)
      return(2)
    else
      return(3)
  }
  else if(actionVec[1] == 2){
    if(actionVec[2] == 1)
      return(4)
    else if(actionVec[2] == 2)
      return(5)
    else
      return(6)
  }
  else if(actionVec[1] == 3){
    if(actionVec[2] == 1)
      return(7)
    else if(actionVec[2] == 2)
      return(8)
    else
      return(9)
  }
  else return -1
}

# take a state which looks like a ternary representation and convert it to decimal
stateToDec <- function(boardVec){
  dec <- 0
  for(i in 1:length(boardVec)){
    dec <- dec + boardVec[i]*3^(i-1)
  }
  return(dec)
}

decToState <- function(numState){
  tern <- decToTern(numState)
  # pad left with 0's
  lenPad <- 9 - length(tern)
  padZeros <- vector("numeric",length=lenPad)
  state <- c(padZeros, tern)
  return(state)
}

decToTern <- function(decimal){
  if(decimal == 0)
    return(NULL)
  tern <- c(decimal %% 3, decToTern(decimal %/% 3))
  return(tern)
}

# reward function
reward <- function(winner, playingAs){
  # returns a reward of -1,0,1 based on the winner + who you are playing as
  if(winner == 0)
    return(0) # want agent to win
  else if(winner == 1 && playingAs == 1)
    return(100)
  else if(winner == 1 && playingAs == 2)
    return(-100)
  else if(winner == 2 && playingAs == 1)
    return(-100)
  else
    return(100)
}

# simulate with state approximations - i.e. return all the states and actions that
# are being tracked
simulUTTTAgent1 <- function(theseed){
  # To reproduce experiment
  set.seed(theseed)
  
  # Start game
  masterBoard <- array(0, dim = c(3,3,3,3))
  statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
  winner <- 0  # Initially a tie
  
  # track the states 
  boardStates <- matrix(0, ncol = 81)
  currentBoardState <- matrix(0, ncol = 9) # for Q learninng
  actionList <- list() # for Q learning
  
  # First move
  player <- 1
  
  # random method
  move <- sample(c(1,2,3), size = 4, replace = T)
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[3], move[4])
  player <- player %% 2 + 1
  
  # add state to tracker
  boardStates <- rbind(boardStates, boardToVector(masterBoard))
  
  # add action to tracker
  actionList <- append(actionList, vecToActionMapping(forcedMove))
  
  # The game continue normally
  while (T) {
    validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
    
    if (length(validMoves) == 0)
      break
    
    move <- sample(validMoves, size = 1)
    move <- move[[1]]
    
    
    if(player == 1){
    # add current board to state tracker
    currentBoardState <- rbind(currentBoardState, suboardToVector(masterBoard[move[1],move[2],,]))
    }
    
    # make the action
    masterBoard[move[1], move[2], move[3], move[4]] <- player
    forcedMove <- c(move[3], move[4])
    
    if(player == 1){
    # add action to state tracker
    actionList <- append(actionList, vecToActionMapping(forcedMove))
    }
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
  
  return(list(winner,boardStates,currentBoardState,actionList))
}
# create a dataset that contains runs of random plays (states) + of the winner (rewards)
nepis <- 10000
StateActionReward <- rep(NULL, nepis)

# playing as player 1
for(i in 1:nepis){
  run <- simulUTTTAgent1(i)
  rewardR <- reward(run[[1]],playingAs = 1)
  StateActionReward[[i]] <- list(rewardR,run[[3]],run[[4]])
}

# use Watkin's Q Learning Techinque - Input all the simulations as a list of rewards, states and actions
# in this function the mapping is done in the QLearning function
ApplyQLearning <- function(qInit,episodeSimu,stepSize){
  qEstim <- qInit
  
  for(episode in episodeSimu){
    reward <- episode[[1]]
    states <- episode[[2]]
    actions <- episode[[3]]

    Tt <- dim(states)[1]

    for(t in 1:(Tt-1)){
      S_t <- states[t,]
      A_t <- actions[[t]][1]
      
      R_tplus1 <- 0
      if(t == Tt-1) # at the beginning will do a lot of of 0 updates
        R_tplus1 <- reward
      S_tplus1 <- states[t+1,]
      
      # convert states to decimal representation
      S_t <- stateToDec(S_t) 
      S_tplus1 <- stateToDec(S_tplus1)

      # add 1 to every S because r indexing starts at 0
      # undiscounted rewards
      qEstim[S_t+1,A_t] <-  qEstim[S_t+1,A_t] + stepSize*(R_tplus1 + max(qEstim[S_tplus1+1,]) - qEstim[S_t+1,A_t]) 
    }
  }
  return(qEstim)
}

# apply q learning
stepsize <- 0.1

qEstimQ <- matrix(0,nrow = 3^9, ncol = 9)

qEstimQ <- ApplyQLearning(qEstimQ,StateActionReward,stepsize)


# this agent uses qEstim from Q learning to play against a random bot
simulUTTTQLearning1 <- function(theseed,qEstim){
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
  
  # The game continue normally
  while (T) {
    validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
    
    if (length(validMoves) == 0)
      break
    
    # random move for opponent or for player 1 in special case
    if(player == 2){
      move <- sample(validMoves, size = 1)
      move <- move[[1]]
    }
    else
    { 
      if(validMoves[[1]][1] != forcedMove[1] || validMoves[[1]][2] != forcedMove[2]){
        # in the case where move isn't in forced move subboard
        randomSuboard <- sample(validMoves,size = 1)
        randomSuboard <- randomSuboard[[1]]
        forcedMove <- c(randomSuboard[1],randomSuboard[2])
        # restrict valid moves only to that suboard
        temp <- list()
        for(i in 1:length(validMoves)){
          if(validMoves[[i]][1] == forcedMove[1] && validMoves[[i]][2] == forcedMove[2])
            temp <- append(temp, list(validMoves[[i]]))
        }
        validMoves <- temp
      }
      # otherwise player 1 can pick only within sub-board
      # in this case can use qEstim
      stateDecimal <-  stateToDec(suboardToVector(masterBoard[forcedMove[1],forcedMove[2],,]))
      validActionsDecimal <- c() # list of numbers
      
      # go through list of moves and coonvert them to decimal
      for(move in validMoves){
        validActionsDecimal <- c(validActionsDecimal, vecToActionMapping(c(move[3],move[4])))
      }
      
      # find the max action based on values in qEstim
      maxValue <- -Inf
      maxIndex <- -1
      for(actionIndex in validActionsDecimal){
        if(qEstim[stateDecimal+1,actionIndex] > maxValue)
          maxIndex <- actionIndex
      }
      
      # so the move will be
      actionVector <- actionToVecMapping(maxIndex)
      move <- c(forcedMove[1],forcedMove[2],actionVector[1],actionVector[2])
    }
    
    # Play  the move
    masterBoard[move[1], move[2], move[3], move[4]] <- player
    forcedMove <- c(move[3], move[4])
    
    # add state to tracker
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

# run and find win ratio
# playing as player 1 - with QLearning technique + State Approx 1

wins <- 0 
draws <- 0
losses <- 0
nepis <- 10000

winRatio <- vector("numeric",length = nepis)
drawRatio <- vector("numeric",length = nepis)
lossRatio <- vector("numeric",length = nepis)

for(i in 1:nepis){
  result <- simulUTTTQLearning1(i,qEstimQ)[[1]]
  
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

# win ratio of about 0.435, 3% improvement
plot(x=1:nepis,winRatio,ylab= "Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(drawRatio,col="blue")
lines(lossRatio,col="red")
legend("topright",legend=c("Win Ratio","Draw Ratio","Loss Ratio"),
       col=c("darkgreen","blue","red"),lty=1,cex = 0.8)


# print the win, loss and draw ratios
cat("The win ratio given a QL policy was ", winRatio[nepis],"\n")
cat("The draw ratio given a QL policy was ", drawRatio[nepis],"\n")
cat("The loss ratio given a QL policy was ", lossRatio[nepis],"\n")



