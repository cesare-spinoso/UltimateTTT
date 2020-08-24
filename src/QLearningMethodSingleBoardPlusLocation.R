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

# take the state and its number location (1 to 9) and convert it to a unique decimal from 0 to 3^11-1
stateToDec <- function(boardVec, location){
  dec <- ternToDec(boardVec)
  # shift the decimal  by location-1 * 3^9 to create unique mapping
  dec <- dec + (location-1)*3^9
  return(dec)
}

# takes a vector of size 2 and returns number from 1 to 9
locationToDec <- function(boardVec){
  if(boardVec[1] == 1){
    if(boardVec[2] == 1)
      return(1)
    else if(boardVec[2] == 2)
      return(2)
    else
      return(3)
  }
  else if(boardVec[1] == 2){
    if(boardVec[2] == 1)
      return(4)
    else if(boardVec[2] == 2)
      return(5)
    else
      return(6)
  }
  else if(boardVec[1] == 3){
    if(boardVec[2] == 1)
      return(7)
    else if(boardVec[2] == 2)
      return(8)
    else
      return(9)
  }
  else return -1
}

# take a state which looks like a ternary representation and convert it to decimal
ternToDec <- function(boardVec){
  dec <- 0
  for(i in 1:length(boardVec)){
    dec <- dec + boardVec[i]*3^(i-1)
  }
  return(dec)
}

# convert the decimal representation to ternary or vector representation
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

# simulate with state approximations
simulUTTT <- function(theseed){
  # To reproduce experiment
  set.seed(theseed)
  
  # Start game
  masterBoard <- array(0, dim = c(3,3,3,3))
  statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
  winner <- 0  # Initially a tie
  
  # track the states 
  boardStates <- matrix(0, ncol = 81)
  # for Q learning
  currentBoardState <- matrix(0, ncol = 9) 
  locationStates <- c()
  actionList <- c() 
  
  # First move
  player <- 1
  
  # random method
  move <- sample(c(1,2,3), size = 4, replace = T)
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[3], move[4])
  player <- player %% 2 + 1
  
  # add state to tracker
  boardStates <- rbind(boardStates, boardToVector(masterBoard))
  
  # add location to tracker
  locationStates <- c(locationStates, locationToDec(c(move[1],move[2])))
  
  # add action to tracker
  actionList <- c(actionList, vecToActionMapping(forcedMove))
  
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
      # add location to location tracker
      locationStates <- c(locationStates, locationToDec(c(move[1],move[2])))
    }
    
    # make the action
    masterBoard[move[1], move[2], move[3], move[4]] <- player
    forcedMove <- c(move[3], move[4])
    
    if(player == 1){
      # add action to state tracker
      actionList <- c(actionList, vecToActionMapping(forcedMove))
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
  
  return(list(winner,boardStates,currentBoardState,locationStates,actionList))
}
# create a dataset that contains runs of random plays (states) + of the winner (rewards)
# increase number of episodes since state space is larger
nepis <- 25000
StateActionReward <- list()

# playing as player 1
for(i in 1:nepis){
  run <- simulUTTT(i)
  
  rewardR <- reward(run[[1]],playingAs = 1)
  states <- c()
  # need to preprocess the states so that they are in decimal format
  for(i in 1:length(run[[4]])){
    states <- c(states, stateToDec(run[[3]][i,],run[[4]][i]))
  }
  
  StateActionReward <- append(StateActionReward, list(list(rewardR,states,run[[5]])))
  
}


# use Watkin's Q Learning Techinque - Input all the simulations as a list of rewards, states and actions
ApplyQLearning <- function(qInit,episodeSimu,stepSize){
  qEstim <- qInit
  
  
  for(episode in episodeSimu){
    reward <- episode[[1]]
    states <- episode[[2]]
    actions <- episode[[3]]
    # print(reward)
    # print(states)
    # print(actions)
    Tt <- length(states)
    # print(Tt)
    # cat(dim(states)[1],"-----\n")
    for(t in 1:(Tt-1)){
      S_t <- states[t]
      A_t <- actions[[t]][1]
      
      R_tplus1 <- 0
      if(t == Tt-1) # at the beginning will do a lot of of 0 updates
        R_tplus1 <- reward
      S_tplus1 <- states[t+1]
      
      # add 1 to every S because r indexing starts at 0
      # undiscounted rewards
      qEstim[S_t+1,A_t] <-  qEstim[S_t+1,A_t] + stepSize*(R_tplus1 + max(qEstim[S_tplus1+1,]) - qEstim[S_t+1,A_t]) 
    }
  }
  return(qEstim)
}

# apply q learning
require(nnet) # for which.is.max

stepsize <- 0.1

qEstimQ <- matrix(0,nrow = 3^11, ncol = 9) # sloghtly bigger state space


qEstimQ <- ApplyQLearning(qEstimQ,StateActionReward,stepsize)



# used qEstim found by Q Learning to play UTTT
simulUTTTQLearning <- function(theseed,qEstim){
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
  
  # Since we have locations of current board we can pick the optimal initial action
  # using qEstim
  allMoves <- list()
  for(r in 1:3)
    for(c in 1:3)
      for(rprime in 1:3)
        for(cprime in 1:3)
          allMoves <- append(allMoves, list(c(r,c,rprime,cprime)))
  # find the max action based on values in qEstim
  maxValue <- -Inf
  maxMove <- -1
  for(move in allMoves){
    # first find Q(s,a)
    stateDec <- stateToDec(suboardToVector(masterBoard[move[1],move[2],,]),locationToDec(c(move[1],move[2])))
    actionDec <- vecToActionMapping(c(move[3],move[4]))
    valueOfMove <- qEstim[stateDec+1,actionDec]
    
    # find the max
    if(valueOfMove > maxValue){
      maxValue <- valueOfMove
      maxMove <- move
    }
  }
  
  # random method - start with max move according to qEstim
  move <- maxMove
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
      # in this case we don't need to check for the special case that agent can play
      # anywhere on the board since we can handle this with the location parameter of ht
      # state
      
      # find the max action based on values in qEstim
      maxValue <- -Inf
      maxMove <- -1
      
      for(move in validMoves){
        # go through all the valid moves - choose the one with the highe Q(s,a)
        
        # first find Q(s,a)
        stateDec <- stateToDec(suboardToVector(masterBoard[move[1],move[2],,]),locationToDec(c(move[1],move[2])))
        actionDec <- vecToActionMapping(c(move[3],move[4]))
        valueOfMove <- qEstim[stateDec+1,actionDec]
        
        # find the max
        if(valueOfMove > maxValue){
          maxValue <- valueOfMove
          maxMove <- move
        }
      }
      # so the move will be
      move <- maxMove
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
# playing as player 1 - with QLearning technique

wins <- 0 
draws <- 0
losses <- 0
nepis <- 10000

winRatio <- vector("numeric",length = nepis)
drawRatio <- vector("numeric",length = nepis)
lossRatio <- vector("numeric",length = nepis)

for(i in 1:nepis){
  result <- simulUTTTQLearning(i,qEstimQ)[[1]]
  
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

# plots
plot(x=1:nepis,winRatio,ylab= "Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(drawRatio,col="blue")
lines(lossRatio,col="red")
legend("topright",legend=c("Win Ratio","Draw Ratio","Loss Ratio"),
       col=c("darkgreen","blue","red"),lty=1,cex = 0.8)


# print the win, loss and draw rations
cat("The win ratio given a QL policy was ", winRatio[nepis],"\n")
cat("The draw ratio given a QL policy was ", drawRatio[nepis],"\n")
cat("The loss ratio given a QL policy was ", lossRatio[nepis],"\n")

ApplyDoubleQLearning <- function(qInit1,qInit2,episodeSimu,stepsize){
  Q1 <- qInit1 # will be used to select the opti action in simlulation
  Q2 <- qInit2 # used to determine correspinding action value
  
  for(episode in episodeSimu){
    reward <- episode[[1]]
    states <- episode[[2]]
    actions <- episode[[3]]
    # print(reward)
    # print(states)
    # print(actions)
    Tt <- length(states)
    # print(Tt)
    # cat(dim(states)[1],"-----\n")
    for(t in 1:(Tt-1)){
      S_t <- states[t]
      A_t <- actions[[t]][1]
      
      R_tplus1 <- 0
      if(t == Tt-1) # at the beginning will do a lot of of 0 updates
        R_tplus1 <- reward
      S_tplus1 <- states[t+1]
      
      # coin flip to see which one will be updated
      if(sample(c(1,2),1) == 1) # upadte Q1
      {
        Q1[S_t+1,A_t] <- Q1[S_t+1,A_t] <- stepsize*(R_tplus1 + max(Q2[S_tplus1+1,which.is.max(Q1[S_tplus1+1,])]) - Q1[S_t+1,A_t])
      }
      else{
        # update Q2
        Q2[S_t+1,A_t] <- Q2[S_t+1,A_t] <- stepsize*(R_tplus1 + max(Q1[S_tplus1+1,which.is.max(Q2[S_tplus1+1,])]) - Q2[S_t+1,A_t])
        
      }
    }
  }
  return(Q1) # this is the action value function function that will be used
  
}

# create a dataset that contains runs of random plays (states) + of the winner (rewards)
# double number of episode as in QLearning since have to Q's
nepis <- 50000
StateActionReward <- list()

# playing as player 1
for(i in 1:nepis){
  run <- simulUTTT(i)
  
  rewardR <- reward(run[[1]],playingAs = 1)
  states <- c()
  # need to preprocess the states so that they are in decimal format
  for(i in 1:length(run[[4]])){
    states <- c(states, stateToDec(run[[3]][i,],run[[4]][i]))
  }
  
  StateActionReward <- append(StateActionReward, list(list(rewardR,states,run[[5]])))
  
}


# apply q learning
stepsize <- 0.1

qEstim1 <- matrix(0,nrow = 3^11, ncol = 9) # sloghtly bigger state space
qEstim2 <- matrix(0,nrow = 3^11, ncol = 9) # two initial Q's for q learning

require(nnet)
qEstim1 <- ApplyDoubleQLearning(qEstim1,qEstim2,StateActionReward,stepsize)

# run and find win ratio
# playing as player 1 - with QLearning technique

wins <- 0 
draws <- 0
losses <- 0
nepis <- 2000

winRatio <- vector("numeric",length = nepis)
drawRatio <- vector("numeric",length = nepis)
lossRatio <- vector("numeric",length = nepis)

for(i in 1:nepis){
  result <- simulUTTTQLearning(i,qEstim1)[[1]]
  
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

# plots
plot(x=1:nepis,winRatio,ylab= "Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(drawRatio,col="blue")
lines(lossRatio,col="red")
legend("topright",legend=c("Win Ratio","Draw Ratio","Loss Ratio"),
       col=c("darkgreen","blue","red"),lty=1,cex=0.8)


# print the win, loss and draw rations
cat("The win ratio given a QL policy was ", winRatio[nepis],"\n")
cat("The draw ratio given a QL policy was ", drawRatio[nepis],"\n")
cat("The loss ratio given a QL policy was ", lossRatio[nepis],"\n")

