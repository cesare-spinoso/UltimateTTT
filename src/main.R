######### Game Dynamics ########
# All the functions that are used to play the game
hasWonBoard <- function(board,player){
  hasWon <- F
  dimension <- length(board[1,])
  
  # First check horizontal
  for(row in 1:dimension){
    if(board[row,1] == board[row,2] && board[row,2] == board[row,3] && board[row,2] == player){
      hasWon <- T
      break
    }
  }
  if(hasWon)
    return(T)
  
  # Check vertical
  for(col in 1:dimension){
    if(board[1,col] == board[2,col] && board[2,col] == board[3,col] && board[2,col] == player){
      hasWon <- T
      break
    }
  }
  if(hasWon)
    return(T)
  
  # Check the diagonals
  if(board[1,1] == board[2,2] && board[2,2] == board[3,3] && board[2,2] == player)
    return(T)
  if(board[3,1] == board[2,2] && board[2,2] == board[1,3] && board[2,2] == player)
    return(T)
  
  return(F)
}

# Return a list of valid moves
getValidMove <- function(board, forcedMove, boardStatus){
  validMove <- list()
  fullBoard <- T
  
  # subBoard not won
  if (boardStatus[forcedMove[1], forcedMove[2]] == 0){
    for (subX in 1:3)
      for (subY in 1:3)
        if (board[forcedMove[1], forcedMove[2], subX, subY] == 0)
          validMove <- append(validMove, list(c(forcedMove[1], forcedMove[2], subX, subY)))  # Add the valid move
        fullBoard <- F
  }
  if (fullBoard == F){
    return (validMove)
  }
  
  # All moves that are in non-won subBoard and not played
  for (x in 1:3)
    for (y in 1:3)
      if (boardStatus[x, y] == 0)
        for (subX in 1:3)
          for (subY in 1:3)
            if (board[x, y, subX, subY] == 0)
              validMove <- append(validMove, list(c(x, y, subX, subY)))  # Add the valid move
  
  return (validMove)
}

# player move on master board
doMove <- function(position,player,board){
  if(board[postion[1],position[2],position[3],position[4]] == 0){
    board[postion[1],position[2],position[3],position[4]] <- player
  }
  return(board)
}

# prints single individual tile of TTT
printBoard <- function(board){
  for(i in 1:3){
    cat(board[i,1]," | ",board[i,2]," | ",board[i,3],"\n")
    if(i != 3){
      cat("--------------\n")
    }
  }
}


# note [a,b,,] -> a,b are the principal matrix indeces
printMasterBoard <- function(masterBoard){
  for(i in 1:3){
    for(j in 1:3){
      cat(c(masterBoard[i,1,j,], " | ", masterBoard[i,2,j,], " | ", masterBoard[i,3,j,],"\n"))
    }
    if(i != 3){
      cat("--------------------------\n")
    }
  }
}


######### Random Agent ########
# this agent plays randomly against another random agent
simulUTTTRandom <- function(theseed){
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

# run and find win ratio
# playing as player 1 - randomly

wins <- 0 
draws <- 0
losses <- 0
nepis <- 10000

winRatio <- vector("numeric",length = nepis)
drawRatio <- vector("numeric",length = nepis)
lossRatio <- vector("numeric",length = nepis)

for(i in 1:nepis){
  result <- simulUTTTRandom(i)[[1]]
  
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


######### State Approximation 1 + Q-Learning Agent ########
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
# cinvert sub-board to a vector
suboardToVector <- function(subBoard){
  vec <- c()
  for(i in 1:3){
    for(j in 1:3){
      vec <- c(vec,subBoard[i,j])
    }
  }
  return(vec)
}

# convert action {1,...,9} to tis corresponding vector form
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

# convert action in vector form to decimal {1,...,9}
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
# this is the unique mapping to decimal
stateToDec <- function(boardVec){
  dec <- 0
  for(i in 1:length(boardVec)){
    dec <- dec + boardVec[i]*3^(i-1)
  }
  return(dec)
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
ApplyQLearningAgent1 <- function(qInit,episodeSimu,stepSize){
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

qEstimQ <- ApplyQLearningAgent1(qEstimQ,StateActionReward,stepsize)


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


######### State Approximation 2 + Q-Learning + Double Q-Learning Agent #########
# take the state and its number location (1 to 9) and convert it to a unique decimal from 0 to 3^11-1
# this is the unique mapping
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
simulUTTTAgent2 <- function(theseed){
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
  run <- simulUTTTAgent2(i)
  
  rewardR <- reward(run[[1]],playingAs = 1)
  states <- c()
  # need to preprocess the states so that they are in decimal format
  for(i in 1:length(run[[4]])){
    states <- c(states, stateToDec(run[[3]][i,],run[[4]][i]))
  }
  
  StateActionReward <- append(StateActionReward, list(list(rewardR,states,run[[5]])))
  
}


# use Watkin's Q Learning Techinque - Input all the simulations as a list of rewards, states and actions
QLearningAgent2 <- function(qInit,episodeSimu,stepSize){
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


qEstimQ <- QLearningAgent2(qEstimQ,StateActionReward,stepsize)



# used qEstim found by Q Learning to play UTTT
simulUTTTQLearningAgent2 <- function(theseed,qEstim){
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
  result <- simulUTTTQLearningAgent2(i,qEstimQ)[[1]]
  
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

ApplyDoubleQLearningAgent2 <- function(qInit1,qInit2,episodeSimu,stepsize){
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
qEstim1 <- ApplyDoubleQLearningAgent2(qEstim1,qEstim2,StateActionReward,stepsize)

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
  result <- simulUTTTQLearningAgent2(i,qEstim1)[[1]]
  
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


######### State Approximation 3 + Q-Learning (NO Double Q-Learning) #####
# Note: Running this section may require clearing your global environment

# Take subboard and convert to binary vector where
# 1: X is there
# 0: X is not there (i.e. no diff between O and blank)
suboardToBinVector <- function(subBoard){
  vec <- c()
  for(i in 1:3)
    for(j in 1:3){
      if(subBoard[i,j] == 1)
        vec <- c(vec,1)
      else
        vec <- c(vec,0)
    }
  return(vec)
}

# uniquely map currentboard + location + statusboard
stateToDec <- function(currentBoard,location,statusBoardBin){
  # use unique mapping
  ternD <- ternToDec(currentBoard)
  binD <- binToDec(statusBoardBin)
  # unqiue mapping
  dec <- ternD + (location-1)*3^9 + (binD)*3^11
}
# simulate with state approximations
simulUTTTAgent3 <- function(theseed){
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
  statusBoardState <- matrix(0, ncol = 9)
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
      # add status board to states
      statusBoardState <- rbind(statusBoardState, suboardToBinVector(statusBoard))
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
  
  return(list(winner,boardStates,currentBoardState,locationStates,statusBoardState,actionList))
}
# create a dataset that contains runs of random plays (states) + of the winner (rewards)
# increase number of episodes since state space is larger
nepis <- 50000
StateActionReward <- list()

# playing as player 1
for(i in 1:nepis){
  run <- simulUTTTAgent3(i)
  
  rewardR <- reward(run[[1]],playingAs = 1)
  states <- c()
  # need to preprocess the states so that they are in decimal format
  for(i in 1:length(run[[4]])){
    states <- c(states, stateToDec(run[[3]][i,],run[[4]][i],run[[5]][i,]))
  }
  StateActionReward <- append(StateActionReward, list(list(rewardR,states,run[[6]])))
}

# use Watkin's Q Learning Techinque - Input all the simulations as a list of rewards, states and actions
ApplyQLearningAgent3 <- function(qInit,episodeSimu,stepSize){
  # don't reassing qInit because of space complexity
  
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
      qInit[S_t+1,A_t] <-  qInit[S_t+1,A_t] + stepSize*(R_tplus1 + max(qInit[S_tplus1+1,]) - qInit[S_t+1,A_t]) 
    }
  }
  return(qInit)
}

# apply q learning
stepsize <- 0.1

qEstimQ <- matrix(0,nrow = 3^11*2^9, ncol = 9) # biggest possible state space


qEstimQ <- ApplyQLearningAgent3(qEstimQ,StateActionReward,stepsize)


# used qEstim found by Q Learning to play UTTT
simulUTTTQLearningAgent3 <- function(theseed,qEstim){
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
    stateDec <- stateToDec(suboardToVector(masterBoard[move[1],move[2],,]),locationToDec(c(move[1],move[2])),suboardToBinVector(statusBoard))
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
  
  # The game continues normally
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
        stateDec <- stateToDec(suboardToVector(masterBoard[move[1],move[2],,]),
                               locationToDec(c(move[1],move[2])),
                               suboardToBinVector(statusBoard))
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
  result <- simulUTTTQLearningAgent3(i,qEstimQ)[[1]]
  
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


######### Features + One-Step Sarsa (semi-Gradient) #####
# x1 to x9 number of x in sub
# x10 to x18 number of o in sub
# x19 number of x won
# x20 number of o won
x <- function(state,action){
  # state is list of master + status board
  # action is move coordinate
  feature <- vector(mode="numeric",length = 20)
  # update to s'
  state[[1]][action[1],action[2],action[3],action[4]] <- 1
  # update the status board in case
  if(hasWonBoard(state[[1]][action[1],action[2],,],player=1))
    state[[2]][action[1],action[2]] <- 1
  
  # create feature vector
  for(r in 1:3)
    for(c in 1:3)
      for(rprime in 1:3)
        for(cprime in 1:3){
          if(state[[1]][r,c,rprime,cprime] == 1){
            if(r == 1)
              feature[c] <- feature[c] + 1
            else if(r == 2)
              feature[c + 3] <- feature[c + 3] + 1
            else
              feature[c + 6] <- feature[c + 6] + 1
          }
          else if(state[[1]][r,c,rprime,cprime] == 2){
            if(r == 1)
              feature[c + 9] <- feature[c + 9] + 1
            else if(r == 2)
              feature[c + 3 + 9] <- feature[c + 3 + 9] + 1
            else
              feature[c + 6 + 9] <- feature[c + 6 + 9] + 1
          }
        }
  # for status board
  for(r in 1:3)
    for(c in 1:3){
      if(state[[2]][r,c] == 1){
        feature[19] <- feature[19] + 1
      }
      else if(state[[2]][r,c] == 2){
        feature[20] <- feature[20] + 1
      }
    }
  return(feature)
}

# approx state-action value using q(s,a,w)
qHat <- function(state,action,weight) # state should be master + status board in list
{
  return(x(state,action) %*% weight)
}

# gradient q - by construction it's just the feature
gradientQ <- function(state,action,weight){
  return(x(state,action))
}

semiGradientSarsa <- function(stepsize,epsilon,weight,nepis){
  # track win rate
  wins <- 0 
  draws <- 0
  losses <- 0
  nepis <- 10000
  
  winRatio <- vector("numeric",length = nepis)
  drawRatio <- vector("numeric",length = nepis)
  lossRatio <- vector("numeric",length = nepis)
  
  for(i in 1:nepis){
    # To reproduce experiment
    set.seed(i)
    
    # Every 1000 episodes reduce the epsilon by half
    if(i %% 1000 == 0)
      epsilon <- epsilon/2
    
    # Start game
    masterBoard <- array(0, dim = c(3,3,3,3))
    statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
    winner <- 0  # Initially a tie
    
    # First move
    player <- 1
    
    # Since we have q can use epsilon greedy approach
    allMoves <- list()
    for(r in 1:3)
      for(c in 1:3)
        for(rprime in 1:3)
          for(cprime in 1:3)
            allMoves <- append(allMoves, list(c(r,c,rprime,cprime)))
    # epsilon greedy approach
    if(runif(1,0,1) < epsilon) # non-greedy
    {
      move <- sample(allMoves, size = 1)
      move <- move[[1]]
    }
    else{
      
      # find the max action based on values in qEstim
      maxValue <- -Inf
      maxMove <- -1
      for(move in allMoves){
        # first find Q(s,a)
        valueOfMove <- qHat(list(masterBoard,statusBoard),move,weight)
        
        # find the max
        if(valueOfMove > maxValue){
          maxValue <- valueOfMove
          maxMove <- move
        }
      }
      move <- maxMove
    }
    # store state in S
    S <- list(masterBoard,statusBoard)
    
    # start with max move according to qHat
    masterBoard[move[1], move[2], move[3], move[4]] <- player
    forcedMove <- c(move[3], move[4])
    player <- player %% 2 + 1
    
    # A,S',A' for updating the weights
    A <- move
    Sprime <- NULL
    Aprime <- NULL
    
    # The game continues normally
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
        # use epsilon greedy approach
        if(runif(1,0,1) < epsilon) # non-greedy
        {
          move <- sample(allMoves, size = 1)
          move <- move[[1]]
        }
        else {
          # find the max action based on values in qEstim
          maxValue <- -Inf
          maxMove <- -1
          for(move in allMoves){
            # first find Q(s,a)
            valueOfMove <- qHat(list(masterBoard,statusBoard),move,weight)
            
            # find the max
            if(valueOfMove > maxValue){
              maxValue <- valueOfMove
              maxMove <- move
            }
          }
          move <- maxMove
        }
        # store S' and A'
        Aprime <- move
        Sprime <- list(masterBoard,statusBoard)
      }
      
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
      if(player == 1) # update weights for R = 0
      {
        qHatPrime <- qHat(Sprime,Aprime,weight)
        qHatInit <- qHat(S,A,weight)
        grad <- gradientQ(S,A,weight)
        weight <- weight + stepsize*(qHatPrime - qHatInit)*(grad) # R is 0 until the end
        S <- Sprime
        A <- Aprime
      }
      
      player <- player %% 2 + 1  # Change player
    }
    # format states based on winner
    if(winner == 1){
      S <- Sprime
      A <- Aprime
    }
    # record final reward
    R <- reward(winner,playingAs = 1)
    # update weights
    qHatInit <- qHat(S,A,weight)
    grad <- gradientQ(S,A,weight)
    weight <- weight + stepsize*(R - qHatInit)*grad
    
    # for win, draw and loss ratios
    if (winner == 0)
      draws <- draws + 1
    else if(winner == 1)
      wins <- wins + 1
    else
      losses <- losses + 1
    
    winRatio[i] <- wins/i
    drawRatio[i] <- draws/i
    lossRatio[i] <- losses/i
  }
  return(list(winRatio,drawRatio,lossRatio))
}

# test Sarsa w/ features
initW <- vector("numeric",length=20)
nepis <- 10000
stepsize <- 2^-20
epsi <- 0.4
ratios <- semiGradientSarsa(stepsize,epsi,initW,nepis)

# graph
winRatio <- ratios[[1]]
drawRatio <- ratios[[2]]
lossRatio <- ratios[[3]]


plot(x=1:nepis,winRatio,ylab= "Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(drawRatio,col="blue")
lines(lossRatio,col="red")
legend(y=0.6,x=8000,legend=c("Win Ratio","Draw Ratio","Loss Ratio"),
       col=c("darkgreen","blue","red"),lty=1,cex = 0.8)


# print the win, loss and draw ration epsi = 0.4, stepsize = 2^-20
cat("The win ratio given a QL policy was ", winRatio[nepis],"\n")
cat("The draw ratio given a QL policy was ", drawRatio[nepis],"\n")
cat("The loss ratio given a QL policy was ", lossRatio[nepis],"\n")
# test Sarsa w/ features - epsi = 0.2
initW <- vector("numeric",length=20)
nepis <- 10000
stepsize <- 2^-20
epsi <- 0.2
ratios <- semiGradientSarsa(stepsize,epsi,initW,nepis)

# graph
winRatio1 <- ratios[[1]]
drawRatio1 <- ratios[[2]]
lossRatio1 <- ratios[[3]]


plot(x=1:nepis,winRatio1,ylab= "Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(drawRatio1,col="blue")
lines(lossRatio1,col="red")
legend(y=0.6,x=8000,legend=c("Win Ratio","Draw Ratio","Loss Ratio"),
       col=c("darkgreen","blue","red"),lty=1,cex = 0.8)


# print the win, loss and draw ration epsi = 0.2, stepsize = 2^-20
cat("The win ratio given a QL policy was ", winRatio1[nepis],"\n")
cat("The draw ratio given a QL policy was ", drawRatio1[nepis],"\n")
cat("The loss ratio given a QL policy was ", lossRatio1[nepis],"\n")

# test Sarsa w/ features - epsi = 0.1
initW <- vector("numeric",length=20)
nepis <- 10000
stepsize <- 2^-20
epsi <- 0.1
ratios <- semiGradientSarsa(stepsize,epsi,initW,nepis)

# graph
winRatio2 <- ratios[[1]]
drawRatio2 <- ratios[[2]]
lossRatio2 <- ratios[[3]]


plot(x=1:nepis,winRatio2,ylab= "Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(drawRatio2,col="blue")
lines(lossRatio2,col="red")
legend(y=0.6,x=8000,legend=c("Win Ratio","Draw Ratio","Loss Ratio"),
       col=c("darkgreen","blue","red"),lty=1,cex = 0.8)


# print the win, loss and draw ration epsi = 0.1, stepsize = 2^-20
cat("The win ratio given a QL policy was ", winRatio2[nepis],"\n")
cat("The draw ratio given a QL policy was ", drawRatio2[nepis],"\n")
cat("The loss ratio given a QL policy was ", lossRatio2[nepis],"\n")

# win ratio plots
plot(x=1:nepis,winRatio,ylab= "Win Ratios", xlab = "Episodes", type="l",col="darkgreen",ylim = c(0,1))
lines(winRatio1,col="blue")
lines(winRatio2,col="red")
legend("bottomright",legend=c(expression(paste(epsilon == 0.4)),expression(paste(epsilon == 0.2)),expression(paste(epsilon == 0.1))),
       col=c("darkgreen","blue","red"),lty=1,cex = 0.8)