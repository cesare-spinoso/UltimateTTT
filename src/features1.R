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