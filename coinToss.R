library(ggplot2)
library(dplyr)

##Simple heads or tails function to make sure our RGN works
headsOrTails <- function() {
  heads <- 0
  tails <- 0
  for (i in 1:1000000) {
    c = sample(0:1,1)
    if (c == 0) {
      heads <- heads + 1
    } else {
      tails <- tails + 1
    }
  }
  print(tails/(heads + tails))
  print(heads/(heads + tails))
}

headsOrTails()



###Runs a coin flip simulation and outputs a winner or tie.
# a [double] amount of money in player 1 bankroll
# b [double] multiplier for amount of money in player 2 bankroll
#S [int] Sample size
#w [double] the fraction of a gained when a player wins
#l [double] the fraction of a lost when a player loses
#Outputs: a length 3 vector where a 1 represents a positive result for a P1 win, P2 win, or Tie respectively
runSim <- function(a,b,s,w,l) {
  
  player1 <- a
  player2 <- a * b
  output <- c(0,0,0)
  for (i in 1:s) {
    coin = sample(0:1,1)
    if (coin == 0) {
      player1 <- player1 - (l * a)
      player2 <- player2 + (w * a)
    } else {
      player1 <- player1 + (w * a)
      player2 <- player2 - (l * a)
    }
    
    if (player1 <= 0) {
      output[2] <- 1
      output
      break
    } else if (player2 <= 0) {
      output[1] <- 1
      output
      break
    }
    
  }
  
  if (player1 != 0 & player2 != 0) {
    output[3] = 1
    output
  }
  
  output
}




##Runs n amount of simulations to determine P1 win $, P2 win$, Tie%, P2% advantage
##n: amount of sims to run
#a: starting money
#s: sample size required for a single game
#swing: how much a player will win or lose based on outcome
#Player2 bankroll edge
runMetaSim <- function(n, a, s, swing, p2Multiplier) {
  results <- c(0,0,0)
  for (i in 1:n) {
    r <- runSim(a,p2Multiplier,s,swing,swing)
    if (r[1] == 1) {
      results[1] <- results[1] + 1
    } else if (r[2] == 1) {
      results[2] <- results[2] + 1
    } else {
      results[3] <- results[3] + 1
    }
  }
  output <- c(results[1]/sum(results),results[2]/sum(results),results[3]/sum(results),-1 *(1 - p2Multiplier))
  output
}




##Runs x amount of Meta-"Simulations"  gradually increasing the bankroll edge of one player by a given amount
#x[int]: amount of simulations to run
#start[double]: starting edge that player 2 has
#increment: a percentage increase for the succesive simulation
#Outputs: A two dimensional Data Frame: Cols[P1 win %, P2 win %, Tie %, P2$ Advantage]
runAggregateSims <- function(x, start, increment, swing,rounds) {
  cols <- c("P1 Win %","P2 Win %", "Tie %","P2 Bankroll Advantage")
  out <- data.frame(matrix(ncol = 4, nrow = 0))
  
  for (i in 1:x) {
    start <- start + increment
    row <- runMetaSim(rounds,1000,1000000,swing,start)
    
    out <- rbind(out,row)
    print(paste("Sim ", toString(i), "/", toString(x), " Complete"))
  }
  colnames(out) <- cols
  out
 
}

##Runs the aggregate sims using the Huygen's Theorom
runAggregateSims2 <- function(x,start,increment,swing,a,b) {
  cols <- c("P1 Win %","P2 Win %", "Tie %","P2 Bankroll Advantage")
  out <- data.frame(matrix(ncol = 4, nrow = 0))
  
  for (i in 1:x) {
    b <-  b + increment
    p1 <- (a/.02*a)/(((b*a)/.02*a) + (a/(0.2*a)))
    p2 <- ((b*a)/.02*a)/(((b*a)/.02*a) + (a/(0.2*a)))
    
    p1winrate <- p1/(p1 + p2)
    p2winrate <- p2/(p1 + p2)
    
    row <- c(p1winrate,p2winrate,0,-1 * (1 - b))
    out <- rbind(out,row)
    print(paste("Sim ", toString(i), "/", toString(x), " Complete"))
  }
  colnames(out) <- cols
  out
}

#Plotting the Distribution
huygensFormula <- runAggregateSims2(300,1,.01,.02,1000,1)
huygensFormulaPercent <- huygensFormula * 100

qplot(huygensFormulaPercent$`P2 Bankroll Advantage`,huygensFormulaPercent$`P1 Win %`,xlab = "Player 2 Bankroll Advantage %",ylab = "Player 1 Win Likelihood %",
      main = "Probability Player 1 Wins")
qplot(huygensFormulaPercent$`P2 Bankroll Advantage`,huygensFormulaPercent$`P2 Win %`,xlab = "Player 2 Bankroll Advantage %",ylab = "Player 2 Win Likelihood %",
      main = "Probability Player 2 Wins")

percentCombined <- cbind(huygensFormulaPercent,sims)



a <- ggplot() + geom_point(data = simsWithPercent,aes(x= simsWithPercent$`P2 Bankroll Advantage`, y = simsWithPercent$`P2 Win %`),color = "yellow") + xlab("P2 % Bankroll Advantage") + ylab("Player 2 % Win Likelihood") +
geom_line(data = huygensFormulaPercent, aes(x = huygensFormulaPercent$`P2 Bankroll Advantage`,y=huygensFormulaPercent$`P2 Win %`),color = "blue") + ggtitle("Player 2 Winning Distribution: Huygen's Formula vs Simulation") +
theme(legend.position = "top") + theme(legend.title = element_text(colour = "blue", size = 10)) + scale_color_discrete("Legend", c("Huygden's Formula","Sim Results"))
  
a






sims <- runAggregateSims(200,1,.01,.02,1000)
sims2 <- runAggregateSims(100,3,.01,.02,1000)
sims <- rbind(sims,sims2)


sims <- runAggregateSims(300,1,.01,.02,1000)

simsWithPercent <- sims
simsWithPercent$`P2 Bankroll Advantage` <- simsWithPercent$`P2 Bankroll Advantage` * 100
simsWithPercent$`P2 Win %` <- simsWithPercent$`P2 Win %` * 100
simsWithPercent$`P1 Win %` <- simsWithPercent$`P1 Win %` * 100


qplot(simsWithPercent$`P2 Bankroll Advantage`,simsWithPercent$`P2 Win %`,xlab = "Player 2 Bankroll Advantage %",ylab = "Player 2 Win Likelihood %",
      main = "Probability Player 2 Wins")


qplot(simsWithPercent$`P2 Bankroll Advantage`,simsWithPercent$`P1 Win %`,xlab = "Player 2 Bankroll Advantage %",ylab = "Player 1 Win Likelihood %",
      main = "Probability Player 1 Wins")



sim2 <- runAggregateSims()


