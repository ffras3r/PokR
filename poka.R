library(tidyverse)

J <- 11
Q <- 12
K <- 13
A <- 14


numbers <- c(A,2:10,J,Q,K)
suits <- c("H", "S", "C", "D")

deck <- as.matrix(expand.grid(numbers, suits))
colnames(deck) <- c("Rank", "Suit")

Hands <- c("HIGH CARD", "PAIR", "THREE OF A KIND","TWO PAIR", "FULL HOUSE", "FOUR OF A KIND", "STRAIGHT", "FLUSH", "STRAIGHT FLUSH", "ROYAL FLUSH")
TieWin <- c("HIGH", "KICKER")
TieLose <- c("LOW", "KICKED")

np = 5


playerCards <- array(dim = c(np, 2, 2))
communityCards <- array(dim = c(5, 2))

print(playerCards)

#deal <- function(np, deck, playerCards, communityCards){
deal <- function(){
    numCards <- np*2 + 5 + 2
    cards <- deck[sample(nrow(deck), numCards, replace=FALSE),]
    colnames(cards) <- c("Rank", "Suit")
    #print(cards[,1])
    
    for (c in 0:1) for (p in 1:np){
      playerCards[p,c+1,1] <<- cards[(np*c)+p, 1]
      playerCards[p,c+1,2] <<- cards[(np*c)+p, 2]
      
      #print(playerCards)
      
        
    }
    #print(playerCards)
    
    for (c in 1:2) for (i in 1:5){
      communityCards[i,c] <<- cards[(np*2+i), c]
    }
    
    #print(communityCards)
}

highCard <- function(hand){
  print("HIGH CARD")
  return(
    hand[hand[,1] == max(hand[,1]),]
  )
}

checkSet <- function(hand){
  check <- as_tibble(
    as.matrix(
      dist(hand[,1], diag=FALSE, upper=TRUE)
    ))
  check$card <- seq.int(nrow(check))
  check$trick <- rowSums(check==0)
  check <- arrange(check,
                   check$trick
  )
  print(hand)
  print(check)
  best <- list()
  hit <- 0
  sum = 0
  if(length(unique(check$trick)) == 1){
    print()
  }
  for (i in 1:(length(unique(check$trick))-1)){
    best[i] <- c(check[check$trick == check$trick[7-hit], 8])
    hit <- length(best[[i]])
    sum <- sum+hit
  }
  print(best)
  if(sum == 4 && length(best) == 1){
    sum = 6
  }
  
  theCards <- matrix(nrow=0, ncol=2)
  for (i in 1:length(best)){
    theCards <- rbind(theCards, hand[best[[i]],])
  }
  
  print(Hands[sum])
  return(theCards)

}

checkFlush <- function(hand){
  royalflush <- c(10, J, Q, K, A)
  
}

checkRoyalFlush <- function(hand, communityCards){
  
}


count <- function(np, playerCards, communityCards){
  for (i in 1:np){
    
  }
    
}


deal()
# print(playerCards)
# print(communityCards)
hand = matrix(nrow=7, ncol=2)
for (i in 1:1){
  hand[1,] = playerCards[i,1,]
  hand[2,] = playerCards[i,2,]
  for(j in 3:7){
    hand[j,] = communityCards[j-2,]
  }
  # print("____")
  # print(playerCards[i,,])
  # print("----")
  print(hand)
  print(
    checkSet(hand)
  )
  print(
    highCard(hand)
  )
  
}


