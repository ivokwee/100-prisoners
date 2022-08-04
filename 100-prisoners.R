## The 100 prisoners problem is a mathematical problem in probability
## theory and combinatorics. In this problem, 100 numbered prisoners
## must find their own numbers in one of 100 drawers in order to
## survive. The rules state that each prisoner may open only 50
## drawers and cannot communicate with other prisoners. At first
## glance, the situation appears hopeless, but a clever strategy
## offers the prisoners a realistic chance of survival. Danish
## computer scientist Peter Bro Miltersen first proposed the problem
## in 2003.
##
## See more details: https://en.wikipedia.org/wiki/100_prisoners_problem


##------------------------------------------------------------------------
## Define optimal strategy: choose box with own number, check and then
## go to next box according to number in the laste box. Repeat until
## your number is found or fail if 50 steps are exceeded.
## ------------------------------------------------------------------------

strategicTurn <- function(field, i) {
    kmax <- length(field)/2
    for(k in 1:kmax) {
        if(k==1) {
            next.box <- i  ## start with box with own number
            ##next.box <- sample(100,1)            
        } else {
            next.box <- field[last.box]
        }
        if(field[next.box]==i) return(TRUE)
        last.box <- next.box
    }
    return(FALSE) ## failed
}

## random choice strategy... not good
randomTurn <- function(field, i) {
    sel <- sample(100,50)
    opened <- field[sel]
    opened
    if(i %in% opened) return(TRUE)
    return(FALSE)
}


##------------------------------------------------------------------------
## Repeat 1000 times the game: each prisoner adheres to the strategy.
##------------------------------------------------------------------------

niter=1000
result <- rep(NA,niter)
for(i in 1:niter) {
    field <- sample(100)
    ##game  <- sapply(1:100, function(i) randomTurn(field, i))
    game  <- sapply(1:100, function(i) strategicTurn(field, i))    
    result[i] <- all(game==TRUE)*1
}
mean(result)

##------------------------------------------------------------------------
## What is the probability distribution of maximum ring size?
##------------------------------------------------------------------------
max.cycle <- c()
for(i in 1:100000) {

    # sample new game field
    field <- sample(100)

    # define links/edges and make the graph
    edges <- mapply(c, 1:100, field[1:100])
    G <- igraph::make_graph(as.vector(edges))
    ##plot(G, vertex.size=0.4)    

    # get maximum cycle size
    csize <- max(components(G)$csize)
    csize    
    max.cycle <- c(max.cycle, csize)
}

## plot example graph
png("graph.png")
plot(G, vertex.size=7, layout=layout_with_fr)
dev.off()

## histogram of max.cycle
png("histogram.png")
hist(max.cycle, breaks=100)
abline(v=50, lty=2, col="red", lwd=2)
dev.off()

##------------------------------------------------------------------------
## What is the probability that the max ring size is 50 or smaller?
##------------------------------------------------------------------------
mean(max.cycle <= 50)


##------------------------------------------------------------------------
## Does the probability of winning depend on the number of prisoners?
## We repeat the same but for different number of prison sizes (psize).
##------------------------------------------------------------------------
niter=1000  ## increase for more accuracy
psizes = c(10,20,50,100,250,500,1000)
all.results <- c()
for(psize in psizes) {
    result <- rep(NA,niter)
    for(i in 1:niter) {
        field <- sample(psize)
        ##game  <- sapply(1:100, function(i) randomTurn(field, i))
        game  <- sapply(1:psize, function(i) strategicTurn(field, i))    
        result[i] <- all(game==TRUE)*1
    }
    mean(result)
    all.results <- c(all.results, mean(result))
}

all.results
barplot(all.results, names.arg=paste("L=",psizes))

