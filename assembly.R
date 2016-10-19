library(igraph)
library(igraphdata)
library(deSolve)
library(rend)
library(NetIndices)
library(reshape2)

motif_counter <- function(graph.lists){
  require(igraph)
  
  if(!is.list(graph.lists)){
    stop("The input should be a list of graph objects")
  }
  
  triad.count <- lapply(graph.lists, triad.census)
  triad.matrix <- matrix(unlist(triad.count), nrow = length(graph.lists), ncol = 16, byrow = T)
  colnames(triad.matrix) <- c("empty", "single", "mutual", "s5", "s4", "s1", "d4",
                              "d3", "s2", "s3","d8", "d2", "d1", "d5", "d7", "d6")
  
  triad.df <- as.data.frame(triad.matrix)
  
  motif.data.frame <- data.frame(s1 = triad.df$s1, s2 = triad.df$s2, s3 = triad.df$s3, s4 = triad.df$s4, 
                                 s5 = triad.df$s5, d1 = triad.df$d1, d2 = triad.df$d2, d3 = triad.df$d3, d4 = triad.df$d4,
                                 d5 = triad.df$d5, d6 = triad.df$d6, d7 = triad.df$d7, d8 = triad.df$d8)
  
  return(motif.data.frame)
}

getmot <- function(web){
  com <- combn(length(V(web)), 3)
  adj <- get.adjacency(web, sparse = F)
  
  tbt <- lapply(1:ncol(com), function(x){adj[com[,x], com[,x]]})
  conn <- sapply(tbt, function(x) is.connected(graph.adjacency(x)))
  mots <- motif_counter(lapply(tbt[conn], graph.adjacency))
  
  m1 <- melt(lapply(1:sum(conn), function(x) com[,x]))
  m2 <- mots[m1$L1,]
  m3 <- aggregate(m2, list(m1$value), sum)
  
  
  return(m3)
}

ext1 <- function(times, states, parms){
  with(as.list(states),{
    states[states < (10^(-10))] <- 0
    return(c(states))
  })
}

n.vals <- function(S,C){
  niche<-runif(S,0,1)
  r<-rbeta(S,1,((1/(2*C))-1))*niche
  
  ci <- runif(S, r/2, niche)
  
  return(cbind(niche = niche, ci = ci, r = r))
}

n.mat <- function(nv){
  S <- nrow(nv)
  new.mat<-matrix(0,nrow=S,ncol=S)
  
  for(i in 1:S){
    
    for(j in 1:S){
      if(nv[j,1]>(nv[i,2]-(.5*nv[i,3])) && nv[j,1]<(nv[i,2]+.5*nv[i,3])){
        new.mat[j,i]<-1
      }
    }
  }
  
  #new.mat<-new.mat[order(apply(new.mat,2,sum)),order(apply(new.mat,2,sum))]
  return(new.mat)
}

cond <- FALSE
while(!cond){
  nva <- n.vals(10, .15)
  nm1 <- n.mat(nva)
  
  cond <- is.connected(graph.adjacency(nm1))
}
nva2 <- n.vals(100, .1)
nva3 <- n.vals(100, .2)


tind <- TrophInd(nm1)
metsca <- runif(10, .2, .3)
xi <- (((10^2)^tind$TL)/100)^-.25*0.314
ri <- as.numeric(colSums(nm1) == 0)

spp1 <- 1:10
par <- list(K = 1, x.i = xi[spp1], yij = 8, eij = 0.85, xpar = .2, 
            B.o = 0.5, r.i = ri[spp1], A = nm1[spp1, spp1], G.i = Gi, FR = Fij)

out1 <- ode(y = runif(10, .1, 1), times = 1:1000, func = CRmod, parms = par, 
            events = list(func = ext1, time = 1:1000))
matplot(out1[,-1], typ = "l")
matplot(out1[900:1000,-1], typ = "l")
sd(out1[800:1000,-1])/mean(out1[800:1000,-1])

cv1 <- apply(out1[,-1], 2, function(x) sd(x[800:1000])/mean(x[800:1000]))
cv1[is.nan(cv1)] <- 0

plot(cv1~tind$TL)
tind2 <- TrophInd(nm1[out1[1000,-1] > 0, out1[1000,-1] > 0])
plot((cv1[out1[1000,-1] > 0])~tind2$TL)
plot(cv1~metsca)


#mot1 <- getmot(graph.adjacency(nm1))



par <- list(K = 1, x.i = xi, yij = 8, eij = 0.85, xpar = 0.2, 
            B.o = 0.5, r.i = ri, A = nm1, G.i = Gi, FR = Fij)

nm2 <- nm1[out1[1000,-1] > 0,out1[1000,-1] > 0]
ri <- as.numeric(colSums(nm1) == 0)
#nm2 <-nm1
out2 <- lapply(1:10, function(x) matrix(0, nrow = 2000, ncol = 100))
for(i in 1:10){
  newrow <- rbinom(ncol(nm2)+1, 1, .2)
  newrow[ri == 1] <- 0
  nm2 <- rbind(cbind(nm2, rbinom(nrow(nm2), 1, .2)), newrow)
  tind <- TrophInd(nm2)
  
  xi <- (((10^2)^tind$TL)/100)^-.25*0.314
  ri <- as.numeric(colSums(nm2) == 0)
  
  par <- list(K = 1, x.i = xi, yij = 8, eij = 0.85, xpar = 0.2, 
              B.o = 0.5, r.i = ri, A = nm2, G.i = Gi, FR = Fij)
  
  if(i == 1){states1 <- c(out1[1000,-1][out1[1000,-1] > 0], .1)}else{states1 <- c(out2[[i-1]][2000,-1], .1)}
  res <- ode(y = states1, times = 1:2000, func = CRmod, parms = par, 
              events = list(func = ext1, time = 1:2000))
  out2[[i]][,1:ncol(nm2)] <- res[,2:ncol(res)]
  print(i)
}

out2[[20]][1000,-1]

plot(graph.adjacency(nm2[out2[[20]][1000,-1]>0,out2[[20]][1000,-1]>0]))

sapply(out2, function(x) sum(x[1000,-1]>0))

matplot(out2[[20]][,-1], typ = "l")
