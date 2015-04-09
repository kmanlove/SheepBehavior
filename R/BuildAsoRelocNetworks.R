BuildAsoRelocNetworks <- function(data, ewes.without.lambs, yearlings) {
  # function that builds Asotin networks and calculated assortativity
  #
  # Args:
  #
  # data = clean relocation data
  # ewes.without.lambs = character vector of all animal IDs for ewes who don't have lambs
  # years = character vector of animalIDs for all yearlings
  #
  # Return:
  # 
  # graph.out = list of [[1]] graph object and [[2]] weighted edgelist
  # assort.mat = assortativity matrix as returns by assortnet, rounded to 3 decimals
  #
  edgelists.nozeros <- edgelists <- inds <- output.info <- data.subsets <- list(NA, 1)
  #-- build list of relevant data subsets.
  data.subsets[[1]] <- data
  data.subsets[[2]] <- subset(data, Year == 2013)
  data.subsets[[3]] <- subset(data, Year == 2014)
  
  edgeweight.min <- .1
  pop.id <- "Summer2014"
  year.id <- "2014"
  
  
  for(i in 1:3) {
    if(dim(data.subsets[[i]])[1] == 0 | length(levels(factor(data.subsets[[i]]$EWEID))) == 1){
      edgelists[[i]] <- NA
      inds[[i]] <- NA
      output.info[[i]] <- NA
    } else{
      out <- AssocTimeVect(AssocData=data.subsets[[i]])
      inds[[i]] <- out[[1]]
      edgelists[[i]] <- out[[2]]
      edgelists[[i]]$edgeweights <- as.numeric(as.character(edgelists[[i]]$TimesTogether)) / (as.numeric(as.character(edgelists[[i]]$TotalInd1)) + as.numeric(as.character(edgelists[[i]]$TotalInd2)) - as.numeric(as.character(edgelists[[i]]$TimesTogether)))
      #-- subset to remove all edges where edgeweight == 0 --#
      #   edgelists.nozeros[[i]] <- subset(edgelists[[i]], edgeweights != 0)
      edgelists.nozeros[[i]] <- subset(edgelists[[i]], edgeweights >=
                                         edgeweight.min)
      #-- included edges with weight = 0.  No good. 
      el <- cbind(as.character(edgelists.nozeros[[i]]$Ind1), as.character(edgelists.nozeros[[i]]$Ind2))
      #-- Were any associations observed? Generate list of isolated nodes --#
      #-- to add to network later --#  
      connected.nodes <- levels(factor(c(el[, 1], el[, 2])))
      isolated.nodes <- inds[[i]][! (inds[[i]] %in% connected.nodes) == T] 
      
      if(dim(el)[1] == 0){  #-- if NO associations observed:   
        res.component <- no.components <- max.component.size <- res.component.size <- node.degree <- cv.edgeweights <- mean.edgeweights <- sd.edgeweights <- local.efficiency <- rep(NA, length(out[[1]]))
        for(m in 1:length(isolated.nodes)){
          res.component[m] <- m
          no.components[m] <- length(isolated.nodes)
          max.component.size[m] <- 1
          node.degree[m] <- cv.edgeweights[m] <- mean.edgeweights[m] <- sd.edgeweights[m] <- local.efficiency[m] <- 0
        }
        individ.ids <- as.character(isolated.nodes)
        #-- leave everything as NAs for now....
      } else {
        static.graph.orig <- graph.edgelist(el, directed=F)
        static.graph.weighted <- set.edge.attribute(static.graph.orig, "weight", value = edgelists.nozeros[[i]]$edgeweights)
        E(static.graph.weighted)$weight<-edgelists.nozeros[[i]]$edgeweights
        if (length(isolated.nodes) >= 1){
          static.graph <- add.vertices(static.graph.weighted, nv = length(isolated.nodes), name = as.character(isolated.nodes))
        } else {
          static.graph <- static.graph.weighted
        }
        graph.clusters <- clusters(static.graph)
        obs.clique.communities <- clique.community(static.graph, k = 3)
        components <- length(levels(factor(graph.clusters$membership)))
        numK3communities <- length(obs.clique.communities)
        k3communities <- res.component <- no.components <- max.component.size <- res.component.size <- node.degree <- cv.edgeweights <- mean.edgeweights <- sd.edgeweights <- rep(NA, length(out[[1]]))
        no.k3com <- max.k3com.size <- no.communities.for.this.node <- rep(NA, length(out[[1]]))
        res.k3com <- vector("list", length(out[[1]]))
        for(m in 1:length(V(static.graph)$name)){
          if(dim(edgelists.nozeros[[i]])[1] == 0){ #-- if the graph has no edges
            node.degree[m] <- cv.edgeweights[m] <- mean.edgeweights[m] <- sd.edgeweights[m] <- local.efficiency[m] <- 0
            no.components[m] <- length(levels(factor(V(static.graph)$name)))
            max.component.size[m] <- 1
            res.component.size[m] <- 1
            res.component[m] <- m
            mean.edgeweights[m] <- 0
            sd.edgeweights[m] <- NA
            cv.edgeweights[m] <- NA
            res.k3com[[m]] <- NA
            no.k3com[m] <- NA
            no.communities.for.this.node[m] <- NA
            max.k3com.size[m] <- 1
            k3communities[m] <- numK3communities
          } else {
            #-- pull all edges linked to node m --#
            ind.edges <- subset(edgelists.nozeros[[i]], Ind1 ==
                                  as.character(V(static.graph)$name)[m] | Ind2 ==
                                  as.character(V(static.graph)$name)[m])
            if(dim(ind.edges)[1] == 0){
              node.degree[m] <- cv.edgeweights[m] <- mean.edgeweights[m] <- sd.edgeweights[m] <- 0
              no.components[m] <- length(levels(factor(graph.clusters$membership)))
              max.component.size[m] <- max(graph.clusters$csize)
              res.component.size[m] <- graph.clusters$csize[graph.clusters$membership[m]]
              res.component[m] <- graph.clusters$membership[m]
              mean.edgeweights[m] <- 0
              sd.edgeweights[m] <- NA
              cv.edgeweights[m] <- NA
              res.k3com[[m]] <- NA
              no.communities.for.this.node[m] <- NA
              max.k3com.size[m] <- max(unlist(lapply(obs.clique.communities, length)))
              k3communities[m] <- numK3communities
            } 
            else {
              cv.edgeweights[m] <- (sd(ind.edges$edgeweights)) / (mean(ind.edges$edgeweights))
              mean.edgeweights[m] <- mean(ind.edges$edgeweights)
              sd.edgeweights[m] <- sd(ind.edges$edgeweights)[1]
              node.degree[m] <- degree(static.graph.weighted)[as.character(V(static.graph)$name)[m]]
              no.components[m] <- length(levels(factor(graph.clusters$membership)))
              max.component.size[m] <- max(graph.clusters$csize)
              res.component.size[m] <- graph.clusters$csize[graph.clusters$membership[m]]
              res.component[m] <- graph.clusters$membership[m]
              res.k3com[[m]] <-  which(unlist(lapply(obs.clique.communities, clique.test.fun)) == T)
              no.communities.for.this.node[m] <- length(res.k3com[[m]])
              max.k3com.size[m] <- max(unlist(lapply(obs.clique.communities, length)))
              k3communities[m] <- numK3communities
            }
          }
        }
        individ.ids <- as.character(V(static.graph)$name)
      }
      pop <- rep(paste(pop.id),length(V(static.graph)$name))
      year <- rep(paste(year.id),length(V(static.graph)$name))
      output.info[[i]] <- as.data.frame(cbind(individ.ids,node.degree,
                                              cv.edgeweights,mean.edgeweights,
                                              sd.edgeweights, no.components,
                                              max.component.size,
                                              res.component.size, res.component,
                                              no.communities.for.this.node,
                                              max.k3com.size, k3communities, rep(as.character(pop.id)[i], length(individ.ids)), rep(year.id[i], length(individ.ids))))
    }
  }
  
  #-- build in lamb status by hand --#
  
  data.subsets[[1]]$LambStatus <- rep(NA, dim(data.subsets[[1]])[1])
  for(i in 1:dim(data.subsets[[1]])[1]){
    data.subsets[[1]]$LambStatus[i] <- ifelse(data.subsets[[1]]$EWEID[i] %in% ewes.without.lambs, "died", "censored")  
  }
  
  data.subsets[[2]]$LambStatus <- rep(NA, dim(data.subsets[[2]])[1])
  for(i in 1:dim(data.subsets[[2]])[1]){
    data.subsets[[2]]$LambStatus[i] <- ifelse(data.subsets[[2]]$EWEID[i] %in% ewes.without.lambs, "died", "censored")  
  }
  
  data.subsets[[3]]$LambStatus <- rep(NA, dim(data.subsets[[3]])[1])
  for(i in 1:dim(data.subsets[[3]])[1]){
    data.subsets[[3]]$LambStatus[i] <- ifelse(data.subsets[[3]]$EWEID[i] %in% ewes.without.lambs.2014, "died", "censored")  
  }
  
  #-- build network --#
  edgeweight.min <- .1
  graph.out <- BuildGraph(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = 3)
  adj.out <- get.adjacency(graph.out[[1]])
  adj.out.labels <- ifelse(V(graph.out[[1]])$name %in% ewes.without.lambs == T, "EweWithoutLamb", 
                            ifelse(V(graph.out[[1]])$name %in% yearlings == T, "Yearling", "EweWithLamb"))
  
  # calculate assortativity among demographi groups
  assort.obj <- assortment.discrete(adj.out, types = adj.out.labels, weighted = T, SE = TRUE, M = 100)
  assort.obj$r
  assort.obj$SE
  assort.obj.final <- round(assort.obj$mixing_matrix, 3)
  
  return(list(graph.out = graph.out, assort.matrix = assort.obj.final))
}