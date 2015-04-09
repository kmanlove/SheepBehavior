# install.packages("assortnet")
# require(assortnet)
# 
# # build graph adjacency
# #-- read in 2014 Asotin Relocations --#
# data <- read.csv("~/work/Kezia/Research/FieldStudies/HCBighornContactStudies/Data/RelocationData/AsotinLocationsFinal_20July2014.csv", header = T, sep = "\t")

# data$UTMN <- data$Longitude
# data$UTME <- data$Latitude
# data$DATE <- data$Date

#-- pull out three ram records from Asotin, as well as uncollareds --#
# ram.ids <- c("08AS31", "12AS19", "12AS23", "12AS22", "11AS06", "08AS34", "12AS21", "", "14AS16", "14AS47")
# data <- subset(data, !(EWEID %in% ram.ids))

#sumimvewes <- eweids <- levels(factor(data$EWEID))

# require(igraph)
# 
# #-- source in function to build association matrix --#
# source("~/work/Kezia/Research/EcologyPapers/ClustersAssocations_V2/ClustersAssociations/Code/DataCleaning/RelocationsToNetworks/StaticNetworkAssocMat.R")

edgelists.nozeros <- edgelists <- inds <- output.info <- data.subsets <- list(NA, 1)
#-- build list of relevant data subsets.
data.subsets[[1]] <- data
data.subsets[[2]] <- subset(data, Year == 2013)
data.subsets[[3]] <- subset(data, Year == 2014)


# clique.community <- function(graph, k){
#   clq <- cliques(graph, min = k, max = k)
#   edges <- c()
#   for(i in seq_along(clq)){
#     for(j in seq_along(clq)){
#       if((length(unique(c(clq[[i]], clq[[j]]))) == k + 1)){
#         edges <- c(edges, c(i, j) )
#       }
#     }
#   }
#   clq.graph <- simplify(graph(edges))
#   V(clq.graph)$name <- seq_len(vcount(clq.graph))
#   comps <- decompose.graph(clq.graph)
#   lapply(comps, function(x){
#     unique(unlist(clq[V(x)$name]))
#   })
# }
# clique.test.fun <- function(x){
#   y <- i %in% x 
#   return(y)
# }

edgeweight.min <- .1
pop.id <- "Summer2014"
year.id <- "2014"
#for(i in 1:length(data.subsets)){
# for(i in 1:6){
#   if(dim(data.subsets[[i]])[1] == 0 | length(levels(factor(data.subsets[[i]]$EWEID))) == 1){
#     edgelists[[i]] <- NA
#     inds[[i]] <- NA
#     output.info[[i]] <- NA
#   } else{
#     out <- AssocTimeVect(AssocData=data.subsets[[i]])
#     inds[[i]] <- out[[1]]
#     edgelists[[i]] <- out[[2]]
#     edgelists[[i]]$edgeweights <- as.numeric(as.character(edgelists[[i]]$TimesTogether)) / (as.numeric(as.character(edgelists[[i]]$TotalInd1)) + as.numeric(as.character(edgelists[[i]]$TotalInd2)) - as.numeric(as.character(edgelists[[i]]$TimesTogether)))
#     #-- subset to remove all edges where edgeweight == 0 --#
#     #   edgelists.nozeros[[i]] <- subset(edgelists[[i]], edgeweights != 0)
#     edgelists.nozeros[[i]] <- subset(edgelists[[i]], edgeweights >=
#                                        edgeweight.min)
#     #-- included edges with weight = 0.  No good. 
#     el <- cbind(as.character(edgelists.nozeros[[i]]$Ind1), as.character(edgelists.nozeros[[i]]$Ind2))
#     #-- Were any associations observed? Generate list of isolated nodes --#
#     #-- to add to network later --#  
#     connected.nodes <- levels(factor(c(el[, 1], el[, 2])))
#     isolated.nodes <- inds[[i]][! (inds[[i]] %in% connected.nodes) == T] 
#     
#     if(dim(el)[1] == 0){  #-- if NO associations observed:   
#       res.component <- no.components <- max.component.size <- res.component.size <- node.degree <- cv.edgeweights <- mean.edgeweights <- sd.edgeweights <- local.efficiency <- rep(NA, length(out[[1]]))
#       for(m in 1:length(isolated.nodes)){
#         res.component[m] <- m
#         no.components[m] <- length(isolated.nodes)
#         max.component.size[m] <- 1
#         node.degree[m] <- cv.edgeweights[m] <- mean.edgeweights[m] <- sd.edgeweights[m] <- local.efficiency[m] <- 0
#       }
#       individ.ids <- as.character(isolated.nodes)
#       #-- leave everything as NAs for now....
#     } else {
#       static.graph.orig <- graph.edgelist(el, directed=F)
#       static.graph.weighted <- set.edge.attribute(static.graph.orig, "weight", value = edgelists.nozeros[[i]]$edgeweights)
#       E(static.graph.weighted)$weight<-edgelists.nozeros[[i]]$edgeweights
#       if (length(isolated.nodes) >= 1){
#         static.graph <- add.vertices(static.graph.weighted, nv = length(isolated.nodes), name = as.character(isolated.nodes))
#       } else {
#         static.graph <- static.graph.weighted
#       }
#       graph.clusters <- clusters(static.graph)
#       obs.clique.communities <- clique.community(static.graph, k = 3)
#       components <- length(levels(factor(graph.clusters$membership)))
#       numK3communities <- length(obs.clique.communities)
#       k3communities <- res.component <- no.components <- max.component.size <- res.component.size <- node.degree <- cv.edgeweights <- mean.edgeweights <- sd.edgeweights <- rep(NA, length(out[[1]]))
#       no.k3com <- max.k3com.size <- no.communities.for.this.node <- rep(NA, length(out[[1]]))
#       res.k3com <- vector("list", length(out[[1]]))
#       for(m in 1:length(V(static.graph)$name)){
#         if(dim(edgelists.nozeros[[i]])[1] == 0){ #-- if the graph has no edges
#           node.degree[m] <- cv.edgeweights[m] <- mean.edgeweights[m] <- sd.edgeweights[m] <- local.efficiency[m] <- 0
#           no.components[m] <- length(levels(factor(V(static.graph)$name)))
#           max.component.size[m] <- 1
#           res.component.size[m] <- 1
#           res.component[m] <- m
#           mean.edgeweights[m] <- 0
#           sd.edgeweights[m] <- NA
#           cv.edgeweights[m] <- NA
#           res.k3com[[m]] <- NA
#           no.k3com[m] <- NA
#           no.communities.for.this.node[m] <- NA
#           max.k3com.size[m] <- 1
#           k3communities[m] <- numK3communities
#         } else {
#           #-- pull all edges linked to node m --#
#           ind.edges <- subset(edgelists.nozeros[[i]], Ind1 ==
#                                 as.character(V(static.graph)$name)[m] | Ind2 ==
#                                 as.character(V(static.graph)$name)[m])
#           if(dim(ind.edges)[1] == 0){
#             node.degree[m] <- cv.edgeweights[m] <- mean.edgeweights[m] <- sd.edgeweights[m] <- 0
#             no.components[m] <- length(levels(factor(graph.clusters$membership)))
#             max.component.size[m] <- max(graph.clusters$csize)
#             res.component.size[m] <- graph.clusters$csize[graph.clusters$membership[m]]
#             res.component[m] <- graph.clusters$membership[m]
#             mean.edgeweights[m] <- 0
#             sd.edgeweights[m] <- NA
#             cv.edgeweights[m] <- NA
#             res.k3com[[m]] <- NA
#             no.communities.for.this.node[m] <- NA
#             max.k3com.size[m] <- max(unlist(lapply(obs.clique.communities, length)))
#             k3communities[m] <- numK3communities
#           } 
#           else {
#             cv.edgeweights[m] <- (sd(ind.edges$edgeweights)) / (mean(ind.edges$edgeweights))
#             mean.edgeweights[m] <- mean(ind.edges$edgeweights)
#             sd.edgeweights[m] <- sd(ind.edges$edgeweights)[1]
#             node.degree[m] <- degree(static.graph.weighted)[as.character(V(static.graph)$name)[m]]
#             no.components[m] <- length(levels(factor(graph.clusters$membership)))
#             max.component.size[m] <- max(graph.clusters$csize)
#             res.component.size[m] <- graph.clusters$csize[graph.clusters$membership[m]]
#             res.component[m] <- graph.clusters$membership[m]
#             res.k3com[[m]] <-  which(unlist(lapply(obs.clique.communities, clique.test.fun)) == T)
#             no.communities.for.this.node[m] <- length(res.k3com[[m]])
#             max.k3com.size[m] <- max(unlist(lapply(obs.clique.communities, length)))
#             k3communities[m] <- numK3communities
#           }
#         }
#       }
#       individ.ids <- as.character(V(static.graph)$name)
#     }
#     pop <- rep(paste(pop.id),length(V(static.graph)$name))
#     year <- rep(paste(year.id),length(V(static.graph)$name))
#     output.info[[i]] <- as.data.frame(cbind(individ.ids,node.degree,
#                                             cv.edgeweights,mean.edgeweights,
#                                             sd.edgeweights, no.components,
#                                             max.component.size,
#                                             res.component.size, res.component,
#                                             no.communities.for.this.node,
#                                             max.k3com.size, k3communities, rep(as.character(pop.id)[i], length(individ.ids)), rep(year.id[i], length(individ.ids))))
#   }
# }

#-- source in functions and packages to build association matrix --#
# source("~/work/Kezia/Research/EcologyPapers/ClustersAssocations_V2/ClustersAssociations/Code/Plots/NetworkPlottingSourceFunctions_30Nov2013.R")

#-- build in lamb status by hand --#
ewes.without.lambs.2014 <- c("14AS01", "13AS05", "14AS44", "14AS25", "14AS22")
yearlings.2014 <- c("14AS51", "14AS15", "14AS18", "14AS20", "14AS21", "14AS45")

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
graph.2014 <- BuildGraph(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = 3)
adj.2014 <- get.adjacency(graph.2014[[1]])
adj.2014.labels <- ifelse(V(graph.2014[[1]])$name %in% ewes.without.lambs.2014 == T, "EweWithoutLamb", 
                          ifelse(V(graph.2014[[1]])$name %in% yearlings.2014 == T, "Yearling", "EweWithLamb"))

assort.obj <- assortment.discrete(adj.2014, types = adj.2014.labels, weighted = T, SE = TRUE, M = 100)
assort.obj$r
assort.obj$SE
round(assort.obj$mixing_matrix, 3)
