#-- read in 2014 Asotin Relocations --#
data <- read.csv("~/work/Kezia/Research/FieldStudies/HCBighornContactStudies/Data/RelocationData/AsotinLocationsFinal_20July2014.csv", header = T, sep = "\t")

data$UTMN <- data$Longitude
data$UTME <- data$Latitude
data$DATE <- data$Date

#-- pull out three ram records from Asotin, as well as uncollareds --#
ram.ids <- c("08AS31", "12AS19", "12AS23", "12AS22", "11AS06", "08AS34", "12AS21", "", "14AS16", "14AS47")
data <- subset(data, !(EWEID %in% ram.ids))

#sumimvewes <- eweids <- levels(factor(data$EWEID))

require(igraph)

#-- source in function to build association matrix --#
source("~/work/Kezia/Research/EcologyPapers/ClustersAssocations_V2/ClustersAssociations/Code/DataCleaning/RelocationsToNetworks/StaticNetworkAssocMat.R")

edgelists.nozeros <- edgelists <- inds <- output.info <- data.subsets <- list(NA, 1)
#-- build list of relevant data subsets.
data.subsets[[1]] <- data
data.subsets[[2]] <- subset(data, Year == 2013)
data.subsets[[3]] <- subset(data, Year == 2014)
data.subsets[[4]] <- subset(data, Year == 2014 & JulianDate <= 150)
data.subsets[[5]] <- subset(data, Year == 2014 & JulianDate >= 151 & JulianDate <= 180)
data.subsets[[6]] <- subset(data, Year == 2014 & JulianDate >= 181)

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
for(i in 1:6){
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

#-- source in functions and packages to build association matrix --#
# source("~/work/Kezia/Research/EcologyPapers/ClustersAssocations_V2/ClustersAssociations/Code/Plots/NetworkPlottingSourceFunctions_30Nov2013.R")

#-- build in lamb status by hand --#
ewes.without.lambs.2014 <- c("14AS01", "13AS05", "14AS44", "14AS25", "14AS22", "14AS51", "14AS15", "14AS18", "14AS20", "14AS21", "14AS45")

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

data.subsets[[4]]$LambStatus <- rep(NA, dim(data.subsets[[4]])[1])
for(i in 1:dim(data.subsets[[4]])[1]){
  data.subsets[[4]]$LambStatus[i] <- ifelse(data.subsets[[4]]$EWEID[i] %in% ewes.without.lambs.2014, "died", "censored")  
}

data.subsets[[5]]$LambStatus <- rep(NA, dim(data.subsets[[5]])[1])
for(i in 1:dim(data.subsets[[5]])[1]){
  data.subsets[[5]]$LambStatus[i] <- ifelse(data.subsets[[5]]$EWEID[i] %in% ewes.without.lambs.2014, "died", "censored")  
}

data.subsets[[6]]$LambStatus <- rep(NA, dim(data.subsets[[6]])[1])
for(i in 1:dim(data.subsets[[6]])[1]){
  data.subsets[[6]]$LambStatus[i] <- ifelse(data.subsets[[6]]$EWEID[i] %in% ewes.without.lambs.2014, "died", "censored")  
}


#-- build network --#
edgeweight.min <- .1
trial.graph <- BuildGraph(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = 3)
minC <- rep(-Inf, vcount(trial.graph[[1]]))
maxC <- rep(Inf, vcount(trial.graph[[1]]))
minC[1] <- maxC[1] <- 0
set.seed(123)  # fixes layout positions so that plots are repeatable
co <- layout.fruchterman.reingold(trial.graph[[1]], minx=minC, maxx=maxC,
                                  miny=minC, maxy=maxC)
names.positions <- as.data.frame(cbind(co, V(trial.graph[[1]])$name))

#-- NEED TO FIGURE OUT HOW TO MATCH EACH NODE'S NAME TO ITS CORRESPONDING VERTEX/POSITION.... --#

full2014.graph <- BuildGraph(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = 3)
may.graph <- BuildGraph(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = 4)
june.graph <- BuildGraph(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = 5)
july.graph <- BuildGraph(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = 6)


#-------------------------------------#
#-- Eigenvector centrality of nodes --#
#-------------------------------------#

eigencent <- evcent(full2014.graph[[1]])$vector
node.names <- names(evcent(full2014.graph[[1]])$vector)
hist(eigencent, col = "grey70")

#-- map node.names to frequencies --#
node.name.new <- ifelse(node.names == "11AS14", "149.849",
                 ifelse(node.names == "13AS04", "150.36",
                 ifelse(node.names == "14AS22", "151.871",
                 ifelse(node.names == "11AS09", "150.145",
                 ifelse(node.names == "12AS03", "151.389",
                 ifelse(node.names == "08AS37", "151.891",
                 ifelse(node.names == "11AS12", "150.319",
                 ifelse(node.names == "11AS11", "150.65",
                 ifelse(node.names == "14AS43", "150.19",
                 ifelse(node.names == "14AS44", "151.21",
                 ifelse(node.names == "13AS04", "151.851",
                 ifelse(node.names == "08AS26", "151.561",
                 ifelse(node.names == "14AS15", "orange15",
                 ifelse(node.names == "14AS41", "yellow41",
                 ifelse(node.names == "08AS29", "151.832",
                 ifelse(node.names == "14AS01", "150.02",
                 ifelse(node.names == "14AS08", "150.18",
                 ifelse(node.names == "14AS25", "151.22",
                 ifelse(node.names == "11AS10", "151.6",
                 ifelse(node.names == "14AS21", "orange21",
                 ifelse(node.names == "HornsAskew", "hornsaskew",
                 ifelse(node.names == "14AS51", "white51",
                 ifelse(node.names == "FloppyEar", "floppy",
                 ifelse(node.names == "White45", "white45",
                 ifelse(node.names == "DarkEwe", "darkewe",
                 ifelse(node.names == "14AS18", "orange18",
                 ifelse(node.names == "14AS20", "orange20", NA
                )))))))))))))))))))))))))))

#----------------------------------------#
#-- END eigenvector centrality explore --#
#----------------------------------------#

#may.names <- subset(names.positions, V3 %in% V(may.graph[[1]])$name)
may.pos <- matrix(NA, nrow = length(V(may.graph[[1]])$name), ncol = 2)
for(i in 1:length(V(may.graph[[1]])$name)){
  may.pos[i, 1] <- as.numeric(as.character(subset(names.positions, V3 == V(may.graph[[1]])$name[i])[, 1]))
  may.pos[i, 2] <- as.numeric(as.character(subset(names.positions, V3 == V(may.graph[[1]])$name[i])[, 2]))
}

june.pos <- matrix(NA, nrow = length(V(june.graph[[1]])$name), ncol = 2)
for(i in 1:length(V(june.graph[[1]])$name)){
  june.pos[i, 1] <- as.numeric(as.character(subset(names.positions, V3 == V(june.graph[[1]])$name[i])[, 1]))
  june.pos[i, 2] <- as.numeric(as.character(subset(names.positions, V3 == V(june.graph[[1]])$name[i])[, 2]))
}

july.pos <- matrix(NA, nrow = length(V(july.graph[[1]])$name), ncol = 2)
for(i in 1:length(V(july.graph[[1]])$name)){
  july.pos[i, 1] <- as.numeric(as.character(subset(names.positions, V3 == V(july.graph[[1]])$name[i])[, 1]))
  july.pos[i, 2] <- as.numeric(as.character(subset(names.positions, V3 == V(july.graph[[1]])$name[i])[, 2]))
}

par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
plotDiseaseFreeGraph(static.graph = trial.graph[[1]], data.subsets = data.subsets, index = 1, layout.in = co, xlim = c(-2, 7), ylim = c(-8, 5), vertex.label.cex = 1.1, main = "")

#-- trial graph names
V(trial.graph[[1]])$name

plotDiseaseFreeGraph(static.graph = may.graph[[1]], data.subsets = data.subsets, index = 4, layout.in = may.pos, xlim = c(-2, 7), ylim = c(-8, 5), vertex.label.cex = 1.1, main = "May 2014 Asotin")
plotDiseaseFreeGraph(static.graph = june.graph[[1]], data.subsets = data.subsets, index = 5, layout.in = june.pos, xlim = c(-2, 7), ylim = c(-8, 5), vertex.label.cex = 1.1, main = "June 2014 Asotin")
plotDiseaseFreeGraph(static.graph = july.graph[[1]], data.subsets = data.subsets, index = 6, layout.in = july.pos, xlim = c(-2, 7), ylim = c(-8, 5), vertex.label.cex = 1.1, main = "July 2014 Asotin")

#----------------------------------------------------------------#
#-- do edgeweights in 2013 correlate with edgeweights in 2014? --#
#----------------------------------------------------------------#

#-- 1) get list of ewes in both years --#
ewes.2013 <- levels(factor(data.subsets[[2]]$EWEID))
ewes.2014 <- levels(factor(data.subsets[[3]]$EWEID))
both.year.ewes <- ewes.2013[which(ewes.2013 %in% ewes.2014)]
ewe.combos <- combn(both.year.ewes, 2)

edgeweight.mat <- matrix(NA, nrow = dim(ewe.combos)[2], ncol = 5)
edgeweight.mat[ ,1] <- ewe.combos[1, ]
edgeweight.mat[ ,2] <- ewe.combos[2, ]

for(i in 1:dim(ewe.combos)[2]){
  k <- subset(edgelists[[2]], (Ind1 == edgeweight.mat[i, 1] | Ind2 == edgeweight.mat[i, 1]) & (Ind1 == edgeweight.mat[i, 2] | Ind2 == edgeweight.mat[i, 2]))
  edgeweight.mat[i, 3] <- as.numeric(as.character(k$AssociationIndex[1]))
  
  j <- subset(edgelists[[3]], (Ind1 == edgeweight.mat[i, 1] | Ind2 == edgeweight.mat[i, 1]) & (Ind1 == edgeweight.mat[i, 2] | Ind2 == edgeweight.mat[i, 2]))
  edgeweight.mat[i, 4] <- as.numeric(as.character(j$AssociationIndex[1]))
}

edgeweight.mat <- as.data.frame(edgeweight.mat)
names(edgeweight.mat) <- c("Ind1", "Ind2", "Assoc2013", "Assoc2014", "")
edgeweight.mat$Assoc2013 <- as.numeric(as.character(edgeweight.mat$Assoc2013))
edgeweight.mat$Assoc2014 <- as.numeric(as.character(edgeweight.mat$Assoc2014))

plot(edgeweight.mat$Assoc2014 ~ edgeweight.mat$Assoc2013, pch = 16, xlab = "2013 Association Index", ylab = "2014 Association Index")
cor(edgeweight.mat$Assoc2014, edgeweight.mat$Assoc2013)

scatterhist <- function(x, y, xlab = "", ylab = "", plottitle="", 
                        xsize=1, cleanup=TRUE,...){
  # save the old graphics settings-- they may be needed
  def.par <- par(no.readonly = TRUE)
  
  zones <- matrix(c(1,1,1, 0,5,0, 2,6,4, 0,3,0), ncol = 3, byrow = TRUE)
  layout(zones, widths=c(0.5,4,2), heights = c(1,5,10,.75))
  
  # tuning to plot histograms nicely
  xhist <- hist(x, plot = FALSE)
  yhist <- hist(y, plot = FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  
  # for all three titles: 
  #   drop the axis titles and omit boxes, set up margins
  par(xaxt="n", yaxt="n",bty="n",  mar = c(.3,2,.3,0) +.05)
  # fig 1 from the layout
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(plottitle), cex=2)
  # fig 2
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(ylab), cex=1.5, srt=90)
  # fig 3
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(xlab), cex=1.5)
  
  # fig 4, the first histogram, needs different margins
  # no margin on the left
  par(mar = c(2,0,1,1))
  barplot(yhist$counts, axes = FALSE, xlim = c(0, top),
          space = 0, horiz = TRUE)
  # fig 5, other histogram needs no margin on the bottom
  par(mar = c(0,2,1,1))
  barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
  # fig 6, finally, the scatterplot-- needs regular axes, different margins
  par(mar = c(2,2,.5,.5), xaxt="s", yaxt="s", bty="n")
  # this color allows traparency & overplotting-- useful if a lot of points
  plot(x, y , pch=19, col="#00000022", cex=xsize, ...)
  
  # reset the graphics, if desired 
  if(cleanup) {par(def.par)}
}

scatterhist(edgeweight.mat$Assoc2013, edgeweight.mat$Assoc2014, ylab="2014 Association Indices", xlab="2013 Association Indices",   
            "Asotin Association Indices 2013 and 2014", xsize =2)

#-------------------------------------------------#
#-- end comparison of 2013 and 2014 edgeweights --#
#-------------------------------------------------#
may.degree.14 <- degree(may.graph[[1]])
june.degree.14 <- degree(june.graph[[1]])
july.degree.14 <- degree(july.graph[[1]])

par(mfrow = c(3, 1))
hist(may.degree.14, col = "grey60", xlim = c(0, 24))
hist(june.degree.14, col = "grey60", xlim = c(0, 24))
hist(july.degree.14, col = "grey60", xlim = c(0, 24))

#-------------------------------------------------#
#-- degree distribution by month and age-class ---#
#-------------------------------------------------#



#-------------------------------------------------#
#-- END degree distribution ----------------------#
#-------------------------------------------------#

#-- what does edgeweight distribution look like? --#
hist(edgelists[[1]]$edgeweight, breaks = 20, col = "grey80", xlab = "Edgeweight", main = "")
hist(edgelists[[2]]$edgeweight, breaks = 20, col = "grey80", xlab = "Edgeweight", main = "")
hist(edgelists[[3]]$edgeweight, breaks = 20, col = "grey80", xlab = "Edgeweight", main = "")

#-- split into visual clusters --#
clust.1 <- c("08AS37", "14AS41", "14AS43", "11AS11", "11AS12", "14AS15", "14AS22", "12AS03", "13AS04", "11AS14")
clust1.data <- subset(data, EWEID %in% clust.1)

clust.2 <- c("14AS20", "14AS01", "14AS51", "11AS10", "14AS25", "13AS05", "08AS29", "14AS08", "14AS21", "08AS26", "14AS18", "HornsAskew", "DarkEwe", "FloppyEar", "White45")
clust2.data <- subset(data, EWEID %in% clust.2)

data$clust.ind <- ifelse(data$EWEID %in% clust.1, 1, ifelse(data$EWEID %in% clust.2, 2, 3))
data.no3 <- subset(data, clust.ind != 3)
par(mfrow = c(1, 1))
plot(data$Longitude ~ data$Latitude, pch = 16, col = data$clust.ind, xlim = c(46.2, 46.34), ylim = c(-117.4, -117.25), xlab = "Latitude", ylab = "Longitude")

plot(data.no3$Latitude ~ data.no3$Longitude, pch = 16, col = data.no3$clust.ind, ylim = c(46.2, 46.35), xlim = c(-117.4, -117.25), ylab = "Latitude", xlab = "Longitude")

may.no3 <- subset(data.no3, JulianDate >= 120 & JulianDate <= 150)
june.no3 <- subset(data.no3, JulianDate >=151 & JulianDate <= 181 )
july.no3 <- subset(data.no3, JulianDate >= 182)

par(mfrow = c(1, 3), oma = c(4, 0, 0, 0), mar = c(3, 3, 3, 1))
plot(may.no3$Latitude ~ may.no3$Longitude, main = "May", pch = 16, col = may.no3$clust.ind, ylim = c(46.2, 46.35), xlim = c(-117.4, -117.25), ylab = "Latitude", xlab = "Longitude")
plot(june.no3$Latitude ~ june.no3$Longitude, main = "June", pch = 16, col = june.no3$clust.ind, ylim = c(46.2, 46.35), xlim = c(-117.4, -117.25), ylab = "Latitude", xlab = "Longitude")
plot(july.no3$Latitude ~ july.no3$Longitude, main = "July", pch = 16, col = july.no3$clust.ind, ylim = c(46.2, 46.35), xlim = c(-117.4, -117.25), ylab = "Latitude", xlab = "Longitude")
mtext(side = 1, outer = T, line = 1, "Asotin relocations colored by cluster membership.")

#---------------------------------------#
#-- Dyad-specific association indices --#
#---------------------------------------#

AssociationData <- out[[2]]
AssociationData$TimesTogether <- as.numeric(as.character(AssociationData$TimesTogether))
AssociationData$TotalInd1 <- as.numeric(as.character(AssociationData$TotalInd1))
AssociationData$TotalInd2 <- as.numeric(as.character(AssociationData$TotalInd2))
AssociationData$AssociationIndex <- AssociationData$TimesTogether / (AssociationData$TotalInd1 + AssociationData$TotalInd2 - AssociationData$TimesTogether)
hist(AssociationData$AssociationIndex, col = "grey90", xlab = "Association Index for all Asotin dyads", ylab = "Frequency", main = "")

#-- autocorrelation in dyad-togetherness --#
#-- for each dyad, need longitudinal sequence of together/not --#
#-- then, look at variation in autocorrelation functions --#
#-- then, get mean-field autocorrelation function --#

#-- consider regressing dyad autocorrelation on association (or model fission separate from fusion?) --#
#-- consider imputing days when neither ewe was observed --#

#-- build two matrices that are dyads by days --#
together.mat <- matrix(NA, ncol = dim(AssociationData)[1], nrow = max(data$JulianDate) - min(data$JulianDate))
for(j in 1:dim(together.mat)[2]){
  today <- subset(data, JulianDate == (j + 120))
  for(i in 1:dim(AssociationData)[1]){
    ind1sub <- subset(today, as.character(EWEID) == as.character(AssociationData$Ind1[i]))
    ind2sub <- subset(today, as.character(EWEID) == as.character(AssociationData$Ind2[i]))
    if(dim(ind1sub)[1] == 0){
      if(dim(ind2sub)[1] == 0){
        together.mat[i, j] <- NA
      } else{
        together.mat[i, j] <- 0
      }
    } else{
      if(dim(ind2sub)[1] == 0){
        together.mat[i, j] <- NA
        } else{
        together.mat[i, j] <- ifelse(ind1sub$Latitude[1] == ind2sub$Latitude[1] & ind1sub$Longitude[1] == ind2sub$Longitude[1], 1, 0)
      }
    }
  }  
}

change.mat <- fiz.mat <- fuz.mat <- matrix(NA, ncol = dim(AssociationData)[1], nrow = max(data$JulianDate) - min(data$JulianDate))
for(i in 1:dim(AssociationData)[1]){
    for(j in 2:dim(together.mat)[2]){
      change.mat[i, j] <- ifelse(together.mat[i, j] != together.mat[i, j - 1], 1, 0)
  }
}

#-- ratio of 0's to 1's throughout whole change.mat should give me probability of fiz/fuz per day --#
table(change.mat)
prob.change <- 260 / (917 + 260)
#-- 1/.221  = 4.5249 --#


#-- heat map of association indices --#
#-- start with who has most associations total, then decrease sequentially? --#
individs <- levels(factor(data$EWEID))
tot.assoc <- rep(NA, length(individs))
for(i in 1:length(individs)){
  k <- subset(AssociationData, as.character(Ind1) == individs[i] | as.character(Ind2) == individs[i])
  tot.assoc[i] <- sum(k$AssociationIndex)
}

image.dat <- matrix(NA, nrow = length(individs), ncol = length(individs))
for(i in 1:length(individs)){
  ind1 <- subset(AssociationData, as.character(Ind1) == individs[i] | as.character(Ind2) == individs[i])
    for(j in 1:length(individs)){
      pairing <- subset(ind1, as.character(Ind2) == individs[j] | as.character(Ind1) == individs[j])
      if(j != i){
        image.dat[i, j] <- image.dat[j, i] <- pairing$AssociationIndex[1]
      }
    }
}

individs.reorder <- individs[c(6,  4,  2, 9,  1, 10, 8,  12, 7, 11, 3, 5)]
image.dat.reorder <- matrix(NA, nrow = length(individs.reorder), ncol = length(individs.reorder))
for(i in 1:length(individs.reorder)){
  ind1 <- subset(AssociationData, as.character(Ind1) == individs.reorder[i] | as.character(Ind2) == individs.reorder[i])
  for(j in 1:length(individs.reorder)){
    pairing <- subset(ind1, as.character(Ind2) == individs.reorder[j] | as.character(Ind1) == individs.reorder[j])
    if(j != i){
      image.dat.reorder[i, j] <- image.dat.reorder[j, i] <- pairing$AssociationIndex[1]
    }
  }
}

image(image.dat.reorder,xaxt = "n", yaxt = "n")
axis(side = 1, at = seq(0, 1, length.out = 12), labels = individs.reorder, cex = .8, las = 2)
axis(side = 2, at = seq(0, 1, length.out = 12), labels = individs.reorder, cex = .8, las = 2)

#-- consider subsetting by time... doing separate association matrices for each month --#