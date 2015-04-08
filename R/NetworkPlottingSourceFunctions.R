
#-- graph building function --#
BuildGraph <- function(data.subsets = data.subsets, edgeweight.min = edgeweight.min, index = index){
  subset.to.use <- data.subsets[[index]]
  if(dim(subset.to.use)[1] == 0 | length(levels(factor(subset.to.use$EWEID))) == 1){
    edgelists <- NA
    inds <- NA
    output.info <- NA
    static.graph <- NA
    edgelists.nozeros <- NA
  } else{
    out <- AssocTimeVect(AssocData=subset.to.use)
    inds <- out[[1]]
    edgelists <- out[[2]]
    edgelists$edgeweights <- as.numeric(as.character(edgelists$TimesTogether)) / (as.numeric(as.character(edgelists$TotalInd1)) + as.numeric(as.character(edgelists$TotalInd2)) - as.numeric(as.character(edgelists$TimesTogether)))
    #-- subset to remove all edges where edgeweight == 0 --#
    edgelists.nozeros <- subset(edgelists, edgeweights >= edgeweight.min)
    el <- cbind(as.character(edgelists.nozeros$Ind1), as.character(edgelists.nozeros$Ind2))
    #-- Were any associations observed? Generate list of isolated nodes --#
    #-- to add to network later --#	
    connected.nodes <- levels(factor(c(el[, 1], el[, 2])))
    isolated.nodes <- inds[! (inds %in% connected.nodes) == T] 
    
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
      static.graph.weighted <- set.edge.attribute(static.graph.orig, "weight", value = edgelists.nozeros$edgeweights)
      E(static.graph.weighted)$weight<-edgelists.nozeros$edgeweights
      if (length(isolated.nodes) >= 1){
        static.graph <- add.vertices(static.graph.weighted, nv = length(isolated.nodes), name = as.character(isolated.nodes))
      } else {
        static.graph <- static.graph.weighted
      }
    }
  }
  return(list(static.graph = static.graph, edgelists.nozeros = edgelists.nozeros))
}
      

#-- 30 Nov 2013: new functionalized graph plotting code --#
#-- builds network plot for a given population-year.  Color-codes nodes based --#
#-- on lamb survival status; weights edges based on association index --#
#'static.graph = graph object (likely produced by BuildGraph above)
#'data.subsets = list of pop-year data subsets that contains information on each ewe's lamb's survival
# status, population, year, pop-year disease status ("CLASS_SUSP"), and summer lamb survival ("SLS")
#'index = index corresponding to particular pop-year subset to use

plotDiseaseGraph <- function(static.graph = static.graph, data.subsets = data.subsets, index = index){
  subset.to.use <- data.subsets[[index]]

  #-- set node attributes (color and size) --#
  node.color <- node.size <- rep(NA, length(V(static.graph)$name))
  for(i in 1:length(V(static.graph)$name)){
    k <- subset(subset.to.use, as.character(ID) == as.character(V(static.graph)$name[i]))
    node.color[i] <- ifelse(is.na(k$CENSOR2[1]) == T, "white", ifelse(k$CENSOR2[1] == 1, "grey70", "black"))
    node.size[i] <- dim(k)[1]/2
  }

  #-- edgeweights set in static graph code generation. --#
  #-- layout --#
  minC <- rep(-Inf, vcount(static.graph))
  maxC <- rep(Inf, vcount(static.graph))
  minC[1] <- maxC[1] <- 0
  set.seed(123)  # fixes layout positions so that plots are repeatable
  co <- layout.fruchterman.reingold(static.graph, minx=minC, maxx=maxC,
                                    miny=minC, maxy=maxC)

  plot(static.graph, layout = co, vertex.color = node.color, vertex.label.color = "black", vertex.label.dist = .8, vertex.label.cex = .8, vertex.size = node.size, edge.width = E(static.graph)$weight*5)
  title(paste(toupper(subset.to.use$Pop[1]), " ", subset.to.use$Year[1], ", ", subset.to.use$CLASS_SUSP[1], ", SLS = ", subset.to.use$SLS[1], sep = ""))
}

#-- Dec 30 2013: new function to plot graphs without disease --3
plotDiseaseFreeGraph <- function(static.graph = static.graph, data.subsets = data.subsets, index = index, layout.in = layout.in, xlim = xlim, ylim = ylim, vertex.label.cex = vertex.label.cex, main = main){
  subset.to.use <- data.subsets[[index]]
  
  #-- set node attributes (color and size) --#
  node.size <- node.color <- rep(NA, length(V(static.graph)$name))
  for(i in 1:length(V(static.graph)$name)){
    k <- subset(subset.to.use, as.character(EWEID) == as.character(V(static.graph)$name[i]))
    node.color[i] <- ifelse(as.character(k$LambStatus[1]) == "died", "white", "grey70")
#    node.size[i] <- dim(k)[1] / 5
    node.size[i] <- 10
  }
  
  #-- edgeweights set in static graph code generation. --#
  #-- layout --#
  minC <- rep(-Inf, vcount(static.graph))
  maxC <- rep(Inf, vcount(static.graph))
  minC[1] <- maxC[1] <- 0
  set.seed(123)  # fixes layout positions so that plots are repeatable
 # co <- layout.fruchterman.reingold(static.graph, minx=minC, maxx=maxC,
 #                                   miny=minC, maxy=maxC)
  co <- layout.in
  plot(static.graph, layout = co, vertex.color = node.color, vertex.label.color = "black", vertex.label.dist = .8, vertex.size = node.size, edge.width = E(static.graph)$weight*10, xlim = xlim, ylim = ylim, rescale = F, vertex.label.cex = vertex.label.cex, main = main)
#  title(paste(toupper(subset.to.use$Pop[1]), " ", subset.to.use$Year[1], ", ", subset.to.use$CLASS_SUSP[1], ", SLS = ", subset.to.use$SLS[1], sep = ""))
  title("")
}


#-- 30 Nov 2013: new functionalized heatmap plotting code --#
#-- builds heatmap from cluster simulation output for parameter space likely --#
#-- to create given pop-year lamb mortality patters. --#
#'static.graph = graph object (likely produced by BuildGraph above)
#'indices = index corresponding to particular pop-year subset to use
#'param.space = n x 2 matrix of mort.rate and chron.rate parameters over which simulation was run 
# gets loaded from same .RData file as simulation output 
#'lamb.dat = lamb survival data across all population-years; gets used here to calculate empirical --#
# lamb survival rate which is used as criterion that simulations must recapitulate. --#

OutputHeatMap <- function(static.graph, indices, param.space, lamb.dat){  
  #-- okay to index like this, since it's the same for all sims --# 
  names(param.space) <- c("mort.rate", "chron.rate") 
  chron.range <- mort.range <- vector("list", length(indices))
  #-- loop over elements of output.obj (8 pop-yrs per batch)--# 
           param.space.new <- param.space 
           #-- build param.space mat just for this popyear --#
           pop <- pop.id[indices]
           year <- year.id[indices]
           #-- Figure out pop repro rate for pop-year of interest --#
           popyr.dat <- subset(lamb.dat, HERD == pop & YEAR == year) 
           lambs.survd <- subset(popyr.dat, CENSOR2 == 0)
           repro.rate <- dim(lambs.survd)[1] / length(V(static.graph)$name) 
                               
           #-- unpack output object into parameter space and simulation output
           se <- sqrt((repro.rate) * (1 - repro.rate) / length(V(static.graph)$name))
           lb <- floor((repro.rate - se) * length(V(static.graph)$name))
           ub <- ceiling((repro.rate + se) * length(V(static.graph)$name))
                               
           #-- build a vector to store records of consistent simulations --#
           param.space.new$sims.consistent <- rep(NA, dim(param.space.new)[1])
           for(i in 1:dim(param.space.new)[1]){
              k <- table(as.numeric(as.character(output.obj[[1]][[2]][[i]][, 1])) %in% lb:ub)["TRUE"]
              param.space.new$sims.consistent[i] <- ifelse(is.na(k), 0, k / dim(output.obj[[1]][[2]][[i]])[1])
            }
                                                              
            #-- generate heatmap with ggplot --#
            p <- ggplot(param.space.new, aes(x = chron.rate, y = mort.rate))
            plot.path <- "~/work/Kezia/Research/EcologyPapers/ClustersAssociations/Plots/RevisedPlots_19Sept2013/ParamSpaceHeatMaps_25Nov2013/"
            full.plot <- (p + geom_tile(aes(fill = sims.consistent)) + scale_fill_gradient(low = "white", high = "black") + theme_bw())
            full.plot                 
            #-- write heatmap as svg and jpg --#
#            ggsave(paste(plot.path, pop, "_", year, ".jpg", sep = ""), full.plot)
#            ggsave(paste(plot.path, pop, "_", year, ".svg", sep = ""), full.plot)
  return(list(param.space.new, full.plot))
}

#-- 30 Nov 2013: new functionalized heatmap plotting code --#
#-- builds heatmap from cluster simulation output for parameter space likely --#
#-- to create given pop-year lamb mortality patters. --#
#'static.graph = graph object (likely produced by BuildGraph above)
#'indices = index corresponding to particular pop-year subset to use
#'param.space = n x 2 matrix of mort.rate and chron.rate parameters over which simulation was run 
# gets loaded from same .RData file as simulation output 
#'lamb.dat = lamb survival data across all population-years; gets used here to calculate empirical --#
# lamb survival rate which is used as criterion that simulations must recapitulate. --#

EpiSizeHeatMap <- function(static.graph, indices, param.space){  
  #-- okay to index like this, since it's the same for all sims --# 
  names(param.space) <- c("mort.rate", "chron.rate") 
  chron.range <- mort.range <- vector("list", length(indices))
  #-- loop over elements of output.obj (8 pop-yrs per batch)--# 
  param.space.new <- param.space 
  #-- build param.space mat just for this popyear --#
  pop <- pop.id[indices]
  year <- year.id[indices]
  #-- build a vector to store records of epi size --#
  param.space.new$sims.consistent <- rep(NA, dim(param.space.new)[1])
  for(i in 1:dim(param.space.new)[1]){
    output.obj.mod <- output.obj[[1]][[2]][[i]]
    output.obj.mod[ , 1] <- ifelse(param.space.new[i, 1] <= 0.3, ifelse(as.numeric(as.character(output.obj.mod[ , 1])) == 1, as.numeric(as.character(output.obj.mod[1, 2])), as.numeric(as.character(output.obj.mod[ ,1]))), as.numeric(as.character(output.obj.mod[ ,1])))
#    k <- median((as.numeric(as.character(output.obj[[1]][[2]][[i]][1, 2])) - as.numeric(as.character(output.obj[[1]][[2]][[i]][, 1]))) / as.numeric(as.character(output.obj[[1]][[2]][[i]][1, 2])))
    k <- median((as.numeric(as.character(output.obj.mod[1, 2])) - as.numeric(as.character(output.obj.mod[, 1]))) / as.numeric(as.character(output.obj.mod[1, 2])))
    param.space.new$med.epi.size[i] <- ifelse(is.na(k), 0, k )
  }
  
  #-- generate heatmap with ggplot --#
  p <- ggplot(param.space.new, aes(x = chron.rate, y = mort.rate))
  plot.path <- "~/work/Kezia/Research/EcologyPapers/ClustersAssociations/Plots/RevisedPlots_19Sept2013/ParamSpaceHeatMaps_25Nov2013/"
  full.plot <- (p + geom_tile(aes(fill = med.epi.size)) + scale_fill_gradient(low = "white", high = "black") + theme_bw())
  full.plot                 
  #-- write heatmap as svg and jpg --#
  #            ggsave(paste(plot.path, pop, "_", year, ".jpg", sep = ""), full.plot)
  #            ggsave(paste(plot.path, pop, "_", year, ".svg", sep = ""), full.plot)
  return(list(param.space.new, full.plot))
}


DiffInEpiSizeHeatMap <- function(static.graph, indices, param.space){  
  #-- okay to index like this, since it's the same for all sims --# 
  names(param.space) <- c("mort.rate", "chron.rate") 
  chron.range <- mort.range <- vector("list", length(indices))
  #-- loop over elements of output.obj (8 pop-yrs per batch)--# 
  param.space.new <- param.space 
  #-- build param.space mat just for this popyear --#
  pop <- pop.id[indices]
  year <- year.id[indices]
  #-- Figure out pop repro rate for pop-year of interest --#
  popyr.dat <- subset(lamb.dat, HERD == pop & YEAR == year) 
  lambs.survd <- dim(subset(popyr.dat, CENSOR2 == 0))[1]
#  repro.rate <- dim(lambs.survd)[1] / length(V(static.graph)$name) 
  
  #-- unpack output object into parameter space and simulation output
  #  se <- sqrt((repro.rate) * (1 - repro.rate) / length(V(static.graph)$name))
  #  lb <- floor((repro.rate - se) * length(V(static.graph)$name))
  #  ub <- ceiling((repro.rate + se) * length(V(static.graph)$name))
  
  #-- build a vector to store records of consistent simulations --#
  param.space.new$epi.size.diff <- param.space.new$med.epi.size <- rep(NA, dim(param.space.new)[1])
  for(i in 1:dim(param.space.new)[1]){
    #    k <- table(as.numeric(as.character(output.obj[[1]][[2]][[i]])) %in% lb:ub)["TRUE"]
    output.obj.mod <- output.obj[[1]][[2]][[i]]
    output.obj.mod[ , 1] <- ifelse(param.space.new[i, 1] <= 0.3, ifelse(as.numeric(as.character(output.obj.mod[ , 1])) == 1, as.numeric(as.character(output.obj.mod[1, 2])), as.numeric(as.character(output.obj.mod[ ,1]))), as.numeric(as.character(output.obj.mod[ ,1])))
    #    k <- median((as.numeric(as.character(output.obj[[1]][[2]][[i]][1, 2])) - as.numeric(as.character(output.obj[[1]][[2]][[i]][, 1]))) / as.numeric(as.character(output.obj[[1]][[2]][[i]][1, 2])))
    k <- median((as.numeric(as.character(output.obj.mod[, 1]))))
    param.space.new$med.epi.size[i] <- ifelse(is.na(k), 0, k )
    param.space.new$epi.size.diff[i] <- abs(ifelse(is.na(k), (lambs.survd / as.numeric(as.character(output.obj.mod[1, 2]))), (lambs.survd - k)/as.numeric(as.character(output.obj.mod[1, 2]))))
  }
  
  #-- generate heatmap with ggplot --#
  p <- ggplot(param.space.new, aes(x = chron.rate, y = mort.rate))
#  plot.path <- "~/work/Kezia/Research/EcologyPapers/ClustersAssociations/Plots/RevisedPlots_19Sept2013/ParamSpaceHeatMaps_25Nov2013/"
#  full.plot <- (p + geom_tile(aes(fill = epi.size.diff)) + scale_fill_gradient(low = "grey40", high = "red2") + theme_bw())
  full.plot <- p + geom_tile(aes(fill = epi.size.diff)) + scale_fill_gradient(limits = c(0, 1), low = "grey90", high = "red") + theme_bw() + geom_density2d()
  #  svg(paste(write.path, pop.name, "_", year, ".svg", sep = ""), height = 3*2, width = 6)
#  full.plot
#  dev.off()
  
  #-- write heatmap as svg and jpg --#
  #            ggsave(paste(plot.path, pop, "_", year, ".jpg", sep = ""), full.plot)
  #            ggsave(paste(plot.path, pop, "_", year, ".svg", sep = ""), full.plot)
  return(list(param.space.new, full.plot))
}
