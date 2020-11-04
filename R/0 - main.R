

#' Hypothesis test for graph statistics
#' apply a graph statistic to a graph, as well as randomised versions of it, and test the null-hypothesis that the statistic of the input graph was produced by a uniform random edge distribution.
#' Assumes normal distribution of the test statistic (returns normality test results along with p-values and results)
#' @param g igraph object
#' @param simulation.n how many simulations to run
#' @param scalar.graph.statistic a function that takes an igraph graph object as input and returns a scalar
#' @param alpha the confidence level
#' @param na.rm remove NAs and NaNs when they are produced in the graph statistic for ramdomised graphs
#' @value the randomised mean and original statistic, as well as the p-value for the null-hypothesis that the statistic of the input graph was produced by a uniform random edge distribution
#' @export
g_test_scalar<-function(g,simulation.n=100,scalar.graph.statistic,alpha=0.05,na.rm = FALSE){
  V(g)$name<-1:(length(V(g))) # need names to work but dont care what they are
  g_stat_simulated<-g_randomise_edges_stat(as.undirected(g),
                                           simulation.n = simulation.n,
                                           graph.statistic = scalar.graph.statistic) %>%
    do.call(cbind,.)
  if(na.rm){
    g_stat_simulated<-g_stat_simulated[!is.na(g_stat_simulated) & !is.nan(g_stat_simulated)]
  }
  if(any(is.na(g_stat_simulated))){"test statistic returned NA for simulated graphs"}
  if(any(is.nan(g_stat_simulated))){"test statistic returned NaN for simulated graphs"}
  if(any(!is.finite(g_stat_simulated))){"test statistic not finite for simulated graphs"}
  test_sd<-sd(g_stat_simulated)
  test_mean<-mean(g_stat_simulated)


  g_stat_original<-scalar.graph.statistic(g)


  # pnorm returns the integral from −∞ to q of the pdf of the normal distribution (where q is a Z-score)
  left_sided<-ifelse(g_stat_original<test_mean,TRUE,FALSE)
  p_value<-pnorm(g_stat_original, test_mean, test_sd,lower.tail = left_sided)*2

  simulated_stat_normal<-shapiro.test(g_stat_simulated)$p.value>0.05

  return(list(observed=g_stat_original,
              simulated=g_stat_simulated,
              simulated.mean=test_mean,
              simulated.sd=test_sd,
              normality.condition.met=simulated_stat_normal,
              p.value=p_value
  ))
}

#' apply graph statistic to randomised copies of a graph
#'
#' @param g igraph object
#' @param simulation.n how many simulations to run
#' @param any.graph.statistic a function that takes a graph as input and returns any object
#' @return a list with two items: 'simulation': a list, length equal 'simulation.n', with the resulting statistic for each randomised graph. 'original': the graph statistic for the original input graph
g_randomised_statistic<-function(g,simulation.n,any.graph.statistic){
  g_stat_simulated<-g_randomise_edges_stat(as.undirected(g),simulation.n = 100,graph.statistic = any.graph.statistic) %>% do.call(cbind,.)
  g_stat_original<-any.graph.statistic(g)
  list(simulation=g_stat_simulated,original=g_stat_original)
}




#' histogram of graph hypothesis test
#'
#' @param g_test_scalar_result result from g_test_scalar()
#' @param alpha what alpha to use to plot the 'plausible range'
#' @param ... arguments passed to hist()
#' @return the provided g_test_scalar_result as is. (function is only for plot side effect)
#' @export
g_test_scalar_plot<-function(g_test_scalar_result,alpha=0.05,
                                 breaks=30,
                                 col='black',
                                 xlim=minmax(c(g_test_scalar_result$simulated,
                                               g_test_scalar_result$observed)),
                                 xlab='statistic',
                                 ylab='frequency',
                                 main='Observed (compared to distribution of randomised graphs)',
                                 sub=paste0('p <',
                                            ceiling(g_test_scalar_result$p.value*1000)/1000,'; assumption of normality',
                                            ifelse(g_test_scalar_result$normality.condition.met," ",' not '),"met"),...){
  minmax<-function(x){c(min(x),max(x))}
  hist(g_test_scalar_result$simulated,
       breaks=breaks,
       col=col,
       xlab=xlab,
       ylab=ylab,
       main=main,
       sub=sub,
       xlim=xlim,...)


  empirical_plausible_range<-qnorm(c(alpha/2,1-(alpha/2)),
                                   mean = g_test_scalar_result$simulated.mean,
                                   sd = g_test_scalar_result$simulated.sd)
  abline(v=empirical_plausible_range,col='red',lty=2)

  abline(v=g_test_scalar_result$observed,col='red')
  text(g_test_scalar_result$observed,y=0,label='observed')
  NULL
}



#' calculate graph statistics for randomised versions of a graph
#' unweighted, directed graphs only; graph is treated as simplified (no self-loops; no double edges)
g_randomise_edges_stat<-function(g,simulation.n=100,graph.statistic){
    g<-simplify(g)
  n_edges<-E(g) %>% length
  nodelist<-igraph::as_data_frame(g,'vertices')
  possible_edge_combs<-expand.grid(V(g)$name,V(g)$name) %>%  apply(1,paste,collapse='------lotsofbits--------')

  g_randomised_stat<-function(g){

    randomised<-sample(possible_edge_combs,n_edges,replace = F) %>%  # get random combos
      strsplit('------lotsofbits--------') %>% do.call(rbind,.) %>% # make edgelist
      graph_from_data_frame(directed=T,vertices = nodelist) %>% # make graph
      graph.statistic
  }

  stat<-lapply(1:simulation.n,function(i){
    g_randomised_stat(g)
  })

}

#' Get largest connected component from graph
#' @param g igraph garph
#' @param mode passed to igraph::components (whether edge direction is considered,defaults to 'strong' (alernative: 'weak'))
#' @export
g_largest_component<-function(g,mode='strong'){
  g.components<-components(g,mode=mode)
  giant.component<-subgraph(g,names(g.components$membership)[g.components$membership==which.max(g.components$csize)])
}
