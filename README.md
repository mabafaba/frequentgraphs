## frequentgraphs

A network/graph hypothesis testing framework

Evaluates a function of a network that returns a scalar on a provided network, as well as randomised versions of it test the null hypothesis that the provided network statistic was produced by a random process.


### Example

Say we're interested in some custom scalar summary statistic of a graph - for example the mean closeness of largest connected component of the graph:

```
mean_closeness<-function(g,...){
  giant_comp <- g_largest_component(g)
  mean(closeness(giant_comp,...))
  }
```

This statistic alone doesn't tell you much (is the number relatively high or relatively low?)
`frequentgraphs` allows you to quickly compare it to the distribution of that statistic in a random graph with the same number of nodes and edges as the provided original graph (and gives you a p-value for the null hypothesis that the measured summary statistic comes from a graph that is produced by the same random process): 

```
g_test <- g_test_scalar(g =   my_graph,
                              simulation.n = 300,
                              scalar.graph.statistic = mean_closeness)

g_test$observed
g_test$simulated.mean
g_test$p.value
g_test$normality.condition.met
```


