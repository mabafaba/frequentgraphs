% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0 - main.R
\name{g_test_scalar_plot}
\alias{g_test_scalar_plot}
\title{histogram of graph hypothesis test}
\usage{
g_test_scalar_plot(
  g_test_scalar_result,
  alpha = 0.05,
  breaks = 30,
  col = "black",
  xlim = minmax(c(g_test_scalar_result$simulated, g_test_scalar_result$observed)),
  xlab = "statistic",
  ylab = "frequency",
  main = "Observed (compared to distribution of randomised graphs)",
  sub = paste0("p <", ceiling(g_test_scalar_result$p.value * 1000)/1000,
    "; assumption of normality", ifelse(g_test_scalar_result$normality.condition.met,
    " ", " not "), "met"),
  ...
)
}
\arguments{
\item{g_test_scalar_result}{result from g_test_scalar()}

\item{alpha}{what alpha to use to plot the 'plausible range'}

\item{...}{arguments passed to hist()}
}
\value{
the provided g_test_scalar_result as is. (function is only for plot side effect)
}
\description{
histogram of graph hypothesis test
}
