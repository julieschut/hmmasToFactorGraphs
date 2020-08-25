# hmmasToFactorGraphs

[Asymmetric hidden Markov models](https://doi.org/10.1016/j.ijar.2017.05.011) (HMM-As) are probabilistic graphical models that capture dynamic processes, including local (in)dependencies. Based on Hidden Markov Models (HMMs), HMM-As offer additional modelling capabilities by capturing local asymmetries in each state. 
Factor graphs are bipartite graphs that capture the factorization of functions and enable efficient computing. This project enables transforming HMM-A to factor graph to be able to be used to calculate probabilities with [libDai](https://staff.fnwi.uva.nl/j.m.mooij/libDAI/), a C++ library, created for approximate inference in graphical models. This code will become part of the [CRAN package 'hmma'](https://cran.r-project.org/web/packages/hmma/index.html).

