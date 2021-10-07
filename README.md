
The **pecora** (permutation conditional random) package provides functions to perform the one-sample and two-samples t-tests using permutations/sign-flipping. 

To install it:

```{r}
devtools::install_github("angeella/pecora")

library(pecora)
```

A toy example:

```{r}
X <- matrix(rnorm(10*100000,5,sd=5),100000,10)

system.time(out1 <- oneSample(X,1000))
```


