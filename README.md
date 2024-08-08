# kMeansEqual
`kMeansEqual` Assigns `k` cluster identifiers to a data frame `kdat` whereby each cluster is of (nearly) equal size.

## Description

This function is based on [a blog post](https://rviews.rstudio.com/2019/06/13/equal-size-kmeans/) post by Harrison Schramm and Carol DeZwarte.
I have optimized their code for performance and put it into a function `kMeansEqual`.

## Installation

This R package has a single function `kMeansEqual` and can be installed via:

```
library(devtools)
install_github("ludgergoeminne/kMeansEqual")
```
