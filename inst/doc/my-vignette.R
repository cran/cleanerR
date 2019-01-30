## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example1------------------------------------------------------------
require(cleanerR)
z=generate_candidates(df=iris,goal=5,maxi=3,repetitions=100,trigger=0)
print(z[[1]])
cat("error rate\n")
print(z[[2]])

