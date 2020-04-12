library(Rglpk)

obj <- c(2, 4, 3)
mat <- matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3)
dir <- c("<=", "<=", "<=")
rhs <- c(60, 40, 80)
max <- TRUE

Rglpk_solve_LP(obj, mat, dir, rhs, max = max)

##       #stock weights needs to be choice variable, x1, x2, x3
# then put into contraints, instead of 1,1,1 needs to be exepected return



Sys.Date()
"2020-04-10"