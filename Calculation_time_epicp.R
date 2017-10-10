# What takes so long in the calculation of covariate patterns?
# How can this problem be overcome

# Applying epiR::epi.cp() to a data set of 11,000 rows and five independent variables took nearly six minutes on my computer. I found out that this is due to two nested for()-cycles in that function.

# I created an alternative function popp.cp() that does the same thing much faster, using variants of apply() instead. 

# The only difference between the output of these functions is, that epi.cp uses the row numbers of the aggregated data frame (based on covariate patterns) as ID-variable, whereas popp.cp uses row numbers of the first case from the original data, that contributes to the covariate pattern.

# Johann Popp
# 2017-10-10
#######################


########
# Test data

dat <- data.frame(
  # dv = sample(c(TRUE, FALSE), 5000, replace = TRUE),
  iv1 = sample(LETTERS, 3000, replace = TRUE),
  iv2 = sample(letters, 3000, replace = TRUE),
  age = sample(18:100, 3000, replace = TRUE))


library(epiR)
######
# this is the function epi.cp
#########
# function (dat)
# {
#   obs <- as.data.frame(cbind(id = 1:nrow(dat), cp = rep(0,
#                                                         times = nrow(dat))))
#   nvar <- dim(dat)[2]
#   cp <- unique(dat)
#   cp <- cbind(id = 1:nrow(cp), cp)
#   
#   system.time({
#     for (i in 1:nrow(cp)) {
#       tmp <- rep(0, times = nrow(dat))
#       for (j in 1:nvar) {
#         tmp. <- as.numeric(dat[, j] == cp[i, (j + 1)])
#         tmp <- cbind(tmp, tmp.)
#       }
#       tmp <- apply(tmp, MARGIN = 1, FUN = sum)
#       id <- tmp == nvar
#       obs$cp[id] <- cp$id[i]
#     }
#   })
#   n <- hist(obs$cp, breaks = seq(from = 0, to = nrow(cp), by = 1),
#             plot = FALSE)
#   n <- n$counts
#   end <- nvar + 1
#   cov.pattern <- as.data.frame(cbind(id = cp[, 1], n, cp[,
#                                                          2:end]))
#   rval <- list(cov.pattern = cov.pattern, id = obs$cp)
#   rval
# }

##########
# creating a faster version of epiR::epi.cp



popp.cp <- function(dat){
  
dat <- data.frame(id = 1:nrow(dat), dat)

# add an indicator variable for covariate patterns
dat$indi <- apply(dat[,ncol(dat):2], 1, function(x) as.factor(paste(x, collapse = "")))

# order according to the indicator variable
dat <- dat[order(dat$indi),]

# creating a variable that indicates all the cases of each covariate pattern
cp.id <- tapply(dat$id, dat$indi, function(x) paste(x, collapse = ","))

n <- as.numeric(unlist(lapply(strsplit(cp.id, ","), length)))
# obs <- tapply(testMod$model[,1], dat$indi, sum)


# Creating a data.frame of covariate patterns
cp <- unique(dat[,2:ncol(dat)])
n <- as.numeric(unlist(lapply(strsplit(cp.id, ","), length)))
id <- tapply(dat$id, dat$indi, function(x) (x)[1])
cov.pattern <- data.frame(id, n, cp[,-ncol(cp)])
rownames(cov.pattern) <- rownames(cp)
## Create a vector with the covariate pattern for each case
id <- as.numeric(unlist(lapply(strsplit(cp.id, ","), function(x) rep(min(as.numeric(unlist(x))), length(x)))))[order(dat$id)]

list(cov.pattern = cov.pattern, id = id)

}




##############
# testing

system.time({
wo <- epi.cp(dat)
})
system.time({
wi <- popp.cp(dat)
})

wo$cov.pattern[wo$cov.pattern$n > 1,][1:10,]
wi$cov.pattern[wi$cov.pattern$n > 1,][1:10,]

cbind(wo$id, wi$id)[2000:2030,] # the is's partly different

# The id from epi.cp picks the rownumber of the covariate pattern, my one picks the rownumber of the full data.frame but in the end they are picking the same cases
i <- 2000:2030
cbind(dat[is.element(wo$id, wo$cov.pattern$id[i]),], dat[is.element(wi$id, wi$cov.pattern$id[i]),], wo$cov.pattern$id[i],wi$cov.pattern$id[i])




