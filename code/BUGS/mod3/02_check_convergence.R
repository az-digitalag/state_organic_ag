# Load coda object to inspect for convergence
# Save starting values (initials)

# Load packages
library(coda)
library(mcmcplots)
library(postjags)

# Load coda
load("coda/coda_out_3a.Rdata")

# View chains to inspect for convergence visually
mcmcplot(coda_out, parms = c("deviance", "Dsum", "Astar", "mu.natl", "tau.natl"))
caterplot(coda_out, regex = c("Estar\\[1"), reorder = FALSE)
caterplot(coda_out, regex = c("Estar\\[2"), reorder = FALSE)
caterplot(coda_out, regex = c("Estar\\[3"), reorder = FALSE)

# Extract final iteration, save initials for reuse
newinits <- initfind(coda_out, OpenBUGS = TRUE)
saved.state <- removevars(initsin = newinits, 
                          variables=c(2:7))

save(saved.state, file = "inits/inits_3a.Rdata")

# If not converged and some chains in local minima, reinitialize with lowest Dsum
# round(apply(coda_out[[1]][,7:9],2,mean), 3)
# round(apply(coda_out[[2]][,7:9],2,mean), 3)
# round(apply(coda_out[[3]][,7:9],2,mean), 3)
# round(colMeans(coda_out[[1]][,grep("Estar\\[\\d{1,2},1\\]", colnames(coda_out[[1]]))]), 3)
# round(colMeans(coda_out[[2]][,grep("Estar\\[\\d{1,2},1\\]", colnames(coda_out[[2]]))]), 3)
# round(colMeans(coda_out[[3]][,grep("Estar\\[\\d{1,2},1\\]", colnames(coda_out[[3]]))]), 3)

# newits <- list(saved.state[[2]][[1]], saved.state[[2]][[1]], saved.state[[2]][[1]])

# newits <- saved.state[[2]]
# for(i in 1:3){
#   newits[[i]]$tau.Eps[3] <- runif(1, 0, 1)
# }

# Check convergence with Gelman diagnostic
gel <- gelman.diag(coda_out, multivariate = F)
str(gel)
gel$psrf[match("Dsum[1]", row.names(gel$psrf)):match("Dsum[3]", row.names(gel$psrf)),]
Bs<-matrix(NA, ncol = 3, nrow = 50)
for(i in 1:3){
  for(j in 1:50){
    Bs[j, i]<-paste0("B[", i, ",", j, "]")
  }
}
gel$psrf[match(Bs[,1], row.names(gel$psrf)),]
gel$psrf[match(Bs[,2], row.names(gel$psrf)),]
gel$psrf[match(Bs[,3], row.names(gel$psrf)),]

Estars<-matrix(NA, ncol = 3, nrow = 50)
for(i in 1:3){
  for(j in 1:50){
    Estars[j,i]<-paste0("Estar[", i, ",", j, "]")
  }
}
gel$psrf[match(Estars[,1], row.names(gel$psrf)),]
gel$psrf[match(Estars[,2], row.names(gel$psrf)),]
gel$psrf[match(Estars[,3], row.names(gel$psrf)),]

# population level mean and precision
gel$psrf[grep("mu.natl", row.names(gel$psrf)),]
gel$psrf[grep("tau.natl", row.names(gel$psrf)),]

#Sig or sd of each response
gel$psrf[match("Sig[1]", row.names(gel$psrf)):match("Sig[3]", row.names(gel$psrf)),]
#Rho, or correlation coefficient between each pair of traits
gel$psrf[match("Rho[3,1]", row.names(gel$psrf)):match("Rho[3,2]", row.names(gel$psrf)),]
gel$psrf[match("Rho[2,1]", row.names(gel$psrf)),]
#Omega (precision matrix)
gel$psrf[match("omega[1,1]", row.names(gel$psrf)):match("omega[3,3]", row.names(gel$psrf)),]
