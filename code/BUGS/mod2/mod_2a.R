model{
  for(i in 1:N){
    # Multivariate likelihood
    farm[i, 1:3] ~ dmnorm(mu[i, 1:3], omega[1:3, 1:3])
    # Replicated data
    farm.rep[i, 1:3] ~ dmnorm(mu[i, 1:3], omega[1:3, 1:3])
    
    for(k in 1:3){ # 3 farm responses
      # Mean model
      mu[i, k] <- B[1, k] + # intercept
        B[2, k]*year[i] + # year effect
        eps[st[i], k] # state random effect
      
      # Posterior predictive loss calculation
      Sqdiff[i, k] <- pow(farm.rep[i, k] - farm[i, k],2)
      
      # Convert from log scale
      farm.rep.e[i, k] <- exp(farm.rep[i, k])
      mu.e[i, k] <- exp(mu[i, k])
    }
    
  }
  
  ### Exp then add national total
  for(y in 1:ts){
    for(k in 1:3){
      natl.rep[y, k] <- sum(farm.rep.e[(y*50 - 49):(y*50), k])
      natl.mu[y, k] <- sum(mu.e[(y*50 - 49):(y*50), k])
    }
  }
  
  ### Priors
  # Random intercept for state
  for(k in 1:3){ # 3 farm responses
    for(s in 1:Nst){ # 50 states
      # non-identifiable random effects
      eps[s, k] ~ dnorm(0, tau.eps[k])
      # identifable random effects
      Estar[s, k] <- eps[s, k] - mean.eps[k]
    }
  }
  for(k in 1:3){ # 3 farm response parameters
    # posterior mean of average Eps value for each response column
    mean.eps[k] <- mean(eps[,k])
    
    # folded-t prior for random-effect precision
    tau.Eps[k] ~ dt(0, Bb, 2)
    sig.eps[k] <- abs(tau.Eps[k])
    tau.eps[k] <- pow(sig.eps[k], -2)
    
    #Posterior predictive loss is the posterior mean of Dsum
    Dsum[k] <- sum(Sqdiff[,k])
  }
  
  #params for folded t, set as data
  Bb <- 1/(Ab*Ab) #Ab set as data, relatively large standard deviation
  
  # Diffuse normal priors for regression parameters
  for(p in 1:2){
    for(k in 1:3){
      B[p, k] ~ dnorm(0, 0.0001)
    }
  }
  
  # Wishart prior for precision matrix
  omega[1:3, 1:3] ~ dwish(R[1:3, 1:3], 4)
  #variance-covariance matrix
  Sig2[1:3, 1:3] <- inverse(omega[1:3, 1:3])
  #extracting the standard deviations
  for(k in 1:3){
    Sig[k] <- sqrt(Sig2[k,k])# std dev of each response
  }
  #calculating the correlation between traits
  for(i in 2:3){
    for(j in 1:(i-1)){
      Rho[i,j] <- Sig2[i,j]/(sqrt(Sig2[i,i])*sqrt(Sig2[j,j]))
    }
  }
}