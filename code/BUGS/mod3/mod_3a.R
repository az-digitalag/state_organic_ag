model{
  for(i in 1:N){ # observation loop
    # Multivariate likelihood
    farm[i, 1:3] ~ dmnorm(mu[i, 1:3], omega[1:3, 1:3])
    # Replicated data
    farm.rep[i, 1:3] ~ dmnorm(mu[i, 1:3], omega[1:3, 1:3])
    
    for(k in 1:3){ # 3 farm responses
      # Mean model
      mu[i, k] <- A[k] + # intercept
        B[k, st[i]]*year[i] + # year effect, varying by state
        eps[k, st[i]] # state random effect
      
      # Posterior predictive loss calculation
      Sqdiff[i, k] <- pow(farm.rep[i, k] - farm[i, k],2)
      
      # Convert from log scale
      farm.rep.e[i, k] <- exp(farm.rep[i, k])
      mu.e[i, k] <- exp(mu[i, k])
    }
    
  }
  
  ### Sum national totals (requires data to be organized by year then state)
  for(y in 1:ts){ # number of timesteps
    for(k in 1:3){ # 3 farm responses
      # Prediction interval
      natl.rep[y, k] <- sum(farm.rep.e[(y*50 - 49):(y*50), k])
      # Confidence interval
      natl.mu[y, k] <- sum(mu.e[(y*50 - 49):(y*50), k])
    }
  }
  
  ### Priors
  # Root node priors for intercept random effects by state
  for(k in 1:3){ # 3 farm responses
    for(s in 1:Nst){ # 50 states
      # Non-identifiable random effects
      eps[k, s] ~ dnorm(0, tau.eps[k])
      # Identifiable random effects
      Estar[k, s] <- eps[k, s] - mean.eps[k]
    }
  }
  for(k in 1:3){ # 3 farm response parameters
    # Mean of random effects for each response
    mean.eps[k] <- mean(eps[k,])
    
    # Folded-t prior for random-effect precision
    tau.Eps[k] ~ dt(0, Bb, 2)
    sig.eps[k] <- abs(tau.Eps[k])
    tau.eps[k] <- pow(sig.eps[k], -2)
    
    # Posterior predictive loss is the posterior mean of Dsum
    Dsum[k] <- sum(Sqdiff[,k])
    
    # root node of A, intercept
    A[k] ~ dnorm(0, 0.0001)
    # identifiable intercept
    Astar[k] <- A[k] + mean.eps[k]
  }
  
  # Sigma parameter for folded t, set as data in control script
  Bb <- 1/(Ab*Ab) # relatively large standard deviation
  
  # Hierarchical normal priors for slope parameters
  for(k in 1:3){ # 3 farm responses
    for(s in 1:Nst){ # 50 states
       B[k, s] ~ dnorm(mu.natl[k], tau.natl[k])
    }
  }
  
  # Root node priors for national level slope parameters
  for(k in 1:3){ # 3 farm responses
    mu.natl[k] ~ dnorm(0, 0.0001)
    tau.natl[k] ~ dgamma(0.01, 0.01)
    sig.natl[k] <- pow(tau.natl[k], -0.5)
  }
  
  # Wishart prior for precision matrix (3 by 3)
  omega[1:3, 1:3] ~ dwish(R[1:3, 1:3], 4)
  
  # Variance-covariance matrix
  Sig2[1:3, 1:3] <- inverse(omega[1:3, 1:3])
  
  # Standard deviations (matrix diagonals)
  for(k in 1:3){
    Sig[k] <- sqrt(Sig2[k,k])# std dev of each response
  }
  
  # Correlation between traits (off-diagonals of matrix)
  for(i in 2:3){
    for(j in 1:(i-1)){
      Rho[i,j] <- Sig2[i,j]/(sqrt(Sig2[i,i])*sqrt(Sig2[j,j]))
    }
  }
  
}