simulate_sir <- function(week,
                         year,
                         brate,
                         drate,
                         pop,
                         gamma,
                         S0,
                         I0,
                         rho,
                         logbeta, 
                         epsilon,
                         omega,
                         phi,
                         tmax,
                         seed=101) {
  set.seed(seed)
  
  Svec <- Ivec <- Cvec <- rep(0, tmax)
  
  Svec[1] <- S0 * pop[1]
  Ivec[1] <- I0 * pop[1]
  Cvec[1] <- NA
  
  for (i in 2:tmax) {
    beta_eff <- exp(logbeta[week[i]]+epsilon[year[i]])
    
    foi <- beta_eff * (Ivec[i-1]+omega)/pop[1]
    
    Sout <- (1-exp(-(foi+drate[i]))) * Svec[i-1]
    StoI <- foi/(foi+drate[i]) * Sout
    Iout <- (1-exp(-(gamma+drate[i]))) * Ivec[i-1]
    ItoR <- gamma/(gamma+drate[i]) * Iout
    
    Svec[i] <- Svec[i-1] - Sout + brate[i] * pop[i]
    Ivec[i] <- Ivec[i-1] + StoI - Iout
    Cvec[i] <- ItoR * rho
  }
  
  cases <- rnbinom(tmax, mu=Cvec, size=phi)
  
  data.frame(
    S=Svec,
    I=Ivec,
    C=Cvec,
    cases=cases
  )
}

simulate_sir_stoch <- function(week,
                         year,
                         brate,
                         drate,
                         pop,
                         gamma,
                         S0,
                         I0,
                         rho,
                         logbeta, 
                         epsilon,
                         omega,
                         phi,
                         tmax,
                         seed=101) {
  set.seed(seed)
  
  Svec <- Ivec <- Rvec <- Cvec <- rep(0, tmax)
  
  birthvec <- deathvec <- popvec <- rep(0, tmax)
  
  Svec[1] <- round(S0 * pop[1])
  Ivec[1] <- round(I0 * pop[1])
  Rvec[1] <- round(pop[1] - Svec[1] - Ivec[1])
  Cvec[1] <- NA
  birthvec[1] <- round(brate[1] * pop[1])
  deathvec[1] <- round(drate[1] * pop[1])
  popvec[1] <- round(pop[1])
  
  for (i in 2:tmax) {
    beta_eff <- exp(logbeta[week[i]]+epsilon[year[i]])
    
    foi <- beta_eff * (Ivec[i-1]+omega)/popvec[1]
    
    birth <- birthvec[i] <- rpois(1, brate[i] * popvec[i-1])
    
    Sout <- rbinom(1, Svec[i-1], (1-exp(-(foi+drate[i]))))
    StoI <- rbinom(1, Sout, foi/(foi+drate[i]))
    Iout <- rbinom(1, Ivec[i-1], (1-exp(-(gamma+drate[i]))))
    ItoR <- rbinom(1, Iout, gamma/(gamma+drate[i]))
    Rout <- rbinom(1, Rvec[i-1], 1-exp(-drate[i]))
    
    Svec[i] <- Svec[i-1] - Sout + birth
    Ivec[i] <- Ivec[i-1] + StoI - Iout
    Rvec[i] <- Rvec[i-1] + ItoR - Rout
    Cvec[i] <- ItoR * rho
    
    popvec[i] <- Svec[i] + Ivec[i] + Rvec[i]
    
    deathvec[i] <- Sout-StoI+Iout-ItoR+Rout
  }
  
  cases <- rnbinom(tmax, mu=Cvec, size=phi)
  
  data.frame(
    S=Svec,
    I=Ivec,
    C=Cvec,
    cases=cases,
    birthvec=birthvec,
    deathvec=deathvec,
    popvec=popvec
  )
}
