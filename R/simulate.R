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
  
  Svec <- Ivec <- Cvec <- rep(0, tmax)
  
  Svec[1] <- round(S0 * pop[1])
  Ivec[1] <- round(I0 * pop[1])
  Cvec[1] <- NA
  
  for (i in 2:tmax) {
    beta_eff <- exp(logbeta[week[i]]+epsilon[year[i]])
    
    foi <- beta_eff * (Ivec[i-1]+omega)/pop[1]
    
    birth <- rpois(1, brate[i] * pop[i])
    
    Sout <- rbinom(1, Svec[i-1], (1-exp(-(foi+drate[i]))))
    StoI <- rbinom(1, Sout, foi/(foi+drate[i]))
    Iout <- rbinom(1, Ivec[i-1], (1-exp(-(gamma+drate[i]))))
    ItoR <- rbinom(1, Iout, gamma/(gamma+drate[i]))
    
    Svec[i] <- Svec[i-1] - Sout + birth
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
