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
    Cvec[i] <- StoI * rho
  }
  
  cases <- rnbinom(tmax, mu=Cvec, size=phi)
  
  data.frame(
    S=Svec,
    I=Ivec,
    C=Cvec,
    cases=cases
  )
}

simulate_sir_vari <- function(week,
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
                              delta,
                              tmax,
                              seed=101) {
  Svec <- Ivec <- Cvec <- rep(0, tmax)
  
  Svec[1] <- S0 * pop[1]
  Ivec[1] <- I0 * pop[1]
  Cvec[1] <- NA
  
  deltavec <- (exp(1:tmax/tmax)-1)*delta
  
  for (i in 2:tmax) {
    beta_eff <- exp(logbeta[week[i]]+epsilon[year[i]])
    
    foi <- beta_eff * (Ivec[i-1]+omega)/pop[1]
    
    Sout <- (1-exp(-(foi+drate[i]+deltavec[i]))) * Svec[i-1]
    StoI <- foi/(foi+drate[i]+deltavec[i]) * Sout
    Iout <- (1-exp(-(gamma+drate[i]))) * Ivec[i-1]
    ItoR <- gamma/(gamma+drate[i]) * Iout
    
    Svec[i] <- Svec[i-1] - Sout + brate[i] * pop[i]
    Ivec[i] <- Ivec[i-1] + StoI - Iout
    Cvec[i] <- StoI * rho
  }
  
  cases <- rnbinom(tmax, mu=Cvec, size=phi)

  data.frame(
    S=Svec,
    I=Ivec,
    C=Cvec,
    cases=cases
  )
}
