sir_default_ncores <- function() {
  requested <- suppressWarnings(as.integer(Sys.getenv("SIR_NCORES", "")))
  if (length(requested) == 1L && !is.na(requested) && requested > 0L) {
    return(requested)
  }

  available <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(available) != 1L || is.na(available)) {
    available <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  if (length(available) != 1L || is.na(available)) {
    available <- 1L
  }

  max(1L, min(4L, available))
}

run_one <- function(i, model, standata, iter = 1e4) {
  fit <- try(
    model$optimize(
      data = standata,
      seed = i,
      jacobian = TRUE,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE,
      iter = iter
    ),
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    warning(sprintf("Optimization failed for seed %d", i), call. = FALSE)
    return(NULL)
  }

  lp <- try(fit$lp(), silent = TRUE)
  if (inherits(lp, "try-error") || length(lp) != 1L || !is.finite(lp)) {
    warning(sprintf("No finite log density for seed %d", i), call. = FALSE)
    return(NULL)
  }

  data.frame(seed = i, value = lp)
}

run_parallel_optimizations <- function(model,
                                       standata,
                                       nfit = 100L,
                                       ncores = sir_default_ncores(),
                                       iter = 1e4) {
  nfit <- as.integer(nfit)
  ncores <- min(as.integer(ncores), nfit)

  if (nfit < 1L || ncores < 1L) {
    stop("nfit and ncores must both be positive integers")
  }

  seeds <- seq_len(nfit)
  run_seed <- function(i) run_one(i, model, standata, iter)

  if (ncores > 1L && .Platform$OS.type != "windows") {
    reslist <- parallel::mclapply(
      seeds,
      run_seed,
      mc.cores = ncores,
      mc.preschedule = FALSE
    )
  } else {
    if (ncores > 1L) {
      warning("Parallel optimization is not available on Windows; running sequentially")
    }
    reslist <- lapply(seeds, run_seed)
  }

  reslist <- Filter(Negate(is.null), reslist)
  if (!length(reslist)) {
    stop("All optimization attempts failed")
  }

  optim <- do.call(rbind, reslist)
  optim[order(optim$seed), , drop = FALSE]
}

fit_sir_perturb_demog_dd_rep_nc <- function(
    rep_id,
    nfit = 100L,
    ncores = sir_default_ncores(),
    iter = 1e4,
    data_file = "../simulate/simulate_rep_nochange.rda",
    stan_file = "../stanmodel/sir_perturb_demog_dd.stan",
    output_dir = ".") {
  rep_id <- as.integer(rep_id)
  if (length(rep_id) != 1L || is.na(rep_id) || rep_id < 1L) {
    stop("rep_id must be a positive integer")
  }

  simulation_env <- new.env(parent = emptyenv())
  loaded_objects <- load(data_file, envir = simulation_env)
  if (!"simulate_rep_nochange" %in% loaded_objects) {
    stop("simulate_rep_nochange was not found in ", data_file)
  }
  if (rep_id > length(simulation_env$simulate_rep_nochange)) {
    stop("rep_id exceeds the number of available no-change simulation replicates")
  }

  simulation_rep <- simulation_env$simulate_rep_nochange[[rep_id]]
  simulated_data <- simulation_rep[[1]]
  cases <- simulated_data$cases
  missing <- as.integer(is.na(cases))
  cases[missing == 1L] <- 0L

  standata <- list(
    N = nrow(simulated_data),
    week = simulated_data$week,
    year = simulated_data$year - min(simulated_data$year) + 1,
    Nyear = length(unique(simulated_data$year)),
    cases = cases,
    missing = missing,
    brate = simulated_data$brate,
    drate = simulated_data$drate,
    pop = simulated_data$pop,
    gamma = -log(1 - 1 / 3)
  )

  model <- cmdstanr::cmdstan_model(stan_file)

  message(sprintf(
    "No-change replicate %d: running %d optimizations on %d worker(s)",
    rep_id,
    nfit,
    ncores
  ))
  optim <- run_parallel_optimizations(
    model = model,
    standata = standata,
    nfit = nfit,
    ncores = ncores,
    iter = iter
  )

  if (interactive()) {
    plot(
      optim$seed,
      optim$value,
      xlab = "Seed",
      ylab = "Log density",
      main = sprintf("No-change replicate %d optimization results", rep_id)
    )
  }

  best_seed <- optim$seed[which.max(optim$value)]
  message(sprintf(
    "No-change replicate %d: refitting best seed %d",
    rep_id,
    best_seed
  ))
  final_fit <- model$optimize(
    data = standata,
    seed = best_seed,
    jacobian = TRUE,
    refresh = 0,
    show_messages = FALSE,
    iter = iter
  )

  message(sprintf(
    "No-change replicate %d: running Laplace approximation",
    rep_id
  ))
  laplace_fit <- model$laplace(
    data = standata,
    mode = final_fit,
    jacobian = TRUE
  )
  laplace_summary <- laplace_fit$summary()

  optim_name <- sprintf(
    "stanfit_sir_perturb_demog_dd_cmdstanr_optim_rep_nc%d",
    rep_id
  )
  laplace_name <- sprintf(
    "stanfit_sir_perturb_demog_dd_cmdstanr_laplace_rep_nc%d",
    rep_id
  )
  summary_name <- sprintf(
    "summary_sir_perturb_demog_dd_cmdstanr_laplace_rep_nc%d",
    rep_id
  )

  results <- setNames(
    list(optim, laplace_fit, laplace_summary),
    c(optim_name, laplace_name, summary_name)
  )
  list2env(results, envir = .GlobalEnv)

  output_file <- file.path(
    output_dir,
    sprintf("stanfit_sir_perturb_demog_dd_cmdstanr_rep_nc%d.rda", rep_id)
  )
  save(list = names(results), envir = .GlobalEnv, file = output_file)
  message("Saved ", output_file)

  invisible(results)
}
