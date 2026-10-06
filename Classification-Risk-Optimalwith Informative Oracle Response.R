# ============================================================
# Classification-Risk-Optimal Label Acquisition
# with Informative Oracle Response
#
# REVISED simulation program
#
# Study A: ORACLE acquisition
#   - acquisition scores use the TRUE model parameters;
#   - all methods still estimate the classifier/response model from
#     the data they actually acquire.
#
# Study B: ADAPTIVE acquisition
#   - acquisition scores use estimates updated from the queried data.
#
# This separation lets us test:
#   (i) the theoretical acquisition principle itself, and
#   (ii) the practical cost of learning the response mechanism.
#
# All methods use the SAME full observed-data likelihood after querying.
# They differ only in the acquisition rule.
# ============================================================

rm(list = ls())
options(stringsAsFactors = FALSE)
options(width = 120)

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

needed <- c("ggplot2", "patchwork", "scales")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  stop(
    "Please install the following packages first:\n",
    paste0(
      "install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      "))"
    )
  )
}

library(ggplot2)
library(patchwork)
library(scales)

# ------------------------------------------------------------
# 1. User controls
# ------------------------------------------------------------

DEBUG <- FALSE

# DEBUG:
#   TRUE  -> fast diagnostic run
#   FALSE -> final Monte Carlo run
N_REP <- if (DEBUG) 20L else 500L

SEED <- 260905L

N_POOL <- 3000L
BUDGETS <- c(300L, 600L, 900L)
MAX_BUDGET <- max(BUDGETS)

PILOT_SIZE <- 100L
BATCH_SIZE <- 25L

# Equalize the marginal oracle-response rate across response regimes.
TARGET_MEAN_RESPONSE <- 0.40

# Save detailed query locations only for a subset of replications.
TRACE_REPS <- if (DEBUG) min(10L, N_REP) else 50L

# Main studies.
STUDY_MODES <- c("Oracle", "Adaptive")

# Optional pilot-size sensitivity study.
# Keep FALSE for the first debug run.
# After the main simulation is checked, this can be switched on.
RUN_PILOT_SENSITIVITY <- FALSE
PILOT_SIZES_SENSITIVITY <- c(100L, 200L, 400L)
PILOT_SENSITIVITY_SCENARIO <- "Strong"
PILOT_SENSITIVITY_BUDGET <- 900L
N_REP_PILOT_SENSITIVITY <- if (DEBUG) 20L else 300L

OUT_DIR <- "simulation_response_aware_revised"
FIG_DIR <- file.path(OUT_DIR, "figures")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 2. Professional figure settings
# ------------------------------------------------------------

# Colorblind-friendly, journal-style palette.
METHOD_COLORS <- c(
  Random           = "#4D4D4D",
  Entropy          = "#0072B2",
  OrdinaryRisk     = "#E69F00",
  DiscountedRisk   = "#009E73",
  ResponseAware    = "#D55E00"
)

METHOD_LABELS <- c(
  Random         = "Random",
  Entropy        = "Entropy",
  OrdinaryRisk   = "Ordinary risk",
  DiscountedRisk = "Response-discounted",
  ResponseAware  = "Response-aware"
)

METHOD_LINETYPES <- c(
  Random         = "solid",
  Entropy        = "dashed",
  OrdinaryRisk   = "dotdash",
  DiscountedRisk = "longdash",
  ResponseAware  = "solid"
)

METHOD_SHAPES <- c(
  Random         = 16,
  Entropy        = 17,
  OrdinaryRisk   = 15,
  DiscountedRisk = 3,
  ResponseAware  = 18
)

SCENARIO_LABELS <- c(
  Noninformative = "Noninformative response",
  Moderate       = "Moderately informative response",
  Strong         = "Strongly informative response"
)

theme_paper <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.35),
      axis.line = element_line(color = "#333333", linewidth = 0.45),
      axis.ticks = element_line(color = "#333333", linewidth = 0.4),
      axis.text = element_text(color = "#222222"),
      axis.title = element_text(color = "#111111"),
      plot.title = element_text(face = "bold", size = rel(1.04)),
      plot.subtitle = element_text(color = "#555555"),
      strip.text = element_text(face = "bold", color = "#111111"),
      strip.background = element_rect(fill = "#F3F3F3", color = NA),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.box = "horizontal",
      plot.margin = margin(8, 10, 8, 8)
    )
}

# ------------------------------------------------------------
# 3. Data-generating classifier
# ------------------------------------------------------------

TRUE_ALPHA <- -0.6
TRUE_BETA  <-  2.0
TRUE_B     <- -TRUE_ALPHA / TRUE_BETA

# ------------------------------------------------------------
# 4. Basic mathematical functions
# ------------------------------------------------------------

clip_prob <- function(p, eps = 1e-10) {
  pmin(pmax(p, eps), 1 - eps)
}

binary_entropy <- function(p) {
  p <- clip_prob(p)
  -(p * log(p) + (1 - p) * log(1 - p))
}

tau_fun <- function(y, alpha, beta) {
  plogis(alpha + beta * y)
}

response_prob <- function(y, alpha, beta, eta0, eta1) {
  tau <- tau_fun(y, alpha, beta)
  U <- binary_entropy(tau)
  plogis(eta0 + eta1 * U)
}

calibrate_eta0 <- function(
    eta1,
    target = TARGET_MEAN_RESPONSE,
    alpha = TRUE_ALPHA,
    beta = TRUE_BETA,
    n_grid = 100000L
) {
  u <- (seq_len(n_grid) - 0.5) / n_grid
  y <- qnorm(u)
  U <- binary_entropy(tau_fun(y, alpha, beta))
  
  objective <- function(eta0) {
    mean(plogis(eta0 + eta1 * U)) - target
  }
  
  uniroot(objective, interval = c(-20, 20), tol = 1e-12)$root
}

# ------------------------------------------------------------
# 5. Response-mechanism scenarios
# ------------------------------------------------------------

eta1_values <- c(
  Noninformative =  0,
  Moderate       = -3,
  Strong         = -5
)

SCENARIOS <- data.frame(
  scenario = names(eta1_values),
  eta1 = as.numeric(eta1_values),
  stringsAsFactors = FALSE
)

SCENARIOS$eta0 <- vapply(
  SCENARIOS$eta1,
  calibrate_eta0,
  numeric(1)
)

SCENARIOS$mean_response_target <- TARGET_MEAN_RESPONSE

SCENARIOS$boundary_response <- plogis(
  SCENARIOS$eta0 + SCENARIOS$eta1 * log(2)
)

write.csv(
  SCENARIOS,
  file.path(OUT_DIR, "scenario_parameters.csv"),
  row.names = FALSE
)

print(SCENARIOS)

# ------------------------------------------------------------
# 6. Full observed-data likelihood
# ------------------------------------------------------------

joint_nll <- function(par, y, R, z) {
  alpha <- par[1]
  beta  <- par[2]
  eta0  <- par[3]
  eta1  <- par[4]
  
  tau <- clip_prob(tau_fun(y, alpha, beta))
  U <- binary_entropy(tau)
  r <- clip_prob(plogis(eta0 + eta1 * U))
  
  ll_response <- R * log(r) + (1 - R) * log(1 - r)
  
  ll_label <- numeric(length(y))
  id <- which(R == 1L)
  
  if (length(id) > 0L) {
    zz <- z[id]
    tt <- tau[id]
    
    ll_label[id] <-
      zz * log(tt) +
      (1 - zz) * log(1 - tt)
  }
  
  ans <- -sum(ll_response + ll_label)
  
  if (!is.finite(ans)) {
    ans <- 1e100
  }
  
  ans
}

initial_par <- function(y, R, z, previous = NULL) {
  if (!is.null(previous) && all(is.finite(previous))) {
    return(previous)
  }
  
  alpha0 <- 0
  beta0 <- 1
  
  responded <- which(R == 1L)
  
  if (
    length(responded) >= 10L &&
    length(unique(z[responded])) == 2L
  ) {
    fit_z <- try(
      glm(z[responded] ~ y[responded], family = binomial()),
      silent = TRUE
    )
    
    if (!inherits(fit_z, "try-error")) {
      cc <- coef(fit_z)
      
      if (length(cc) == 2L && all(is.finite(cc))) {
        alpha0 <- cc[1]
        beta0 <- max(0.10, abs(cc[2]))
      }
    }
  }
  
  tau0 <- tau_fun(y, alpha0, beta0)
  U0 <- binary_entropy(tau0)
  
  eta00 <- qlogis(clip_prob(mean(R), eps = 1e-4))
  eta10 <- 0
  
  if (length(unique(R)) == 2L && sd(U0) > 1e-8) {
    fit_r <- try(
      glm(R ~ U0, family = binomial()),
      silent = TRUE
    )
    
    if (!inherits(fit_r, "try-error")) {
      cc <- coef(fit_r)
      
      if (length(cc) == 2L && all(is.finite(cc))) {
        eta00 <- cc[1]
        eta10 <- cc[2]
      }
    }
  }
  
  c(
    alpha0,
    beta0,
    pmin(pmax(eta00, -8), 8),
    pmin(pmax(eta10, -8), 8)
  )
}

fit_joint_model <- function(y, R, z, previous = NULL) {
  start <- initial_par(y, R, z, previous)
  
  fit <- try(
    optim(
      par = start,
      fn = joint_nll,
      y = y,
      R = R,
      z = z,
      method = "L-BFGS-B",
      lower = c(-8, 0.05, -12, -12),
      upper = c( 8, 8.00,  12,  12),
      control = list(
        maxit = 1200,
        factr = 1e7
      )
    ),
    silent = TRUE
  )
  
  if (
    inherits(fit, "try-error") ||
    !all(is.finite(fit$par))
  ) {
    return(
      list(
        par = start,
        convergence = 999L,
        value = NA_real_
      )
    )
  }
  
  list(
    par = fit$par,
    convergence = fit$convergence,
    value = fit$value
  )
}

# ------------------------------------------------------------
# 7. Information components
# ------------------------------------------------------------

component_values <- function(y, par) {
  alpha <- par[1]
  beta  <- par[2]
  eta0  <- par[3]
  eta1  <- par[4]
  
  g <- alpha + beta * y
  tau <- clip_prob(plogis(g))
  s <- tau * (1 - tau)
  U <- binary_entropy(tau)
  
  r <- clip_prob(plogis(eta0 + eta1 * U))
  w <- r * (1 - r)
  
  X <- cbind(1, y)
  
  mult <- -eta1 * g * s
  qtheta <- X * mult
  
  qeta <- cbind(1, U)
  qfull <- cbind(qtheta, qeta)
  
  list(
    X = X,
    g = g,
    tau = tau,
    s = s,
    U = U,
    r = r,
    w = w,
    qtheta = qtheta,
    qeta = qeta,
    qfull = qfull
  )
}

safe_inverse <- function(M, ridge = 1e-8) {
  M <- (M + t(M)) / 2
  
  ev <- eigen(
    M,
    symmetric = TRUE,
    only.values = TRUE
  )$values
  
  add <- if (min(ev) < ridge) {
    ridge - min(ev)
  } else {
    0
  }
  
  solve(
    M + diag(add + ridge, nrow(M))
  )
}

risk_curvature_direction <- function(par) {
  alpha <- par[1]
  beta <- par[2]
  
  b <- -alpha / beta
  v <- c(1, b)
  
  # Positive scalar f(b)/(2 beta) does not affect candidate rankings.
  tcrossprod(v)
}

info_ordinary <- function(y_query, par) {
  comp <- component_values(y_query, par)
  crossprod(comp$X, comp$X * comp$s)
}

info_discounted <- function(y_query, par) {
  comp <- component_values(y_query, par)
  crossprod(
    comp$X,
    comp$X * (comp$r * comp$s)
  )
}

info_full <- function(y_query, par) {
  comp <- component_values(y_query, par)
  
  M <- matrix(0, 4, 4)
  
  # Expected returned-label information.
  M[1:2, 1:2] <-
    crossprod(
      comp$X,
      comp$X * (comp$r * comp$s)
    )
  
  # Response/abstention information.
  M <-
    M +
    crossprod(
      comp$qfull,
      comp$qfull * comp$w
    )
  
  M
}

# ------------------------------------------------------------
# 8. Acquisition scores
# ------------------------------------------------------------

score_candidates <- function(method, y_candidate, y_query, par_score) {
  comp_c <- component_values(y_candidate, par_score)
  H2 <- risk_curvature_direction(par_score)
  
  if (method == "Entropy") {
    return(comp_c$U)
  }
  
  if (method == "OrdinaryRisk") {
    M <- info_ordinary(y_query, par_score)
    Minv <- safe_inverse(M)
    
    A <- Minv %*% H2 %*% Minv
    XA <- comp_c$X %*% A
    
    return(
      comp_c$s *
        rowSums(XA * comp_c$X)
    )
  }
  
  if (method == "DiscountedRisk") {
    M <- info_discounted(y_query, par_score)
    Minv <- safe_inverse(M)
    
    A <- Minv %*% H2 %*% Minv
    XA <- comp_c$X %*% A
    
    return(
      comp_c$r *
        comp_c$s *
        rowSums(XA * comp_c$X)
    )
  }
  
  if (method == "ResponseAware") {
    M <- info_full(y_query, par_score)
    Minv <- safe_inverse(M)
    
    H4 <- matrix(0, 4, 4)
    H4[1:2, 1:2] <- H2
    
    A4 <- Minv %*% H4 %*% Minv
    
    X4 <- cbind(comp_c$X, 0, 0)
    
    psi_label <-
      comp_c$r *
      comp_c$s *
      rowSums((X4 %*% A4) * X4)
    
    psi_response <-
      comp_c$w *
      rowSums(
        (comp_c$qfull %*% A4) *
          comp_c$qfull
      )
    
    return(psi_label + psi_response)
  }
  
  stop("Unknown acquisition method: ", method)
}

# ------------------------------------------------------------
# 9. Exact/stable risk evaluation
# ------------------------------------------------------------

N_RISK_GRID <- 150000L

risk_y <- qnorm(
  (seq_len(N_RISK_GRID) - 0.5) / N_RISK_GRID
)

risk_tau <- tau_fun(
  risk_y,
  TRUE_ALPHA,
  TRUE_BETA
)

cum_tau <- cumsum(risk_tau)
cum_one_minus <- cumsum(1 - risk_tau)
total_one_minus <- sum(1 - risk_tau)

risk_from_boundary <- function(boundary) {
  k <- findInterval(boundary, risk_y)
  
  left <-
    if (k == 0L) 0 else cum_tau[k]
  
  right <-
    if (k >= N_RISK_GRID) {
      0
    } else {
      total_one_minus -
        if (k == 0L) 0 else cum_one_minus[k]
    }
  
  (left + right) / N_RISK_GRID
}

BAYES_RISK <- risk_from_boundary(TRUE_B)

# ------------------------------------------------------------
# 10. Evaluation function
# ------------------------------------------------------------

evaluate_state <- function(
    par_hat,
    queried,
    y_pool,
    R_pool,
    true_tau_pool,
    scenario,
    study,
    method,
    rep_id,
    budget,
    convergence,
    true_eta0,
    true_eta1
) {
  bhat <- -par_hat[1] / par_hat[2]
  risk <- risk_from_boundary(bhat)
  
  data.frame(
    study = study,
    scenario = scenario,
    method = method,
    rep = rep_id,
    budget = budget,
    
    risk = risk,
    excess_risk = risk - BAYES_RISK,
    
    boundary_hat = bhat,
    boundary_abs_error = abs(bhat - TRUE_B),
    
    alpha_hat = par_hat[1],
    beta_hat  = par_hat[2],
    eta0_hat  = par_hat[3],
    eta1_hat  = par_hat[4],
    
    true_eta0 = true_eta0,
    true_eta1 = true_eta1,
    
    n_returned = sum(R_pool[queried]),
    response_rate = mean(R_pool[queried]),
    
    mean_abs_distance_to_boundary =
      mean(abs(y_pool[queried] - TRUE_B)),
    
    mean_true_entropy_queried =
      mean(binary_entropy(true_tau_pool[queried])),
    
    convergence = convergence,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 11. Methods
# ------------------------------------------------------------

METHODS <- c(
  "Random",
  "Entropy",
  "OrdinaryRisk",
  "DiscountedRisk",
  "ResponseAware"
)

# ------------------------------------------------------------
# 12. One acquisition path
# ------------------------------------------------------------

run_method_path <- function(
    study,
    method,
    y_pool,
    z_pool,
    R_pool,
    true_tau_pool,
    pilot_idx,
    scenario,
    rep_id,
    true_par,
    save_trace = FALSE
) {
  queried <- pilot_idx
  previous_par <- NULL
  
  result_list <- list()
  trace_list <- list()
  
  if (save_trace) {
    trace_list[[1]] <- data.frame(
      study = study,
      scenario = scenario,
      method = method,
      rep = rep_id,
      query_order = seq_along(queried),
      y = y_pool[queried],
      pilot = TRUE,
      responded = R_pool[queried]
    )
  }
  
  current_budget <- length(queried)
  
  repeat {
    # Everyone is evaluated using the SAME fitted full likelihood.
    fit <- fit_joint_model(
      y = y_pool[queried],
      R = R_pool[queried],
      z = z_pool[queried],
      previous = previous_par
    )
    
    par_hat <- fit$par
    previous_par <- par_hat
    
    if (current_budget %in% BUDGETS) {
      key <- as.character(current_budget)
      
      result_list[[key]] <- evaluate_state(
        par_hat = par_hat,
        queried = queried,
        y_pool = y_pool,
        R_pool = R_pool,
        true_tau_pool = true_tau_pool,
        scenario = scenario,
        study = study,
        method = method,
        rep_id = rep_id,
        budget = current_budget,
        convergence = fit$convergence,
        true_eta0 = true_par[3],
        true_eta1 = true_par[4]
      )
    }
    
    if (current_budget >= MAX_BUDGET) {
      break
    }
    
    remaining <- setdiff(seq_len(N_POOL), queried)
    take <- min(BATCH_SIZE, MAX_BUDGET - current_budget)
    
    if (method == "Random") {
      new_idx <- sample(remaining, take)
    } else {
      # ORACLE study: score with true parameters.
      # ADAPTIVE study: score with current estimates.
      par_score <-
        if (study == "Oracle") {
          true_par
        } else {
          par_hat
        }
      
      scores <- score_candidates(
        method = method,
        y_candidate = y_pool[remaining],
        y_query = y_pool[queried],
        par_score = par_score
      )
      
      # Deterministic stable tie-breaking by pool index.
      # This guarantees exact collapse of ResponseAware and DiscountedRisk
      # in the oracle noninformative-response scenario when their scores agree.
      ord <- order(
        -scores,
        remaining,
        na.last = NA
      )
      
      if (length(ord) < take) {
        chosen <- remaining[ord]
        extra_pool <- setdiff(remaining, chosen)
        new_idx <- c(
          chosen,
          sample(extra_pool, take - length(chosen))
        )
      } else {
        new_idx <- remaining[ord[seq_len(take)]]
      }
    }
    
    queried <- c(queried, new_idx)
    
    if (save_trace) {
      old_n <- current_budget
      
      trace_list[[length(trace_list) + 1L]] <-
        data.frame(
          study = study,
          scenario = scenario,
          method = method,
          rep = rep_id,
          query_order = old_n + seq_along(new_idx),
          y = y_pool[new_idx],
          pilot = FALSE,
          responded = R_pool[new_idx]
        )
    }
    
    current_budget <- length(queried)
  }
  
  list(
    results = do.call(rbind, result_list),
    trace =
      if (save_trace) {
        do.call(rbind, trace_list)
      } else {
        NULL
      }
  )
}

# ------------------------------------------------------------
# 13. One Monte Carlo replication
# ------------------------------------------------------------

run_replication <- function(rep_id, scenario_row) {
  scenario_name <- scenario_row$scenario
  eta0_true <- scenario_row$eta0
  eta1_true <- scenario_row$eta1
  
  true_par <- c(
    TRUE_ALPHA,
    TRUE_BETA,
    eta0_true,
    eta1_true
  )
  
  scenario_number <- match(
    scenario_name,
    SCENARIOS$scenario
  )
  
  set.seed(
    SEED +
      100000L * scenario_number +
      rep_id
  )
  
  y_pool <- rnorm(N_POOL)
  
  true_tau_pool <- tau_fun(
    y_pool,
    TRUE_ALPHA,
    TRUE_BETA
  )
  
  z_pool <- rbinom(
    N_POOL,
    size = 1,
    prob = true_tau_pool
  )
  
  true_r_pool <- response_prob(
    y_pool,
    TRUE_ALPHA,
    TRUE_BETA,
    eta0_true,
    eta1_true
  )
  
  # Potential response under every possible query.
  # Shared across all methods and both studies within a replication.
  response_uniform <- runif(N_POOL)
  R_pool <- as.integer(
    response_uniform < true_r_pool
  )
  
  # Shared pilot across methods and study modes.
  pilot_idx <- sample(
    seq_len(N_POOL),
    PILOT_SIZE
  )
  
  result_blocks <- list()
  trace_blocks <- list()
  
  for (study in STUDY_MODES) {
    for (method in METHODS) {
      obj <- run_method_path(
        study = study,
        method = method,
        y_pool = y_pool,
        z_pool = z_pool,
        R_pool = R_pool,
        true_tau_pool = true_tau_pool,
        pilot_idx = pilot_idx,
        scenario = scenario_name,
        rep_id = rep_id,
        true_par = true_par,
        save_trace = rep_id <= TRACE_REPS
      )
      
      key <- paste(study, method, sep = "__")
      
      result_blocks[[key]] <- obj$results
      
      if (!is.null(obj$trace)) {
        trace_blocks[[key]] <- obj$trace
      }
    }
  }
  
  list(
    results = do.call(rbind, result_blocks),
    trace =
      if (length(trace_blocks) > 0L) {
        do.call(rbind, trace_blocks)
      } else {
        NULL
      }
  )
}

# ------------------------------------------------------------
# 14. Geometry figure
# ------------------------------------------------------------

make_geometry_figure <- function() {
  scen <- SCENARIOS[
    SCENARIOS$scenario == "Strong",
  ]
  
  par_true <- c(
    TRUE_ALPHA,
    TRUE_BETA,
    scen$eta0,
    scen$eta1
  )
  
  qy <- qnorm(
    (seq_len(100000L) - 0.5) / 100000L
  )
  
  rho_ref <- 0.50
  
  H2 <- risk_curvature_direction(par_true)
  
  H4 <- matrix(0, 4, 4)
  H4[1:2, 1:2] <- H2
  
  M_ord <-
    rho_ref *
    info_ordinary(qy, par_true) /
    length(qy)
  
  M_dis <-
    rho_ref *
    info_discounted(qy, par_true) /
    length(qy)
  
  M_full <-
    rho_ref *
    info_full(qy, par_true) /
    length(qy)
  
  Iord <- safe_inverse(M_ord)
  Idis <- safe_inverse(M_dis)
  Ifull <- safe_inverse(M_full)
  
  Aord <- Iord %*% H2 %*% Iord
  Adis <- Idis %*% H2 %*% Idis
  Afull <- Ifull %*% H4 %*% Ifull
  
  ygrid <- seq(-3, 3, length.out = 1600L)
  cc <- component_values(ygrid, par_true)
  
  X <- cc$X
  X4 <- cbind(X, 0, 0)
  
  psi_ord <-
    cc$s *
    rowSums((X %*% Aord) * X)
  
  psi_dis <-
    cc$r *
    cc$s *
    rowSums((X %*% Adis) * X)
  
  psi_L <-
    cc$r *
    cc$s *
    rowSums((X4 %*% Afull) * X4)
  
  psi_R <-
    cc$w *
    rowSums(
      (cc$qfull %*% Afull) *
        cc$qfull
    )
  
  psi_full <- psi_L + psi_R
  
  v <- c(1, TRUE_B)
  
  raw_label <-
    cc$r *
    cc$s *
    (as.vector(X %*% v))^2
  
  raw_response <-
    cc$w *
    (as.vector(cc$qtheta %*% v))^2
  
  norm01 <- function(x) {
    mx <- max(x, na.rm = TRUE)
    
    if (!is.finite(mx) || mx <= 0) {
      return(rep(0, length(x)))
    }
    
    x / mx
  }
  
  boundary_line <- geom_vline(
    xintercept = TRUE_B,
    linewidth = 0.55,
    linetype = "dotted",
    color = "#333333"
  )
  
  d1 <- rbind(
    data.frame(
      y = ygrid,
      value = cc$tau,
      curve = "Class probability"
    ),
    data.frame(
      y = ygrid,
      value = cc$r,
      curve = "Response probability"
    )
  )
  
  p1 <- ggplot(
    d1,
    aes(
      x = y,
      y = value,
      color = curve,
      linetype = curve
    )
  ) +
    geom_line(linewidth = 0.95) +
    boundary_line +
    scale_color_manual(
      values = c(
        "Class probability" = "#0072B2",
        "Response probability" = "#D55E00"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Class probability" = "solid",
        "Response probability" = "dashed"
      )
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      x = expression(y),
      y = "Probability",
      title = "(a) Class and oracle-response probabilities"
    ) +
    theme_paper()
  
  d2 <- rbind(
    data.frame(
      y = ygrid,
      value = norm01(raw_label),
      component = "Returned-label information"
    ),
    data.frame(
      y = ygrid,
      value = norm01(raw_response),
      component = "Response information"
    )
  )
  
  p2 <- ggplot(
    d2,
    aes(
      x = y,
      y = value,
      color = component,
      linetype = component
    )
  ) +
    geom_line(linewidth = 0.95) +
    boundary_line +
    scale_color_manual(
      values = c(
        "Returned-label information" = "#009E73",
        "Response information" = "#CC79A7"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Returned-label information" = "dashed",
        "Response information" = "solid"
      )
    ) +
    labs(
      x = expression(y),
      y = "Normalized value",
      title = "(b) Decision-relevant information geometry"
    ) +
    theme_paper()
  
  d3 <- rbind(
    data.frame(
      y = ygrid,
      value = norm01(psi_ord),
      method = "Ordinary risk"
    ),
    data.frame(
      y = ygrid,
      value = norm01(psi_dis),
      method = "Response-discounted"
    ),
    data.frame(
      y = ygrid,
      value = norm01(psi_full),
      method = "Response-aware"
    )
  )
  
  p3 <- ggplot(
    d3,
    aes(
      x = y,
      y = value,
      color = method,
      linetype = method
    )
  ) +
    geom_line(linewidth = 0.95) +
    boundary_line +
    scale_color_manual(
      values = c(
        "Ordinary risk" = "#E69F00",
        "Response-discounted" = "#009E73",
        "Response-aware" = "#D55E00"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Ordinary risk" = "dotdash",
        "Response-discounted" = "longdash",
        "Response-aware" = "solid"
      )
    ) +
    labs(
      x = expression(y),
      y = "Normalized acquisition value",
      title = "(c) Candidate acquisition values"
    ) +
    theme_paper()
  
  d4 <- rbind(
    data.frame(
      y = ygrid,
      value = norm01(psi_L),
      component = "Returned-label value"
    ),
    data.frame(
      y = ygrid,
      value = norm01(psi_R),
      component = "Response value"
    ),
    data.frame(
      y = ygrid,
      value = norm01(psi_full),
      component = "Total value"
    )
  )
  
  p4 <- ggplot(
    d4,
    aes(
      x = y,
      y = value,
      color = component,
      linetype = component
    )
  ) +
    geom_line(linewidth = 0.95) +
    boundary_line +
    scale_color_manual(
      values = c(
        "Returned-label value" = "#009E73",
        "Response value" = "#CC79A7",
        "Total value" = "#D55E00"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Returned-label value" = "dashed",
        "Response value" = "dotdash",
        "Total value" = "solid"
      )
    ) +
    labs(
      x = expression(y),
      y = "Normalized value",
      title = "(d) Decomposition of the proposed criterion"
    ) +
    theme_paper()
  
  fig <- (
    p1 + p2
  ) / (
    p3 + p4
  ) +
    plot_annotation(
      title = "Classification and response-information geometry",
      subtitle = paste0(
        "Strong informative-response regime; vertical line marks the true Bayes boundary b = ",
        format(TRUE_B, digits = 2)
      ),
      theme = theme(
        plot.title = element_text(
          face = "bold",
          size = 16
        ),
        plot.subtitle = element_text(
          color = "#555555",
          size = 11
        )
      )
    )
  
  ggsave(
    file.path(FIG_DIR, "Figure_1_geometry.pdf"),
    fig,
    width = 11.2,
    height = 8.4,
    device = cairo_pdf
  )
  
  ggsave(
    file.path(FIG_DIR, "Figure_1_geometry.png"),
    fig,
    width = 11.2,
    height = 8.4,
    dpi = 500,
    bg = "white"
  )
  
  geometry_data <- data.frame(
    y = ygrid,
    tau = cc$tau,
    response_probability = cc$r,
    raw_label_information = raw_label,
    raw_response_information = raw_response,
    psi_ordinary = psi_ord,
    psi_discounted = psi_dis,
    psi_label_component = psi_L,
    psi_response_component = psi_R,
    psi_response_aware = psi_full
  )
  
  write.csv(
    geometry_data,
    file.path(OUT_DIR, "geometry_curves.csv"),
    row.names = FALSE
  )
  
  invisible(fig)
}

# ------------------------------------------------------------
# 15. Run main Monte Carlo studies
# ------------------------------------------------------------

message("\nCreating analytical geometry figure...")
make_geometry_figure()

message("\nStarting revised Monte Carlo simulation...")
message("DEBUG = ", DEBUG)
message("N_REP = ", N_REP)
message("Studies = ", paste(STUDY_MODES, collapse = ", "))

all_results <- list()
all_traces <- list()

counter <- 0L
total_jobs <- nrow(SCENARIOS) * N_REP

for (s in seq_len(nrow(SCENARIOS))) {
  scenario_row <- SCENARIOS[s, ]
  
  message(
    "\nScenario: ",
    scenario_row$scenario,
    "   eta0 = ",
    round(scenario_row$eta0, 4),
    "   eta1 = ",
    scenario_row$eta1
  )
  
  for (r in seq_len(N_REP)) {
    counter <- counter + 1L
    
    if (
      r == 1L ||
      r %% 5L == 0L ||
      r == N_REP
    ) {
      message(
        "  replication ",
        r,
        "/",
        N_REP,
        "   [overall ",
        counter,
        "/",
        total_jobs,
        "]"
      )
    }
    
    obj <- run_replication(
      r,
      scenario_row
    )
    
    key <- paste(
      scenario_row$scenario,
      r,
      sep = "_"
    )
    
    all_results[[key]] <- obj$results
    
    if (!is.null(obj$trace)) {
      all_traces[[key]] <- obj$trace
    }
    
    # Progressive save after every replication.
    current_results <-
      do.call(rbind, all_results)
    
    write.csv(
      current_results,
      file.path(
        OUT_DIR,
        "simulation_raw_results.csv"
      ),
      row.names = FALSE
    )
    
    if (length(all_traces) > 0L) {
      current_trace <-
        do.call(rbind, all_traces)
      
      write.csv(
        current_trace,
        file.path(
          OUT_DIR,
          "query_trace.csv"
        ),
        row.names = FALSE
      )
    }
  }
}

RESULTS <- do.call(rbind, all_results)
rownames(RESULTS) <- NULL

TRACE <-
  if (length(all_traces) > 0L) {
    do.call(rbind, all_traces)
  } else {
    NULL
  }

# ------------------------------------------------------------
# 16. Summary functions
# ------------------------------------------------------------

mean_se <- function(x) {
  x <- x[is.finite(x)]
  
  c(
    mean = mean(x),
    se = sd(x) / sqrt(length(x))
  )
}

summarise_metric <- function(dat, metric) {
  split_id <- interaction(
    dat$study,
    dat$scenario,
    dat$method,
    dat$budget,
    drop = TRUE,
    lex.order = TRUE
  )
  
  blocks <- lapply(
    split(dat, split_id),
    function(dd) {
      tmp <- mean_se(dd[[metric]])
      
      data.frame(
        study = dd$study[1],
        scenario = dd$scenario[1],
        method = dd$method[1],
        budget = dd$budget[1],
        metric = metric,
        mean = unname(tmp["mean"]),
        se = unname(tmp["se"]),
        n = sum(is.finite(dd[[metric]]))
      )
    }
  )
  
  do.call(rbind, blocks)
}

metrics_to_summarise <- c(
  "risk",
  "excess_risk",
  "boundary_abs_error",
  "n_returned",
  "response_rate",
  "mean_abs_distance_to_boundary",
  "mean_true_entropy_queried",
  "alpha_hat",
  "beta_hat",
  "eta0_hat",
  "eta1_hat"
)

SUMMARY_LONG <- do.call(
  rbind,
  lapply(
    metrics_to_summarise,
    function(m) {
      summarise_metric(
        RESULTS,
        m
      )
    }
  )
)

write.csv(
  SUMMARY_LONG,
  file.path(
    OUT_DIR,
    "simulation_summary_long.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 17. Convergence diagnostics
# ------------------------------------------------------------

CONVERGENCE_SUMMARY <- aggregate(
  I(convergence == 0) ~
    study + scenario + method + budget,
  data = RESULTS,
  FUN = mean
)

names(CONVERGENCE_SUMMARY)[5] <-
  "proportion_optim_converged"

write.csv(
  CONVERGENCE_SUMMARY,
  file.path(
    OUT_DIR,
    "convergence_summary.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 18. Oracle null-collapse diagnostic
# ------------------------------------------------------------

oracle_null <- RESULTS[
  RESULTS$study == "Oracle" &
    RESULTS$scenario == "Noninformative" &
    RESULTS$method %in%
    c("DiscountedRisk", "ResponseAware"),
]

null_disc <- oracle_null[
  oracle_null$method == "DiscountedRisk",
  c(
    "rep",
    "budget",
    "excess_risk",
    "boundary_hat",
    "n_returned"
  )
]

null_ra <- oracle_null[
  oracle_null$method == "ResponseAware",
  c(
    "rep",
    "budget",
    "excess_risk",
    "boundary_hat",
    "n_returned"
  )
]

names(null_disc)[3:5] <-
  paste0(
    names(null_disc)[3:5],
    "_discounted"
  )

names(null_ra)[3:5] <-
  paste0(
    names(null_ra)[3:5],
    "_response_aware"
  )

NULL_COLLAPSE <- merge(
  null_disc,
  null_ra,
  by = c("rep", "budget")
)

NULL_COLLAPSE$abs_excess_risk_difference <-
  abs(
    NULL_COLLAPSE$excess_risk_discounted -
      NULL_COLLAPSE$excess_risk_response_aware
  )

NULL_COLLAPSE$abs_boundary_difference <-
  abs(
    NULL_COLLAPSE$boundary_hat_discounted -
      NULL_COLLAPSE$boundary_hat_response_aware
  )

write.csv(
  NULL_COLLAPSE,
  file.path(
    OUT_DIR,
    "oracle_null_collapse_check.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 19. Paired response-aware vs discounted comparisons
# ------------------------------------------------------------

ra <- RESULTS[
  RESULTS$method == "ResponseAware",
  c(
    "study",
    "scenario",
    "rep",
    "budget",
    "excess_risk"
  )
]

dr <- RESULTS[
  RESULTS$method == "DiscountedRisk",
  c(
    "study",
    "scenario",
    "rep",
    "budget",
    "excess_risk"
  )
]

names(ra)[5] <- "response_aware"
names(dr)[5] <- "response_discounted"

paired <- merge(
  ra,
  dr,
  by = c(
    "study",
    "scenario",
    "rep",
    "budget"
  ),
  all = FALSE
)

paired$difference <-
  paired$response_aware -
  paired$response_discounted

paired_summary <- do.call(
  rbind,
  lapply(
    split(
      paired,
      interaction(
        paired$study,
        paired$scenario,
        paired$budget,
        drop = TRUE
      )
    ),
    function(dd) {
      d <- dd$difference
      
      data.frame(
        study = dd$study[1],
        scenario = dd$scenario[1],
        budget = dd$budget[1],
        mean_difference = mean(d),
        se_difference = sd(d) / sqrt(length(d)),
        proportion_response_aware_better =
          mean(d < 0),
        n = length(d)
      )
    }
  )
)

write.csv(
  paired,
  file.path(
    OUT_DIR,
    "paired_response_aware_vs_discounted_raw.csv"
  ),
  row.names = FALSE
)

write.csv(
  paired_summary,
  file.path(
    OUT_DIR,
    "paired_response_aware_vs_discounted.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# 20. Professional learning-curve figure
# ------------------------------------------------------------

plot_dat <- SUMMARY_LONG[
  SUMMARY_LONG$metric == "excess_risk",
]

plot_dat$method <- factor(
  plot_dat$method,
  levels = METHODS
)

plot_dat$scenario <- factor(
  plot_dat$scenario,
  levels = c(
    "Noninformative",
    "Moderate",
    "Strong"
  ),
  labels = SCENARIO_LABELS[
    c(
      "Noninformative",
      "Moderate",
      "Strong"
    )
  ]
)

plot_dat$study <- factor(
  plot_dat$study,
  levels = c(
    "Oracle",
    "Adaptive"
  )
)

p_learning <- ggplot(
  plot_dat,
  aes(
    x = budget,
    y = mean,
    color = method,
    linetype = method,
    shape = method,
    group = method
  )
) +
  geom_ribbon(
    aes(
      ymin = pmax(0, mean - 1.96 * se),
      ymax = mean + 1.96 * se,
      fill = method
    ),
    alpha = 0.075,
    color = NA,
    show.legend = FALSE
  ) +
  geom_line(
    linewidth = 0.95
  ) +
  geom_point(
    size = 2.5,
    stroke = 0.9
  ) +
  facet_grid(
    study ~ scenario,
    scales = "free_y"
  ) +
  scale_color_manual(
    values = METHOD_COLORS,
    labels = METHOD_LABELS
  ) +
  scale_fill_manual(
    values = METHOD_COLORS,
    labels = METHOD_LABELS
  ) +
  scale_linetype_manual(
    values = METHOD_LINETYPES,
    labels = METHOD_LABELS
  ) +
  scale_shape_manual(
    values = METHOD_SHAPES,
    labels = METHOD_LABELS
  ) +
  scale_x_continuous(
    breaks = BUDGETS
  ) +
  scale_y_continuous(
    labels = label_number(
      accuracy = 0.0001
    )
  ) +
  labs(
    x = "Number of oracle queries",
    y = "Mean excess classification risk",
    title = "Classification performance under oracle and adaptive acquisition",
    subtitle = "Lines show Monte Carlo means; shaded bands show approximate 95% Monte Carlo intervals"
  ) +
  theme_paper(base_size = 11.5) +
  theme(
    legend.position = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  )

ggsave(
  file.path(
    FIG_DIR,
    "Figure_2_learning_curves.pdf"
  ),
  p_learning,
  width = 13.2,
  height = 7.5,
  device = cairo_pdf
)

ggsave(
  file.path(
    FIG_DIR,
    "Figure_2_learning_curves.png"
  ),
  p_learning,
  width = 13.2,
  height = 7.5,
  dpi = 500,
  bg = "white"
)

# ------------------------------------------------------------
# 21. Query-location figure
# ------------------------------------------------------------

if (!is.null(TRACE) && nrow(TRACE) > 0L) {
  qdat <- TRACE[
    TRACE$scenario == "Strong" &
      !TRACE$pilot,
  ]
  
  qdat$method <- factor(
    qdat$method,
    levels = METHODS
  )
  
  qdat$study <- factor(
    qdat$study,
    levels = c(
      "Oracle",
      "Adaptive"
    )
  )
  
  p_query <- ggplot(
    qdat,
    aes(
      x = y,
      color = method,
      linetype = method
    )
  ) +
    geom_density(
      linewidth = 0.95,
      adjust = 1.05
    ) +
    geom_vline(
      xintercept = TRUE_B,
      linetype = "dotted",
      linewidth = 0.65,
      color = "#222222"
    ) +
    facet_wrap(
      ~ study,
      nrow = 1
    ) +
    coord_cartesian(
      xlim = c(-3, 3)
    ) +
    scale_color_manual(
      values = METHOD_COLORS,
      labels = METHOD_LABELS
    ) +
    scale_linetype_manual(
      values = METHOD_LINETYPES,
      labels = METHOD_LABELS
    ) +
    labs(
      x = expression(y),
      y = "Density of queried observations",
      title = "Where the acquisition rules query",
      subtitle = "Strong informative-response regime; dotted vertical line marks the true Bayes boundary"
    ) +
    theme_paper(base_size = 12)
  
  ggsave(
    file.path(
      FIG_DIR,
      "Figure_3_query_locations.pdf"
    ),
    p_query,
    width = 11.2,
    height = 5.4,
    device = cairo_pdf
  )
  
  ggsave(
    file.path(
      FIG_DIR,
      "Figure_3_query_locations.png"
    ),
    p_query,
    width = 11.2,
    height = 5.4,
    dpi = 500,
    bg = "white"
  )
}

# ------------------------------------------------------------
# 22. Oracle-vs-adaptive comparison for the proposed method
# ------------------------------------------------------------

ra_study <- RESULTS[
  RESULTS$method == "ResponseAware",
]

p_mode <- ggplot(
  ra_study,
  aes(
    x = factor(budget),
    y = excess_risk,
    color = study
  )
) +
  geom_boxplot(
    width = 0.62,
    outlier.alpha = 0.25
  ) +
  facet_wrap(
    ~ factor(
      scenario,
      levels = c(
        "Noninformative",
        "Moderate",
        "Strong"
      ),
      labels = SCENARIO_LABELS[
        c(
          "Noninformative",
          "Moderate",
          "Strong"
        )
      ]
    ),
    scales = "free_y"
  ) +
  scale_color_manual(
    values = c(
      Oracle = "#0072B2",
      Adaptive = "#D55E00"
    )
  ) +
  labs(
    x = "Number of oracle queries",
    y = "Excess classification risk",
    title = "Oracle versus adaptive response-aware acquisition"
  ) +
  theme_paper(base_size = 11.5)

ggsave(
  file.path(
    FIG_DIR,
    "Figure_4_oracle_vs_adaptive_response_aware.pdf"
  ),
  p_mode,
  width = 11,
  height = 4.8,
  device = cairo_pdf
)

ggsave(
  file.path(
    FIG_DIR,
    "Figure_4_oracle_vs_adaptive_response_aware.png"
  ),
  p_mode,
  width = 11,
  height = 4.8,
  dpi = 500,
  bg = "white"
)

# ------------------------------------------------------------
# 23. Optional pilot-size sensitivity study
# ------------------------------------------------------------

if (RUN_PILOT_SENSITIVITY) {
  message("\nStarting pilot-size sensitivity study...")
  
  scen <- SCENARIOS[
    SCENARIOS$scenario ==
      PILOT_SENSITIVITY_SCENARIO,
  ]
  
  pilot_results <- list()
  
  for (pilot_n in PILOT_SIZES_SENSITIVITY) {
    for (rep_id in seq_len(N_REP_PILOT_SENSITIVITY)) {
      set.seed(
        SEED +
          9000000L +
          10000L * pilot_n +
          rep_id
      )
      
      y_pool <- rnorm(N_POOL)
      
      tau_pool <- tau_fun(
        y_pool,
        TRUE_ALPHA,
        TRUE_BETA
      )
      
      z_pool <- rbinom(
        N_POOL,
        1,
        tau_pool
      )
      
      r_pool <- response_prob(
        y_pool,
        TRUE_ALPHA,
        TRUE_BETA,
        scen$eta0,
        scen$eta1
      )
      
      R_pool <- as.integer(
        runif(N_POOL) < r_pool
      )
      
      pilot_idx <- sample(
        seq_len(N_POOL),
        pilot_n
      )
      
      true_par <- c(
        TRUE_ALPHA,
        TRUE_BETA,
        scen$eta0,
        scen$eta1
      )
      
      obj <- run_method_path(
        study = "Adaptive",
        method = "ResponseAware",
        y_pool = y_pool,
        z_pool = z_pool,
        R_pool = R_pool,
        true_tau_pool = tau_pool,
        pilot_idx = pilot_idx,
        scenario = scen$scenario,
        rep_id = rep_id,
        true_par = true_par,
        save_trace = FALSE
      )
      
      dd <- obj$results
      dd <- dd[
        dd$budget ==
          PILOT_SENSITIVITY_BUDGET,
      ]
      
      dd$pilot_size <- pilot_n
      
      pilot_results[[
        paste(
          pilot_n,
          rep_id,
          sep = "_"
        )
      ]] <- dd
    }
  }
  
  PILOT_RESULTS <-
    do.call(
      rbind,
      pilot_results
    )
  
  write.csv(
    PILOT_RESULTS,
    file.path(
      OUT_DIR,
      "pilot_sensitivity_raw.csv"
    ),
    row.names = FALSE
  )
  
  p_pilot <- ggplot(
    PILOT_RESULTS,
    aes(
      x = factor(pilot_size),
      y = excess_risk
    )
  ) +
    geom_boxplot(
      fill = "#D55E00",
      alpha = 0.28,
      width = 0.6,
      outlier.alpha = 0.25
    ) +
    labs(
      x = "Pilot query size",
      y = "Excess classification risk",
      title = "Sensitivity of adaptive response-aware acquisition to pilot size",
      subtitle = paste0(
        PILOT_SENSITIVITY_SCENARIO,
        " response regime; total query budget = ",
        PILOT_SENSITIVITY_BUDGET
      )
    ) +
    theme_paper(base_size = 12)
  
  ggsave(
    file.path(
      FIG_DIR,
      "Figure_S_pilot_sensitivity.pdf"
    ),
    p_pilot,
    width = 7.4,
    height = 5.1,
    device = cairo_pdf
  )
  
  ggsave(
    file.path(
      FIG_DIR,
      "Figure_S_pilot_sensitivity.png"
    ),
    p_pilot,
    width = 7.4,
    height = 5.1,
    dpi = 500,
    bg = "white"
  )
}

# ------------------------------------------------------------
# 24. Session information
# ------------------------------------------------------------

sink(
  file.path(
    OUT_DIR,
    "sessionInfo.txt"
  )
)
print(sessionInfo())
sink()

message("\n======================================================")
message("Simulation finished.")
message("======================================================")
message("Results directory: ", normalizePath(OUT_DIR))
message("Bayes risk = ", signif(BAYES_RISK, 8))

message("\nBefore any final 500-replication run, inspect:")
message("  1. convergence_summary.csv")
message("  2. oracle_null_collapse_check.csv")
message("  3. paired_response_aware_vs_discounted.csv")
message("  4. Figure_1_geometry.png")
message("  5. Figure_2_learning_curves.png")
message("  6. Figure_3_query_locations.png")
message("  7. Figure_4_oracle_vs_adaptive_response_aware.png")

message("\nThe most important debug check is:")
message("In Oracle + Noninformative response,")
message("ResponseAware and DiscountedRisk should collapse to the same path")
message("up to numerical precision.")
