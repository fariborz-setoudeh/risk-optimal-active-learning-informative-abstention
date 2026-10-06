
# ============================================================

# ============================================================
# 0. Packages
# ============================================================

pkgs <- c(
  "readxl", "dplyr", "tidyr", "purrr", "stringr",
  "ggplot2", "forcats", "quanteda", "Matrix",
  "irlba", "readr", "tibble"
)

to_install <- pkgs[
  !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(forcats)
library(quanteda)
library(Matrix)
library(irlba)
library(readr)
library(tibble)

set.seed(20260906)

# ============================================================
# 1. Paths and experiment settings
# ============================================================

# Run R from the repository root.
MAIN_DIR <- normalizePath(
  ".", winslash = "/", mustWork = TRUE
)

if (!dir.exists(MAIN_DIR)) {
  stop("Project folder not found: ", MAIN_DIR)
}

ZIP_FILE <- file.path(
  MAIN_DIR,
  "al_rcta-main.zip"
)

REPO_DIR <- file.path(
  MAIN_DIR,
  "al_rcta-main"
)

if (!dir.exists(REPO_DIR)) {
  stopifnot(file.exists(ZIP_FILE))
  unzip(ZIP_FILE, exdir = MAIN_DIR)
}

CC_DIR <- file.path(
  REPO_DIR,
  "Data_ConsumerComplaints"
)

ANN_DIR <- file.path(
  CC_DIR,
  "Annotations"
)

MAIN_FILE <- file.path(
  CC_DIR,
  "Cleaned_Dataset_All.xlsx"
)

RUN_TAG <- format(Sys.time(), "%Y%m%d_%H%M%S")
OUT_BASE <- file.path(
  MAIN_DIR,
  paste0("Consumer_Complaints_Corrected_Curvature_", RUN_TAG)
)
OUT_DIR <- OUT_BASE
run_suffix <- 0L
while (dir.exists(OUT_DIR)) {
  run_suffix <- run_suffix + 1L
  OUT_DIR <- paste0(OUT_BASE, "_", run_suffix)
}

CHECKPOINT_DIR <- file.path(
  OUT_DIR,
  "checkpoints"
)

FIG_DIR <- file.path(
  OUT_DIR,
  "figures"
)

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  CHECKPOINT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  FIG_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# Main experimental settings
# ------------------------------------------------------------

N_REP <- 30

TEST_FRAC <- 0.30

PILOT_N <- 300

BATCH_SIZE <- 50

BUDGETS <- c(
  150, 300, 450, 600
)

MAX_BUDGET <- max(BUDGETS)

# Number of low-rank text features used by the classifier.
# With 6 classes, D_LATENT = 12 gives
# (12 + 1) * (6 - 1) = 65 classifier parameters.
D_LATENT <- 12

# Ridge penalties.
LAMBDA_BETA <- 1.0
LAMBDA_ETA  <- 0.5

# Tiny information regularization used only for stable inversion.
INFO_RIDGE_BETA <- 1e-3
INFO_RIDGE_ETA  <- 1e-3

# Risk-curvature kernel bandwidth lower bound.
H_MIN_BW <- 0.20

METHODS <- c(
  "Random",
  "Entropy",
  "OrdinaryRisk",
  "DiscountedRisk",
  "ResponseAware"
)

WORKERS <- paste0(
  "W",
  1:10
)

cat("\n============================================================\n")
cat("OUTPUT FOLDER\n")
cat("============================================================\n")
cat(OUT_DIR, "\n")

# Save configuration immediately.
config <- tibble(
  setting = c(
    "N_REP",
    "TEST_FRAC",
    "PILOT_N",
    "BATCH_SIZE",
    "BUDGETS",
    "D_LATENT",
    "LAMBDA_BETA",
    "LAMBDA_ETA",
    "INFO_RIDGE_BETA",
    "INFO_RIDGE_ETA",
    "H_MIN_BW"
  ),
  value = c(
    as.character(N_REP),
    as.character(TEST_FRAC),
    as.character(PILOT_N),
    as.character(BATCH_SIZE),
    paste(BUDGETS, collapse = ","),
    as.character(D_LATENT),
    as.character(LAMBDA_BETA),
    as.character(LAMBDA_ETA),
    as.character(INFO_RIDGE_BETA),
    as.character(INFO_RIDGE_ETA),
    as.character(H_MIN_BW)
  )
)

write_csv(
  config,
  file.path(
    OUT_DIR,
    "00_experiment_configuration.csv"
  )
)

write_csv(tibble(
  setting = c("curvature", "kernel_bandwidth", "representation", "intervals",
              "aggregate_multiplicity", "worker_multiplicity"),
  value = c("top-two kernel weighted by mean tied-class probability; trace normalized",
            "max(25th percentile of top-two absolute log gap, 0.20)",
            "transductive TF-IDF/SVD and scaling before splits; no test labels used",
            "pointwise 95% Student t intervals across 30 replications",
            "Holm across all 16 comparisons; also four budgets per comparator",
            "Holm across ten workers at maximum budget")
), file.path(OUT_DIR, "00C_protocol_notes.csv"))

# ============================================================
# 2. Read benchmark data
# ============================================================

main_raw <- read_excel(
  MAIN_FILE
)

names(main_raw)[1] <- "row_id"

required_main <- c(
  "row_id",
  "Product",
  "Consumer complaint narrative",
  "Complaint ID"
)

stopifnot(
  all(
    required_main %in% names(main_raw)
  )
)

benchmark <- main_raw %>%
  transmute(
    row_id =
      as.integer(row_id),
    
    truth =
      as.integer(Product),
    
    text =
      as.character(
        `Consumer complaint narrative`
      ),
    
    complaint_id =
      as.character(
        `Complaint ID`
      )
  ) %>%
  mutate(
    text_missing =
      is.na(text) |
      trimws(text) == ""
  )

stopifnot(
  !anyDuplicated(
    benchmark$row_id
  )
)

stopifnot(
  all(
    benchmark$truth %in% 1:6
  )
)

# ============================================================
# 3. Read human annotations robustly
# ============================================================

ann_files <- list.files(
  ANN_DIR,
  pattern = "^CC_Upwork_[0-9]+\\.xlsx$",
  full.names = TRUE
)

stopifnot(
  length(ann_files) == 10
)

read_cc_annotation <- function(path) {
  
  x <- read_excel(
    path,
    .name_repair = "unique_quiet"
  )
  
  # First column is the stable original benchmark row index.
  names(x)[1] <- "row_id"
  
  ann_candidates <- intersect(
    c(
      "Annotation",
      "Annotations"
    ),
    names(x)
  )
  
  if (length(ann_candidates) != 1) {
    stop(
      "Cannot identify exactly one annotation column in ",
      basename(path),
      "."
    )
  }
  
  worker_num <- str_extract(
    basename(path),
    "(?<=CC_Upwork_)\\d+"
  )
  
  worker_id <- paste0(
    "W",
    worker_num
  )
  
  out <- x %>%
    transmute(
      row_id =
        as.integer(row_id),
      
      annotation =
        as.integer(
          .data[[ann_candidates]]
        ),
      
      worker =
        worker_id
    )
  
  if (anyDuplicated(out$row_id)) {
    stop(
      "Duplicated row_id in ",
      basename(path)
    )
  }
  
  if (
    !all(
      out$annotation %in% 0:6,
      na.rm = TRUE
    )
  ) {
    stop(
      "Unexpected annotation code in ",
      basename(path)
    )
  }
  
  out
}

annotations <- map_dfr(
  ann_files,
  read_cc_annotation
)

worker_sets <- split(
  annotations$row_id,
  annotations$worker
)

common_ids <- Reduce(
  intersect,
  worker_sets
)

stopifnot(
  length(common_ids) == 3000
)

# ============================================================
# 4. Analysis sample
# ============================================================

# The single item with absent narrative is excluded from the
# acquisition experiment because Y is unavailable for text modelling.
sample_items <- benchmark %>%
  filter(
    row_id %in% common_ids,
    !text_missing
  ) %>%
  arrange(row_id)

stopifnot(
  nrow(sample_items) == 2999
)

# Worker-item response matrix.
oracle_long <- annotations %>%
  filter(
    row_id %in% sample_items$row_id
  ) %>%
  left_join(
    sample_items %>%
      select(
        row_id,
        truth
      ),
    by = "row_id"
  ) %>%
  mutate(
    R =
      as.integer(
        annotation != 0
      )
  )

stopifnot(
  nrow(oracle_long) ==
    10 * nrow(sample_items)
)

# We deliberately do NOT use `annotation` as the returned class.
# If R = 1, the benchmark truth is revealed in the experiment.

# ============================================================
# 5. Unsupervised TF-IDF + low-rank representation
# ============================================================

cat("\nBuilding TF-IDF representation...\n")

corp <- corpus(
  sample_items$text
)

toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks <- tokens_tolower(
  toks
)

toks <- tokens_remove(
  toks,
  stopwords("en")
)

X_dfm <- dfm(
  toks
)

X_dfm <- dfm_trim(
  X_dfm,
  min_docfreq = 5,
  docfreq_type = "count"
)

X_dfm <- dfm_tfidf(
  X_dfm
)

X_sparse <- as(
  X_dfm,
  "dgCMatrix"
)

cat(
  "TF-IDF dimensions:",
  nrow(X_sparse),
  "x",
  ncol(X_sparse),
  "\n"
)

# Pool-based active learning observes Y for all items.
# Therefore this unsupervised representation may use all feature vectors.
set.seed(20260906)

sv <- irlba(
  X_sparse,
  nv = D_LATENT,
  nu = D_LATENT
)

Z <- sv$u %*%
  diag(
    sv$d,
    nrow = D_LATENT,
    ncol = D_LATENT
  )

Z <- scale(
  Z
)

Z <- as.matrix(
  Z
)

colnames(Z) <- paste0(
  "LSA",
  seq_len(ncol(Z))
)

# Add classifier intercept.
X <- cbind(
  Intercept = 1,
  Z
)

# Standardized observable text length for response model.
log_length <- log1p(
  nchar(
    sample_items$text
  )
)

z_length <- as.numeric(
  scale(
    log_length
  )
)

truth <- sample_items$truth
row_ids <- sample_items$row_id

K <- length(
  sort(
    unique(truth)
  )
)

stopifnot(
  K == 6
)

P <- ncol(X)
Q_BETA <- P * (K - 1)
Q_ETA <- 3
Q_FULL <- Q_BETA + Q_ETA

write_csv(
  tibble(
    row_id = row_ids,
    truth = truth,
    z_length = z_length
  ) %>%
    bind_cols(
      as_tibble(
        Z
      )
    ),
  file.path(
    OUT_DIR,
    "01_low_rank_text_representation.csv"
  )
)

# ============================================================
# 6. Mathematical helper functions
# ============================================================

softmax_probs <- function(
    Xmat,
    beta_vec,
    K
) {
  
  p <- ncol(Xmat)
  
  B <- matrix(
    beta_vec,
    nrow = p,
    ncol = K - 1
  )
  
  eta_nonref <- Xmat %*% B
  
  eta_all <- cbind(
    eta_nonref,
    0
  )
  
  row_max <- apply(
    eta_all,
    1,
    max
  )
  
  eta_stable <- eta_all -
    row_max
  
  ex <- exp(
    eta_stable
  )
  
  ex /
    rowSums(ex)
}


normalized_entropy <- function(
    prob
) {
  
  eps <- 1e-12
  
  pp <- pmax(
    prob,
    eps
  )
  
  -rowSums(
    pp * log(pp)
  ) /
    log(
      ncol(prob)
    )
}


entropy_deta <- function(
    prob
) {
  
  # Derivative of normalized entropy with respect to the
  # K-1 non-reference logits.
  eps <- 1e-12
  
  pp <- pmax(
    prob,
    eps
  )
  
  slog <- rowSums(
    pp * log(pp)
  )
  
  p_nonref <- pp[
    ,
    seq_len(
      ncol(pp) - 1
    ),
    drop = FALSE
  ]
  
  out <- p_nonref *
    (
      slog -
        log(
          p_nonref
        )
    ) /
    log(
      ncol(pp)
    )
  
  out
}


joint_objective_gradient <- function(
    par,
    Xq,
    yq,
    Rq,
    zlenq,
    K,
    lambda_beta,
    lambda_eta
) {
  
  p <- ncol(Xq)
  q_beta <- p * (K - 1)
  
  beta_vec <- par[
    seq_len(
      q_beta
    )
  ]
  
  eta_par <- par[
    q_beta +
      seq_len(3)
  ]
  
  B <- matrix(
    beta_vec,
    nrow = p,
    ncol = K - 1
  )
  
  prob <- softmax_probs(
    Xq,
    beta_vec,
    K
  )
  
  eps <- 1e-12
  
  prob_safe <- pmax(
    prob,
    eps
  )
  
  # ----------------------------------------------------------
  # Returned-label likelihood
  # ----------------------------------------------------------
  
  idx <- seq_len(
    nrow(Xq)
  )
  
  label_ll <- sum(
    Rq *
      log(
        prob_safe[
          cbind(
            idx,
            yq
          )
        ]
      )
  )
  
  Y_nonref <- matrix(
    0,
    nrow = nrow(Xq),
    ncol = K - 1
  )
  
  for (j in seq_len(K - 1)) {
    Y_nonref[
      ,
      j
    ] <- as.numeric(
      yq == j
    )
  }
  
  E_label <- (
    prob[
      ,
      seq_len(K - 1),
      drop = FALSE
    ] -
      Y_nonref
  ) *
    Rq
  
  grad_B_label <- crossprod(
    Xq,
    E_label
  )
  
  # ----------------------------------------------------------
  # Response likelihood
  # logit r = eta0 + eta1 U_theta(Y) + eta2 length
  # ----------------------------------------------------------
  
  U <- normalized_entropy(
    prob
  )
  
  response_lp <-
    eta_par[1] +
    eta_par[2] * U +
    eta_par[3] * zlenq
  
  rhat <- plogis(
    response_lp
  )
  
  r_safe <- pmin(
    pmax(
      rhat,
      eps
    ),
    1 - eps
  )
  
  response_ll <- sum(
    Rq * log(r_safe) +
      (1 - Rq) * log(1 - r_safe)
  )
  
  dU_deta <- entropy_deta(
    prob
  )
  
  response_resid <- rhat - Rq
  
  # beta contribution from response likelihood.
  weighted_dU <- dU_deta *
    (
      response_resid *
        eta_par[2]
    )
  
  grad_B_response <- crossprod(
    Xq,
    weighted_dU
  )
  
  # eta contribution.
  Zr <- cbind(
    1,
    U,
    zlenq
  )
  
  grad_eta <- as.vector(
    crossprod(
      Zr,
      response_resid
    )
  )
  
  # ----------------------------------------------------------
  # Ridge penalties
  # ----------------------------------------------------------
  
  pen_B <- B
  
  # Do not penalize classifier intercepts.
  pen_B[1, ] <- 0
  
  penalty_beta <-
    0.5 *
    lambda_beta *
    sum(
      pen_B^2
    )
  
  grad_B_pen <-
    lambda_beta *
    pen_B
  
  pen_eta <- eta_par
  
  # Do not penalize response intercept.
  pen_eta[1] <- 0
  
  penalty_eta <-
    0.5 *
    lambda_eta *
    sum(
      pen_eta^2
    )
  
  grad_eta_pen <-
    lambda_eta *
    pen_eta
  
  value <-
    -label_ll -
    response_ll +
    penalty_beta +
    penalty_eta
  
  grad_B <-
    grad_B_label +
    grad_B_response +
    grad_B_pen
  
  grad_eta_total <-
    grad_eta +
    grad_eta_pen
  
  gradient <- c(
    as.vector(
      grad_B
    ),
    grad_eta_total
  )
  
  list(
    value = value,
    gradient = gradient
  )
}


fit_joint_model <- function(
    Xq,
    yq,
    Rq,
    zlenq,
    K,
    start = NULL,
    lambda_beta = LAMBDA_BETA,
    lambda_eta = LAMBDA_ETA
) {
  
  p <- ncol(Xq)
  q_beta <- p * (K - 1)
  q_full <- q_beta + 3
  
  if (is.null(start)) {
    
    # Mild deterministic initialization.
    start <- rep(
      0,
      q_full
    )
    
    # Initialize response intercept from observed response rate.
    rr <- mean(
      Rq
    )
    
    rr <- min(
      max(
        rr,
        0.02
      ),
      0.98
    )
    
    start[
      q_beta + 1
    ] <- qlogis(
      rr
    )
  }
  
  fn <- function(par) {
    joint_objective_gradient(
      par = par,
      Xq = Xq,
      yq = yq,
      Rq = Rq,
      zlenq = zlenq,
      K = K,
      lambda_beta = lambda_beta,
      lambda_eta = lambda_eta
    )$value
  }
  
  gr <- function(par) {
    joint_objective_gradient(
      par = par,
      Xq = Xq,
      yq = yq,
      Rq = Rq,
      zlenq = zlenq,
      K = K,
      lambda_beta = lambda_beta,
      lambda_eta = lambda_eta
    )$gradient
  }
  
  opt <- optim(
    par = start,
    fn = fn,
    gr = gr,
    method = "BFGS",
    control = list(
      maxit = 300,
      reltol = 1e-8
    )
  )
  
  list(
    par = opt$par,
    objective = opt$value,
    convergence = opt$convergence,
    counts = opt$counts
  )
}


predict_joint <- function(
    fit,
    Xnew,
    zlen_new,
    K
) {
  
  p <- ncol(Xnew)
  q_beta <- p * (K - 1)
  
  beta_vec <- fit$par[
    seq_len(
      q_beta
    )
  ]
  
  eta_par <- fit$par[
    q_beta +
      seq_len(3)
  ]
  
  prob <- softmax_probs(
    Xnew,
    beta_vec,
    K
  )
  
  U <- normalized_entropy(
    prob
  )
  
  rhat <- plogis(
    eta_par[1] +
      eta_par[2] * U +
      eta_par[3] * zlen_new
  )
  
  list(
    prob = prob,
    entropy = U,
    response_prob = rhat,
    beta_vec = beta_vec,
    eta_par = eta_par
  )
}


safe_inverse <- function(
    M,
    rel_floor = 1e-8
) {
  
  M <- (
    M +
      t(M)
  ) / 2
  
  ee <- eigen(
    M,
    symmetric = TRUE
  )
  
  vmax <- max(
    ee$values
  )
  
  floor_value <- max(
    rel_floor * vmax,
    1e-10
  )
  
  vals <- pmax(
    ee$values,
    floor_value
  )
  
  ee$vectors %*%
    diag(
      1 / vals,
      nrow = length(vals)
    ) %*%
    t(
      ee$vectors
    )
}


classifier_J_one <- function(
    x,
    prob
) {
  
  pn <- prob[
    seq_len(
      length(prob) - 1
    )
  ]
  
  C <- diag(
    pn
  ) -
    tcrossprod(
      pn
    )
  
  kronecker(
    C,
    tcrossprod(
      x
    )
  )
}


q_response_one <- function(
    x,
    prob,
    eta_par,
    zlen
) {
  
  pp <- pmax(
    prob,
    1e-12
  )
  
  slog <- sum(
    pp * log(pp)
  )
  
  pn <- pp[
    seq_len(
      length(pp) - 1
    )
  ]
  
  dU_deta <- pn *
    (
      slog -
        log(pn)
    ) /
    log(
      length(pp)
    )
  
  # Matrix p x (K-1), then column-major vector.
  dU_dbeta <- as.vector(
    outer(
      x,
      dU_deta
    )
  )
  
  q_theta <-
    eta_par[2] *
    dU_dbeta
  
  U <-
    -sum(
      pp * log(pp)
    ) /
    log(
      length(pp)
    )
  
  q_eta <- c(
    1,
    U,
    zlen
  )
  
  c(
    q_theta,
    q_eta
  )
}


estimate_H_risk <- function(
    Xall,
    prob_all,
    min_bw = H_MIN_BW
) {
  
  n <- nrow(Xall)
  p <- ncol(Xall)
  K <- ncol(prob_all)
  q_beta <- p * (K - 1)
  
  # Active pair = two largest fitted class probabilities.
  ord <- t(
    apply(
      prob_all,
      1,
      order,
      decreasing = TRUE
    )
  )
  
  top1 <- ord[, 1]
  top2 <- ord[, 2]
  
  eps <- 1e-12
  
  delta <- abs(
    log(
      pmax(
        prob_all[
          cbind(
            seq_len(n),
            top1
          )
        ],
        eps
      )
    ) -
      log(
        pmax(
          prob_all[
            cbind(
              seq_len(n),
              top2
            )
          ],
          eps
        )
      )
  )
  
  bw <- max(
    as.numeric(
      quantile(
        delta,
        probs = 0.25,
        na.rm = TRUE
      )
    ),
    min_bw
  )
  
  # Zero-one risk curvature includes the probability of the tied
  # classes, not just proximity to an active log-probability boundary.
  # The top-two gap is folded (nonnegative); the resulting common
  # factor 1/2 is immaterial after trace normalization.
  idx1 <- cbind(seq_len(n), top1)
  idx2 <- cbind(seq_len(n), top2)
  tie_probability <- 0.5 * (
    prob_all[idx1] + prob_all[idx2]
  )

  weight <- tie_probability * dnorm(delta / bw) / bw
  
  H <- matrix(
    0,
    nrow = q_beta,
    ncol = q_beta
  )
  
  for (i in seq_len(n)) {
    
    g <- rep(
      0,
      q_beta
    )
    
    k1 <- top1[i]
    k2 <- top2[i]
    
    x <- Xall[
      i,
    ]
    
    if (k1 < K) {
      
      ii <- (
        (k1 - 1) * p + 1
      ):(
        k1 * p
      )
      
      g[ii] <- g[ii] + x
    }
    
    if (k2 < K) {
      
      ii <- (
        (k2 - 1) * p + 1
      ):(
        k2 * p
      )
      
      g[ii] <- g[ii] - x
    }
    
    H <- H +
      weight[i] *
      tcrossprod(
        g
      )
  }
  
  H <- H /
    n
  
  # Normalize only for numerical scale; rankings are unaffected
  # by multiplying H by a common positive constant.
  trH <- sum(
    diag(H)
  )
  
  if (
    is.finite(trH) &&
    trH > 0
  ) {
    H <- H *
      (
        q_beta / trH
      )
  }
  
  H +
    diag(
      1e-8,
      q_beta
    )
}


build_label_information <- function(
    queried,
    fit_pred,
    Xall,
    R_worker,
    K
) {
  
  q_beta <- ncol(Xall) * (K - 1)
  
  M <- diag(
    INFO_RIDGE_BETA,
    q_beta
  )
  
  # Use actually returned labels for the accumulated
  # conventional classifier information.
  for (i in queried) {
    
    if (R_worker[i] == 1L) {
      
      M <- M +
        classifier_J_one(
          Xall[i, ],
          fit_pred$prob[i, ]
        )
    }
  }
  
  M
}


build_full_information <- function(
    queried,
    fit_pred,
    Xall,
    zlen_all,
    K
) {
  
  q_beta <- ncol(Xall) * (K - 1)
  q_full <- q_beta + 3
  
  ridge_diag <- c(
    rep(
      INFO_RIDGE_BETA,
      q_beta
    ),
    rep(
      INFO_RIDGE_ETA,
      3
    )
  )
  
  M <- diag(
    ridge_diag,
    q_full
  )
  
  for (i in queried) {
    
    pr <- fit_pred$prob[
      i,
    ]
    
    r <- fit_pred$response_prob[i]
    
    w <- r * (1 - r)
    
    J <- classifier_J_one(
      Xall[i, ],
      pr
    )
    
    J_aug <- matrix(
      0,
      nrow = q_full,
      ncol = q_full
    )
    
    J_aug[
      seq_len(q_beta),
      seq_len(q_beta)
    ] <- J
    
    qv <- q_response_one(
      x = Xall[i, ],
      prob = pr,
      eta_par = fit_pred$eta_par,
      zlen = zlen_all[i]
    )
    
    M <- M +
      r * J_aug +
      w * tcrossprod(qv)
  }
  
  M
}


risk_scores <- function(
    candidates,
    method,
    fit_pred,
    H,
    queried,
    Xall,
    zlen_all,
    R_worker,
    K
) {
  
  n_cand <- length(
    candidates
  )
  
  if (method == "Random") {
    return(
      runif(
        n_cand
      )
    )
  }
  
  if (method == "Entropy") {
    return(
      fit_pred$entropy[
        candidates
      ]
    )
  }
  
  q_beta <- ncol(Xall) * (K - 1)
  
  # ----------------------------------------------------------
  # Ordinary / response-discounted risk
  # ----------------------------------------------------------
  
  if (
    method %in%
    c(
      "OrdinaryRisk",
      "DiscountedRisk"
    )
  ) {
    
    M_beta <- build_label_information(
      queried = queried,
      fit_pred = fit_pred,
      Xall = Xall,
      R_worker = R_worker,
      K = K
    )
    
    Minv <- safe_inverse(
      M_beta
    )
    
    A <- Minv %*%
      H %*%
      Minv
    
    score <- numeric(
      n_cand
    )
    
    for (jj in seq_along(candidates)) {
      
      i <- candidates[jj]
      
      J <- classifier_J_one(
        Xall[i, ],
        fit_pred$prob[i, ]
      )
      
      value <- sum(
        A * t(J)
      )
      
      if (
        method ==
        "DiscountedRisk"
      ) {
        
        value <- fit_pred$response_prob[i] *
          value
      }
      
      score[jj] <- value
    }
    
    return(score)
  }
  
  # ----------------------------------------------------------
  # Full response-aware risk
  # ----------------------------------------------------------
  
  if (
    method ==
    "ResponseAware"
  ) {
    
    M_full <- build_full_information(
      queried = queried,
      fit_pred = fit_pred,
      Xall = Xall,
      zlen_all = zlen_all,
      K = K
    )
    
    Minv <- safe_inverse(
      M_full
    )
    
    q_full <- nrow(
      M_full
    )
    
    H_aug <- matrix(
      0,
      nrow = q_full,
      ncol = q_full
    )
    
    H_aug[
      seq_len(q_beta),
      seq_len(q_beta)
    ] <- H
    
    A <- Minv %*%
      H_aug %*%
      Minv
    
    score <- numeric(
      n_cand
    )
    
    for (jj in seq_along(candidates)) {
      
      i <- candidates[jj]
      
      pr <- fit_pred$prob[
        i,
      ]
      
      r <- fit_pred$response_prob[i]
      
      w <- r *
        (1 - r)
      
      J <- classifier_J_one(
        Xall[i, ],
        pr
      )
      
      # Label contribution only needs beta block.
      label_value <-
        r *
        sum(
          A[
            seq_len(q_beta),
            seq_len(q_beta)
          ] *
            t(J)
        )
      
      qv <- q_response_one(
        x = Xall[i, ],
        prob = pr,
        eta_par = fit_pred$eta_par,
        zlen = zlen_all[i]
      )
      
      response_value <-
        w *
        as.numeric(
          crossprod(
            qv,
            A %*% qv
          )
        )
      
      score[jj] <-
        label_value +
        response_value
    }
    
    return(score)
  }
  
  stop(
    "Unknown method: ",
    method
  )
}


stratified_test_indices <- function(
    y,
    frac,
    seed
) {
  
  set.seed(seed)
  
  idx <- seq_along(
    y
  )
  
  out <- integer(
    0
  )
  
  for (
    cl in sort(
      unique(y)
    )
  ) {
    
    ii <- idx[
      y == cl
    ]
    
    n_take <- floor(
      frac *
        length(ii)
    )
    
    out <- c(
      out,
      sample(
        ii,
        size = n_take,
        replace = FALSE
      )
    )
  }
  
  sort(
    out
  )
}


classification_metrics <- function(
    prob,
    truth_vec
) {
  
  pred <- max.col(
    prob,
    ties.method = "first"
  )
  
  err <- mean(
    pred != truth_vec
  )
  
  acc <- 1 - err
  
  eps <- 1e-12
  
  ll <- -mean(
    log(
      pmax(
        prob[
          cbind(
            seq_along(truth_vec),
            truth_vec
          )
        ],
        eps
      )
    )
  )
  
  c(
    error = err,
    accuracy = acc,
    logloss = ll
  )
}


fit_full_supervised_reference <- function(
    train_idx,
    test_idx,
    Xall,
    yall,
    zlen_all,
    K
) {
  
  # Reference classifier using all training-pool labels.
  # Response is set to 1 for every training item, so the
  # response part carries no useful variation. We fit only the
  # multinomial label likelihood through the same joint routine
  # with a tiny artificial response variation avoided by a
  # dedicated label-only optimizer below.
  
  Xtr <- Xall[
    train_idx,
    ,
    drop = FALSE
  ]
  
  ytr <- yall[
    train_idx
  ]
  
  p <- ncol(
    Xtr
  )
  
  q_beta <- p * (K - 1)
  
  fn <- function(beta_vec) {
    
    prob <- softmax_probs(
      Xtr,
      beta_vec,
      K
    )
    
    eps <- 1e-12
    
    ll <- sum(
      log(
        pmax(
          prob[
            cbind(
              seq_along(ytr),
              ytr
            )
          ],
          eps
        )
      )
    )
    
    B <- matrix(
      beta_vec,
      nrow = p,
      ncol = K - 1
    )
    
    pen <- B
    pen[1, ] <- 0
    
    -ll +
      0.5 *
      LAMBDA_BETA *
      sum(
        pen^2
      )
  }
  
  gr <- function(beta_vec) {
    
    prob <- softmax_probs(
      Xtr,
      beta_vec,
      K
    )
    
    Yn <- matrix(
      0,
      nrow = nrow(Xtr),
      ncol = K - 1
    )
    
    for (j in seq_len(K - 1)) {
      Yn[, j] <- as.numeric(
        ytr == j
      )
    }
    
    E <- prob[
      ,
      seq_len(K - 1),
      drop = FALSE
    ] - Yn
    
    G <- crossprod(
      Xtr,
      E
    )
    
    B <- matrix(
      beta_vec,
      nrow = p,
      ncol = K - 1
    )
    
    pen <- B
    pen[1, ] <- 0
    
    G <- G +
      LAMBDA_BETA *
      pen
    
    as.vector(
      G
    )
  }
  
  opt <- optim(
    par = rep(
      0,
      q_beta
    ),
    fn = fn,
    gr = gr,
    method = "BFGS",
    control = list(
      maxit = 300,
      reltol = 1e-8
    )
  )
  
  prob_test <- softmax_probs(
    Xall[
      test_idx,
      ,
      drop = FALSE
    ],
    opt$par,
    K
  )
  
  classification_metrics(
    prob_test,
    yall[
      test_idx
    ]
  )
}

# ============================================================
# 7. Single method trajectory
# ============================================================

run_method <- function(
    method,
    worker,
    rep_id,
    train_idx,
    test_idx,
    pilot_idx,
    R_worker,
    full_reference_metrics
) {
  
  queried <- pilot_idx
  
  available <- setdiff(
    train_idx,
    queried
  )
  
  # Fit shared joint model from the common pilot.
  fit <- fit_joint_model(
    Xq = X[
      queried,
      ,
      drop = FALSE
    ],
    yq = truth[
      queried
    ],
    Rq = R_worker[
      queried
    ],
    zlenq = z_length[
      queried
    ],
    K = K
  )
  
  postpilot_queried <- integer(
    0
  )
  
  trace_list <- list()
  
  result_list <- list()
  
  # Evaluate pilot-only model.
  pred_all <- predict_joint(
    fit,
    X,
    z_length,
    K
  )
  
  pilot_test_metrics <- classification_metrics(
    pred_all$prob[
      test_idx,
      ,
      drop = FALSE
    ],
    truth[
      test_idx
    ]
  )
  
  result_list[["B0"]] <- tibble(
    rep = rep_id,
    worker = worker,
    method = method,
    budget = 0L,
    total_queries_including_pilot = length(queried),
    postpilot_queries = 0L,
    postpilot_responses = 0L,
    postpilot_response_rate = NA_real_,
    test_error = unname(
      pilot_test_metrics["error"]
    ),
    test_accuracy = unname(
      pilot_test_metrics["accuracy"]
    ),
    test_logloss = unname(
      pilot_test_metrics["logloss"]
    ),
    full_supervised_error = unname(
      full_reference_metrics["error"]
    ),
    full_supervised_accuracy = unname(
      full_reference_metrics["accuracy"]
    ),
    fit_convergence = fit$convergence,
    eta0 = pred_all$eta_par[1],
    eta_entropy = pred_all$eta_par[2],
    eta_length = pred_all$eta_par[3]
  )
  
  n_batches <- ceiling(
    MAX_BUDGET /
      BATCH_SIZE
  )
  
  for (
    batch_id in seq_len(
      n_batches
    )
  ) {
    
    # Current fitted quantities over all items.
    pred_all <- predict_joint(
      fit,
      X,
      z_length,
      K
    )
    
    # Estimate multiclass local risk curvature from observed X
    # and current classifier.
    H <- estimate_H_risk(
      Xall = X[
        train_idx,
        ,
        drop = FALSE
      ],
      prob_all = pred_all$prob[
        train_idx,
        ,
        drop = FALSE
      ]
    )
    
    candidates <- available
    
    scores <- risk_scores(
      candidates = candidates,
      method = method,
      fit_pred = pred_all,
      H = H,
      queried = queried,
      Xall = X,
      zlen_all = z_length,
      R_worker = R_worker,
      K = K
    )
    
    n_select <- min(
      BATCH_SIZE,
      length(candidates),
      MAX_BUDGET -
        length(postpilot_queried)
    )
    
    if (n_select <= 0) {
      break
    }
    
    if (method == "Random") {
      
      # Random scores are already generated, so highest values
      # give a random sample without replacement.
      selected_local <- order(
        scores,
        decreasing = TRUE
      )[
        seq_len(
          n_select
        )
      ]
      
    } else {
      
      # Deterministic tie-breaking by row index.
      selected_local <- order(
        -scores,
        candidates
      )[
        seq_len(
          n_select
        )
      ]
    }
    
    selected <- candidates[
      selected_local
    ]
    
    selected_score <- scores[
      selected_local
    ]
    
    trace_list[[
      paste0(
        "batch",
        batch_id
      )
    ]] <- tibble(
      rep = rep_id,
      worker = worker,
      method = method,
      batch = batch_id,
      budget_after_batch =
        length(postpilot_queried) +
        n_select,
      item_index = selected,
      row_id = row_ids[
        selected
      ],
      truth = truth[
        selected
      ],
      R = R_worker[
        selected
      ],
      score = selected_score,
      entropy_at_query =
        pred_all$entropy[
          selected
        ],
      response_prob_hat_at_query =
        pred_all$response_prob[
          selected
        ],
      z_length =
        z_length[
          selected
        ]
    )
    
    queried <- c(
      queried,
      selected
    )
    
    postpilot_queried <- c(
      postpilot_queried,
      selected
    )
    
    available <- setdiff(
      available,
      selected
    )
    
    # Refit joint observed-data likelihood.
    fit <- fit_joint_model(
      Xq = X[
        queried,
        ,
        drop = FALSE
      ],
      yq = truth[
        queried
      ],
      Rq = R_worker[
        queried
      ],
      zlenq = z_length[
        queried
      ],
      K = K,
      start = fit$par
    )
    
    current_budget <- length(
      postpilot_queried
    )
    
    if (
      current_budget %in%
      BUDGETS
    ) {
      
      pred_now <- predict_joint(
        fit,
        X,
        z_length,
        K
      )
      
      test_metrics <- classification_metrics(
        pred_now$prob[
          test_idx,
          ,
          drop = FALSE
        ],
        truth[
          test_idx
        ]
      )
      
      post_R <- R_worker[
        postpilot_queried
      ]
      
      result_list[[
        paste0(
          "B",
          current_budget
        )
      ]] <- tibble(
        rep = rep_id,
        worker = worker,
        method = method,
        budget = current_budget,
        total_queries_including_pilot =
          length(queried),
        postpilot_queries =
          length(
            postpilot_queried
          ),
        postpilot_responses =
          sum(
            post_R
          ),
        postpilot_response_rate =
          mean(
            post_R
          ),
        test_error = unname(
          test_metrics["error"]
        ),
        test_accuracy = unname(
          test_metrics["accuracy"]
        ),
        test_logloss = unname(
          test_metrics["logloss"]
        ),
        full_supervised_error = unname(
          full_reference_metrics["error"]
        ),
        full_supervised_accuracy = unname(
          full_reference_metrics["accuracy"]
        ),
        fit_convergence = fit$convergence,
        eta0 = pred_now$eta_par[1],
        eta_entropy = pred_now$eta_par[2],
        eta_length = pred_now$eta_par[3]
      )
    }
  }
  
  list(
    results = bind_rows(
      result_list
    ),
    trace = bind_rows(
      trace_list
    )
  )
}

# ============================================================
# 7B. Preflight numerical identities (no data-driven tuning)
# ============================================================

run_preflight_checks <- function() {
  K0 <- 3L
  p0 <- 3L
  X0 <- cbind(1, seq(-1, 1, length.out = 8), cos(seq_len(8)))
  y0 <- rep(1:3, length.out = 8)
  R0 <- c(1, 0, 1, 1, 0, 1, 0, 1)
  len0 <- seq(-0.7, 0.7, length.out = 8)
  beta0 <- seq(-0.25, 0.35, length.out = p0 * (K0 - 1L))
  eta0 <- c(0.4, -1.2, 0.3)
  par0 <- c(beta0, eta0)
  obj <- function(v) joint_objective_gradient(
    v, X0, y0, R0, len0, K0, LAMBDA_BETA, LAMBDA_ETA
  )
  eps0 <- 1e-6
  analytic <- obj(par0)$gradient
  finite_diff <- vapply(seq_along(par0), function(j) {
    plus <- minus <- par0
    plus[j] <- plus[j] + eps0
    minus[j] <- minus[j] - eps0
    (obj(plus)$value - obj(minus)$value) / (2 * eps0)
  }, numeric(1))
  grad_error <- max(abs(analytic - finite_diff))

  prob0 <- softmax_probs(X0, beta0, K0)
  ent0 <- normalized_entropy(prob0)
  q0 <- q_response_one(X0[1, ], prob0[1, ], eta0, len0[1])
  response_logit <- function(v) {
    pp <- softmax_probs(X0[1, , drop = FALSE], v[seq_along(beta0)], K0)
    ev <- v[length(beta0) + seq_len(3)]
    as.numeric(ev[1] + ev[2] * normalized_entropy(pp) + ev[3] * len0[1])
  }
  q_fd <- vapply(seq_along(par0), function(j) {
    plus <- minus <- par0
    plus[j] <- plus[j] + eps0
    minus[j] <- minus[j] - eps0
    (response_logit(plus) - response_logit(minus)) / (2 * eps0)
  }, numeric(1))
  response_gradient_error <- max(abs(q0 - q_fd))

  fp <- list(prob = prob0, response_prob = plogis(
    eta0[1] + eta0[2] * ent0 + eta0[3] * len0
  ), eta_par = eta0)
  M <- build_full_information(seq_len(nrow(X0)), fp, X0, len0, K0)
  qb <- length(beta0)
  ib <- seq_len(qb)
  ie <- qb + seq_len(3)
  H0 <- diag(seq(0.5, 1.5, length.out = qb))
  Haug <- matrix(0, nrow(M), ncol(M)); Haug[ib, ib] <- H0
  Minv <- solve(M)
  A0 <- Minv %*% Haug %*% Minv
  J0 <- classifier_J_one(X0[1, ], prob0[1, ])
  r0 <- fp$response_prob[1]
  w0 <- r0 * (1 - r0)
  full_score <- r0 * sum(A0[ib, ib] * t(J0)) +
    w0 * as.numeric(crossprod(q0, A0 %*% q0))
  B0 <- M[ib, ie] %*% solve(M[ie, ie])
  E0 <- M[ib, ib] - B0 %*% M[ie, ib]
  Einv <- solve(E0)
  T0 <- Einv %*% H0 %*% Einv
  q_eff <- q0[ib] - as.vector(B0 %*% q0[ie])
  efficient_score <- r0 * sum(T0 * t(J0)) +
    w0 * as.numeric(crossprod(q_eff, T0 %*% q_eff))
  score_error <- abs(full_score - efficient_score) / max(1, abs(full_score))

  # Two exact top-class ties with different common probabilities.
  # Their orthogonal feature directions must receive curvature weights
  # in the ratio 0.45/0.34, not equal weights. This detects omission of
  # the class-probability factor independently of kernel normalization.
  Htie <- estimate_H_risk(
    diag(2), rbind(c(0.45, 0.45, 0.10), c(0.34, 0.34, 0.32))
  )
  tie_ratio <- (Htie[1, 1] - 1e-8) / (Htie[2, 2] - 1e-8)
  curvature_error <- abs(tie_ratio - 0.45 / 0.34)
  checks <- tibble(
    check = c("joint_likelihood_gradient", "response_score_gradient",
              "full_vs_schur_score", "tied_class_probability_ratio"),
    error = c(grad_error, response_gradient_error, score_error, curvature_error),
    tolerance = c(1e-5, 1e-6, 1e-8, 1e-10)
  ) %>% mutate(passed = is.finite(error) & error <= tolerance)
  write_csv(checks, file.path(OUT_DIR, "00B_preflight_checks.csv"))
  print(checks)
  if (!all(checks$passed)) stop("Numerical preflight failed; acquisition not started.")
  invisible(checks)
}

run_preflight_checks()

# ============================================================
# 8. Full repeated experiment with checkpoints in the fresh run folder
# ============================================================

# ------------------------------------------------------------
# Pre-run validation of the ten real human oracles
# ------------------------------------------------------------

oracle_rate_check <- oracle_long %>%
  group_by(worker) %>%
  summarise(
    n_items = n(),
    response_rate = mean(R),
    abstention_rate = mean(1 - R),
    .groups = "drop"
  ) %>%
  arrange(
    match(worker, WORKERS)
  )

cat("\n============================================================\n")
cat("WORKER RESPONSE-RATE VALIDATION\n")
cat("============================================================\n")
print(oracle_rate_check, n = Inf)

write_csv(
  oracle_rate_check,
  file.path(
    OUT_DIR,
    "01B_worker_response_rate_validation.csv"
  )
)

stopifnot(
  nrow(oracle_rate_check) == 10
)

stopifnot(
  all(oracle_rate_check$n_items == nrow(sample_items))
)

# This guards specifically against the previous indexing bug:
# the ten workers must not collapse to one identical response rate.
stopifnot(
  dplyr::n_distinct(
    round(
      oracle_rate_check$response_rate,
      6
    )
  ) > 1
)

cat("\nWorker validation passed: distinct real response patterns are being used.\n")

cat("\n============================================================\n")
cat("STARTING ACQUISITION EXPERIMENT\n")
cat("============================================================\n")

for (
  rep_id in seq_len(
    N_REP
  )
) {
  
  cat(
    "\n--------------------------------------------\n"
  )
  
  cat(
    "Replication",
    rep_id,
    "of",
    N_REP,
    "\n"
  )
  
  split_seed <-
    100000 +
    rep_id
  
  test_idx <- stratified_test_indices(
    y = truth,
    frac = TEST_FRAC,
    seed = split_seed
  )
  
  train_idx <- setdiff(
    seq_along(truth),
    test_idx
  )
  
  set.seed(
    200000 +
      rep_id
  )
  
  pilot_idx <- sample(
    train_idx,
    size = PILOT_N,
    replace = FALSE
  )
  
  # Same fully supervised benchmark for all workers and methods
  # in this replication.
  full_reference_metrics <-
    fit_full_supervised_reference(
      train_idx = train_idx,
      test_idx = test_idx,
      Xall = X,
      yall = truth,
      zlen_all = z_length,
      K = K
    )
  
  cat(
    "Fully supervised test error:",
    round(
      full_reference_metrics["error"],
      4
    ),
    "\n"
  )
  
  for (
    worker in WORKERS
  ) {
    
    # --------------------------------------------------------
    # CRITICAL: use explicit base-R indexing here.
    #
    # Do NOT write
    #   filter(.data$worker == worker)
    # because inside dplyr the RHS `worker` can resolve to the
    # column itself, turning the condition into worker == worker.
    # --------------------------------------------------------
    
    worker_map <- oracle_long[
      oracle_long$worker == worker,
      c("row_id", "R")
    ]
    
    # Exactly one response record per analysis item for this worker.
    stopifnot(
      nrow(worker_map) == length(row_ids)
    )
    
    stopifnot(
      !anyDuplicated(worker_map$row_id)
    )
    
    R_worker <- worker_map$R[
      match(
        row_ids,
        worker_map$row_id
      )
    ]
    
    stopifnot(
      length(R_worker) == length(row_ids)
    )
    
    stopifnot(
      !anyNA(R_worker)
    )
    
    # Cross-check against the worker-specific response rate obtained
    # directly from the long annotation table.
    direct_worker_rate <- mean(
      oracle_long$R[
        oracle_long$worker == worker
      ]
    )
    
    stopifnot(
      isTRUE(
        all.equal(
          mean(R_worker),
          direct_worker_rate,
          tolerance = 1e-12
        )
      )
    )
    
    cat(
      " Worker",
      worker,
      "overall response rate =",
      sprintf("%.4f", mean(R_worker)),
      "\n"
    )
    
    for (
      method in METHODS
    ) {
      
      run_stem <- sprintf(
        "rep_%03d_%s_%s",
        rep_id,
        worker,
        method
      )
      
      result_file <- file.path(
        CHECKPOINT_DIR,
        paste0(
          run_stem,
          "_results.csv"
        )
      )
      
      trace_file <- file.path(
        CHECKPOINT_DIR,
        paste0(
          run_stem,
          "_trace.csv"
        )
      )
      
      if (
        file.exists(result_file) &&
        file.exists(trace_file)
      ) {
        
        cat(
          "   ",
          method,
          ": checkpoint exists, skipping.\n"
        )
        
        next
      }
      
      cat(
        "   Running",
        method,
        "...\n"
      )
      
      set.seed(
        300000 +
          rep_id * 100 +
          match(
            worker,
            WORKERS
          ) * 10 +
          match(
            method,
            METHODS
          )
      )
      
      ans <- tryCatch(
        run_method(
          method = method,
          worker = worker,
          rep_id = rep_id,
          train_idx = train_idx,
          test_idx = test_idx,
          pilot_idx = pilot_idx,
          R_worker = R_worker,
          full_reference_metrics =
            full_reference_metrics
        ),
        error = function(e) {
          
          cat(
            "ERROR in",
            run_stem,
            ":",
            conditionMessage(e),
            "\n"
          )
          
          NULL
        }
      )
      
      if (
        !is.null(ans)
      ) {
        
        write_csv(
          ans$results,
          result_file
        )
        
        write_csv(
          ans$trace,
          trace_file
        )
      }
    }
  }
}

# ============================================================
# 8B. Checkpoint-completeness diagnostic
# ============================================================

expected_runs <- N_REP * length(WORKERS) * length(METHODS)

completed_result_files <- list.files(
  CHECKPOINT_DIR,
  pattern = "_results\\.csv$",
  full.names = TRUE
)

completed_trace_files <- list.files(
  CHECKPOINT_DIR,
  pattern = "_trace\\.csv$",
  full.names = TRUE
)

cat("\n============================================================\n")
cat("CHECKPOINT COMPLETENESS\n")
cat("============================================================\n")
cat("Expected runs:", expected_runs, "\n")
cat("Completed result files:", length(completed_result_files), "\n")
cat("Completed trace files:", length(completed_trace_files), "\n")

if (
  length(completed_result_files) != expected_runs ||
  length(completed_trace_files) != expected_runs
) {
  stop(
    "Experiment incomplete; no aggregate summaries will be produced. ",
    "Keep this output folder for diagnosis. Expected ", expected_runs,
    " runs, found ", length(completed_result_files), " result files and ",
    length(completed_trace_files), " trace files."
  )
}

# ============================================================
# 9. Combine checkpoints
# ============================================================

result_files <- list.files(
  CHECKPOINT_DIR,
  pattern = "_results\\.csv$",
  full.names = TRUE
)

trace_files <- list.files(
  CHECKPOINT_DIR,
  pattern = "_trace\\.csv$",
  full.names = TRUE
)

if (length(result_files) == 0) {
  stop(
    "No result checkpoints were created."
  )
}

all_results <- map_dfr(
  result_files,
  read_csv,
  show_col_types = FALSE
)

all_trace <- map_dfr(
  trace_files,
  read_csv,
  show_col_types = FALSE
)

# Do not average over missing, duplicated, or malformed result cells.
expected_cells <- tidyr::expand_grid(
  rep = seq_len(N_REP), worker = WORKERS, method = METHODS,
  budget = c(0L, BUDGETS)
)
key_columns <- c("rep", "worker", "method", "budget")
actual_cells <- all_results %>% select(all_of(key_columns))
if (anyDuplicated(actual_cells) ||
    nrow(anti_join(expected_cells, actual_cells, by = key_columns)) > 0L ||
    nrow(anti_join(actual_cells, expected_cells, by = key_columns)) > 0L ||
    any(!is.finite(all_results$test_error)) ||
    any(all_results$total_queries_including_pilot != PILOT_N + all_results$budget) ||
    any(all_results$postpilot_queries != all_results$budget)) {
  stop("Raw result protocol/completeness check failed; summaries stopped.")
}
if (any(all_results$fit_convergence != 0L)) {
  warning("Some evaluated fits have nonzero optimizer codes; inspect raw results.")
}

method_levels <- METHODS

all_results <- all_results %>%
  mutate(
    method = factor(
      method,
      levels = method_levels
    ),
    worker = factor(
      worker,
      levels = WORKERS
    )
  )

all_trace <- all_trace %>%
  mutate(
    method = factor(
      method,
      levels = method_levels
    ),
    worker = factor(
      worker,
      levels = WORKERS
    )
  )

write_csv(
  all_results,
  file.path(
    OUT_DIR,
    "02_all_raw_acquisition_results.csv"
  )
)

write_csv(
  all_trace,
  file.path(
    OUT_DIR,
    "03_all_query_trace.csv"
  )
)

# ============================================================
# 10. Main summaries
# ============================================================

# ------------------------------------------------------------
# 10A. Worker-specific summary
# ------------------------------------------------------------

worker_summary <- all_results %>%
  filter(
    budget > 0
  ) %>%
  group_by(
    worker,
    method,
    budget
  ) %>%
  summarise(
    n_rep = n(),
    mean_test_error =
      mean(
        test_error
      ),
    se_test_error =
      sd(
        test_error
      ) /
      sqrt(
        n()
      ),
    mean_test_accuracy =
      mean(
        test_accuracy
      ),
    mean_logloss =
      mean(
        test_logloss
      ),
    mean_response_rate =
      mean(
        postpilot_response_rate
      ),
    mean_returned_labels =
      mean(
        postpilot_responses
      ),
    convergence_rate =
      mean(
        fit_convergence == 0
      ),
    mean_eta_entropy =
      mean(
        eta_entropy
      ),
    .groups = "drop"
  )

write_csv(
  worker_summary,
  file.path(
    OUT_DIR,
    "04_worker_specific_summary.csv"
  )
)

# ------------------------------------------------------------
# 10B. Primary aggregate:
# first average over the 10 workers within replication,
# then compute uncertainty across independent replications.
# ------------------------------------------------------------

rep_worker_average <- all_results %>%
  filter(
    budget > 0
  ) %>%
  group_by(
    rep,
    method,
    budget
  ) %>%
  summarise(
    test_error =
      mean(
        test_error
      ),
    test_accuracy =
      mean(
        test_accuracy
      ),
    test_logloss =
      mean(
        test_logloss
      ),
    response_rate =
      mean(
        postpilot_response_rate
      ),
    returned_labels =
      mean(
        postpilot_responses
      ),
    .groups = "drop"
  )

aggregate_summary <- rep_worker_average %>%
  group_by(
    method,
    budget
  ) %>%
  summarise(
    n_rep = n(),
    mean_test_error =
      mean(
        test_error
      ),
    se_test_error =
      sd(
        test_error
      ) /
      sqrt(
        n()
      ),
    ci_low =
      mean_test_error -
      qt(0.975, df = n() - 1L) *
      se_test_error,
    ci_high =
      mean_test_error +
      qt(0.975, df = n() - 1L) *
      se_test_error,
    mean_test_accuracy =
      mean(
        test_accuracy
      ),
    mean_logloss =
      mean(
        test_logloss
      ),
    mean_response_rate =
      mean(
        response_rate
      ),
    mean_returned_labels =
      mean(
        returned_labels
      ),
    .groups = "drop"
  )

write_csv(
  aggregate_summary,
  file.path(
    OUT_DIR,
    "05_primary_aggregate_summary.csv"
  )
)

# ============================================================
# 11. Paired ResponseAware versus comparators
# ============================================================

paired_wide <- rep_worker_average %>%
  select(
    rep,
    method,
    budget,
    test_error
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = test_error
  )

paired_comparisons <- map_dfr(
  setdiff(
    METHODS,
    "ResponseAware"
  ),
  function(comp) {
    
    paired_wide %>%
      transmute(
        rep = rep,
        budget = budget,
        comparator = comp,
        difference =
          ResponseAware -
          .data[[comp]]
      )
  }
) %>%
  group_by(
    comparator,
    budget
  ) %>%
  summarise(
    n_rep = n(),
    mean_difference =
      mean(
        difference
      ),
    se_difference =
      sd(
        difference
      ) /
      sqrt(
        n()
      ),
    ci_low =
      mean_difference -
      qt(0.975, df = n() - 1L) *
      se_difference,
    ci_high =
      mean_difference +
      qt(0.975, df = n() - 1L) *
      se_difference,
    response_aware_better =
      mean(
        difference < 0
      ),
    .groups = "drop"
  )

paired_comparisons <- paired_comparisons %>%
  mutate(
    p_value = ifelse(
      se_difference > 0,
      2 * pt(-abs(mean_difference / se_difference), df = n_rep - 1L),
      ifelse(mean_difference == 0, 1, 0)
    ),
    p_holm_all_16 = p.adjust(p_value, method = "holm")
  ) %>%
  group_by(comparator) %>%
  mutate(p_holm_within_comparator_4_budgets = p.adjust(p_value, method = "holm")) %>%
  ungroup()

write_csv(
  paired_comparisons,
  file.path(
    OUT_DIR,
    "06_paired_responseaware_comparisons.csv"
  )
)

# ============================================================
# 12. Query-behaviour summaries
# ============================================================

query_summary <- all_trace %>%
  group_by(
    worker,
    method,
    budget_after_batch
  ) %>%
  summarise(
    n_queries = n(),
    mean_entropy =
      mean(
        entropy_at_query
      ),
    mean_predicted_response =
      mean(
        response_prob_hat_at_query
      ),
    observed_response_rate =
      mean(
        R
      ),
    .groups = "drop"
  )

write_csv(
  query_summary,
  file.path(
    OUT_DIR,
    "07_query_behavior_summary.csv"
  )
)

aggregate_query_summary <- all_trace %>%
  group_by(
    rep,
    method,
    budget_after_batch
  ) %>%
  summarise(
    rep_mean_entropy =
      mean(
        entropy_at_query
      ),
    rep_observed_response_rate =
      mean(
        R
      ),
    rep_mean_predicted_response =
      mean(
        response_prob_hat_at_query
      ),
    .groups = "drop"
  ) %>%
  group_by(
    method,
    budget_after_batch
  ) %>%
  summarise(
    mean_entropy =
      mean(
        rep_mean_entropy
      ),
    se_entropy =
      sd(
        rep_mean_entropy
      ) /
      sqrt(
        n()
      ),
    mean_response_rate =
      mean(
        rep_observed_response_rate
      ),
    se_response_rate =
      sd(
        rep_observed_response_rate
      ) /
      sqrt(
        n()
      ),
    mean_predicted_response =
      mean(
        rep_mean_predicted_response
      ),
    se_predicted_response =
      sd(
        rep_mean_predicted_response
      ) /
      sqrt(
        n()
      ),
    .groups = "drop"
  )

write_csv(
  aggregate_query_summary,
  file.path(
    OUT_DIR,
    "08_aggregate_query_behavior.csv"
  )
)

# ============================================================
# 13. Figures
# ============================================================

# ------------------------------------------------------------
# Figure R1: main learning curves
# ------------------------------------------------------------

p1 <- ggplot(
  aggregate_summary,
  aes(
    x = budget,
    y = mean_test_error,
    group = method,
    linetype = method,
    shape = method
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = ci_low,
      ymax = ci_high
    ),
    width = 15,
    linewidth = 0.35
  ) +
  labs(
    x = "Additional query budget after common pilot",
    y = "Held-out classification error",
    linetype = "Acquisition rule",
    shape = "Acquisition rule"
  ) +
  theme_bw(
    base_size = 11
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    FIG_DIR,
    "Figure_R1_learning_curves.pdf"
  ),
  p1,
  width = 7.5,
  height = 5.0
)

# ------------------------------------------------------------
# Figure R2: query uncertainty
# ------------------------------------------------------------

p2 <- ggplot(
  aggregate_query_summary,
  aes(
    x = budget_after_batch,
    y = mean_entropy,
    group = method,
    linetype = method,
    shape = method
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 1.8
  ) +
  labs(
    x = "Additional query budget after common pilot",
    y = "Mean predictive entropy of queried items",
    linetype = "Acquisition rule",
    shape = "Acquisition rule"
  ) +
  theme_bw(
    base_size = 11
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    FIG_DIR,
    "Figure_R2_query_uncertainty.pdf"
  ),
  p2,
  width = 7.5,
  height = 5.0
)

# ------------------------------------------------------------
# Figure R3: observed response rates
# ------------------------------------------------------------

p3 <- ggplot(
  aggregate_query_summary,
  aes(
    x = budget_after_batch,
    y = mean_response_rate,
    group = method,
    linetype = method,
    shape = method
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 1.8
  ) +
  labs(
    x = "Additional query budget after common pilot",
    y = "Observed response rate among queried items",
    linetype = "Acquisition rule",
    shape = "Acquisition rule"
  ) +
  theme_bw(
    base_size = 11
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    FIG_DIR,
    "Figure_R3_response_rates.pdf"
  ),
  p3,
  width = 7.5,
  height = 5.0
)

# ------------------------------------------------------------
# Figure R4: worker-specific ResponseAware minus DiscountedRisk
# at maximum budget
# ------------------------------------------------------------

worker_pair <- all_results %>%
  filter(
    budget == MAX_BUDGET,
    method %in%
      c(
        "DiscountedRisk",
        "ResponseAware"
      )
  ) %>%
  select(
    rep,
    worker,
    method,
    test_error
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = test_error
  ) %>%
  mutate(
    difference =
      ResponseAware -
      DiscountedRisk
  )

worker_pair_summary <- worker_pair %>%
  group_by(
    worker
  ) %>%
  summarise(
    mean_difference =
      mean(
        difference
      ),
    se_difference =
      sd(
        difference
      ) /
      sqrt(
        n()
      ),
    ci_low =
      mean_difference -
      qt(0.975, df = n() - 1L) *
      se_difference,
    ci_high =
      mean_difference +
      qt(0.975, df = n() - 1L) *
      se_difference,
    .groups = "drop"
  )

worker_pair_summary <- worker_pair_summary %>%
  mutate(
    n_rep = N_REP,
    p_value = ifelse(
      se_difference > 0,
      2 * pt(-abs(mean_difference / se_difference), df = n_rep - 1L),
      ifelse(mean_difference == 0, 1, 0)
    ),
    p_holm_10_workers = p.adjust(p_value, method = "holm")
  )

write_csv(
  worker_pair_summary,
  file.path(
    OUT_DIR,
    "09_worker_responseaware_minus_discounted.csv"
  )
)

p4 <- ggplot(
  worker_pair_summary,
  aes(
    x = fct_reorder(
      worker,
      mean_difference
    ),
    y = mean_difference
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  geom_point(
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = ci_low,
      ymax = ci_high
    ),
    width = 0.15
  ) +
  coord_flip() +
  labs(
    x = "Human oracle",
    y = "Response-aware minus response-discounted test error"
  ) +
  theme_bw(
    base_size = 11
  )

ggsave(
  file.path(
    FIG_DIR,
    "Figure_R4_worker_specific_difference.pdf"
  ),
  p4,
  width = 6.5,
  height = 5.0
)

# ============================================================
# 14. Final console summary
# ============================================================

cat("\n============================================================\n")
cat("PRIMARY AGGREGATE RESULTS\n")
cat("============================================================\n")

print(
  aggregate_summary,
  n = Inf
)

cat("\n============================================================\n")
cat("PAIRED RESPONSE-AWARE COMPARISONS\n")
cat("Negative difference favors ResponseAware.\n")
cat("============================================================\n")

print(
  paired_comparisons,
  n = Inf
)

cat("\n============================================================\n")
cat("WORKER-SPECIFIC RESPONSE-AWARE MINUS DISCOUNTED\n")
cat("at maximum budget\n")
cat("============================================================\n")

print(
  worker_pair_summary,
  n = Inf
)

# Save session information.
sink(
  file.path(
    OUT_DIR,
    "sessionInfo.txt"
  )
)

print(
  sessionInfo()
)

sink()

cat("\n============================================================\n")
cat("EXPERIMENT COMPLETE\n")
cat("============================================================\n")
cat("All results saved in:\n")
cat(OUT_DIR, "\n")
cat("\nMain files to upload back to ChatGPT:\n")
cat("  00_experiment_configuration.csv\n")
cat("  00B_preflight_checks.csv\n")
cat("  00C_protocol_notes.csv\n")
cat("  02_all_raw_acquisition_results.csv\n")
cat("  05_primary_aggregate_summary.csv\n")
cat("  06_paired_responseaware_comparisons.csv\n")
cat("  04_worker_specific_summary.csv\n")
cat("  08_aggregate_query_behavior.csv\n")
cat("  09_worker_responseaware_minus_discounted.csv\n")
cat("  figures/Figure_R1_learning_curves.pdf\n")
cat("  figures/Figure_R2_query_uncertainty.pdf\n")
cat("  figures/Figure_R3_response_rates.pdf\n")
cat("  figures/Figure_R4_worker_specific_difference.pdf\n")
cat("  sessionInfo.txt\n")
