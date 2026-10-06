
# ============================================================
# FINAL REAL-DATA GEOMETRY FIGURE
# Current paper:
# Classification-Risk-Optimal Label Acquisition
# with Informative Oracle Response
#
# Consumer Complaints -- uncertainty-response geometry
#
# Main scientific question:
# How does response-aware acquisition differ from response-discounted
# acquisition when both predictive uncertainty and oracle response
# probability vary across candidate observations?
#
# This script uses ONLY the corrected query trace and DOES NOT rerun
# the acquisition experiment.
#
# Panels:
#   (a) empirical candidate/reference geometry in
#       predictive entropy vs estimated response probability
#   (b) Response-discounted queried observations
#   (c) Response-aware queried observations
#
# Each method panel also shows the batch-wise mean acquisition
# trajectory as the budget increases.
#
# IMPORTANT:
#   * x = entropy_at_query
#   * y = response_prob_hat_at_query
#   * Random acquisition provides a descriptive reference cloud.
#     Coordinates come from evolving fitted models, not one fixed model.
#   * The displayed representative replication is chosen objectively:
#     the replication whose RA-minus-DR test-error difference is
#     closest to the median across the 30 W8 runs at B = 600.
# ============================================================

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

pkgs <- c(
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "patchwork",
  "scales",
  "tibble"
)

to_install <- pkgs[
  !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

set.seed(20260906)

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

MAIN_DIR <-
  "C:/Users/SETOUDEHTAZANGI/Downloads/my SSL papers/paper 3 on ssl"

RESULT_DIR <- file.path(
  MAIN_DIR,
  "Consumer_Complaints_Corrected_Curvature_20261006_132351"
)

TRACE_FILE <- file.path(
  RESULT_DIR,
  "03_all_query_trace.csv"
)

RAW_FILE <- file.path(
  RESULT_DIR,
  "02_all_raw_acquisition_results.csv"
)

OUT_DIR <- file.path(
  RESULT_DIR,
  "geometry_entropy_response_corrected_curvature"
)

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

stopifnot(
  file.exists(TRACE_FILE),
  file.exists(RAW_FILE)
)

# ------------------------------------------------------------
# 2. Read corrected outputs
# ------------------------------------------------------------

trace <- readr::read_csv(
  TRACE_FILE,
  show_col_types = FALSE
)

raw <- readr::read_csv(
  RAW_FILE,
  show_col_types = FALSE
)

required_trace <- c(
  "rep",
  "worker",
  "method",
  "batch",
  "budget_after_batch",
  "row_id",
  "R",
  "entropy_at_query",
  "response_prob_hat_at_query"
)

required_raw <- c(
  "rep",
  "worker",
  "method",
  "budget",
  "test_error"
)

missing_trace <- setdiff(
  required_trace,
  names(trace)
)

missing_raw <- setdiff(
  required_raw,
  names(raw)
)

if (length(missing_trace) > 0) {
  stop(
    "Missing columns in trace file: ",
    paste(missing_trace, collapse = ", ")
  )
}

if (length(missing_raw) > 0) {
  stop(
    "Missing columns in raw-results file: ",
    paste(missing_raw, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 3. Figure settings
# ------------------------------------------------------------

ORACLE <- "W8"
BUDGET_SHOW <- 600

# W8 is chosen because it has the richest genuine abstention pattern,
# not because it gives the largest performance gain.

# ------------------------------------------------------------
# 4. Objectively choose representative replication
# ------------------------------------------------------------

rep_diff <- raw %>%
  dplyr::filter(
    worker == ORACLE,
    budget == BUDGET_SHOW,
    method %in% c(
      "DiscountedRisk",
      "ResponseAware"
    )
  ) %>%
  dplyr::select(
    rep,
    method,
    test_error
  ) %>%
  tidyr::pivot_wider(
    names_from = method,
    values_from = test_error
  ) %>%
  dplyr::mutate(
    diff_RA_minus_DR =
      ResponseAware -
      DiscountedRisk
  )

if (nrow(rep_diff) != 30L || anyDuplicated(rep_diff$rep) ||
    !setequal(rep_diff$rep, seq_len(30L)) ||
    any(!is.finite(rep_diff$diff_RA_minus_DR))) {
  stop("Expected 30 complete paired W8 results at budget 600.")
}

med_diff <- stats::median(
  rep_diff$diff_RA_minus_DR,
  na.rm = TRUE
)

REP_SHOW <- rep_diff %>%
  dplyr::mutate(
    distance_to_median =
      abs(
        diff_RA_minus_DR -
          med_diff
      )
  ) %>%
  dplyr::arrange(
    distance_to_median,
    rep
  ) %>%
  dplyr::slice(1) %>%
  dplyr::pull(rep)

rep_info <- rep_diff %>%
  dplyr::filter(
    rep == REP_SHOW
  )

cat("\n============================================================\n")
cat("REPRESENTATIVE RUN\n")
cat("============================================================\n")
cat("Oracle:", ORACLE, "\n")
cat("Budget:", BUDGET_SHOW, "\n")
cat("Representative replication:", REP_SHOW, "\n")
cat(
  "Median RA - DR difference:",
  round(med_diff, 6),
  "\n"
)
print(rep_info)
cat("\n")

# ------------------------------------------------------------
# 5. Build empirical reference cloud
# ------------------------------------------------------------

# Random-query coordinates are pooled across replications and updates.
# This is a descriptive reference, not the geometry of a fixed fitted model.

reference_all <- trace %>%
  dplyr::filter(
    worker == ORACLE,
    method == "Random",
    is.finite(entropy_at_query),
    is.finite(response_prob_hat_at_query)
  )

# Use all reference observations in the 2-D density layer.
# Thin only the visible point cloud to keep the figure clean.

N_REF_POINTS <- min(
  12000L,
  nrow(reference_all)
)

reference_points <- reference_all %>%
  dplyr::slice_sample(
    n = N_REF_POINTS
  )

# ------------------------------------------------------------
# 6. Representative-run method data
# ------------------------------------------------------------

rep_methods <- trace %>%
  dplyr::filter(
    rep == REP_SHOW,
    worker == ORACLE,
    budget_after_batch <= BUDGET_SHOW,
    method %in% c(
      "DiscountedRisk",
      "ResponseAware"
    ),
    is.finite(entropy_at_query),
    is.finite(response_prob_hat_at_query)
  ) %>%
  dplyr::mutate(
    method_label = dplyr::recode(
      method,
      DiscountedRisk =
        "Response-discounted",
      ResponseAware =
        "Response-aware"
    ),
    method_label = factor(
      method_label,
      levels = c(
        "Response-discounted",
        "Response-aware"
      )
    )
  )

# Check that each displayed method has exactly 12 complete batches.
counts <- rep_methods %>%
  dplyr::count(method, budget_after_batch, name = "n_queries")
if (nrow(counts) != 24L || any(counts$n_queries != 50L) ||
    anyDuplicated(rep_methods[, c("method", "row_id")]) ||
    !setequal(unique(rep_methods$budget_after_batch), seq(50, 600, by = 50))) {
  stop("Displayed acquisition traces are incomplete or duplicated.")
}

# ------------------------------------------------------------
# 7. Batch-wise mean acquisition trajectories
# ------------------------------------------------------------

trajectory <- rep_methods %>%
  dplyr::group_by(
    method_label,
    budget_after_batch
  ) %>%
  dplyr::summarise(
    mean_entropy =
      mean(
        entropy_at_query,
        na.rm = TRUE
      ),
    mean_response =
      mean(
        response_prob_hat_at_query,
        na.rm = TRUE
      ),
    n_queries =
      dplyr::n(),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 8. Common axis limits
# ------------------------------------------------------------

all_plot_data <- dplyr::bind_rows(
  reference_all %>%
    dplyr::transmute(
      entropy = entropy_at_query,
      response = response_prob_hat_at_query
    ),
  rep_methods %>%
    dplyr::transmute(
      entropy = entropy_at_query,
      response = response_prob_hat_at_query
    )
)

x_rng <- range(
  all_plot_data$entropy,
  na.rm = TRUE
)

y_rng <- range(
  all_plot_data$response,
  na.rm = TRUE
)

# Entropy is normalized in the experiment, so [0,1] is natural.
# Response probability is also [0,1].
x_lim <- c(
  max(0, x_rng[1]),
  min(1, x_rng[2])
)

y_lim <- c(
  max(0, y_rng[1]),
  min(1, y_rng[2])
)

# ------------------------------------------------------------
# 9. Panel (a): empirical uncertainty-response geometry
# ------------------------------------------------------------

panel_a <- ggplot2::ggplot() +
  ggplot2::stat_density_2d(
    data = reference_all,
    ggplot2::aes(
      x = entropy_at_query,
      y = response_prob_hat_at_query,
      fill = ggplot2::after_stat(level)
    ),
    geom = "polygon",
    contour = TRUE,
    bins = 12,
    alpha = 0.78
  ) +
  ggplot2::geom_point(
    data = reference_points,
    ggplot2::aes(
      x = entropy_at_query,
      y = response_prob_hat_at_query
    ),
    color = "grey15",
    alpha = 0.055,
    size = 0.55
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "C",
    direction = -1,
    guide = "none"
  ) +
  ggplot2::coord_cartesian(
    xlim = x_lim,
    ylim = y_lim,
    expand = FALSE
  ) +
  ggplot2::labs(
    title = "(a) Uncertainty-response geometry",
    x = "Predictive entropy",
    y = "Estimated oracle response probability"
  ) +
  ggplot2::theme_bw(
    base_size = 11.5
  ) +
  ggplot2::theme(
    panel.grid.minor =
      ggplot2::element_blank(),
    panel.grid.major =
      ggplot2::element_line(
        linewidth = 0.22,
        color = "grey92"
      ),
    plot.title =
      ggplot2::element_text(
        face = "bold",
        size = 11.8
      )
  )

# ------------------------------------------------------------
# 10. Helper function for method panels
# ------------------------------------------------------------

make_method_panel <- function(
    method_name,
    title_text,
    point_color
) {
  
  dat <- rep_methods %>%
    dplyr::filter(
      method_label == method_name
    )
  
  traj <- trajectory %>%
    dplyr::filter(
      method_label == method_name
    )
  
  start_point <- traj %>%
    dplyr::slice_min(
      budget_after_batch,
      n = 1,
      with_ties = FALSE
    )
  
  end_point <- traj %>%
    dplyr::slice_max(
      budget_after_batch,
      n = 1,
      with_ties = FALSE
    )
  
  ggplot2::ggplot() +
    
    # reference population
    ggplot2::stat_density_2d(
      data = reference_all,
      ggplot2::aes(
        x = entropy_at_query,
        y = response_prob_hat_at_query
      ),
      color = "grey75",
      linewidth = 0.33,
      bins = 7
    ) +
    
    ggplot2::geom_point(
      data = reference_points,
      ggplot2::aes(
        x = entropy_at_query,
        y = response_prob_hat_at_query
      ),
      color = "grey65",
      alpha = 0.08,
      size = 0.48
    ) +
    
    # actual queried observations
    ggplot2::geom_point(
      data = dat,
      ggplot2::aes(
        x = entropy_at_query,
        y = response_prob_hat_at_query
      ),
      color = point_color,
      alpha = 0.42,
      size = 0.90
    ) +
    
    # acquisition trajectory
    ggplot2::geom_path(
      data = traj,
      ggplot2::aes(
        x = mean_entropy,
        y = mean_response
      ),
      color = point_color,
      linewidth = 1.35,
      lineend = "round"
    ) +
    
    ggplot2::geom_point(
      data = traj,
      ggplot2::aes(
        x = mean_entropy,
        y = mean_response
      ),
      color = point_color,
      fill = "white",
      shape = 21,
      stroke = 1.05,
      size = 2.8
    ) +
    
    # start
    ggplot2::geom_point(
      data = start_point,
      ggplot2::aes(
        x = mean_entropy,
        y = mean_response
      ),
      shape = 21,
      fill = "white",
      color = point_color,
      stroke = 1.25,
      size = 4.0
    ) +
    
    # end
    ggplot2::geom_point(
      data = end_point,
      ggplot2::aes(
        x = mean_entropy,
        y = mean_response
      ),
      shape = 23,
      fill = "white",
      color = point_color,
      stroke = 1.25,
      size = 4.2
    ) +
    
    ggplot2::coord_cartesian(
      xlim = x_lim,
      ylim = y_lim,
      expand = FALSE
    ) +
    
    ggplot2::labs(
      title = title_text,
      x = "Predictive entropy",
      y = "Estimated oracle response probability"
    ) +
    
    ggplot2::theme_bw(
      base_size = 11.5
    ) +
    
    ggplot2::theme(
      panel.grid.minor =
        ggplot2::element_blank(),
      panel.grid.major =
        ggplot2::element_line(
          linewidth = 0.22,
          color = "grey92"
        ),
      plot.title =
        ggplot2::element_text(
          face = "bold",
          size = 11.8
        )
    )
}

# ------------------------------------------------------------
# 11. Panels (b) and (c)
# ------------------------------------------------------------

panel_b <- make_method_panel(
  method_name =
    "Response-discounted",
  title_text =
    "(b) Response-discounted acquisition",
  point_color =
    "#2B6CB0"
)

panel_c <- make_method_panel(
  method_name =
    "Response-aware",
  title_text =
    "(c) Response-aware acquisition",
  point_color =
    "#B2182B"
)

# ------------------------------------------------------------
# 12. Assemble final 3-panel figure
# ------------------------------------------------------------

final_fig <- panel_a + panel_b + panel_c +
  patchwork::plot_layout(
    nrow = 1,
    widths = c(
      1.05,
      1,
      1
    )
  ) +
  patchwork::plot_annotation(
    title =
      "Informative oracle response changes where labels are queried",
    subtitle = paste0(
      "Consumer Complaints, ",
      ORACLE,
      ", representative replication ",
      REP_SHOW,
      "; circles trace batch-wise mean query locations and the diamond marks B = ",
      BUDGET_SHOW
    ),
    theme = ggplot2::theme(
      plot.title =
        ggplot2::element_text(
          face = "bold",
          size = 15
        ),
      plot.subtitle =
        ggplot2::element_text(
          size = 10.5,
          color = "grey25"
        )
    )
  )

# ------------------------------------------------------------
# 13. Save final figure
# ------------------------------------------------------------

PDF_FILE <- file.path(
  OUT_DIR,
  "Figure_realdata_entropy_response_geometry.pdf"
)

PNG_FILE <- file.path(
  OUT_DIR,
  "Figure_realdata_entropy_response_geometry.png"
)

ggplot2::ggsave(
  PDF_FILE,
  final_fig,
  width = 12.3,
  height = 4.7,
  device = grDevices::cairo_pdf
)

ggplot2::ggsave(
  PNG_FILE,
  final_fig,
  width = 12.3,
  height = 4.7,
  dpi = 500
)

# ------------------------------------------------------------
# 14. Save individual panels
# ------------------------------------------------------------

ggplot2::ggsave(
  file.path(
    OUT_DIR,
    "Panel_A_uncertainty_response_geometry.png"
  ),
  panel_a,
  width = 4.4,
  height = 4.3,
  dpi = 400
)

ggplot2::ggsave(
  file.path(
    OUT_DIR,
    "Panel_B_response_discounted_geometry.png"
  ),
  panel_b,
  width = 4.4,
  height = 4.3,
  dpi = 400
)

ggplot2::ggsave(
  file.path(
    OUT_DIR,
    "Panel_C_response_aware_geometry.png"
  ),
  panel_c,
  width = 4.4,
  height = 4.3,
  dpi = 400
)

# ------------------------------------------------------------
# 15. Save exact trajectory values
# ------------------------------------------------------------

readr::write_csv(
  trajectory,
  file.path(
    OUT_DIR,
    "entropy_response_trajectory_values.csv"
  )
)

figure_meta <- tibble::tibble(
  source_result_folder = basename(RESULT_DIR),
  raw_results_md5 = unname(tools::md5sum(RAW_FILE)),
  query_trace_md5 = unname(tools::md5sum(TRACE_FILE)),
  oracle = ORACLE,
  budget = BUDGET_SHOW,
  representative_rep = REP_SHOW,
  median_RA_minus_DR = med_diff,
  displayed_rep_RA_minus_DR =
    rep_info$diff_RA_minus_DR
)

readr::write_csv(
  figure_meta,
  file.path(
    OUT_DIR,
    "entropy_response_figure_metadata.csv"
  )
)

# ------------------------------------------------------------
# 16. Console summary
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("FINAL ENTROPY-RESPONSE GEOMETRY FIGURE CREATED\n")
cat("============================================================\n")
cat("Saved to:\n")
cat(PDF_FILE, "\n")
cat(PNG_FILE, "\n\n")

cat("Representative replication:", REP_SHOW, "\n")
cat("Oracle:", ORACLE, "\n")
cat("Median RA - DR difference:", med_diff, "\n\n")

cat("Trajectory values:\n")
print(
  trajectory,
  n = Inf
)
