#' Geometry of a cross-level interaction (random slopes fan plot)
#'
#' Produces a "fan plot" of predicted lines for each cluster to illustrate
#' a cross-level interaction (random slope) in a multilevel model.
#'
#' @param model A fitted \code{lmerMod} model with a random slope term
#'   of the form \code{(x_within | cluster)}.
#' @param x_within Name of the Level-1 predictor with a random slope (string).
#' @param cluster Name of the clustering factor (string).
#' @param n_points Number of points to plot along the x-axis. Defaults to 20.
#' @param n_clusters Maximum number of clusters to display (sampled) for clarity.
#'
#' @return A ggplot object showing predicted lines by cluster.
#'
#' @examples
#' \dontrun{
#' m <- lme4::lmer(math_score ~ SES_c + SES_mean + (SES_c | school_id), data = d)
#' hlm_xint_geom(m, x_within = "SES_c", cluster = "school_id")
#' }
#' @export
hlm_xint_geom <- function(model, x_within, cluster,
                          n_points = 20, n_clusters = 20) {
  if (!inherits(model, "lmerMod")) {
    stop("hlm_xint_geom currently supports models fitted with lme4::lmer().")
  }

  re_list <- lme4::ranef(model)
  if (!cluster %in% names(re_list)) {
    stop("Random effects for the specified 'cluster' not found in model.")
  }
  re <- re_list[[cluster]]

  if (!x_within %in% colnames(re)) {
    stop("Random slope for 'x_within' not found in the random-effects structure.")
  }

  fe <- lme4::fixef(model)
  intercept_name <- "(Intercept)"
  if (!intercept_name %in% names(fe)) {
    stop("Model must include a fixed intercept.")
  }
  beta0 <- fe[intercept_name]
  beta1 <- fe[x_within]

  cl_ids <- rownames(re)
  if (length(cl_ids) > n_clusters) {
    set.seed(123)
    cl_ids <- sample(cl_ids, n_clusters)
  }
  re_sub <- re[cl_ids, , drop = FALSE]

  x_seq <- seq(-2, 2, length.out = n_points)

  pred_list <- lapply(seq_along(cl_ids), function(i) {
    cl <- cl_ids[i]
    b0i <- beta0 + re_sub[cl, "(Intercept)"]
    b1i <- beta1 + re_sub[cl, x_within]
    y_hat <- b0i + b1i * x_seq
    data.frame(
      cluster = cl,
      x       = x_seq,
      y       = y_hat
    )
  })

  pred_df <- dplyr::bind_rows(pred_list)

  ggplot2::ggplot(pred_df, ggplot2::aes(x = x, y = y, group = cluster)) +
    ggplot2::geom_line(alpha = 0.4) +
    ggplot2::labs(
      x = x_within,
      y = "Predicted outcome",
      title = "Cross-level interaction geometry: random slopes"
    ) +
    ggplot2::theme_minimal()
}
