#' Longitudinal three-level variance decomposition (B-P-W)
#'
#' Convenience wrapper around \code{hlm_decompose()} for 3-level data:
#' clusters (e.g., schools), persons within clusters, and repeated measures
#' within persons over time.
#'
#' @param data A data frame.
#' @param var Name of the focal variable (string).
#' @param cluster Cluster ID (e.g., school/classroom) (string).
#' @param id Person ID within cluster (string).
#' @param time Optional time variable (string); stored in the output for
#'   reference but not required for the algebra.
#'
#' @return An object of class \code{hlm_decompose} with between-cluster (B),
#'   between-person (P), and within-person (W) variance components and shares.
#'
#' @examples
#' \dontrun{
#' hlm_decompose_long(data = d_long,
#'                    var = "math_score",
#'                    cluster = "school_id",
#'                    id = "student_id",
#'                    time = "wave")
#' }
#' @export
hlm_decompose_long <- function(data, var, cluster, id, time = NULL) {
  hlm_decompose(data = data, var = var, cluster = cluster, id = id, time = time)
}
