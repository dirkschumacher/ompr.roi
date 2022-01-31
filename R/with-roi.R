#' Configures a solver based on 'ROI'
#'
#' This function makes all solvers in the R package 'ROI' available to solve
#' 'ompr' models.
#'
#' @param solver the 'ROI' solver name (character vector of length 1)
#' @param ... optional parameters passed to ROI_solve
#'
#' Note: it does only support column duals. It currently does not export
#' row duals.
#'
#' @return a function: Model -> Solution that can be used
#' together with \code{\link[ompr]{solve_model}}. You can find \code{ROI}'s
#' original solver \code{message} and \code{status} information in
#' \code{<return_value>$ROI}. The \code{ompr} status code is \code{"success"}
#' if \code{ROI} returns \code{code = 0} and is \code{"error"} otherwise.
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' library(ompr)
#' library(ROI)
#' library(ROI.plugin.glpk)
#' add_variable(MIPModel(), x, type = "continuous") %>%
#'  set_objective(x, sense = "max") %>%
#'  add_constraint(x <= 5) %>%
#'  solve_model(with_ROI(solver = "glpk", verbose = TRUE))
#' }
#' @references
#' Kurt Hornik, David Meyer, Florian Schwendinger and Stefan Theussl (2016).
#' ROI: R Optimization Infrastructure. <https://CRAN.R-project.org/package=ROI>
#'
#' @export
with_ROI <- function(solver, ...) {
  registered_solvers <- ROI::ROI_registered_solvers()
  if (!(solver %in% names(registered_solvers))) {
    stop(paste0(solver, " is not among the registered ROI solvers. "))
  }
  function(model) {
    # build objective coeffcient vector
    objective <- model$objective
    obj <- ompr::objective_function(model)
    obj_constant <- obj$constant

    vars <- ompr::nvars(model)
    is_lp <- (vars[["integer"]] + vars[["binary"]]) == 0L

    op <- as_ROI_model(model)

    result <- ROI::ROI_solve(op, solver, ...)

    status <- if (result$status$code == 0) "success" else "error"
    solution <- ROI::solution(result, type = "primal", force = TRUE)

    variable_names <- ompr::variable_keys(model)
    if (is_lp) {
      dual_solution <- ROI::solution(result, type = "dual", force = TRUE)
      row_duals <- ROI::solution(result, "aux")
      solution_column_duals <- function() {
        stats::setNames(dual_solution, variable_names)
      }
      solution_row_duals <- function() {
        n_constraints <- ompr::nconstraints(model)
        if (is.null(row_duals[["dual"]])) {
          warning("ompr.roi cannot extract the row duals from the solution. Please report this as an issue", call. = FALSE)
          rep.int(NA_real_, n_constraints)
        } else {
          duals <- row_duals[["dual"]]
          stopifnot(length(duals) == n_constraints)
          duals
        }
      }
    } else {
      solution_column_duals <- solution_row_duals <- function() NA_real_
    }

    # the solution should be named
    names(solution) <- variable_names
    solution <- ompr::new_solution(
      status = status,
      model = model,
      objective_value = result$objval + obj_constant,
      solution = solution,
      solution_column_duals = solution_column_duals,
      solution_row_duals = solution_row_duals,
      additional_solver_output = list(
        ROI = list(
          status = result$status,
          message = result$message
        )
      )
    )
    solution
  }
}
