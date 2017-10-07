#' Configures a solver based on 'ROI'
#'
#' This function makes all solvers in the R package 'ROI' available to solve
#' 'ompr' models.
#'
#' @param solver the 'ROI' solver name (character vector of length 1)
#' @param ... optional parameters passed to ROI_solve
#'
#' ROI only returns two status codes, optimal or infeasible. In particular,
#' this means that when the search is terminated by the user (e.g. due to a time limit),
#' ROI flags that solution as 'infeasible'.
#' It is currently not possible for `ompr.roi` to decide, if a solution
#' is feasible, but not optimal. Therefore only optimal solutions are returned.
#'
#' @return a function: Model -> Solution that can be used
#' together with \code{\link[ompr]{solve_model}}.
#'
#' @examples
#' library(magrittr)
#' library(ompr)
#' library(ROI)
#' library(ROI.plugin.glpk)
#' add_variable(MIPModel(), x, type = "continuous") %>%
#'  set_objective(x, sense = "max") %>%
#'  add_constraint(x <= 5) %>%
#'  solve_model(with_ROI(solver = "glpk", verbose = TRUE))
#'
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

    op <- as_ROI_model(model)

    result <- ROI::ROI_solve(op, solver, ...)

    # we access the solution element directly to extract user
    # terminated searches
    solution <- result[["solution"]]
    is_optimal <- ROI::ROI_plugin_solution_status_code(result) == 0
    status <- if (is_optimal) {
                "optimal"
              } else {
                "infeasible"
              }

    # the solution should be named
    names(solution) <- ompr::variable_keys(model)
    obj_val <- ROI::ROI_plugin_solution_objval(result) + obj_constant
    solution <- ompr::new_solution(status = status,
                    model = model,
                    objective_value = obj_val,
                    solution = solution)
    solution
  }
}
