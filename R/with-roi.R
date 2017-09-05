#' Configures a solver based on 'ROI'
#'
#' This function makes all solvers in the R package 'ROI' available to solve
#' 'ompr' models.
#'
#' @param solver the 'ROI' solver name (character vector of length 1)
#' @param ... optional parameters passed to ROI_solve
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

    status <- if (result$status$code == 0) "optimal" else "infeasible"
    solution <- ROI::solution(result)

    # the solution should be named
    names(solution) <- ompr::variable_keys(model)
    solution <- ompr::new_solution(status = status,
                    model = model,
                    objective_value = result$objval + obj_constant,
                    solution = solution)
    solution
  }
}
