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
    # define the order of all variables
    column_types <- ompr::variable_types(model)
    ncols <- length(column_types)

    # build column bounds
    bounds <- ompr::variable_bounds(model)
    column_bounds_l <- bounds$lower
    column_bounds_u <- bounds$upper

    # build objective coeffcient vector
    objective <- model$objective
    has_objective <- !is.null(objective)
    obj <- ompr::objective_function(model)
    obj_vector <- obj$solution
    obj_constant <- obj$constant

    # build constraint matrix
    constraints <- ompr::extract_constraints(model)
    constraint_matrix <- constraints$matrix
    constraint_rhs <- constraints$rhs
    constraint_dir <- constraints$sense

    # convert types to ROI codes
    column_types_chr <- character(length(column_types))
    column_types_chr[column_types == "binary"] <- "B"
    column_types_chr[column_types == "integer"] <- "I"
    column_types_chr[column_types == "continuous"] <- "C"

    # build ROI OP (optimization problem)
    obj_fun <- ROI::L_objective(obj_vector)
    if (length(constraint_dir) == 0) {
      constraint_matrix <- matrix(nrow = 0, ncol = ncols)
      constraint_rhs <- integer(0)
      constraint_dir <- character(0)
    }
    constraints <- ROI::L_constraint(L = constraint_matrix,
                                    dir = constraint_dir,
                                    rhs = constraint_rhs)

    # remove those lbs that are 0
    # and those ubs that are Inf
    # otherwise ROI throws a warnning
    li <- seq_along(column_bounds_l)
    ui <- seq_along(column_bounds_u)
    lb <- column_bounds_l
    ub <- column_bounds_u
    if (length(li) > 0) {
      lb_zero <- lb == 0
      li <- li[!lb_zero]
      lb <- lb[!lb_zero]
    }
    if (length(ui) > 0) {
      ub_inf <- ub == Inf
      ui <- ui[!ub_inf]
      ub <- ub[!ub_inf]
    }
    if (length(ui) + length(li) > 0) {
      bounds <- ROI::V_bound(
        li = li,
        ui = ui,
        lb = lb,
        ub = ub,
        nobj = length(obj_fun)
      )
    } else {
      bounds <- NULL
    }
    is_max <- !has_objective || model$objective$sense == "max"
    op <- ROI::OP(obj_fun,
                  constraints,
                  bounds = bounds,
                  types = column_types_chr,
                  max = is_max)
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
