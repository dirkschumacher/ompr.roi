#' Configures a solver based on ROI
#'
#' @param solver the ROI solver name
#' @param verbose should status information be printed
#'
#' @return a function: Model -> Solution
#'
#' @export
with_ROI <- function(solver, verbose = FALSE) {
  registered_solvers <- ROI::ROI_registered_solvers()
  if (!(solver %in% names(registered_solvers))) {
    stop(paste0(solver, " is not among the registered ROI solvers. "))
  }
  function(model) {
    # define the order of all variables
    vars <- model@variables
    var_list <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      if (length(var@instances) == 1 && nchar(var@instances) == 0) {
        key
      } else {
        paste0(key, "_", var@instances)
      }
    }))
    column_types <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      rep.int(x = var@type, times = length(var@instances))
    }))
    ncols <- length(var_list)
    nrows <- length(model@constraints)

    # build column bounds
    column_bounds_l <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      lb <- if (length(var@lb) == 0) -Inf else var@lb
      if (length(var@lb) == 0 && var@type == "binary") lb <- 0
      n_instances <- length(var@instances)
      n_bounds <- length(lb)
      if (n_instances != n_bounds) {
        rep.int(x = lb, times = n_instances)
      } else {
        lb
      }
    }))

    column_bounds_u <- unlist(lapply(names(vars), function(key) {
      var <- vars[[key]]
      ub <- if (length(var@ub) == 0) Inf else var@ub
      if (length(var@ub) == 0 && var@type == "binary") ub <- 1
      n_instances <- length(var@instances)
      n_bounds <- length(ub)
      if (n_instances != n_bounds) {
        rep.int(x = ub, times = n_instances)
      } else {
        ub
      }
    }))

    # build objective coeffcient vector
    objective <- model@objective
    build_coefficent_vector <- function(extracted_coefficients) {
      coef_vector <- rep.int(0, ncols)
      coefficients <- extracted_coefficients
      names(coefficients) <- NULL
      obj_constant <- extracted_coefficients$constant
      bound_coefs <- unlist(Map(function(var_coef) {
        var_ast <- var_coef$ast
        if (is.call(var_ast) && length(var_ast) > 1) {
          var_name <- as.character(var_ast[[2]])
          search_key <- paste0(c(var_name, as.character(var_ast[3:length(var_ast)])), collapse = "_")
        } else {
          var_name <- as.character(var_ast)
          search_key <- var_name
        }
        setNames(var_coef$coef, search_key)
      }, coefficients))
      coef_positions <- match(names(bound_coefs), var_list)
      coef_positions <- coef_positions[!is.na(coef_positions)]
      if (length(coef_positions) > 0) {
        coef_vector[coef_positions] <- as.numeric(bound_coefs)
      }
      coef_vector
    }
    if (!is.null(objective)) {
      coefficients <- ompr::extract_coefficients(model@objective@expression[[1]])
      obj_constant <- coefficients$constant
      if (!is.numeric(obj_constant)) obj_constant <- 0
      coefficients <- coefficients$coefficients
      names(coefficients) <- NULL
      obj_vector <- build_coefficent_vector(coefficients)
    } else {
      obj_constant <- 0
      obj_vector <- rep.int(0, ncols)
    }

    # build constraint matrix
    matrices <- lapply(model@constraints, function(constraint) {
      coefficients_lhs <- ompr::extract_coefficients(constraint@lhs[[1]])
      coefficients_rhs <- ompr::extract_coefficients(constraint@rhs[[1]])
      direction <- constraint@direction
      list(
        lhs = build_coefficent_vector(coefficients_lhs$coefficients),
        rhs = build_coefficent_vector(coefficients_rhs$coefficients),
        direction = direction,
        lhs_constant = coefficients_lhs$constant,
        rhs_constant = coefficients_rhs$constant
      )
    })

    constraint_matrix <- t(rbind(sapply(matrices, function(constraint) {
      constraint$lhs - constraint$rhs
    })))

    # build row upper bound (aka b)
    constraint_rhs <- sapply(matrices, function(constraint) {
      constraint$rhs_constant - constraint$lhs_constant
    })

    constraint_dir <- sapply(matrices, function(constraint) {
      constraint$direction
    })

    binary_vars <-
      row_ub <- sapply(matrices, function(constraint) {
        if (constraint$direction != ">=") {
          constraint$rhs_constant - constraint$lhs_constant
        } else {
          Inf
        }
      })
    # build row lower bound
    row_lb <- sapply(matrices, function(constraint) {
      if (constraint$direction != "<=") {
        constraint$rhs_constant - constraint$lhs_constant
      } else {
        -Inf
      }
    })

    column_types <- column_types
    column_types[column_types == "binary"] <- "B"
    column_types[column_types == "integer"] <- "I"
    column_types[column_types == "continuous"] <- "C"

    # build ROI OP (optimization problem)

    obj_fun <- ROI::L_objective(obj_vector)
    constraints <- ROI::L_constraint(L = constraint_matrix,
                                    dir = constraint_dir,
                                    rhs = constraint_rhs)
    bounds <- ROI::V_bound(
      li = seq_len(length(column_bounds_l)),
      ui = seq_len(length(column_bounds_u)),
      lb = column_bounds_l,
      ub = column_bounds_u
    )
    op <- ROI::OP(obj_fun,
                  constraints,
                  bounds = bounds,
                  types = column_types,
                  max = model@objective@direction == "max")
    solver_options <- list(verbose = verbose)
    if (solver == "symphony") {
      solver_options$verbosity <- if (verbose) 1 else -2
    }
    result <- ROI::ROI_solve(op, solver, control = solver_options)

    status <- if (result$status$code == 0) "optimal" else "infeasible"
    solution <- ROI::solution(result)

    # the solution should be named
    names(solution) <- sapply(var_list, function(var) {
      if (grepl(x = var, pattern = "_", fixed = TRUE)) {
        splited_els <- strsplit(var, "_", fixed = TRUE)[[1]]
        paste0(splited_els[1], "[",
               paste0(splited_els[2:length(splited_els)], collapse = ","),
               "]")
      } else {
        var
      }
    })
    solution <- new("Solution",
                    status = status,
                    model = model,
                    objective_value = result$objval + obj_constant,
                    solution = solution)
    solution
  }
}
