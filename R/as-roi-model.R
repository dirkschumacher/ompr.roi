#' Export to ROI::OP
#'
#' This function can be used to transform an ompr model to a ROI::OP object.
#'
#' @param model an ompr model
#'
#' @return an object of S3 class `ROI::OP`
#'
#' @export
as_ROI_model <- function(model) {
  if (class(model) != "optimization_model") {
    stop("This function only works with models of type",
        "'optimization_model' from package ompr", call. = FALSE)
  }

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
  obj_vector <- slam::simple_triplet_matrix(rep.int(1L, length(obj_vector@i)),
                                            obj_vector@i,
                                            obj_vector@x,
                                            nrow = 1,
                                            ncol = length(obj_vector))
  obj_fun <- ROI::L_objective(obj_vector)
  if (length(constraint_dir) == 0) {
    constraint_matrix <- matrix(nrow = 0, ncol = ncols)
    constraint_rhs <- integer(0)
    constraint_dir <- character(0)
  }

  # convert to triplet matrix
  constraint_matrix <- methods::as(constraint_matrix, "dgTMatrix")
  constraint_matrix <- slam::simple_triplet_matrix(
    i = constraint_matrix@i + 1,
    j = constraint_matrix@j + 1,
    v = constraint_matrix@x,
    nrow = constraint_matrix@Dim[1],
    ncol = constraint_matrix@Dim[2]
  )
  constraints <- ROI::L_constraint(L = constraint_matrix,
                                   dir = constraint_dir,
                                   rhs = constraint_rhs)

  # remove those lbs that are 0
  # and those ubs that are Inf
  # otherwise ROI throws a warnning
  li <- seq_len(length(column_bounds_l))
  ui <- seq_len(length(column_bounds_u))
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
      nobj = max(li, ui)
    )
  } else {
    bounds <- NULL
  }
  is_max <- !has_objective || model$objective$sense == "max"
  ROI::OP(obj_fun,
          constraints,
          bounds = bounds,
          types = column_types_chr,
          max = is_max)
}
