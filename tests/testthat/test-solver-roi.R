context("ROI")

library(ompr)
library(ROI)

test_that("solver fails with unkown solver", {
  expect_error(with_ROI(solver = "some_strange_solver"))
})

test_that("ROI stops when there is no constraint", {
  m <- add_variable(MIPModel(), x, type = "continuous", ub = 10) %>%
    set_objective(x, sense = "max")
  expect_error(solve_model(with_ROI(solver = "glpk")))
})

test_that("ROI correctly flags an unbounded problem", {
  result <- add_variable(MIPModel(), x, type = "continuous") %>%
    set_objective(x, sense = "max") %>%
    add_constraint(x >= 0) %>%
    solve_model(with_ROI(solver = "glpk"))
  expect_equal(result$status, "infeasible")
})

test_that("ROI correctly flags an infeasible problem", {
  result <- add_variable(MIPModel(), x, type = "continuous", lb = 10) %>%
    set_objective(x, sense = "max") %>%
    add_constraint(x <= 3) %>%
    solve_model(with_ROI(solver = "glpk"))
  expect_equal(result$status, "infeasible")
})

test_that("ROI has a verbose option", {
  m <- add_variable(MIPModel(), x, type = "continuous", lb = 1) %>%
    set_objective(x, sense = "max") %>%
    add_constraint(x <= 3)
  expect_output(solve_model(m, with_ROI(solver = "glpk", verbose = TRUE)))
})

test_that("ROI interprets obj. max sense correctly", {
  result <- add_variable(MIPModel(), x, type = "continuous", ub = 10) %>%
    set_objective(x, sense = "max") %>%
    add_constraint(x <= 80) %>%
    solve_model(with_ROI(solver = "glpk"))
  expect_equal(result$objective_value, 10)
  expect_equal(names(result$solution), c("x"))
})

test_that("ROI interprets obj. min sense correctly", {
  result <- add_variable(MIPModel(), x, type = "continuous", lb = 10) %>%
    set_objective(x, sense = "min") %>%
    add_constraint(x >= 0) %>%
    solve_model(with_ROI(solver = "glpk"))
  expect_equal(result$objective_value, 10)
})

test_that("symphony can solve a model", {
  weights <- c(1, 2, 3)
  result <- add_variable(MIPModel(), x[i], i = 1:3, type = "binary") %>%
    add_constraint(sum_expr(x[i], i = 1:3) == 1) %>%
    set_objective(sum_expr(x[i] * weights[i], i = 1:3) + 5) %>%
    solve_model(with_ROI(solver = "glpk"))
  expect_equal(result$objective_value, 8)
  expect_equal(names(result$solution), c("x[1]", "x[2]", "x[3]"))
})

test_that("ROI can solve a bin packing problem", {
  max_bins <- 5
  bin_size <- 3
  n <- 5
  weights <- rep.int(1, n)
  m <- MIPModel()
  m <- add_variable(m, y[i], i = 1:max_bins, type = "binary")
  m <- add_variable(m, x[i, j], i = 1:max_bins, j = 1:n, type = "binary")
  m <- set_objective(m, sum_expr(y[i], i = 1:max_bins), "min")
  for (i in 1:max_bins) {
    m <- add_constraint(m,
          sum_expr(weights[j] * x[i, j], j = 1:n) <= y[i] * bin_size)
  }
  for (j in 1:n) {
    m <- add_constraint(m, sum_expr(x[i, j], i = 1:max_bins) == 1)
  }
  result <- solve_model(m, with_ROI(solver = "glpk"))
  expect_equal(result$objective_value, 2)
})

test_that("quantified constraints work", {
  max_bins <- 5
  bin_size <- 3
  n <- 5
  weights <- rep.int(1, n)
  m <- MIPModel()
  m <- add_variable(m, y[i], i = 1:max_bins, type = "binary")
  m <- add_variable(m, x[i, j], i = 1:max_bins, j = 1:n, type = "binary")
  m <- set_objective(m, sum_expr(y[i], i = 1:max_bins), sense = "min")
  m <- add_constraint(m,
        sum_expr(weights[j] * x[i, j], j = 1:n) <= y[i] * bin_size,
        i = 1:max_bins)
  m <- add_constraint(m, sum_expr(x[i, j], i = 1:max_bins) == 1, j = 1:n)
  result <- solve_model(m, with_ROI(solver = "glpk"))
  expect_equal(result$objective_value, 2)
})

test_that("bug 20160704: did not correctly convert constraint", {
  n <- 2
  r <- MIPModel() %>%
   add_variable(x[i, j], i = 1:n, j = 1:n, type = "binary") %>%
   add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%
   set_objective(0) %>%
   add_constraint(u[i] + 1 <= u[j] + n * (1 - x[i, j]), i = 1:n, j = 1:n) %>%
   solve_model(with_ROI(solver = "glpk"))
  expect_equal(r$status, "optimal")
})

test_that("can solve a model with variable bounds", {
  n <- 2
  r <- MIPModel() %>%
    add_variable(x[i, j], i = 1:n, j = 1:n,
                 type = "integer", lb = 0, ub = 1) %>%
    set_bounds(x[i, j], i = 1:n, j = 1:n, lb = 0, ub = 0) %>%
    set_objective(sum_expr(x[i, j], i = 1:n, j = 1:n)) %>%
    add_constraint(sum_expr(x[i, j], i = 1:n, j = 1:n) <= 10) %>%
    solve_model(with_ROI(solver = "glpk"))
  result <- get_solution(r, x[i, j])
  expect_equal(nrow(result[result$value == 1, ]), 0)
  expect_equal(nrow(result), 4)
  expect_equal(r$status, "optimal")
})

test_that("bug 20161006 #75: warning messge when setting bound on single var", {
  expect_silent({
    MIPModel() %>%
    add_variable(x, type = "integer") %>%
    add_variable(y, type = "continuous", lb = 0) %>%
    set_bounds(x, lb = 0) %>%
    set_objective(x + y, "max") %>%
    add_constraint(x + y <= 11.25) %>%
    solve_model(with_ROI(solver = "glpk"))
  })
})

test_that("bug 20161011 #82: problems with bound indexes", {
  N <- 10
  model <- MIPModel() %>%
    add_variable(x[i], lb = 0, i = 1:N) %>%
    add_variable(a[i, j, s], type = "binary", i = 1:N, j = 1:N, s = 1:3) %>%
    set_objective(0) %>%
    add_constraint(x[1] <= 1)
  solve_model(model, with_ROI("glpk"))
})

test_that("bug 20161116 #107: it works with no objective function", {
  model <- MIPModel()
  model <- add_variable(model, x, type = "continuous", lb = 11, ub = 13)
  result <- solve_model(model, with_ROI("glpk"))
  x_val <- get_solution(result, x)
  expect_true(x_val >= 11 && x_val <= 13)
})

test_that("bug 20161031 #102: model with no constraint crashes", {
  model <- MIPModel()
  model <- add_variable(model, x, type = "continuous", lb = 11, ub = 13)
  model <- set_objective(model, x, "min")
  expect_silent(result <- solve_model(model, with_ROI("glpk")))
  expect_equal(11, as.numeric(get_solution(result, x)))
})
