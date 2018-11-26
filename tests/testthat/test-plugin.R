
test_that("can solve simple lp problem", {
  LP <- ROI::OP(c(2, 4, 3),
            ROI::L_constraint(
               L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
               dir = c("<=", "<=", "<="),
               rhs = c(60, 40, 80)),
            max = TRUE )
  result <- ROI::ROI_solve(LP, "cbc")
  expect_equal(76.66667, result$objval, tolerance = .003)
  expect_equal(c(0, 6.666667, 16.666667), result$solution, tolerance = .003)
})

test_that("can solve simple lp problem with lower row bounds", {
  LP <- ROI::OP(c(2, 4, 3),
                 ROI::L_constraint(
                   L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
                   dir = c("<=", ">=", "<="),
                   rhs = c(60, 40, 80)),
                 max = TRUE )
  result <- ROI::ROI_solve(LP, "cbc")
  expect_equal(90, result$objval)
  expect_equal(c(0, 0, 30), result$solution)
})

test_that("can solve simple IP problem with lower row bounds", {
  IP <- ROI::OP(c(2, 4, 3),
                ROI::L_constraint(
                  L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
                  dir = c("<=", ">=", "<="),
                  rhs = c(60, 40, 80)),
                 types = c("I", "I", "B"),
                 max = TRUE )
  result <- ROI::ROI_solve(IP, "cbc")
  expect_equal(41, result$objval)
  expect_equal(c(19, 0, 1), result$solution)
})

test_that("supports equality constraints", {
  IP <- ROI::OP(c(2),
                 ROI::L_constraint(L = matrix(1, nrow = 1),
                                   dir = "==",
                                   rhs = 60),
                 types = c("I"),
                 max = TRUE )
  result <- ROI::ROI_solve(IP, "cbc")
  expect_equal(120, result$objval)
  expect_equal(60, result$solution)
})

test_that("can solve simple IP problem with lower col bounds and lower columns bounds", {
  IP <- ROI::OP(c(2, 4, 3),
                ROI::L_constraint(
                  L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
                  dir = c("<=", ">=", "<="),
                  rhs = c(60, 40, 80)),
                types = c("I", "I", "B"),
                max = TRUE )
  bounds(IP) <- V_bound(li=1:3, lb=c(20,-1,1))
  result <- ROI::ROI_solve(IP, "cbc")
  expect_equal(39, result$objval)
  expect_equal(c(20, -1, 1), result$solution)
})

test_that("can solve simple lp problem with lower row bounds and lower columns bounds", {
  LP <- ROI::OP(c(2, 4, 3),
                ROI::L_constraint(
                  L = matrix(c(3, 2, 1, 4, 1, 3, 2, 2, 2), nrow = 3),
                  dir = c("<=", ">=", "<="),
                  rhs = c(60, 40, 80)),
                max = TRUE )
  bounds(LP) <- V_bound(li=1:3, lb=c(10,0,0))
  result <- ROI::ROI_solve(LP, "cbc")
  expect_equal(65, result$objval)
  expect_equal(c(10, 0, 15), result$solution)
})

