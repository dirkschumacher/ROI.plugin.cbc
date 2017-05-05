#' ROI plugin for COIN CBC
#'
#' @docType package
#' @keywords package
#' @name ROI.plugin.cbc
NULL

#' @noRd
solver_name <- ROI::.ROI_plugin_get_solver_name(getPackageName())

#' @noRd
solver <- function(x, control) {
  obj <- as.numeric(as.matrix(stats::terms(ROI::objective(x))[["L"]]))

  # convert verbose control to cbc logLevel
  # TODO: check how the control mechanism really works
  verbose_in_control <- !is.null(control[["verbose"]])
  if (verbose_in_control) {
    control$logLevel <- if (control[["verbose"]]) 1 else 0
    control$verbose <- NULL
  }
  n_cols <- length(obj)

  # build column bounds
  # 0 as lb is the default in ROI
  col_lb <- rep.int(0, n_cols)
  col_ub <- rep.int(Inf, n_cols)

  col_types <- ROI::types(x)
  if (!is.null(col_types)) {
    stopifnot(length(col_types) == n_cols)
    col_ub[col_types == "B"] <- 1
    is_integer <- col_types %in% c("I", "B")
  } else {
    is_integer <- rep.int(FALSE, n_cols)
  }

  # build row bounds
  constraints <- ROI::constraints(x)
  row_dir <- constraints$dir
  row_ub <- constraints$rhs
  row_ub[row_dir == ">="] <- Inf
  row_lb <- constraints$rhs
  row_lb[row_dir == "<="] <- -Inf

  mat <- constraints$L
  mat <- Matrix::sparseMatrix(mat$i, mat$j, x = mat$v)
  result <- rcbc::cbc_solve(obj = obj,
                         mat = mat,
                         row_lb = row_lb,
                         row_ub = row_ub,
                         col_lb = col_lb,
                         col_ub = col_ub,
                         max = x$maximum,
                         is_integer = is_integer,
                         cbc_args = control)

  status_codes <- c(
    "optimal" = 5L,
    "infeasible" = 3L,
    "unbounded" = 6L,
    "nodelimit" = 7L,
    "solutionlimit" = 8L,
    "abandoned" = 9L,
    "iterationlimit" = 10L
  )
  status_code <- status_codes[rcbc::solution_status(result)]
  ROI::.ROI_plugin_canonicalize_solution(
    solution = rcbc::column_solution(result),
    optimum = rcbc::objective_value(result),
    status = status_code,
    solver = solver_name)
}

.onLoad <- function(libname, pkgname) {
  solver_not_available <- !pkgname %in% ROI::ROI_registered_solvers()
  if (solver_not_available) {
    solver_signature <- ROI::.ROI_plugin_make_signature(objective = "L",
                                                       constraints = "L",
                                                       types = c("C", "I", "B",
                                                                 "CI", "CB",
                                                                 "IB", "CIB"),
                                                       bounds = c("X", "V"),
                                                       maximum = c(TRUE,
                                                                   FALSE))
    ROI::.ROI_plugin_get_solver_name(pkgname)
    ROI::.ROI_plugin_register_solver_method(signatures = solver_signature,
                                        solver = solver_name,
                                        method = solver)
    # from here
    # https://projects.coin-or.org/CoinBinary/export/1059/OptimizationSuite/
    # trunk/Installer/files/doc/cbcCommandLine.pdf
    # pre processing
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "prep",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "passp",
                                        "X")

    # Cut Generation
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "cuts",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "clique",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "lift",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "mixed",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "two",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "knapsack",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "flow",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "probing",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "residual",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "cutD",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "cutL",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "passC",
                                        "X")

    # heuristics

    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "heur",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "round",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "feas",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "passF",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "local",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "pivotAndC",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "pivotAndF",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "combine",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "Combine2",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "rins",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "rens",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "vnd",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "divingG",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "divingP",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "divingF",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "divingS",
                                        "X")

    # limits
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                         "sec",
                                         "max_time")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "maxN",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "maxS",
                                        "X")
    ROI::.ROI_plugin_register_solver_control(solver_name,
                                        "logLevel",
                                        "verbosity_level")


    ROI::.ROI_plugin_add_status_code_to_db(solver_name,
                                      5L,
                                      "optimal",
                                      "Solution is optimal.",
                                      0L
    )
    ROI::.ROI_plugin_add_status_code_to_db(solver_name,
                                      3L,
                                      "infeasible",
                                      "Solution is infeasible."
    )
    ROI::.ROI_plugin_add_status_code_to_db(solver_name,
                                      6L,
                                      "unbounded",
                                      "Solution is unbounded."
    )
    ROI::.ROI_plugin_add_status_code_to_db(solver_name,
                                      7L,
                                      "nodelimit",
                                      "Node limit reached."
    )
    ROI::.ROI_plugin_add_status_code_to_db(solver_name,
                                      8L,
                                      "solutionlimit",
                                      "Solution limit reached.",
                                      0L
    )
    ROI::.ROI_plugin_add_status_code_to_db(solver_name,
                                      9L,
                                      "abandoned",
                                      "Numerical instability."
    )
    ROI::.ROI_plugin_add_status_code_to_db(solver_name,
                                      10L,
                                      "iterationlimit",
                                      "Iteration limit reached."
    )
  }
}
