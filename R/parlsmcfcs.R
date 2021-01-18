#' Run smcfcs in parallel
#'
#' @param seed Optional seed
#' @param m Number of imputed datasets
#' @param n_cores Number of cores over which to split the m imputations
#' @param cl_type Either "PSOCK" or "FORK"
#' @param ... Additional arguments to pass on to smcfcs::smcfcs
#'
#' @return An object of type "smcfcs"
#' @export
#'
#' @importFrom survival Surv
#' @importFrom smcfcs smcfcs
#'
#' @examples
#' # Example here later...
parlsmcfcs <- function(seed = NULL,
                       m = 5,
                       n_cores = parallel::detectCores() - 1,
                       cl_type = "PSOCK",
                       ...) {

  # Check smcfcs arguments
  args <- list(...)
  args_smcfcs <- names(formals(smcfcs::smcfcs))
  check_args <- !(names(args) %in% args_smcfcs)

  if (any(check_args)) {
    wrong_args <- paste(names(args)[check_args], collapse = ", ")
    mssg <- paste0("The following are not valid arguments of smcfcs::smcfcs : ",
                   wrong_args)
    stop(mssg)
  }

  # Check parallel arguments
  checkmate::assert_numeric(x = seed, null.ok = TRUE, any.missing = FALSE, len = 1)
  checkmate::assert_int(x = m, lower = 1)
  checkmate::matchArg(x = cl_type, choices = c("PSOCK", "FORK"))
  checkmate::assert_int(x = n_cores, lower = 1, upper = min(parallel::detectCores(), m))

  # Standard smcfcs if n_cores = 1
  if (n_cores == 1) {
    if (!is.null(seed)) set.seed(seed)
    args$m <- m
    res <- do.call(smcfcs::smcfcs, args)

  } else {

    # Determine number of imputations per core
    modul <- m %% n_cores
    imp_specs <- rep(floor(m / n_cores), times = n_cores)

    # Edit m for last core by adding remainder
    if (modul != 0) imp_specs[length(imp_specs)] <- imp_specs[length(imp_specs)] + modul

    # Set up the cluster
    cl <- parallel::makeCluster(n_cores, type = cl_type)
    if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)

    parallel::clusterExport(
      cl = cl,
      varlist = c("args", "imp_specs", "seed", "m", "n_cores", "cl_type",
                  "Surv", "smcfcs"),
      envir = environment()
    )

    # Run the imputations
    imps <- parallel::parLapply(cl = cl, X = 1:n_cores, function(x) {
      args$m <- imp_specs[x]
      do.call(smcfcs::smcfcs, args)
    })

    parallel::stopCluster(cl)

    # Combine imputations
    res <- combine_smcfcs_objects(imps)
  }

  return(res)
}


combine_smcfcs_objects <- function(smcfcs_list) {

  # Combine imputed datasets
  ls_impdats <- Reduce(x = lapply(smcfcs_list, "[[", "impDatasets"), f = "c")

  # Combine monitoring of imputations
  coef_array <- Reduce(
    x = lapply(smcfcs_list, "[[", "smCoefIter"),
    f = function(a1, a2) {
      total_m <- dim(a1)[1] + dim(a2)[1]
      new_array <- array(c(a1, a2), dim = c(total_m, dim(a1)[-1]))
      return(new_array)
    }
  )

  # Polish and return
  res <- list(
    impDatasets = ls_impdats,
    smCoefIter = coef_array,
    smInfo = list(
      smtype = smcfcs_list[[1]]$smInfo$smtype,
      smformula = smcfcs_list[[1]]$smInfo$smformula
    )
  )

  class(res) <- "smcfcs"
  return(res)
}
