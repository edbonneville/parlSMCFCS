#' Run smcfcs in parallel
#'
#' @param seed Optional seed
#' @param m Number of imputed datasets
#' @param n_cores Number of cores over which to split the m imputations
#' @param m_per_core Optional integer of number of imputations per core.
#' Default is `floor(m / n_cores)`, which it cannot be larger than.
#' @param cl_type Either "PSOCK" or "FORK"
#' @param outfile Optional character path to location for
#' output from the workers. Useful to diagnose rejection sampling warnings.
#' File path must end with output-filenames.txt
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
                       m_per_core = NULL,
                       n_cores = parallel::detectCores() - 1,
                       cl_type = "PSOCK",
                       outfile = "",
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
  checkmate::assert_int(x = m_per_core, lower = 1, upper = floor(m / n_cores), null.ok = TRUE)
  checkmate::matchArg(x = cl_type, choices = c("PSOCK", "FORK"))
  checkmate::assert_int(x = n_cores, lower = 1, upper = min(parallel::detectCores(), m))
  if (outfile != "") checkmate::assert_path_for_output(x = outfile)

  # Standard smcfcs if n_cores = 1
  if (n_cores == 1) {
    if (!is.null(seed)) set.seed(seed)
    args$m <- m
    res <- do.call(smcfcs::smcfcs, args)

  } else {

    # Determine number of imputations per core
    imp_specs <- determine_imp_specs(n_cores, m, m_per_core)

    # Set up the cluster
    cl <- parallel::makeCluster(n_cores, type = cl_type, outfile = outfile)
    if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)

    parallel::clusterExport(
      cl = cl,
      varlist = c("args", "imp_specs", "seed", "m", "n_cores", "cl_type",
                  "Surv", "smcfcs"),
      envir = environment()
    )

    # Run the imputations
    pbapply::pboptions(type = "txt")
    imps <- pbapply::pblapply(cl = cl, X = 1:length(imp_specs), function(x) {
      args$m <- imp_specs[x]
      do.call(smcfcs::smcfcs, args)
    })

    parallel::stopCluster(cl)

    # Combine imputations
    res <- combine_smcfcs_objects(imps)
  }

  return(res)
}


# Prepare imputations per core
determine_imp_specs <- function(n_cores,
                                m,
                                m_per_core) {

  if (!is.null(m_per_core)) {
    imp_specs <- rep(m_per_core, times = floor(m / m_per_core))
    modul <- m %% m_per_core
  } else {
    imp_specs <- rep(floor(m / n_cores), times = n_cores)
    modul <- m %% n_cores
  }

  # Add remaining imps to add to m
  if (modul != 0) imp_specs[length(imp_specs)] <- imp_specs[length(imp_specs)] + modul
  return(imp_specs)
}


# Helper to combine smcfcs objects
combine_smcfcs_objects <- function(smcfcs_list) {

  # Combine imputed datasets
  ls_impdats <- do.call("c",  lapply(smcfcs_list, "[[", "impDatasets"))

  # Combine monitoring of imputations
  coef_array <- abind::abind(lapply(smcfcs_list, "[[", "smCoefIter"), along = 1)

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

