# Benchmarking - run these on slurm.. with ntasks = 3

# Function to benchmark
bench_smcfcs <- function(m, n_cores) {

  parlSMCFCS::parlsmcfcs(
    seed = m, # use m as seed
    n_cores = n_cores, # number of cores - 1
    originaldata = smcfcs::ex_compet,
    cl_type = "FORK", # since running on centos slurm cluster
    m = m,
    smtype = "compet",
    smformula = list(
      "Surv(t, d == 1) ~ x1 + x2",
      "Surv(t, d == 2) ~ x1 + x2"
    ),
    method = c("", "", "norm", "norm")
  )
}

# Start from 3
m <- c(3, 5, 10, 15, 25, 50, 100)

benches <- purrr::map_dfr(
  .x = m,
  .f = ~ {

    # Benchmark
    bench <- microbenchmark::microbenchmark(
      "Sequential" = bench_smcfcs(m = .x, n_cores = 1),
      "2 cores" = bench_smcfcs(m = .x, n_cores = 2),
      "3 cores" = bench_smcfcs(m = .x, n_cores = 3),
      times = 5 # Set to higher number for more reliable timings
    )

    # Bind to summarise
    res <- cbind.data.frame(
      "Cores" = summary(bench)$expr,
      "Time" = summary(bench)$median,
      "m" = .x
    )

    return(res)
  }
)

# Save file for plotting in readme
saveRDS(benches, "benchmarking/timings_data.rds")
