# Benchmarking - run these on slurm.. with ntasks = 3

# Function to benchmark
# Load packages
devtools::load_all()

# Prepare benchmark function
bench_smcfcs <- function(m, n_cores) {

  parlSMCFCS::parlsmcfcs(
    seed = m, # use m as seed, for seq, 2 cores and 3 cores will still be different result
    n_cores = n_cores, 
    originaldata = smcfcs::ex_compet,
    cl_type = "FORK", # Running on centos slurm cluster
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

# Run benchmarks
benches <- purrr::map_dfr(
  .x = m,
  .f = ~ {

    # Benchmark
    bench <- microbenchmark::microbenchmark(
      "Sequential" = bench_smcfcs(m = .x, n_cores = 1),
      "2 cores" = bench_smcfcs(m = .x, n_cores = 2),
      "3 cores" = bench_smcfcs(m = .x, n_cores = 3),
      times = 5 # Set to higher number for more reliable timings, and assessment of variability
    )

    # Bind to summarise
    res <- cbind.data.frame(
      "Cores" = summary(bench)$expr,
      "median_time" = summary(bench)$median,
      "mean_time" = summary(bench)$mean,
      "m" = .x
    )

    return(res)
  }
)

# Save file for plotting in readme
saveRDS(benches, "benchmarking/timings_data.rds")
