## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)
knitr::opts_chunk$set(cache=TRUE)
knitr::opts_knit$set(cache.extra = 234) # seed
options(warnPartialMatchArgs = FALSE,
        warnPartialMatchDollar = FALSE,
        warnPartialMatchAttr = FALSE)

## ---- cache=TRUE--------------------------------------------------------------
library(glmmboot)
data(test_data)

head(test_data)

## ---- cache=TRUE--------------------------------------------------------------
library(glmmTMB)
model_formula <- as.formula(y ~ x_var1 + x_var2 + x_var2 + (1 | subj))

base_run <- glmmTMB(formula = model_formula,
                    data = test_data,
                    family = binomial)

## ---- cache=TRUE--------------------------------------------------------------
bootstrap_over_subj <- bootstrap_model(base_model = base_run,
                                       base_data = test_data,
                                       resamples = 99)

## ---- cache=TRUE--------------------------------------------------------------
print(bootstrap_over_subj)

## ---- cache=TRUE--------------------------------------------------------------
b_list1 <- bootstrap_model(base_model = base_run,
                           base_data = test_data,
                           resamples = 29,
                           return_coefs_instead = TRUE)
b_list2 <- bootstrap_model(base_model = base_run,
                           base_data = test_data,
                           resamples = 30,
                           return_coefs_instead = TRUE)
b_list3 <- bootstrap_model(base_model = base_run,
                           base_data = test_data,
                           resamples = 30,
                           return_coefs_instead = TRUE)

## ---- cache=TRUE--------------------------------------------------------------
print(combine_resampled_lists(b_list1, b_list2, b_list3))

## ---- cache=TRUE--------------------------------------------------------------
list_of_lists_output <- list(b_list1, b_list2, b_list3)

## ---- cache=TRUE--------------------------------------------------------------
print(combine_resampled_lists(list_of_lists_output))

## ---- eval=FALSE--------------------------------------------------------------
#  ## will use `parallel::detectCores() - 1` cores
#  model_results <- bootstrap_model(base_model = some_base_run,
#                                   base_data = some_interesting_data,
#                                   resamples = 9999,
#                                   parallelism = "parallel")
#  
#  ## will use 4 cores:
#  model_results <- bootstrap_model(base_model = some_base_run,
#                                   base_data = some_interesting_data,
#                                   resamples = 9999,
#                                   parallelism = "parallel",
#                                   num_cores = 4)

## ---- eval=FALSE--------------------------------------------------------------
#  library(future)
#  plan("multiprocess") # "multiprocess" should work across Windows / Mac / Linux
#  
#  model_results <- bootstrap_model(base_model = some_base_run,
#                                   base_data = some_interesting_data,
#                                   resamples = 9999,
#                                   parallelism = "future",
#                                   future_packages = "glmmTMB")

## ---- eval=FALSE--------------------------------------------------------------
#  model_results <- bootstrap_model(base_model = some_base_run,
#                                   base_data = some_interesting_data,
#                                   resamples = 9999,
#                                   num_cores = 8)

## ---- cache=TRUE--------------------------------------------------------------
log_remaining <- function(start_time,
                          cur_time,
                          j,
                          total_iters,
                          time_units = "hours"){
    cur_time <- Sys.time()
    total_time <- difftime(cur_time, start_time, units = time_units)
    est_remaining <- (total_iters - j) * (total_time / j)
    paste0("[", cur_time, "] [iteration ", j, "] ",
           "total ", time_units, ": ", round(total_time, 3), " // ",
           "est remaining ", time_units, ": ", round(est_remaining, 3))
}

## ---- cache=TRUE--------------------------------------------------------------
total_iterations <- 5
start_time <- Sys.time()
for (j in 1:total_iterations) {
    ## simulate expensive operation...
    Sys.sleep(2)

    print(log_remaining(start_time, Sys.time(), j, total_iterations, "secs"))
}

## ---- eval=FALSE--------------------------------------------------------------
#  library(future)
#  plan("multiprocess")
#  
#  results_list <- list()
#  num_blocks <- 50
#  runs_per_block <- 100
#  
#  start_time <- Sys.time()
#  for (j in 1:num_blocks) {
#      results_list[[j]] <- bootstrap_model(base_model = base_run,
#                                           base_data = test_data,
#                                           resamples = runs_per_block,
#                                           parallelism = "future",
#                                           return_coefs_instead = TRUE,
#                                           suppress_sampling_message = TRUE)
#      print(log_remaining(start_time, Sys.time(), j, num_blocks, "secs"))
#  }
#  
#  combined_results <- combine_resampled_lists(results_list)

## ---- eval=FALSE--------------------------------------------------------------
#  owls <- transform(Owls,
#                    nest = reorder(Nest, NegPerChick),
#                    ncalls = SiblingNegotiation,
#                    ft = FoodTreatment)
#  
#  fit_zipoisson <- glmmTMB(
#      ncalls ~ (ft + ArrivalTime) * SexParent +
#          offset(log(BroodSize)) + (1 | nest),
#      data = owls,
#      ziformula = ~1,
#      family = poisson)

## ---- eval=FALSE--------------------------------------------------------------
#  zero_boot <- bootstrap_model(base_model = fit_zipoisson,
#                               base_data = owls,
#                               resamples = 9999)

## ---- fig.show='hold'---------------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(mtcars, 10))

