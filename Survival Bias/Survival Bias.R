if (!require("pacman")) install.packages("pacman")

if (!require("artyfarty")) pacman::p_install_gh("datarootsio/artyfarty")

pacman::p_load(tidyverse, sampleSelection, rsimsum, parallel, 
               doFuture, future.apply, truncreg, artyfarty)

### define core variables ###

age_factor_005 <- -.005
age_factor_01 <- -.01
age_factor_02 <- -.02

min_age <- 31

mean_OPS  <- .740 ### <- approx weighted mean non-pitcher MLB OPS from 2010-2019
sd_OPS <- .08 ### <- approx weighted SD non-pitcher MLB OPS from 2010-2019 for 31-35YO  

dropout_delta_40 <- .040
dropout_delta_100 <- .100
dropout_delta_200 <- .200
dropout_delta_300 <- .300

##### parallel simulation: specs and setup #####

registerDoFuture()
plan(multisession, workers = detectCores() - 1)
getDoParWorkers()

n_sims <- 5e4 ### hopefully you have a lot of cores; if not, 1e4+ should be in the ballpark

########################################  
##### apologies for the lack of repeatable functions; this was an iterative process
#####
##### if you wish to check an ascending age slope, 
## flip signs on age factor above so it is positive and simulate ages 23 to 27 instead; 
## truncation should be set closer to the 5% to 10% range
## results appear to be broadly similar with respect to bias / bias avoidance
########################################   

################# simulations for .005 age factor ##################

##### 40 points, .005 aging factor #####

sys.time.start <- Sys.time()

set.seed(1234)

delta_40_age_005 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005 - dropout_delta_40, 
      pool = 0)
  
  ### model gaps, 40 points of OPS ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_40_age_005, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_40_age_005, function(t) t$mod.surv)

delta_40_age_005_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_005),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_005)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_005),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_005)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_40_age_005_mods

##### 100 points, .005 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_100_age_005 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005 - dropout_delta_100, 
      pool = 0)
  
  ### model gaps, 100 points of OPS, .005 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_100_age_005, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_100_age_005, function(t) t$mod.surv)

delta_100_age_005_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_005),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_005)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_005),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_005)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_100_age_005_mods

##### 200 points, .005 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_200_age_005 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005 - dropout_delta_200, 
      pool = 0)
  
  ### model gaps, 200 points of OPS, .005 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_200_age_005, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_200_age_005, function(t) t$mod.surv)

delta_200_age_005_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_005),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_005)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_005),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_005)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_200_age_005_mods

#####  300 points, .005 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_300_age_005 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005 - dropout_delta_300, 
      pool = 0)
  
  ### model gaps, 300 points of OPS, .005 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_300_age_005, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_300_age_005, function(t) t$mod.surv)

delta_300_age_005_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_005),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_005)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_005),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_005)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_300_age_005_mods

################# simulations for .01 age factor ##################

##### 40 points, .005 aging factor #####

sys.time.start <- Sys.time()

set.seed(1234)

delta_40_age_01 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01 - dropout_delta_40, 
      pool = 0)
  
  ### model gaps, 40 points of OPS ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_40_age_01, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_40_age_01, function(t) t$mod.surv)

delta_40_age_01_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_01),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_01)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_01),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_01)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_40_age_01_mods

##### 100 points, .01 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_100_age_01 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01 - dropout_delta_100, 
      pool = 0)
  
  ### model gaps, 100 points of OPS, .01 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_100_age_01, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_100_age_01, function(t) t$mod.surv)

delta_100_age_01_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_01),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_01)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_01),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_01)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_100_age_01_mods

##### 200 points, .01 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_200_age_01 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01 - dropout_delta_200, 
      pool = 0)
  
  ### model gaps, 200 points of OPS, .01 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_200_age_01, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_200_age_01, function(t) t$mod.surv)

delta_200_age_01_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_01),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_01)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_01),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_01)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_200_age_01_mods

#####  300 points, .01 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_300_age_01 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01 - dropout_delta_300, 
      pool = 0)
  
  ### model gaps, 300 points of OPS, .01 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_300_age_01, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_300_age_01, function(t) t$mod.surv)

delta_300_age_01_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_01),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_01)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_01),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_01)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_300_age_01_mods

################# simulations for .02 age factor ##################

##### 40 points, .02 aging factor #####

sys.time.start <- Sys.time()

set.seed(1234)

delta_40_age_02 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02 - dropout_delta_40, 
      pool = 0)
  
  ### model gaps, 40 points of OPS ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_40_age_02, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_40_age_02, function(t) t$mod.surv)

delta_40_age_02_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_02),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_02)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_02),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_02)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_40_age_02_mods

##### 100 points, .02 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_100_age_02 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02 - dropout_delta_100, 
      pool = 0)
  
  ### model gaps, 100 points of OPS, .02 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_100_age_02, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_100_age_02, function(t) t$mod.surv)

delta_100_age_02_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_02),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_02)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_02),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_02)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_100_age_02_mods

##### 200 points, .02 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_200_age_02 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02 - dropout_delta_200, 
      pool = 0)
  
  ### model gaps, 200 points of OPS, .02 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_200_age_02, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_200_age_02, function(t) t$mod.surv)

delta_200_age_02_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_02),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_02)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_02),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_02)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_200_age_02_mods

#####  300 points, .02 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

delta_300_age_02 <- future_lapply(seq.int(n_sims), function(x) {
  
  survivors <- tibble(
    perf_id = seq.int(1:2000)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02, 
      pool = 1)
  
  dropouts <- tibble(
    perf_id = seq.int(2001:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02 - dropout_delta_300, 
      pool = 0)
  
  ### model gaps, 300 points of OPS, .02 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=dplyr::bind_rows(survivors, dropouts))
  mod.surv <- lm(expected_OPS ~ age, data=survivors)
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2]
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(delta_300_age_02, function(t) t$mod.all)
mod.surv.vec <- sapply(delta_300_age_02, function(t) t$mod.surv)

delta_300_age_02_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_02),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_02)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_02),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_02)^2)
) %>%
  dplyr::mutate_all(round,5)

delta_300_age_02_mods


################# simulation for truncation ##################

##### truncation, .005 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

trunc_age_005 <- future_lapply(seq.int(n_sims), function(x) {
  
  perf_id.samp <- tibble(
    perf_id = seq.int(1:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_005) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pool = if_else(expected_OPS > quantile(expected_OPS, probs = .2), 1, 0)
    )
  
  ### model trunc .005 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=perf_id.samp)
  mod.surv <- lm(expected_OPS ~ age, data=perf_id.samp %>% dplyr::filter(pool == 1))
  mod.trunc.surv <- truncreg(expected_OPS ~ age, data=perf_id.samp %>% dplyr::filter(pool == 1),
                             point = quantile(perf_id.samp$expected_OPS, probs = .2), direction = "left")
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2],
       "mod.trunc.surv" = mod.trunc.surv$coefficients[2]
       
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(trunc_age_005, function(t) t$mod.all)
mod.surv.vec <- sapply(trunc_age_005, function(t) t$mod.surv)
mod.trunc.surv.vec <- sapply(trunc_age_005, function(t) t$mod.trunc.surv)

trunc_age_005_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_005),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_005)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_005),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_005)^2),
  trunc_survivors_only_bias = mean(mod.trunc.surv.vec - age_factor_005),
  trunc_survivors_only_MSE = mean((mod.trunc.surv.vec - age_factor_005)^2)
) %>%
  dplyr::mutate_all(round,5)

trunc_age_005_mods

##### truncation, .01 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

trunc_age_01 <- future_lapply(seq.int(n_sims), function(x) {
  
  perf_id.samp <- tibble(
    perf_id = seq.int(1:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pool = if_else(expected_OPS > quantile(expected_OPS, probs = .2), 1, 0)
    )
  
  ### model trunc .01 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=perf_id.samp)
  mod.surv <- lm(expected_OPS ~ age, data=perf_id.samp %>% dplyr::filter(pool == 1))
  mod.trunc.surv <- truncreg(expected_OPS ~ age, data=perf_id.samp %>% dplyr::filter(pool == 1),
                             point = quantile(perf_id.samp$expected_OPS, probs = .2), direction = "left")
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2],
       "mod.trunc.surv" = mod.trunc.surv$coefficients[2]
       
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(trunc_age_01, function(t) t$mod.all)
mod.surv.vec <- sapply(trunc_age_01, function(t) t$mod.surv)
mod.trunc.surv.vec <- sapply(trunc_age_01, function(t) t$mod.trunc.surv)

trunc_age_01_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_01),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_01)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_01),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_01)^2),
  trunc_survivors_only_bias = mean(mod.trunc.surv.vec - age_factor_01),
  trunc_survivors_only_MSE = mean((mod.trunc.surv.vec - age_factor_01)^2)
) %>%
  dplyr::mutate_all(round,5)

trunc_age_01_mods

##### truncation, .02 aging factor ##### 

sys.time.start <- Sys.time()

set.seed(1234)

trunc_age_02 <- future_lapply(seq.int(n_sims), function(x) {
  
  perf_id.samp <- tibble(
    perf_id = seq.int(1:2500)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      age = sample(seq.int(31,35), 1, replace=TRUE),
      expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pool = if_else(expected_OPS > quantile(expected_OPS, probs = .2), 1, 0)
    )
  
  ### model trunc .02 aging factor ###
  
  mod.all <- lm(expected_OPS ~ age, data=perf_id.samp)
  mod.surv <- lm(expected_OPS ~ age, data=perf_id.samp %>% dplyr::filter(pool == 1))
  mod.trunc.surv <- truncreg(expected_OPS ~ age, data=perf_id.samp %>% dplyr::filter(pool == 1),
                             point = quantile(perf_id.samp$expected_OPS, probs = .2), direction = "left")
  
  list("mod.all" = mod.all$coefficients[2],
       "mod.surv" = mod.surv$coefficients[2],
       "mod.trunc.surv" = mod.trunc.surv$coefficients[2]
       
  )
  
}, future.seed = TRUE)

Sys.time() - sys.time.start

### summarize results ###

mod.all.vec <- sapply(trunc_age_02, function(t) t$mod.all)
mod.surv.vec <- sapply(trunc_age_02, function(t) t$mod.surv)
mod.trunc.surv.vec <- sapply(trunc_age_02, function(t) t$mod.trunc.surv)

trunc_age_02_mods <- dplyr::tibble(
  all_perf_ids_bias = mean(mod.all.vec - age_factor_02),
  all_perf_ids_MSE = mean((mod.all.vec - age_factor_02)^2),
  survivors_only_bias = mean(mod.surv.vec - age_factor_02),
  survivors_only_MSE = mean((mod.surv.vec - age_factor_02)^2),
  trunc_survivors_only_bias = mean(mod.trunc.surv.vec - age_factor_02),
  trunc_survivors_only_MSE = mean((mod.trunc.surv.vec - age_factor_02)^2)
) %>%
  dplyr::mutate_all(round,5)

trunc_age_02_mods


############## shut down future ################

plan(sequential)

############## Visualizations ################

### mixture distribution, densities plot ###

set.seed(1234)

x <- tibble(survivors = rnorm(1e5, .740, .08), dropouts = rnorm(1e5, .740 - .200, .08))
df <- pivot_longer(x, cols = names(x), names_to = "Pool", values_to = "Expected_OPS")

ggplot(df, aes(x=Expected_OPS, fill = Pool)) + 
  geom_density(alpha=.5) + theme_empty() +
  ggtitle("Performance Pools, 200 OPS Gap")

### dot plots of mixture versus truncated distribution ###

set.seed(1234)

survivors.plot <- tibble(
  perf_id = seq.int(1:2000)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    age = sample(seq.int(31,35), 1, replace=TRUE),
    expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01, 
    pool = "survivors")

dropouts.plot <- tibble(
  perf_id = seq.int(2001:2500)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    age = sample(seq.int(31,35), 1, replace=TRUE),
    expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_01 - dropout_delta_200, 
    pool = "dropouts")

set.seed(1234)

perf_id.samp.plot <- tibble(
  perf_id = seq.int(1:2500)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    age = sample(seq.int(31,35), 1, replace=TRUE),
    expected_OPS = rnorm(1, mean_OPS, sd=sd_OPS) + (age - min_age) * age_factor_02) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    pool = if_else(expected_OPS > quantile(expected_OPS, probs = .2), "survivors", "dropouts")
  )

ggplot(bind_rows(survivors.plot, dropouts.plot), aes(age, expected_OPS)) + 
  geom_jitter(aes(color = pool), height = 0, width = .1) + theme_empty() +
  ggtitle ("Mixture Distribution, 200 OPS gap, 10 point annual age penalty")

ggplot(perf_id.samp.plot, aes(age, expected_OPS)) + 
  geom_jitter(aes(color = pool), height = 0, width = .1) + theme_empty() +
  geom_hline(yintercept = quantile(perf_id.samp.plot$expected_OPS, probs = .2)) +
  ggtitle("Truncated Distribution, 80/20 split, 10 point annual age penalty")
