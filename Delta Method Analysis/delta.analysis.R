########## Load packages and set preferences ##########

if (!require("pacman")) install.packages("pacman")

if (!require("artyfarty")) pacman::p_install_gh("datarootsio/artyfarty")

pacman::p_load(tidyverse,artyfarty,ggthemes,
               mgcv,radiant.data,doFuture,future.apply)

Sys.setenv(TZ='GMT')

options(dplyr.summarise.inform=FALSE)

########## Define defaults for data acquisition and ordering ##########

y.current <- lubridate::year(Sys.Date()) # most recent season

season_min <- 1
PA_min <- 1

best_dir <- "high"


########## Load data, OPS from 1977 through 2019 ##########

df.1 <- read_csv("delta_df.csv") %>%
  select(-c("X1"))
  
### centering function, if desired ###

stat_sel <- function(df=df.1, rate_stat = OPS, stat_denom=PA) {
  
  rate_stat <- enquo(rate_stat)
  stat_denom <- enquo(stat_denom)
  
  df.11 <- df %>%
    dplyr::group_by(year) %>%
    # dplyr::mutate(stat_sel = (!!rate_stat)) %>%
    dplyr::mutate(stat_sel = (!!rate_stat) - weighted.mean((!!rate_stat), (!!stat_denom))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(bb_age = 
                       round(as.vector((ymd(paste(year, 07, 01, sep="-")) - 
                                          ymd(birthdate)) / 365), 0))
  
  df.12 <- df.11 %>%
    dplyr::select(year, batter, bb_age, (!!rate_stat), (!!stat_denom), stat_sel)
  
  return(df.12)
  
}


### hold-out-careers function ###


hold_out_careers <- function(df = df_stat, n_careers = 100){
  
  n_careers <- enquo(n_careers)
  
  df.31 <- df %>%
    dplyr::select(batter) %>%
    dplyr::distinct() %>%
    dplyr::sample_n(., size=(!!n_careers), replace=FALSE)
  
  return(df.31)
  
}


### career stats function ###

career_stats <- function(df = df_stat, PA_min = 1, 
                         career_test = df_test_career,
                         test_year = 2017){
  
  enquo(test_year)
  
  df.41 <- df %>%
    dplyr::filter(PA >= PA_min[1]) %>%
    dplyr::filter(year < (!!test_year)) %>%
    dplyr::anti_join(career_test, by=c("batter"))
  
    if(best_dir == "high"){
      
      df.42 <- df.41 %>%
        dplyr::group_by(batter) %>%
        dplyr::mutate(best_stat = max(stat_sel))
      
    }  else {
      
      df.42 <- df.41 %>%
        dplyr::group_by(batter) %>%
        dplyr::mutate(best_stat = min(stat_sel))
    }
  
  df.43 <- df.42 %>%
    dplyr::mutate(
      first_year_mlb = min(year),
      first_year_age = ifelse(first_year_mlb == year, bb_age, 0),
      last_year_mlb = max(year),
      last_year_age = ifelse(last_year_mlb == year, bb_age, 0),
      stat_peak = ifelse(best_stat == stat_sel, 1, 0),
      peak_at_age = ifelse(stat_peak == 1, bb_age, 0),
      lag_stat = lag(stat_sel),
      lag_PA = lag(PA),
      delta_stat = stat_sel - lag_stat) %>%
    dplyr::summarise(seasons = n(),
                     first_year_age = max(first_year_age),
                     last_year_age = max(last_year_age),
                     last_year_mlb = max(last_year_mlb),
                     career_mean = weighted.mean(stat_sel, PA),
                     career_PA = sum(PA),
                     career_best_stat = max(best_stat),
                     stat_age_peak = max(peak_at_age),
                     mean_delta = mean(delta_stat, na.rm=TRUE), .groups = "drop")
  
  return(df.43)
}

### intra_player function ###

intra_player <- function(df = df_stat,
                         test_year = 2017){
  
  enquo(test_year)

df.51 <- df %>% 
  dplyr::filter(year < (!!test_year)) %>%
  dplyr::anti_join(df_test_careers, by=c("batter")) %>%
  dplyr::arrange(batter, year) %>% 
  dplyr::group_by(batter) %>% 
  dplyr::mutate(lag_stat = lag(stat_sel),
                lag_PA = lag(PA),
                delta_stat = stat_sel - lag_stat,
                min_PA = if_else(PA <= lag_PA, PA, lag_PA), 
                PA_mean = (PA + lag_PA) / 2, 
                PA_hm = 2 / (1/PA + 1/lag_PA),
                max_delta_stat = max(delta_stat, na.rm=TRUE),
                age_max_delta_stat = ifelse(max_delta_stat == delta_stat, 1, 0),
                age_peak_delta_stat = ifelse(max_delta_stat == delta_stat, bb_age, 0)) %>%
  dplyr::ungroup()

# ### how much data loss with intra_player approach (optional) ###

# print(nrow(na.omit(df.1)))
# print(round((nrow(na.omit(df.1)) - nrow(na.omit(df.51))) / nrow(na.omit(df.1)),2))

df.52 <-  df.51 %>%
  dplyr::filter(year < (!!test_year)) %>%
  dplyr::filter(bb_age >= 21 & bb_age <= 41) %>%
  dplyr::group_by(bb_age) %>%
  dplyr::summarise(
    # stat_diff = mean(delta_stat, na.rm=TRUE), # grand mean
    # stat_diff = weighted.mean(delta_stat, PA_mean, na.rm=TRUE), # Arithmetic Mean
    stat_diff = weighted.mean(delta_stat, PA_hm, na.rm=TRUE), # Harmonic Mean
    # stat_diff = weighted.mean(delta_stat, PA_gm, na.rm=TRUE), # Geometric Mean
    # stat_diff = weighted.mean(delta_stat, max_PA, na.rm=TRUE), # Bigger of the two
    # stat_diff = weighted.mean(delta_stat, min_PA, na.rm=TRUE), # Smaller of the two
    # stat_diff_se = weighted.sd(delta_stat, PA_mean, na.rm=TRUE),
    stat_diff_se = weighted.sd(delta_stat, PA_hm, na.rm=TRUE),
    # stat_diff_se = weighted.sd(delta_stat, min_PA, na.rm=TRUE),
    # delta_age_peak = sum(age_max_delta_stat, na.rm=TRUE),
    PA = sum(PA), .groups = "drop") %>%
  dplyr::mutate(
    MGL_corr = case_when(bb_age < 27 ~ .020, ### 8 points of wOBA * 2.5 = ~ OPS
                         bb_age == 27 ~ 0,
                         bb_age > 27 ~ -.007), ### 3 points of wOBA * 2.5 = ~ OPS
    
    ### performance improves with centering, but is there theoretical justification for this? ###
    
    delta_method = cumsum(stat_diff) - weighted.mean(cumsum(stat_diff), PA),
    
    ### uncentered variant below ###
    
    # delta_method = cumsum(stat_diff),
    
    delta_method_corr = cumsum(MGL_corr) + max(delta_method) - max(cumsum(MGL_corr)) 
    # index to main delta peak
  )

return(list(df.52, df.51))

}

inter_player <- function(season_data = df_stat, career_data = df_career_train, 
                         PA_min = 1, season_min = 1,
                         test_year = 2017){
  
  enquo(test_year)
  
  df.61 <- career_data %>%
    dplyr::left_join(season_data %>%
                       dplyr::filter(year < (!!test_year)) %>%
                       dplyr::filter(PA >= PA_min[1]) %>% ### PA_min, change if desired
                       dplyr::anti_join(df_test_careers, by=c("batter")), by=c("batter")) %>%
    dplyr::filter(seasons >= season_min[1]) %>% # seasons filter, if desired
    dplyr::mutate(rel_age = bb_age - stat_age_peak) %>%
    dplyr::select(year, batter:stat_age_peak, stat_sel:rel_age, bb_age, PA) %>%
    dplyr::filter(year < (!!test_year))
    
  return(df.61)
}

### hold-out-year function ###

hold_out_year <- function(season_data = df_stat, career_data = df_career_all, 
                          PA_min = 1, season_min = 1,
                          test_year = 2017){
  
  enquo(test_year)
  
  df.71 <- career_data %>%
    dplyr::left_join(season_data %>%
                       dplyr::filter(PA >= PA_min[1]) %>% ### PA_min, change if desired
                       dplyr::anti_join(df_test_careers, by=c("batter")), by=c("batter")) %>%
    dplyr::filter(seasons >= season_min[1]) %>% # seasons filter, if desired
    dplyr::mutate(rel_age = bb_age - stat_age_peak) %>%
    dplyr::select(year, batter:stat_age_peak, stat_sel:rel_age, bb_age, PA) %>%
    dplyr::filter(year >= (!!test_year))
  
  return(df.71)
}


### testing error function ###

test_year_error <- function(min_age = 21, max_age = 41){
  
  enquo(min_age)
  enquo(max_age)
  
  df_test_years %>% 
    dplyr::left_join(df_intra %>% dplyr::select(-c(PA))) %>% 
    dplyr::left_join(year_preds, by=c("bb_age")) %>%
  dplyr::filter(bb_age >= (!!min_age) & bb_age <= (!!max_age)) %>%
    dplyr::summarise(delta_error = weighted.mean(abs(stat_sel - delta_method), PA, na.rm=TRUE), 
                     delta_method_corr_error = weighted.mean(abs(stat_sel - delta_method_corr), 
                                                             PA, na.rm=TRUE),
                     across_error = weighted.mean(abs(stat_sel - gam_across), PA, na.rm=TRUE),
                     bradbury_error = weighted.mean(abs(stat_sel - bradbury), PA, na.rm=TRUE),
                     age_null = mean(abs(stat_sel)), .groups = "drop") %>% ### for baseline comparison
    as.list()
}

test_career_error <- function(min_age = 21, max_age = 41){
  
  enquo(min_age)
  enquo(max_age)
  
  options(dplyr.summarise.inform=FALSE)
  
  df_test_careers <- hold_out_careers(n_careers = 625)
  df_career_train <- career_stats(career_test = df_test_careers)
  
  df_inter_s1_PA_1 <- inter_player()
  
  gam.mod <- mgcv::gam(stat_sel ~ s(bb_age) 
                       + career_mean, 
                       data=df_inter_s1_PA_1 %>% 
                         dplyr::filter(bb_age >= 21 & bb_age <= 41), weights=PA)
  
  year_preds <- tibble(bb_age = seq.int(21,41))
  year_preds$gam_across = predict(gam.mod, newdata=year_preds %>%
                                    dplyr::mutate(career_mean = 
                                                    weighted.mean(df_inter_s1_PA_1$career_mean,
                                                                  df_inter_s1_PA_1$PA)))
  
  bradbury.mod <- lm(stat_sel ~ bb_age + I(bb_age^2) + 
                       career_mean,
                     data=df_inter_s1_PA_1 %>% 
                       dplyr::filter(bb_age >= 21 & bb_age <= 41), weights=PA)
  
  year_preds$bradbury <- predict(bradbury.mod, newdata=year_preds %>%
                                   dplyr::mutate(career_mean = 
                                                   weighted.mean(df_inter_s1_PA_1$career_mean,
                                                                 df_inter_s1_PA_1$PA)))
  
  df_intra <- intra_player()[[1]]
  
  df_career_all %>%
    dplyr::inner_join(df_test_careers, by=c("batter")) %>%
    dplyr::inner_join(df_stat, by=c("batter")) %>% 
    dplyr::left_join(year_preds, by=c("bb_age")) %>%
    dplyr::left_join(df_intra %>% dplyr::select(-c(PA)), by=c("bb_age")) %>% 
    dplyr::filter(bb_age >= (!!min_age) & bb_age <= (!!max_age)) %>%
    dplyr::summarise(delta_error = weighted.mean(abs(stat_sel - delta_method), PA, na.rm=TRUE), 
                     delta_method_corr_error = weighted.mean(abs(stat_sel - delta_method_corr), 
                                                             PA, na.rm=TRUE),
                     across_error = weighted.mean(abs(stat_sel - gam_across), PA, na.rm=TRUE),
                     bradbury_error = weighted.mean(abs(stat_sel - bradbury), PA, na.rm=TRUE),
                     age_null = mean(abs(stat_sel)), .groups = "drop") %>% ### for baseline comparison
    as.list()
}

### Batters per Year ###

df.1 %>% dplyr::filter(year >= 2010) %>% 
  dplyr::select(batter, year) %>% 
  distinct() %>% 
  group_by(year) %>% 
  dplyr::summarise(batters = n(), .groups = "drop") %>% 
  dplyr::ungroup() %>% 
  dplyr::summarise(mean(batters), .groups = "drop") ### 628 batters, will use 625




### generate master data set ###

df_stat <- stat_sel()




###### Test Years Analysis, Delta Methods vs. GAM ######

### Generate Datasets ###

df_test_careers <- hold_out_careers(n_careers = 0)
df_career_all <- career_stats(career_test = df_test_careers)

df_test_years <- hold_out_year()

df_career_train <- career_stats(career_test = df_test_careers)

df_inter_s1_PA_1 <- inter_player()

### Model and Predict Effects of Age ###

year_preds <- tibble(bb_age = seq.int(21,41))

# GAM #

gam.mod <- mgcv::gam(stat_sel ~ s(bb_age)
                       + career_mean,
                       data=df_inter_s1_PA_1 %>%
                         dplyr::filter(bb_age >= 20 & bb_age <= 41), weights=PA)

year_preds$gam_across = predict(gam.mod, newdata=year_preds %>%
                                    dplyr::mutate(career_mean = 
                                                    weighted.mean(df_inter_s1_PA_1$career_mean,
                                                                  df_inter_s1_PA_1$PA)))
# Bradbury-type model #

bradbury.mod <- lm(stat_sel ~ bb_age + I(bb_age^2) 
                   + career_mean,
                   data=df_inter_s1_PA_1 %>% 
                     dplyr::filter(bb_age >= 20 & bb_age <= 41), weights=PA)

year_preds$bradbury <- predict(bradbury.mod, newdata=year_preds %>%
                                   dplyr::mutate(career_mean = 
                                                   weighted.mean(df_inter_s1_PA_1$career_mean,
                                                                 df_inter_s1_PA_1$PA)))

# Delta Methods #

df_intra <- intra_player()[[1]]

### Test Results ###

test_all_age <- test_year_error(min_age = 21, max_age = 41) %>% as.data.frame() %>%
  dplyr::mutate_if(is.numeric, ~round(.,3))

test_21_25 <- test_year_error(min_age = 21, max_age = 25) %>% as.data.frame() %>%
  dplyr::mutate_if(is.numeric, ~round(.,3))

test_26_30 <- test_year_error(min_age = 26, max_age = 30) %>% as.data.frame() %>%
  dplyr::mutate_if(is.numeric, ~round(.,3))

test_31_35 <- test_year_error(min_age = 31, max_age = 35) %>% as.data.frame() %>%
  dplyr::mutate_if(is.numeric, ~round(.,3))

test_36_41 <- test_year_error(min_age = 36, max_age = 41) %>% as.data.frame() %>%
  dplyr::mutate_if(is.numeric, ~round(.,3))

### Summarize Results ###

test_year_nums <- bind_rows(test_all_age, test_21_25, test_26_30,
                            test_31_35, test_36_41) %>%
  dplyr::mutate(train_years = "1977_2016",
                age_range=c("all_age", "21_25", "26_30", "31_35", "36_41"),
                test_years = "2017_2019") %>%
  dplyr::select(train_years, test_years, age_range, delta_error:age_null)

test_year_table <- test_year_nums %>%
  dplyr::select(train_years:age_range, delta_error, delta_method_corr_error, across_error)

test_year_table 

###### resample careers test data ######

### resample settings and parallel backend initialization ###

n_sims <- 1e2
set.seed(1234)

registerDoFuture()
plan(multisession, workers = detectCores() - 1)
getDoParWorkers()

### run resamples with time measurement ###

begin_time <- Sys.time()

intra_career_test_samples <- future_lapply(seq.int(n_sims), function(x) {
  
  test_career_error(min_age = 21, max_age = 41) ### change as needed for each cohort ###
  
}, future.seed = TRUE)

Sys.time() - begin_time

plan(sequential)

### extract summaries of resamples ###

x2 <- n_sims

tibble(delta_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$delta_error)),
       delta_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$delta_error)),
       delta_method_corr_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$delta_method_corr_error)),
       delta_method_corr_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$delta_method_corr_error)),
       across_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$across_error)),
       across_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$across_error)),
       bradbury_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$bradbury_error)),
       bradbury_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$bradbury_error)),
       age_null = mean(sapply(intra_career_test_samples[1:x2], function(t) t$age_null))) %>%
  dplyr::mutate_if(is.numeric, ~round(.,3)) %>% as.data.frame()

### manually specify each age group to hold results ### 

# age_21_25 <- tibble(delta_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$delta_error)),
#                    delta_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$delta_error)),
#                    delta_method_corr_err = mean(sapply(intra_career_test_samples[1:x2], function(t) t$delta_method_corr_error)),
#                    delta_method_corr_err_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$delta_method_corr_error)),
#                    across_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$across_error)),
#                    across_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$across_error)),
#                    bradbury_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$bradbury_error)),
#                    bradbury_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$bradbury_error)),
#                    gam_all_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$gam_all_error)),
#                    gam_all_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$gam_all_error)),
#                    gam_avg_error = mean(sapply(intra_career_test_samples[1:x2], function(t) t$gam_avg_error)),
#                    gam_avg_error_sd = sd(sapply(intra_career_test_samples[1:x2], function(t) t$gam_avg_error)),
#                    age_null = mean(sapply(intra_career_test_samples[1:x2], function(t) t$age_null))) %>%
#   dplyr::mutate_if(is.numeric, ~round(.,3))


plan(sequential)

### distillation of results, leave-career-out testing, once all age groups below have been run ###

# career_nums <- bind_rows(all_age, age_21_25, age_26_30, age_31_35, age_36_41) %>% 
#   dplyr::mutate(year_range = "1977_2016", 
#                 age_range=c("all_age", "21_25", "26_30", "31_35", "36_41"), sims = 5000) %>% 
#   dplyr::select(year_range, sims, age_range, delta_error:age_null) %>% 
#   dplyr::rename(gam_delta_error = gam_all_error)
# 
# career_table <- career_nums %>%
#   dplyr::select(year_range:age_range, delta_error, delta_method_corr_err, across_error)


##### plot aging curves #####
## filters select which curve or curves get plotted ##

# curve.comp <-
  df_intra %>% dplyr::select(-c(PA)) %>% 
  dplyr::left_join(year_preds, by=c("bb_age")) %>%
  tidyr::pivot_longer(., cols=c("delta_method", "delta_method_corr", "gam_across", "bradbury"),
                      names_to = "Method", values_to = "OPS_AA") %>%
  # dplyr::filter(Method == "delta_method") %>%
  # dplyr::filter(Method == "delta_method" | Method == "delta_method_corr") %>%
  dplyr::filter(Method != "bradbury") %>%
  # dplyr::filter(Method == "gam_across" | Method == "bradbury") %>%
  ggplot(.) +
  theme_flat() +
  scale_color_colorblind() +
  ggtitle("Aging Curves, OPS Above Average, 1977-2016") +
  xlab("bb_age") +
  ylab("OPS Above Average") +
  scale_y_continuous(n.breaks = 6) +
  geom_smooth(aes(x=bb_age, y=OPS_AA, color = Method), se=FALSE)


##### Bradbury tests #####

bradbury.s1.pa1 <- inter_player(PA_min = 1, season_min = 1)
bradbury.s5.pa1 <- inter_player(PA_min = 1, season_min = 5)
bradbury.s10.pa1 <- inter_player(PA_min = 1, season_min = 10)

bradbury.mod.s1 <- lm(stat_sel ~ bb_age + I(bb_age^2) + 
                     career_mean,
                   data=bradbury.s1.pa1 %>% 
                     dplyr::filter(bb_age >= 20 & bb_age <= 41), weights=PA)

bradbury.mod.s5 <- lm(stat_sel ~ bb_age + I(bb_age^2) + 
                        career_mean,
                      data=bradbury.s5.pa1 %>% 
                        dplyr::filter(bb_age >= 20 & bb_age <= 41), weights=PA)

bradbury.mod.s10 <- lm(stat_sel ~ bb_age + I(bb_age^2) + 
                        career_mean,
                      data=bradbury.s10.pa1 %>% 
                        dplyr::filter(bb_age >= 20 & bb_age <= 41), weights=PA)

year_preds <- tibble(bb_age = seq.int(21,41)) 

year_preds$bradbury_s1 <- predict(bradbury.mod, newdata=year_preds %>%
                                   dplyr::mutate(career_mean = 
                                                   weighted.mean(bradbury.s1.pa1$career_mean,
                                                                 bradbury.s1.pa1$PA)))

year_preds$bradbury_s5 <- predict(bradbury.mod, newdata=year_preds %>%
                                      dplyr::mutate(career_mean = 
                                                      weighted.mean(bradbury.s5.pa1$career_mean,
                                                                    bradbury.s5.pa1$PA)))

year_preds$bradbury_s10 <- predict(bradbury.mod, newdata=year_preds %>%
                                      dplyr::mutate(career_mean = 
                                                      weighted.mean(bradbury.s10.pa1$career_mean,
                                                                    bradbury.s10.pa1$PA)))

year_preds ### peak age still 29 for all

####### Delta method stratification #######

delta_strat <- df_career_all %>%
  dplyr::mutate(
    pool =
      round(percent_rank(career_mean),1)
    ) %>%
  dplyr::group_by(pool) %>%
  dplyr::summarise(first_year_age = weighted.mean(first_year_age, career_PA),
                   stat_age_peak = weighted.mean(stat_age_peak, career_PA),
                   career_mean = weighted.mean(career_mean, career_PA),
                   number = n())

####### First Year in MLB by Career Average Performance ######

ggplot(data=df_career_all, aes(x=first_year_age, y=career_mean)) +
  theme_flat() +
  ggtitle("Career Average OPS by First Year in MLB (1977-2016)") +
  xlab("bb_age") +
  ylab("OPS Above Average") +
  scale_color_colorblind() +
  geom_smooth(se=FALSE)


