### load packages ###

if (!require("pacman")) install.packages("pacman")

# install baseballr package by Bill Petti separately: see https://github.com/BillPetti/baseballr 

pacman::p_load(RMySQL,
               tidyverse,
               readxl,
               arm,
               rstanarm,
               brms,
               scales,
               loo,
               baseballr)

### acquire data ###
## MAKE SURE these three files are in the directory as script or R cannot find them

BIP.data <- read_csv("BIP_data_19.csv")
id_data <- read_csv("id_data_19.csv")
fraa_data <- read_csv("fraa_data_19.csv")

### Additional defensive systems tested but for which we do not have publishing rights ### 

# drs_new <- read_csv("drs_full.csv")
# DRS_new_xls <- read_excel("DRS_new.xlsx")
# oaa_all <- read_csv("oaa_all_22720.csv")
# fg_def_stats <- read_csv("fg_def_stats_22720.csv")
# red_def_2019 <- read_csv("red_def_2019.csv")

### prepare data ###

BIP.pbp.data <- BIP.data %>%
  dplyr::filter(
    event == 2 |
      event == 18 |
      event == 19 |
      event == 20 |
      event == 21 | 
      event == 22) %>%
  dplyr::filter(AST_1  == 0 | AST_1 >= 3) %>%
  dplyr::filter(First_PO == 0 | First_PO >= 3) %>%
  dplyr::filter(fld_cd >= 3) %>%
  dplyr::select(pitcher, batter, stadium, event, Pos_3:Pos_9, fld_cd, fld_team) %>%
  dplyr::mutate(out = if_else(event == 2 | event == 19, 1, 0)) 

### calculate player chances ###

n_3.df <- BIP.pbp.data %>% ### BP IDs used for players
  dplyr::group_by(Pos_3) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::rename(bpid = Pos_3)

n_4.df <- BIP.pbp.data %>%
  dplyr::group_by(Pos_4) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::rename(bpid = Pos_4)

n_5.df <- BIP.pbp.data %>%
  dplyr::group_by(Pos_5) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::rename(bpid = Pos_5)

n_6.df <- BIP.pbp.data %>%
  dplyr::group_by(Pos_6) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::rename(bpid = Pos_6)

n_7.df <- BIP.pbp.data %>%
  dplyr::group_by(Pos_7) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::rename(bpid = Pos_7)

n_8.df <- BIP.pbp.data %>%
  dplyr::group_by(Pos_8) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::rename(bpid = Pos_8)

n_9.df <- BIP.pbp.data %>%
  dplyr::group_by(Pos_9) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::rename(bpid = Pos_9)

player_chances <- n_3.df %>%
  dplyr::bind_rows(n_4.df, n_5.df, n_6.df, n_7.df, n_8.df, n_9.df) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(bpid) %>%
  dplyr::summarise(chances = sum(n))

### harmonize IDs and generate rate stats ###

# drs_df <- DRS_new_xls %>%
#   dplyr::filter(season == 2019) %>%
#   dplyr::select(name, MLBdotcomId) %>%
#   dplyr::distinct(.) %>%
#   dplyr::left_join(id_data, by=c("MLBdotcomId" = "mlbcode")) %>%
#   dplyr::left_join(drs_new %>% dplyr::select(name = Name, PART)) %>%
#   dplyr::filter(pos != 'C' & pos != 'P') %>%
#   dplyr::group_by(bpid) %>%
#   dplyr::summarise(PART = sum(PART)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(player_chances) %>%
#   dplyr::mutate(PART_rate = PART / chances,
#                 PART_sc = arm::rescale(PART_rate)) %>%
#   dplyr::select(bpid, PART_sc)
# 
# oaa_df <- oaa_all %>%
#   dplyr::left_join(id_data, by=c("player_id" = "mlbcode")) %>%
#   dplyr::left_join(player_chances) %>%
#   dplyr::mutate(OAA_rate = outs_above_average / chances,
#                 OAA_sc = arm::rescale(OAA_rate)) %>%
#   dplyr::select(bpid, OAA_sc)
# 
# UZR_df <- fg_def_stats %>%
#   dplyr::mutate(UZR_field_runs = RngR + ErrR) %>%
#   dplyr::left_join(chadwick_player_lu_table %>%
#                      dplyr::select(key_mlbam, key_fangraphs),
#                    by=c("playerid" = "key_fangraphs")) %>%
#   dplyr::left_join(id_data, by=c("key_mlbam" = "mlbcode")) %>%
#   dplyr::filter(pos != 'C' & pos != 'P') %>%
#   dplyr::group_by(bpid) %>%
#   dplyr::summarise(UZR_field_runs = sum(UZR_field_runs)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(player_chances) %>%
#   dplyr::mutate(UZR_rate = UZR_field_runs / chances,
#                 UZR_sc = arm::rescale(UZR_rate)) %>%
#   dplyr::select(bpid, UZR_sc)
# 
# drs_old_df <- fg_def_stats %>%
#   dplyr::mutate(drs_old_runs = rPM) %>%
#   dplyr::left_join(chadwick_player_lu_table %>%
#                      dplyr::select(key_mlbam, key_fangraphs),
#                    by=c("playerid" = "key_fangraphs")) %>%
#   dplyr::left_join(id_data, by=c("key_mlbam" = "mlbcode")) %>%
#   dplyr::filter(pos != 'C' & pos != 'P') %>%
#   dplyr::group_by(bpid) %>%
#   dplyr::summarise(drs_old_runs = sum(drs_old_runs)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(player_chances) %>%
#   dplyr::mutate(drs_old_rate = drs_old_runs / chances,
#                 drs_old_sc = arm::rescale(drs_old_rate)) %>%
#   dplyr::select(bpid, drs_old_sc)
# 
# red_df <- red_def_2019 %>%
#   dplyr::filter(PLYR != "Grand Total") %>%
#   dplyr::left_join(chadwick_player_lu_table %>%
#                      dplyr::select(key_mlbam, key_bbref),
#                    by=c("ID" = "key_bbref")) %>%
#   dplyr::left_join(id_data, by=c("key_mlbam" = "mlbcode")) %>%
#   dplyr::filter(pos != 'C' & pos != 'P') %>%
#   dplyr::group_by(bpid) %>%
#   dplyr::summarise(red_oaa = sum(OAA)) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(player_chances) %>%
#   dplyr::mutate(red_oaa_rate = red_oaa / chances,
#                 red_oaa_sc = arm::rescale(red_oaa_rate)) %>%
#   dplyr::select(bpid, red_oaa_sc)

fraa_df <- fraa_data %>%
  dplyr::left_join(id_data, by=c("bpid")) %>%
  dplyr::filter(pos != 'C' & pos != 'P') %>%
  dplyr::group_by(bpid) %>%
  dplyr::summarise(fraa_paa = sum(paa)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(player_chances) %>%
  dplyr::mutate(fraa_rate = fraa_paa / chances,
                fraa_paa_sc = arm::rescale(fraa_rate)) %>%
  dplyr::select(bpid, fraa_paa_sc)

### model and summarize defensive ratings for aggregate testing ###

# oaa.sum.agg <- BIP.pbp.data %>%
#   dplyr::filter(fld_cd >= 3) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_oaa = OAA_sc) %>%  
#   dplyr::left_join(oaa_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_oaa = OAA_sc) %>%
#   dplyr::select(P3_oaa:P9_oaa) %>%
#   dplyr::mutate(oaa_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# drs.sum.agg <- BIP.pbp.data %>%
#   dplyr::filter(fld_cd >= 3) %>%
#   dplyr::left_join(drs_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_DRS = PART_sc) %>%
#   dplyr::select(P3_DRS:P9_DRS) %>%
#   dplyr::mutate(drs_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# drs.old.sum.agg <- BIP.pbp.data %>%
#   dplyr::filter(fld_cd >= 3) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_DRS_old = drs_old_sc) %>%
#   dplyr::select(P3_DRS_old:P9_DRS_old) %>%
#   dplyr::mutate(drs_old_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# uzr.sum.agg <- BIP.pbp.data %>%
#   dplyr::filter(fld_cd >= 3) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_UZR = UZR_sc) %>%
#   dplyr::select(P3_UZR:P9_UZR) %>%
#   dplyr::mutate(uzr_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# red.sum.agg <- BIP.pbp.data %>%
#   dplyr::filter(fld_cd >= 3) %>%
#   dplyr::left_join(red_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_red = red_oaa_sc) %>%
#   dplyr::select(P3_red:P9_red) %>%
#   dplyr::mutate(red_oaa_sum = rowSums(.[1:7], na.rm=TRUE))

fraa.sum.agg <- BIP.pbp.data %>%
  dplyr::filter(fld_cd >= 3) %>%
  dplyr::left_join(fraa_df, by=c("Pos_3" = "bpid")) %>%
  dplyr::rename(P3_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_4" = "bpid")) %>%
  dplyr::rename(P4_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_5" = "bpid")) %>%
  dplyr::rename(P5_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_6" = "bpid")) %>%
  dplyr::rename(P6_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_7" = "bpid")) %>%
  dplyr::rename(P7_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_8" = "bpid")) %>%
  dplyr::rename(P8_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_9" = "bpid")) %>%
  dplyr::rename(P9_fraa = fraa_paa_sc) %>%
  dplyr::select(P3_fraa:P9_fraa) %>%
  dplyr::mutate(fraa_paa_sum = rowSums(.[1:7], na.rm=TRUE))

df.mod.agg <- BIP.pbp.data %>%
  dplyr::bind_cols(
    # oaa.sum.agg %>% dplyr::select(oaa_sum),
    # drs.sum.agg %>% dplyr::select(drs_sum),
    # drs.old.sum.agg %>% dplyr::select(drs_old_sum),
    # uzr.sum.agg %>% dplyr::select(uzr_sum),
    # red.sum.agg %>% dplyr::select(red_oaa_sum),
    fraa.sum.agg %>% dplyr::select(fraa_paa_sum)) %>%
  dplyr::select(pitcher, batter, stadium, fld_team,
                out:fraa_paa_sum, fld_cd)

cores <- parallel::detectCores(logical = FALSE) ### use all "true" cores you have only

# drs.stan.agg <- stan_glm(out ~ drs_sum, family="binomial", data=df.mod.agg, chains=4, cores=, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))
# 
# drs.old.stan.agg <- stan_glm(out ~ drs_old_sum, family="binomial", data=df.mod.agg, chains=4, cores=cores, 
#                            prior = normal(0,.1), prior_intercept = normal(0,1))

fraa.stan.agg <- stan_glm(out ~ fraa_paa_sum, family="binomial", data=df.mod.agg, chains=4, cores=cores, 
                        prior = normal(0,.1), prior_intercept = normal(0,1))

# oaa.stan.agg <- stan_glm(out ~ oaa_sum, family="binomial", data=df.mod.agg, chains=4, cores=cores, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))
# 
# red.stan.agg <- stan_glm(out ~ red_oaa_sum, family="binomial", data=df.mod.agg, chains=4, cores=cores, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))
# 
# uzr.stan.agg <- stan_glm(out ~ uzr_sum, family="binomial", data=df.mod.agg, chains=4, cores=cores, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))
# 
baseline.stan.agg <- stan_glm(out ~ 1, family="binomial", data=df.mod.agg, chains=4, cores=cores,
                            prior = normal(0,.1), prior_intercept = normal(0,1))
# 
# loo.drs.agg <- loo(drs.stan.agg, cores=cores) ### if you have problems with the cores option, just remove it; it is for speed only
# loo.drs.old.agg <- loo(drs.old.stan.agg, cores=cores)
# loo.oaa.agg <- loo(oaa.stan.agg, cores=cores)
# loo.uzr.agg <- loo(uzr.stan.agg, cores=cores)
# loo.red.agg <- loo(red.stan.agg, cores=cores)
loo.fraa.agg <- loo(fraa.stan.agg, cores=cores)
loo.bl.agg <- loo(baseline.stan.agg, cores=cores)

loo.fraa.agg ### elpd_loo should be -69051 (SE 116) or close to it

### model comparisons ###

# Predictive log density comparison

agg.loo.compare <- as.data.frame(loo_compare(
  # loo.drs.agg, loo.drs.old.agg, 
  loo.fraa.agg,
  # loo.oaa.agg, loo.red.agg, loo.uzr.agg, 
  loo.bl.agg
  )) %>%
  tibble::rownames_to_column() %>%
  dplyr::select(
    Metric = rowname,
    Pred_Diff = elpd_diff, 
    Diff_Error = se_diff) %>%
  dplyr::mutate_if(is.numeric, ~round(.,2)) %>%
  dplyr::mutate(Metric = toupper(str_replace(Metric, ".stan.agg", "")))

### FRAA wins 

# Bayesian Model Average

model_list.agg <- list(
  # loo.drs.agg, loo.drs.old.agg, 
  loo.fraa.agg, 
  # loo.oaa.agg, loo.red.agg, loo.uzr.agg, 
  loo.bl.agg
  )

set.seed(1234)
model.weights.bma_plus.agg <- loo_model_weights(model_list.agg, method = "pseudobma", BB=TRUE)

agg.model.weights <- data.frame(
  bma_plus_weight = round(as.numeric(model.weights.bma_plus.agg[1:2]),3), ### change "1:2" to number of models: up to 7 here
  model_name = c(
    # "DRS", "DRS Old", 
    "FRAA", 
    # "OAA", "RED", "UZR", 
    "BASE"
    )
) %>%
  dplyr::mutate(model_weight = scales::percent(bma_plus_weight)) %>%
  dplyr::arrange(desc(bma_plus_weight)) %>%
  dplyr::select(model_name, model_weight)


### model and summarize defensive ratings for positional isolation testing ###
## in mutate_at line, adjust all positions to zero except for the position you are testing
# default here is P9 only allowed, so as to test P9 defensive measurement

# oaa.sum.pos <- BIP.pbp.data %>%
#   dplyr::left_join(oaa_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_oaa = OAA_sc) %>%  
#   dplyr::left_join(oaa_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_oaa = OAA_sc) %>%
#   dplyr::left_join(oaa_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_oaa = OAA_sc) %>%
#   dplyr::select(P3_oaa:P9_oaa) %>%
#   dplyr::mutate_at(vars(contains("P3"):contains("P7"), contains("P8"):contains("P8")), ~0) %>% ### change to desired position
#   dplyr::mutate(oaa_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# drs.sum.pos <- BIP.pbp.data %>%
#   dplyr::left_join(drs_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_DRS = PART_sc) %>%
#   dplyr::left_join(drs_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_DRS = PART_sc) %>%
#   dplyr::select(P3_DRS:P9_DRS) %>%
#   dplyr::mutate_at(vars(contains("P3"):contains("P7"), contains("P8"):contains("P8")), ~0) %>% ### change to desired position
#   dplyr::mutate(drs_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# drs.old.sum.pos <- BIP.pbp.data %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_DRS_old = drs_old_sc) %>%
#   dplyr::left_join(drs_old_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_DRS_old = drs_old_sc) %>%
#   dplyr::select(P3_DRS_old:P9_DRS_old) %>%
#   dplyr::mutate_at(vars(contains("P3"):contains("P7"), contains("P8"):contains("P8")), ~0) %>% ### change to desired position
#   dplyr::mutate(drs_old_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# uzr.sum.pos <- BIP.pbp.data %>%
#   dplyr::left_join(UZR_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_UZR = UZR_sc) %>%
#   dplyr::left_join(UZR_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_UZR = UZR_sc) %>%
#   dplyr::select(P3_UZR:P9_UZR) %>%
#   dplyr::mutate_at(vars(contains("P3"):contains("P7"), contains("P8"):contains("P8")), ~0) %>% ### change to desired position
#   dplyr::mutate(uzr_sum = rowSums(.[1:7], na.rm=TRUE))
# 
# red.sum.pos <- BIP.pbp.data %>%
#   dplyr::left_join(red_df, by=c("Pos_3" = "bpid")) %>%
#   dplyr::rename(P3_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_4" = "bpid")) %>%
#   dplyr::rename(P4_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_5" = "bpid")) %>%
#   dplyr::rename(P5_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_6" = "bpid")) %>%
#   dplyr::rename(P6_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_7" = "bpid")) %>%
#   dplyr::rename(P7_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_8" = "bpid")) %>%
#   dplyr::rename(P8_red = red_oaa_sc) %>%
#   dplyr::left_join(red_df, by=c("Pos_9" = "bpid")) %>%
#   dplyr::rename(P9_red = red_oaa_sc) %>%
#   dplyr::select(P3_red:P9_red) %>%
#   dplyr::mutate_at(vars(contains("P3"):contains("P7"), contains("P8"):contains("P8")), ~0) %>% ### change to desired position
#   dplyr::mutate(red_oaa_sum = rowSums(.[1:7], na.rm=TRUE))

fraa.sum.pos <- BIP.pbp.data %>%
  dplyr::left_join(fraa_df, by=c("Pos_3" = "bpid")) %>%
  dplyr::rename(P3_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_4" = "bpid")) %>%
  dplyr::rename(P4_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_5" = "bpid")) %>%
  dplyr::rename(P5_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_6" = "bpid")) %>%
  dplyr::rename(P6_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_7" = "bpid")) %>%
  dplyr::rename(P7_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_8" = "bpid")) %>%
  dplyr::rename(P8_fraa = fraa_paa_sc) %>%
  dplyr::left_join(fraa_df, by=c("Pos_9" = "bpid")) %>%
  dplyr::rename(P9_fraa = fraa_paa_sc) %>%
  dplyr::select(P3_fraa:P9_fraa) %>%
  dplyr::mutate_at(vars(contains("P3"):contains("P7"), contains("P8"):contains("P8")), ~0) %>% ### change to desired position
  dplyr::mutate(fraa_paa_sum = rowSums(.[1:7], na.rm=TRUE))

df.mod.pos <- BIP.pbp.data %>%
  dplyr::bind_cols(
    # oaa.sum.pos %>% dplyr::select(oaa_sum), 
    # drs.sum.pos %>% dplyr::select(drs_sum), 
    # drs.old.sum.pos %>% dplyr::select(drs_old_sum),
    # uzr.sum.pos %>% dplyr::select(uzr_sum),
    # red.sum.pos %>% dplyr::select(red_oaa_sum),
    fraa.sum.pos %>% dplyr::select(fraa_paa_sum)
  ) %>%
  dplyr::select(pitcher, batter, stadium, fld_team,
                out:fraa_paa_sum, fld_cd
  ) %>%
  dplyr::filter(fld_cd == 9) ### change to desired position

# drs.stan.pos <- stan_glm(out ~ drs_sum, family="binomial", data=df.mod.pos, chains=4, cores=cores, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))
# 
# drs.old.stan.pos <- stan_glm(out ~ drs_old_sum, family="binomial", data=df.mod.pos, chains=4, cores=cores, 
#                            prior = normal(0,.1), prior_intercept = normal(0,1))

fraa.stan.pos <- stan_glm(out ~ fraa_paa_sum, family="binomial", data=df.mod.pos, chains=4, cores=cores, 
                        prior = normal(0,.1), prior_intercept = normal(0,1))

# oaa.stan.pos <- stan_glm(out ~ oaa_sum, family="binomial", data=df.mod.pos, chains=4, cores=cores, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))
# 
# red.stan.pos <- stan_glm(out ~ red_oaa_sum, family="binomial", data=df.mod.pos, chains=4, cores=cores, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))
# 
# uzr.stan.pos <- stan_glm(out ~ uzr_sum, family="binomial", data=df.mod.pos, chains=4, cores=cores, 
#                        prior = normal(0,.1), prior_intercept = normal(0,1))

baseline.stan.pos <- stan_glm(out ~ 1, family="binomial", data=df.mod.pos, chains=4, cores=cores, 
                            prior = normal(0,.1), prior_intercept = normal(0,1))

# loo.drs.pos <- loo(drs.stan.pos, cores=cores)
# loo.drs.old.pos <- loo(drs.old.stan.pos, cores=cores)
# loo.oaa.pos <- loo(oaa.stan.pos, cores=cores)
# loo.uzr.pos <- loo(uzr.stan.pos, cores=cores)
# loo.red.pos <- loo(red.stan.pos, cores=cores)
loo.fraa.pos <- loo(fraa.stan.pos, cores=cores)
loo.bl.pos <- loo(baseline.stan.pos, cores=cores)

loo.compare.pos <- as.data.frame(
  loo_compare(
    # loo.drs.pos, loo.drs.old.pos, 
    loo.fraa.pos, 
    # loo.oaa.pos, loo.red.pos, loo.uzr.pos, 
    loo.bl.pos
    )) %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(
    Metric = case_when(
      # rowname == "model1" ~ "DRS",
      # rowname == "model2" ~ "DRS Old",
      rowname == "model3" ~ "FRAA",
      # rowname == "model4" ~ "OAA",
      # rowname == "model5" ~ "RED",
      # rowname == "model6" ~ "UZR",
      rowname == "model7" ~ "Baseline"
    )) %>%
  dplyr::select(
    Metric,
    Pred_Diff = elpd_diff, 
    Diff_Error = se_diff) %>%
  dplyr::mutate_if(is.numeric, ~round(.,2)) %>%
  dplyr::mutate(Metric = toupper(str_replace(Metric, ".stan.pos", "")))

# Bayesian Model Average

model_list.pos <- list(
  # loo.drs.pos, loo.drs.old.pos, 
  loo.fraa.pos, 
  # loo.oaa.pos, loo.red.pos, loo.uzr.pos, 
  loo.bl.pos
  )

set.seed(1234)

model.weights.bma_plus.pos <- loo_model_weights(model_list.pos, method = "pseudobma", BB=TRUE)

pos.model.weights <- data.frame(
  bma_plus_weight = round(as.numeric(model.weights.bma_plus.pos[1:2]),3), ### change "1:2" to number of models: up to 7 here
  model_name = c(
    # "DRS", "DRS Old", 
    "FRAA", 
    # "OAA", "RED", "UZR", 
    "BASE")
) %>%
  dplyr::mutate(model_weight = scales::percent(bma_plus_weight)) %>%
  dplyr::arrange(desc(bma_plus_weight)) %>%
  dplyr::select(model_name, model_weight)

 

######### what if we accounted for additional covariate factors?  does it matter? ##########
### warning: each of these models takes longer to run, plus they compile each time first ###
### we switch to brms package because although longer to get started, it is faster once it starts exploring ###

mod.prior = c(
  prior(normal(0,.1), class="b"),
  prior(normal(0,1), class="Intercept"),
  prior(exponential(1), class="sd") # add a prior for the random effects
)

# drs.stan.pos.2 <- brm(out ~ drs_sum + (1|stadium) + (1|batter) + 
#                       (1|pitcher) + (1|fld_team), 
#                     family=bernoulli, data=df.mod.pos.2, 
#                     chains=4, cores=cores, prior=mod.prior)
# 
# drs.old.stan.pos.2 <- brm(out ~ drs_old_sum + (1|stadium) + (1|batter) +
#                             (1|pitcher) + (1|fld_team),
#                           family=bernoulli, data=df.mod.pos,
#                           chains=4, cores=cores, prior=mod.prior)

fraa.stan.pos.2 <- brm(out ~ fraa_paa_sum + (1|stadium) + (1|batter) +
                      (1|pitcher) + (1|fld_team),
                    family=bernoulli, data=df.mod.pos,
                    chains=4, cores=cores, prior=mod.prior,
                    control = list(adapt_delta=.95))

# oaa.stan.pos.2 <- brm(out ~ oaa_paa_sum + (1|stadium) + (1|batter) +
#                         (1|pitcher) + (1|fld_team),
#                       family=bernoulli, data=df.mod.pos,
#                       chains=4, cores=cores, prior=mod.prior)
# 
# red.stan.pos.2 <- brm(out ~ red_oaa_sum + (1|stadium) + (1|batter) +
#                         (1|pitcher) + (1|fld_team),
#                       family=bernoulli, data=df.mod.pos,
#                       chains=4, cores=cores, prior=mod.prior)
# 
# uzr.stan.pos.2 <- brm(out ~ uzr_sum + (1|stadium) + (1|batter) +
#                         (1|pitcher) + (1|fld_team),
#                       family=bernoulli, data=df.mod.pos,
#                       chains=4, cores=cores, prior=mod.prior)

baseline.stan.pos.2 <- brm(out ~ (1|stadium) + (1|batter) +
                             (1|pitcher) + (1|fld_team),
                           family=bernoulli, data=df.mod.pos,
                           chains=4, cores=cores, 
                           prior=c(set_prior("normal(0,1)", class = "Intercept"),
                                   set_prior("exponential(1)", class = "sd")),
                           control = list(adapt_delta=.95))

# drs.stan.LL <- brms::log_lik(drs.stan.pos.2, re_formula=NA, nsamples=1e3)
# drs.old.stan.LL <- brms::log_lik(drs.old.stan.pos.2, re_formula=NA, nsamples=1e3)
fraa.stan.LL <- brms::log_lik(fraa.stan.pos.2, re_formula=NA, nsamples=1e3)
# oaa.stan.LL <- brms::log_lik(oaa.stan.pos.2, re_formula=NA, nsamples=1e3)
# red.stan.LL <- brms::log_lik(red.stan.pos.2, re_formula=NA, nsamples=1e3)
# uzr.stan.LL <- brms::log_lik(uzr.stan.pos.2, re_formula=NA, nsamples=1e3)
bl.stan.LL <- brms::log_lik(baseline.stan.pos.2, re_formula=NA, nsamples=1e3)

# loo.drs.pos.2 <- loo(drs.stan.LL, cores=cores) ### warnings can be disregarded: WAIC confirms reasonable accuracy
# loo.drs.old.pos.2 <- loo(drs.old.stan.LL, cores=cores)
# loo.oaa.pos.2 <- loo(oaa.stan.LL, cores=cores)
# loo.uzr.pos.2 <- loo(uzr.stan.LL, cores=cores)
# loo.red.pos.2 <- loo(red.stan.LL, cores=cores)
loo.fraa.pos.2 <- loo(fraa.stan.LL, cores=cores)
loo.bl.pos.2 <- loo(bl.stan.LL, cores=cores)

### model comparisons ###

# Predictive log density comparison

loo.compare.pos.2 <- as.data.frame(
  loo_compare(
    # loo.drs.pos.2, loo.drs.old.pos.2, 
    loo.fraa.pos.2,
    # loo.oaa.pos.2, loo.red.pos.2, loo.uzr.pos.2, 
    loo.bl.pos.2
  )) %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      # rowname == "model1" ~ "DRS",
      # rowname == "model2" ~ "DRS Old",
      rowname == "model3" ~ "FRAA",
      # rowname == "model4" ~ "OAA",
      # rowname == "model5" ~ "RED",
      # rowname == "model6" ~ "UZR",
      rowname == "model7" ~ "Baseline"
    )) %>%
  dplyr::select(
    Metric,
    Pred_Diff = elpd_diff, 
    Diff_Error = se_diff) %>%
  dplyr::mutate_if(is.numeric, ~round(.,2)) %>%
  dplyr::mutate(Metric = toupper(str_replace(Metric, ".stan.pos", "")))

# Bayesian Model Average

model_list.pos.2 <- list(
  # loo.drs.pos.2, loo.drs.old.pos.2, 
  loo.fraa.pos.2, 
  # loo.oaa.pos.2, loo.red.pos.2, loo.uzr.pos.2, 
  loo.bl.pos.2
)

set.seed(1234)

model.weights.bma_plus.pos.2 <- loo_model_weights(model_list.pos.2, method = "pseudobma", BB=TRUE)

pos.model.weights.2 <- data.frame(
  bma_plus_weight = round(as.numeric(model.weights.bma_plus.pos[1:2]),3), ### change "1:2" to reflect number of models: up to 7 here
  model_name = c(
    # "DRS", "DRS Old", 
    "FRAA", 
    # "OAA", "RED", "UZR", 
    "BASE")
) %>%
  dplyr::mutate(model_weight = scales::percent(bma_plus_weight)) %>%
  dplyr::arrange(desc(bma_plus_weight)) %>%
  dplyr::select(model_name, model_weight)

### compare pos.model.weights to pos.model.weights.2: virtually identical ###

pos.model.weights
pos.model.weights.2
