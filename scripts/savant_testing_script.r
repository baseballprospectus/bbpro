### Load Packages ###

if (!require("pacman")) install.packages("pacman")
pacman::p_load(RMySQL,tidyverse,wCorr)

### Load and prepare Savant Data

savant_15a <- read_csv("savant_data_15a.csv") # Download from Savant
savant_16a <- read_csv("savant_data_16a.csv") # Download from Savant
savant_17a <- read_csv("savant_data_17a.csv") # Download from Savant

savant_15a[,4:31] <- apply(savant_15a[,4:31], 2, as.numeric)
savant_16a[,4:31] <- apply(savant_16a[,4:31], 2, as.numeric)
savant_17a[,4:31] <- apply(savant_17a[,4:31], 2, as.numeric)

### Load and prepare BP Data

BP_data <- read_csv("BP.data.csv")
BP_data$X1 <- NULL
BP_data$X1_1 <- NULL

BP_data_15 <- dplyr::filter(BP_data, year==2015)
BP_data_16 <- dplyr::filter(BP_data, year==2016)
BP_data_17 <- dplyr::filter(BP_data, year==2017)

### Combine data sets

df.2015 <- dplyr::inner_join(savant_15a, BP_data_15, by=c("player_id" = "mlbcode"))
df.2016 <- dplyr::inner_join(savant_16a, BP_data_16, by=c("player_id" = "mlbcode"))
df.2017 <- dplyr::inner_join(savant_17a, BP_data_17, by=c("player_id" = "mlbcode"))

df_15_16 <- dplyr::inner_join(df.2015, df.2016, by=c("player_id", "player_name"))
df_16_17 <- dplyr::inner_join(df.2016, df.2017, by=c("player_id", "player_name"))

sav.comp.y1 <- dplyr::bind_rows(df_15_16, df_16_17) %>%
  dplyr::select(player_id, player_name, 
                ba.x, woba.x, xwoba.x, xba.x, hits.x, abs.x,
                ba.y, woba.y, xwoba.y, xba.y, hits.y, abs.y,
                FIP.x, DRA.x, PA.x, 
                FIP.y, DRA.y, PA.y) %>%
  dplyr::mutate(hits_hm = 2 / (1/hits.x + 1/hits.y),
                abs_hm = 2 / (1/abs.x + 1/abs.y),
                PA_hm = 2 / (1/PA.x + 1/PA.y)
  )

sav.comp.y0 <- dplyr::bind_rows(df.2015, df.2016, df.2017) %>%
  dplyr::select(player_id, player_name, 
                ba, woba, xwoba, xba, hits, abs,
                ba, woba, xwoba, xba, hits, abs,
                FIP, DRA, PA, 
                FIP, DRA, PA
  )

sav.comp.y1 <- na.omit(sav.comp.y1)
sav.comp.y0 <- na.omit(sav.comp.y0)

#### metric compare

#### resampled metrics (same year)

xba.ba <- vector()
FIP.ba <- vector()
DRA.ba <- vector()

xwoba.woba <- vector()
FIP.woba <- vector()
DRA.woba <- vector()

set.seed(2018)

# Bayesian bootstrap and summary

for (i in 1:100000){

samp.weights.orig <- matrix( rexp(nrow(sav.comp.y0), 1), ncol = nrow(sav.comp.y0), byrow = TRUE)
samp.weights.norm <- samp.weights.orig / rowSums(samp.weights.orig)

test.1.samp <- dplyr::sample_n(as.data.frame(sav.comp.y0), 
                               size=nrow(sav.comp.y0), 
                               weight = samp.weights.norm,
                               replace=TRUE)


xba.ba[i] <- weightedCorr(test.1.samp$xba, test.1.samp$ba, test.1.samp$abs, method='Spearman')
FIP.ba[i] <- weightedCorr(test.1.samp$FIP, test.1.samp$ba, test.1.samp$abs, method='Spearman')
DRA.ba[i] <- weightedCorr(test.1.samp$DRA, test.1.samp$ba, test.1.samp$abs, method='Spearman')

xwoba.woba[i] <- weightedCorr(test.1.samp$xwoba, test.1.samp$woba, test.1.samp$abs, method='Spearman')
FIP.woba[i] <- weightedCorr(test.1.samp$FIP, test.1.samp$woba, test.1.samp$abs, method='Spearman')
DRA.woba[i] <- weightedCorr(test.1.samp$DRA, test.1.samp$woba, test.1.samp$abs, method='Spearman')

}

print(data_frame("ba.ba.mean" = mean(xba.ba), "ba.ba.sd" = sd(xba.ba)))
print(data_frame("FIP.ba.mean" = mean(FIP.ba), "FIP.ba.sd" = sd(FIP.ba)))
print(data_frame("DRA.ba.mean" = mean(DRA.ba), "DRA.ba.sd" = sd(DRA.ba)))

print(data_frame("xwoba.woba.mean" = mean(xwoba.woba), "xwoba.woba.sd" = sd(xwoba.woba)))
print(data_frame("FIP.woba.mean" = mean(FIP.woba), "FIP.woba.sd" = sd(FIP.woba)))
print(data_frame("DRA.woba.mean" = mean(DRA.woba), "DRA.woba.sd" = sd(DRA.woba)))


#### resampled metrics (year + 1)

xba.ba.x.y <- vector()
ba.ba.x.y <- vector()
FIP.ba.x.y <- vector()
DRA.ba.x.y <- vector()

xwoba.woba.x.y <- vector()
woba.woba.x.y <- vector()
FIP.woba.x.y <- vector()
DRA.woba.x.y <- vector()

xba.xba.x.y <- vector()
ba.ba.x.y <- vector()
xwoba.xwoba.x.y <- vector()
woba.woba.x.y <- vector()
FIP.FIP.x.y  <- vector()
DRA.DRA.x.y <- vector()

set.seed(2018)

# Bayesian bootstrap and summary

for (i in 1:100000){
  
  samp.weights.1 <- matrix( rexp(nrow(sav.comp.y1), 1), ncol = nrow(sav.comp.y1), byrow = TRUE)
  samp.weights <- samp.weights.1 / rowSums(samp.weights.1)
  
  test.1.samp <- dplyr::sample_n(as.data.frame(sav.comp.y1), 
                                 size=nrow(sav.comp.y1), 
                                 weight = samp.weights,
                                 replace=TRUE)
  
  ### xBA predictive versus BA, FIP, and DRA
  
xba.ba.x.y[i] <-   weightedCorr(test.1.samp$xba.x, test.1.samp$ba.y, test.1.samp$abs_hm, method='Spearman')
ba.ba.x.y[i] <-    weightedCorr(test.1.samp$ba.x, test.1.samp$ba.y, test.1.samp$abs_hm, method='Spearman')
FIP.ba.x.y[i] <-   weightedCorr(test.1.samp$FIP.x, test.1.samp$ba.y, test.1.samp$abs_hm, method='Spearman')
DRA.ba.x.y[i] <-   weightedCorr(test.1.samp$DRA.x, test.1.samp$ba.y, test.1.samp$abs_hm, method='Spearman')
  
  ### xwOBA predictive versus BA, FIP, and DRA
  
xwoba.woba.x.y[i] <-    weightedCorr(test.1.samp$xwoba.x, test.1.samp$woba.y, test.1.samp$abs_hm, method='Spearman')
woba.woba.x.y[i] <-     weightedCorr(test.1.samp$woba.x, test.1.samp$woba.y, test.1.samp$abs_hm, method='Spearman')
FIP.woba.x.y[i] <-      weightedCorr(test.1.samp$FIP.x, test.1.samp$woba.y, test.1.samp$abs_hm, method='Spearman')
DRA.woba.x.y[i] <-      weightedCorr(test.1.samp$DRA.x, test.1.samp$woba.y, test.1.samp$abs_hm, method='Spearman')
  
### xBA / xwOBA reliability versus BA, FIP, and DRA

xba.xba.x.y[i] <-         weightedCorr(test.1.samp$xba.x, test.1.samp$xba.y, test.1.samp$abs_hm, method='Spearman')
ba.ba.x.y[i] <-           weightedCorr(test.1.samp$ba.x, test.1.samp$ba.y, test.1.samp$abs_hm, method='Spearman')
xwoba.xwoba.x.y[i] <-     weightedCorr(test.1.samp$xwoba.x, test.1.samp$xwoba.y, test.1.samp$abs_hm, method='Spearman')
woba.woba.x.y[i] <-       weightedCorr(test.1.samp$woba.x, test.1.samp$woba.y, test.1.samp$abs_hm, method='Spearman')
FIP.FIP.x.y[i] <-         weightedCorr(test.1.samp$FIP.x, test.1.samp$FIP.y, test.1.samp$abs_hm, method='Spearman')
DRA.DRA.x.y[i] <-         weightedCorr(test.1.samp$DRA.x, test.1.samp$DRA.y, test.1.samp$abs_hm, method='Spearman')

}

print(data.frame("xba.ba.x.y.mean" = mean(xba.ba.x.y), "xba.ba.x.y.sd" = sd(xba.ba.x.y)))
print(data.frame("ba.ba.x.y.mean" = mean(ba.ba.x.y), "ba.ba.x.y.sd" = sd(ba.ba.x.y)))
print(data.frame("FIP.ba.x.y.mean" = mean(FIP.ba.x.y), "FIP.ba.x.y.sd" = sd(FIP.ba.x.y)))
print(data.frame("DRA.ba.x.y.mean" = mean(DRA.ba.x.y), "DRA.ba.x.y.sd" = sd(DRA.ba.x.y)))

print(data.frame("xwoba.woba.x.y.mean" = mean(xwoba.woba.x.y), "xwoba.woba.x.y.sd" = sd(xwoba.woba.x.y)))
print(data.frame("woba.woba.x.y.mean" = mean(woba.woba.x.y), "woba.woba.x.y.sd" = sd(woba.woba.x.y)))
print(data.frame("FIP.woba.x.y.mean" = mean(FIP.woba.x.y), "FIP.woba.x.y.sd" = sd(FIP.woba.x.y)))
print(data.frame("DRA.woba.x.y.mean" = mean(DRA.woba.x.y), "DRA.woba.x.y.sd" = sd(DRA.woba.x.y)))

print(data.frame("xba.xba.x.y.mean" = mean(xba.xba.x.y), "xba.xba.x.y.sd" = sd(xba.xba.x.y)))
print(data.frame("ba.ba.x.y.mean" = mean(ba.ba.x.y), "ba.ba.x.y.sd" = sd(ba.ba.x.y)))
print(data.frame("xwoba.xwoba.x.y.mean" = mean(xwoba.xwoba.x.y), "xwoba.xwoba.x.y.sd" = sd(xwoba.xwoba.x.y)))
print(data.frame("woba.woba.x.y.mean" = mean(woba.woba.x.y), "woba.woba.x.y.sd" = sd(woba.woba.x.y)))
print(data.frame("FIP.FIP.x.y.mean" = mean(FIP.FIP.x.y), "FIP.FIP.x.y.sd" = sd(FIP.FIP.x.y)))
print(data.frame("DRA.DRA.x.y.mean" = mean(DRA.DRA.x.y), "DRA.DRA.x.y.sd" = sd(DRA.DRA.x.y)))


print(data.frame("xba.ba.x.y.mean" = mean(xba.ba.x.y), "xba.ba.x.y.sd" = sd(xba.ba.x.y)))
print(data.frame("ba.ba.x.y.mean" = mean(ba.ba.x.y), "ba.ba.x.y.sd" = sd(ba.ba.x.y)))
print(data.frame("FIP.ba.x.y.mean" = mean(FIP.ba.x.y), "FIP.ba.x.y.sd" = sd(FIP.ba.x.y)))
print(data.frame("DRA.ba.x.y.mean" = mean(DRA.ba.x.y), "DRA.ba.x.y.sd" = sd(DRA.ba.x.y)))
