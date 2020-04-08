library(dplyr)
library(ggplot2)

options(scipen = 999)
## is it more appropriate to report the linear model or correlation?
## inclusion of 11CO (mostly chilik) has really thrown it off. 

### test for isolation by distance
library(geosphere)

# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)
# Calculates the geodesic distance between two points specified by 
# radian latitude/longitude using the Haversine formula
# Ouputs distance between sites 1 and 2 as meters
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = (R * c)
  return(d) # Distance in meters
}
CalcDists <- function(longlats) {
  name <- list(rownames(longlats), rownames(longlats))
  n <- nrow(longlats)
  z <- matrix(0, n, n, dimnames = name)
  for (i in 1:n) {
    for (j in 1:n) z[i, j] <- gcd.hf(long1 = deg2rad(longlats[i, 2]), 
                                     lat1 = deg2rad(longlats[i, 1]), long2 = deg2rad(longlats[j, 2]), 
                                     lat2 = deg2rad(longlats[j, 1]))
  }
  z <- as.dist(z)
  return(z)
}

## geographic distances
metadat <- read.csv("Git_Repo/info/pop_metadata.csv")
colnames(metadat)
popID_pos <- metadat[,c('Latitude', 'Longitude')]
rownames(popID_pos) <- metadat[,'popID']


popID_dist <- CalcDists(popID_pos)
popID_mat <- as.matrix(popID_dist)
ind <- which(upper.tri(popID_mat, diag = F), arr.ind = TRUE)
nn <- cbind(labels(popID_mat), labels(popID_dist))
popID_long_dist <- data.frame(row = nn[[1]][ind[, 1]],
                              col = nn[[2]][ind[, 2]],
                              km = popID_mat[ind])
nrow(popID_long_dist)

## genetic distances (fst)
fst <- read.csv("Git_Repo/5populations/R50mm75cov4/populations.fst_summary.tsv",
                sep = "\t", stringsAsFactors = F)



rownames(fst) <- fst[,1]
fst <- fst[,2:ncol(fst)]
colnames(fst) <- sub('[X^]', '', colnames(fst))
ind <- which(upper.tri(fst, diag = TRUE), arr.ind = TRUE)
nn <- dimnames(fst)
fst_long <- data.frame(row = nn[[1]][ind[, 1]],
           col = nn[[2]][ind[, 2]],
           fst = fst[ind])
arrange(fst_long)
nrow(fst_long)

ibd <- merge(popID_long_dist, fst_long)

colnames(fst_long)[1:2] <- c('col', 'row')
ibd2 <- merge(popID_long_dist, fst_long)

ibd_test <- rbind(ibd,ibd2)

ibd_test <- ibd_test[!is.na(ibd_test$km),]
ibd_test <- mutate(ibd_test, g_dist=fst/(1-fst), label=paste(row, col, sep = "-"))
arrange(ibd_test, km)

### seperate relevant pops
carinu_pops <- filter(metadat, Grp=="carinulata") %>%
  select("popID")
colnames(carinu_pops)[1] <- "row"
carinu_ibd <- semi_join(ibd_test, carinu_pops)
colnames(carinu_pops)[1] <- "col"
carinu_ibd <- semi_join(carinu_ibd, carinu_pops)
arrange(carinu_ibd, desc(g_dist))
carinu_ibd$Group <- "carinulata"

# write.table(carinu_ibd, "Git_Repo/ibd/carinu_ibd.tsv",
#           sep = "\t",
#           row.names =F,
#           quote = F)
### Manually designated pops as either Fukang, Chilik, or Admixed 
carinu_ibd <- read.delim("Git_Repo/ibd/carinu_ibd.txt")

ggplot(data = carinu_ibd, aes(x=km, y = g_dist, label = label)) +
  stat_smooth(method = "lm", col = "red") +
  # annotate(geom="text", x=250, y=.09, label=paste("Adj R2 = ",signif(summary(carinulata_ibd.lm)$adj.r.squared, 5),
  #                    "Intercept =",signif(carinulata_ibd.lm$coef[[1]],5 ),
  #          " Slope =",signif(carinulata_ibd.lm$coef[[2]], 5),
  #          " P =",signif(summary(carinulata_ibd.lm)$coef[2,4], 5)),
  #          color="red", size = 2) +
  ggrepel::geom_label_repel(size = 4) +
  geom_point(size = 3, aes(shape = Ecotype)) +
  scale_shape_manual(name= "", 
                     values = c(15, 12),
                      labels = c("Across Ecotypes", 
                                 "Within Ecotypes")) +
  theme_bw() +
  ylab("FST/(1-FST)") +
  xlab("Distance between sites (km)")
ggsave("carinulata_ibd.jpg", dpi = 300)

carinu_ibd.lm = lm(g_dist ~ km, data=carinu_ibd) 
summary(carinu_ibd.lm) 
cor.test(carinu_ibd$g_dist, carinu_ibd$km)


### hybrid
hybrid_pops <- filter(metadat, Group=="pecos") %>%
  select("PopID")
colnames(hybrid_pops)[1] <- "row"
hybrid_ibd <- semi_join(ibd_test, hybrid_pops)
colnames(hybrid_pops)[1] <- "col"
hybrid_ibd <- semi_join(hybrid_ibd, hybrid_pops)
head(hybrid_ibd)
hybrid_ibd$Group <- "pecos"

# plot_hybrid_ibd <- 
  ggplot(data = hybrid_ibd, aes(x=km, y = g_dist, label = label)) +
  geom_point(size = 2.5) +
  ggrepel::geom_label_repel() +
  theme_bw() +
  ylab("FST/(1-FST)") +
  xlab("Distance between sites (km)")
# ggsave("pecos_ibd.pdf")

hybrid_ibd.lm = lm(g_dist ~ km, data=hybrid_ibd) 
summary(hybrid_ibd.lm) 



### sublineata
sub_pops <- filter(metadat, Group=="sublineata") %>%
  select("PopID")
colnames(sub_pops)[1] <- "row"
sub_ibd <- semi_join(ibd_test, sub_pops)
colnames(sub_pops)[1] <- "col"
sub_ibd <- semi_join(sub_ibd, sub_pops)
head(sub_ibd)
sub_ibd$Group <- "sublineata"

plot_sub_ibd <- ggplot(data = sub_ibd, aes(x=km, y = g_dist, label = label)) +
  geom_point(size = 2.5) +
  ggrepel::geom_label_repel() +
  theme_classic()+
  ylab("FST/(1-FST)") +
  xlab("Distance between sites (km)")
# + ggsave("sublineata_ibd.pdf")
plot_sub_ibd

carinata_pops <- filter(metadat, Group=="carinata") %>%
  select("PopID")
colnames(carinata_pops)[1] <- "row"
carinata_ibd <- semi_join(ibd_test, carinata_pops)
colnames(carinata_pops)[1] <- "col"
carinata_ibd <- semi_join(carinata_ibd, carinata_pops)
head(carinata_ibd)
carinata_ibd$Group <- "carinata"

carinata_ibd.lm = lm(g_dist ~ km, data=carinata_ibd) 
summary(carinata_ibd.lm) 

plot_carinata_ibd <- ggplot(data = carinata_ibd, aes(x=km, y = g_dist, label = label)) +
  # stat_smooth(method = "lm", col = "red") +
  geom_point() +
  ggrepel::geom_label_repel()+
  theme_bw()+
  ylab("FST/(1-FST)") +
  xlab("Distance between sites (km)") 
  # +  ggsave("carinata_ibd.pdf")
plot_carinata_ibd

### 38 and 39 are super duper close to each other; less than 1 km
metadat$Grp
elonagata_pops <- filter(metadat, Grp=="native_elongata") %>%
  select("popID")
colnames(elonagata_pops)[1] <- "row"
elongata_ibd <- semi_join(ibd_test, elonagata_pops)
colnames(elonagata_pops)[1] <- "col"
elongata_ibd <- semi_join(elongata_ibd, elonagata_pops)
head(elongata_ibd)
elongata_ibd$Group <- "elongata"

plot_elongata_ibd <- ggplot(data = elongata_ibd, aes(x=km, y = g_dist, label = label)) +
  geom_point(size = 2.5) +
  ggrepel::geom_label_repel()+
  theme_bw()+
  ylab("FST/(1-FST)") +
  xlab("Distance between sites (km)")
  # +ggsave("elongata_ibd.pdf")
plot_elongata_ibd

all_ibd <- rbind(elongata_ibd, carinu_ibd, hybrid_ibd, sub_ibd, carinata_ibd)

plot_all_ibd <- ggplot(data = all_ibd, aes(x=km, y = g_dist, label = label, color = Group)) +
  geom_point(size = 2.5) +
  # ggrepel::geom_label_repel() +
  # facet_wrap(~Group) +
  theme_bw()+
  ylab("FST/(1-FST)") +
  xlab("Distance between sites (km)") +
  ggsave("all_ibd.pdf")

plot_all_ibd
plot_carinata_ibd
plot_carinulata_ibd
plot_elongata_ibd
plot_hybrid_ibd
plot_sub_ibd

cor.test(hybrid_ibd$g_dist, hybrid_ibd$km)
cor.test(sub_ibd$g_dist, sub_ibd$km)
cor.test(all_ibd$g_dist, all_ibd$km)
cor.test(elongata_ibd$g_dist, elongata_ibd$km)
cor.test(carinata_ibd$g_dist, carinata_ibd$km)

#### heterozygosity etc ####
metadat <- read.csv("sample_metadata.csv")
colnames(metadat)
popID_pos <- metadat[,c('Latitude', 'Longitude')]
rownames(popID_pos) <- metadat[,'PopID']

popmap <- read.csv("popmap_StahlkeBitume_etal_2019_final.tsv", sep = "\t",  
                   col.names = c("sampID", "PopID"), stringsAsFactors = FALSE, header = FALSE)
length(unique(popmap$popID))
varstats <- read.csv("Git_Repo/5populations/R50/populations.sumstats_summary.tsv", 
                      sep = "\t", nrows = 35, skip = 1)
varstats

varstats <- read.csv("popgen_stats/others_StahlkeBitume_etal_2019.snps.mm50indmissing75.recode.p.sumstats_summary.variant.csv")
varstats <- read.csv("popgen_stats/heir_carinu/carinu.snps.mm50indmissing75.recode.p.sumstats_summary.csv")

varstats <- merge(varstats, metadat)
# varstats <- varstats %>%
#   mutate(exphet_sd = sqrt(Var.4))

het <- ggplot(varstats, aes(x=PopID, y=Exp_Het, fill = Group))  +
  # geom_col(aes()) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar( aes(ymin=Exp_Het-StdErr.4, ymax=Exp_Het+StdErr.4),  alpha=0.5) +
  # geom_errorbar( aes(ymin=Exp_Het-exphet_sd, ymax=Exp_Het+exphet_sd),  alpha=0.9, size=1) +
  facet_grid(~Group, scales = "free_x", space = "free") +
  scale_fill_grey() +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom") 
het
ggsave("carinu_exphet_111519.pdf")

ggplot(varstats, aes(x=PopID, y=Fis, fill = Group))  +
  # geom_col(aes()) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar( aes(ymin=Fis-StdErr.7, ymax=Fis+StdErr.7),  alpha=0.5) +
  # geom_errorbar( aes(ymin=Exp_Het-exphet_sd, ymax=Exp_Het+exphet_sd),  alpha=0.9, size=1) +
  facet_grid(~Group, scales = "free_x", space = "free") +
  scale_fill_grey() +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom") 
ggsave("carinu_fis_111519.pdf")

ggplot(varstats, aes(x=PopID, y=Pi, fill = Group))  +
  # geom_col(aes()) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_errorbar( aes(ymin=Pi-StdErr.6, ymax=Pi+StdErr.6),  alpha=0.5) +
  # geom_errorbar( aes(ymin=Exp_Het-exphet_sd, ymax=Exp_Het+exphet_sd),  alpha=0.9, size=1) +
  facet_grid(~Group, scales = "free_x", space = "free") +
  scale_fill_grey() +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom") 
ggsave("carinu_pi_111519.pdf")
