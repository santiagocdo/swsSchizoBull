### Collaborators (auto-populated):
###
### Santiago Castiello
# remove environment 
rm(list = ls())

# print csv and images?
print_csv <- 0

# load packages
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if (!require(report)) {install.packages("report")}; library(report)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)

# read wide format data base
clean <- read.csv("data/clean.csv")

# schizophrenia ids
sz_ids <- clean$src_subject_id[clean$grp == "sz"]
hc_ids <- clean$src_subject_id[clean$grp == "hc"]

# number of sz participants per run
n_sample <- 27#round(length(sz_ids)/2)

# number of runs for permutation distrubtion
n_runs <- 1000

# now run interactions and prepare data for main figure
beh_pre_post <- t(matrix(c("hradj_pre","hradj_post",
                           "fradj_pre","fradj_post",
                           "dprime_pre","dprime_post",
                           "c_pre","c_post",
                           "pcorrect_pre","pcorrect_post",
                           "preport_pre","preport_post"
                           # "beta_pre","beta_post"
),nrow=2))
# prepare empty data.frame
beh_stats <- data.frame(n_run=NA,sdt_parameter=NA,
                        beh_var=NA,beh_p=NA,
                        grp_var=NA,grp_p=NA,
                        int_var=NA,int_p=NA)
beh_stats <- beh_stats[!is.na(beh_stats),]

# run nested loops: ith for n_runs, and jth for each behavioural variable
for (i in 1:n_runs) {
  # create ids for a ith sample
  ids <- sample(sz_ids)
  ids <- ids[1:n_sample]
  # add healthy controls ids
  ids <- c(ids, hc_ids) 
  
  # filter clean data
  tmp <- clean[clean$src_subject_id %in% ids,]
  
  for (j in 1:nrow(beh_pre_post)) {
    # melt for within subject component in lmer
    tmp2 <- melt(tmp, measure.vars = beh_pre_post[j,])
    
    # fot model
    m <- summary(lmer(value~variable*grp+(1|src_subject_id), tmp2))
    
    # combine them
    beh_stats <- rbind(beh_stats, 
                       data.frame(n_run=i,
                                  sdt_parameter=strsplit(beh_pre_post[j,1],"_")[[1]][1],
                                  beh_var=coef(m)[2,1],beh_p=coef(m)[2,5],
                                  grp_var=coef(m)[3,1],grp_p=coef(m)[3,5],
                                  int_var=coef(m)[4,1],int_p=coef(m)[4,5]))
  } # end jth
} # end ith


dprime  <- beh_stats[beh_stats$sdt_parameter == "dprime",]
sum(dprime$beh_p < .05); sum(dprime$grp_p < .05); sum(dprime$int_p < .05)

fradj <- beh_stats[beh_stats$sdt_parameter == "fradj",]
sum(fradj$beh_p < .05); sum(fradj$grp_p < .05); sum(fradj$int_p < .05)

c <- beh_stats[beh_stats$sdt_parameter == "c",]
sum(c$beh_p < .05); sum(c$grp_p < .05); sum(c$int_p < .05)

hradj <- beh_stats[beh_stats$sdt_parameter == "hradj",]
sum(hradj$beh_p < .05); sum(hradj$grp_p < .05); sum(hradj$int_p < .05)

pcorrect  <- beh_stats[beh_stats$sdt_parameter == "pcorrect",]
sum(pcorrect$beh_p < .05); sum(pcorrect$grp_p < .05); sum(pcorrect$int_p < .05)

preport  <- beh_stats[beh_stats$sdt_parameter == "preport",]
sum(preport$beh_p < .05); sum(preport$grp_p < .05); sum(preport$int_p < .05)



tmp <- melt(beh_stats, measure.vars = c("beh_var","grp_var","int_var"))

prop_data <- tmp %>%
  group_by(variable, sdt_parameter) %>%
  summarise(
    # Calculate the proportion where 'value' is greater than 0
    prop_gt_0 = mean(value > 0),
    prop_lo_0 = mean(value < 0),
    .groups = 'drop'
  )
prop_data$prop <- ifelse(prop_data$prop_gt_0 > prop_data$prop_lo_0, 
                         prop_data$prop_gt_0, prop_data$prop_lo_0)

prop_data$x <- c(-.2,-.2,.03,.1,-.04,.05,
                 -.2,.3,.05,.06,.05,.05,
                 -.25,.3,-.03,.1,.05,.07)
prop_data$y <- c(900,900,900,900,900,900,
                 125,125,125,125,125,125,
                 175,175,175,175,175,175)
prop_data$col <- c(rep("red",6),
                   "red","black","black","red","black","red",
                   "red",rep("black",5))

# Define new labels for the columns (sdt_parameter)
sdt_labels <- c(
  "c" = "Bias (c)",
  "dprime" = "Sensitivity (d')",
  "fradj" = "False Rate (adj.)",
  "hradj" = "Hit Rate (adj.)",
  "pcorrect" = "P(Correct)",
  "preport" = "P(Report)"
)

# Define new labels for the rows (variable)
variable_labels <- c(
  "beh_var" = "Behavioral Effect",
  "grp_var" = "Group Difference",
  "int_var" = "Interaction Effect"
)

ggplot(tmp, aes(x = value)) +
  geom_vline(xintercept = 0, col="black") +
  geom_text(
    data = prop_data,
    aes(x = x, y = y, label = prop, col = "red"),
    # Adjust position slightly to avoid overlap
    hjust = 1, vjust = 1, size = 5
  ) +
  geom_histogram(fill="white", col="black") + # You may need to specify binwidth or bins
  facet_grid(variable ~ sdt_parameter, scales = "free",
             labeller = labeller(
               sdt_parameter = sdt_labels, # Apply to columns
               variable = variable_labels   # Apply to rows
             )) +
  theme_bw()
