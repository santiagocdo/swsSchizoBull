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



# # # # # # # # # # visualization and statistics # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # behaviour by group only # # # # # # # # # # # # # # # # # # 
# which behavioural variables you want to analyze from clean?
# vec_beh <- c("beta_change","c_change","d_change","farate_change","hitrate_change")
vec_beh <- c("c_change","d_change","farate_change","hitrate_change",
             "pcorrect_change","preport_change")

# which behavioural metrics changed after the "unaltered" condition?
beh_t_tests <- data.frame(beh=NA, tscore=NA, mean=NA, p_val=NA, ci_low=NA, ci_high=NA)
beh_t_tests <- beh_t_tests[!is.na(beh_t_tests),]
for (i in 1:length(vec_beh)) {
  tmp <- t.test(clean[,vec_beh[i]],mu=0)
  beh_t_tests <- rbind(beh_t_tests, data.frame(beh=vec_beh[i], 
                                               tscore=tmp$statistic, 
                                               mean=mean(clean[,vec_beh[i]]),
                                               p_val=tmp$p.value, 
                                               ci_low=tmp$conf.int[1], 
                                               ci_high=tmp$conf.int[2]))
}; rm(tmp)



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
beh_stats <- data.frame(sdt_parameter=NA, coef=NA, Coefficient=NA, p=NA, Std_Coefficient=NA, 
                        Std_Coefficient_CI_low=NA, Std_Coefficient_CI_high=NA)
beh_stats <- beh_stats[!is.na(beh_stats),]

for (i in 1:nrow(beh_pre_post)) {
  # rehshape to long format
  tmp <- melt(clean, measure.vars = beh_pre_post[i,])
  # remove columns with pre and post to avoid problems in column naming
  tmp <- tmp[,!(grepl("pre",colnames(tmp)) | grepl("post",colnames(tmp)))]
  # run a linear model with an interaction
  m <-lmer(value~variable*grp+(1|src_subject_id), tmp); res <- residuals(m)
  # m <-lmer(value~variable*bprs_hall+(1|src_subject_id), tmp)
  # print(shapiro.test(res))
  print(ks.test(res,"pnorm",mean=mean(res),sd=sd(res)))
  # hist(res)
  mod <- report_table(m)
  # print(anova(lmer(value~variable*grp+(1|src_subject_id), tmp)))
  # mod <- aov(value~variable*grp+Error(src_subject_id/variable), tmp)
  beh_stats <- rbind(beh_stats, data.frame(sdt_parameter=strsplit(beh_pre_post[i,1],"_")[[1]][1],
                                           coef=c("Phase[Post]","Group[SZ]","Interaction"),
                                           Coefficient=mod$Coefficient[2:4],
                                           p_val=mod$p[2:4],
                                           Std_Coefficient=mod$Std_Coefficient[2:4],
                                           Std_Coefficient_CI_low=mod$Std_Coefficient_CI_low[2:4],
                                           Std_Coefficient_CI_high=mod$Std_Coefficient_CI_high[2:4]))
  # split the variable into parameter and temporal variable (pre and post)
  tmp1 <- do.call(rbind, strsplit(as.character(tmp$variable),"_"))
  colnames(tmp1) <- c("sdt_parameter","phase")
  if (i == 1) {
    supermelt <- cbind(tmp, tmp1)
  } else {
    supermelt <- rbind(supermelt, cbind(tmp, tmp1))
  }
}; rm(tmp, tmp1, mod, beh_pre_post)
# change phase characters for integers
supermelt$phase <- ifelse(supermelt$phase=="pre",0,1)

# labels for the facet_grid (used in ggplot, figure1 and figure2)
sdt.labs <- c("Response Threshold\nmore=less reporting 'yes'",
              "Sensitivity\nmore=better discrimination",
              "False Alarm Rate\nmore='yes' when unintelligible",
              "Hit Rate\nmore='yes' when intelligible",
              "p(correct)\nmore=better",
              "p(report 'yes')\nmore=more reporting 'yes'")
names(sdt.labs) <- c("c", "dprime","fradj","hradj","pcorrect","preport")
supermelt$sdt_parameter <- factor(supermelt$sdt_parameter, levels =  c("dprime","fradj","c","hradj","pcorrect","preport"))
beh_stats$sdt_parameter <- factor(beh_stats$sdt_parameter, levels =  c("dprime","fradj","c","hradj","pcorrect","preport"))



# # # visualize within subject standard errors of the mean # # #
# (Cousineau & O'Brien, 2014, Behavioral Research)

# calculate grand means and add a new column
tmp <- as.data.frame(supermelt %>% group_by(sdt_parameter) %>%
                       mutate(grand_mean = mean(value)) %>% 
                       ungroup())
# averages per participant and parameter type 
tmp <- as.data.frame(tmp %>% group_by(src_subject_id, sdt_parameter) %>%
                       mutate(mean_par = mean(value),
                              # scale scores per participant
                              scale_value = value - mean_par + grand_mean) %>% 
                       ungroup())
# estimate means and standard errors
for_plot <- as.data.frame(tmp %>% group_by(phase, grp, sdt_parameter) %>% 
                            summarise(n=n(),
                                      mean=mean(value),
                                      scale_mean=mean(scale_value),
                                      sd=sd(scale_value),
                                      sem=sd(scale_value)/sqrt(n)*sqrt(2/1), # (J/(J-1)) where J is the within subject measure
                                      error_min=mean-sem,
                                      error_max=mean+sem,
                                      max=max(value),
                                      min=min(value)
                                      ))
as.data.frame(for_plot %>% group_by(sdt_parameter) %>% 
                summarise(min(error_min),
                          max(error_max)))


# # # # # # # # # # Figures # # # # # # # # # # # # # # # # # # # # # # # # # #
if (!require(ggh4x)) {install.packages("ggh4x")}; library(ggh4x)
(figure1A <- ggplot(for_plot, aes(x=phase, y=mean, col=grp, shape=grp, fill=grp)) + 
  labs(x="Phase", y="Score", col="Group",shape="Group",fill="Group") +
    # geom_line(data=supermelt, aes(x=phase, y=value, group=src_subject_id), 
    #           col="grey", alpha=.1) +
    geom_line(linewidth=1,
              position = position_dodge(.2)) +
    geom_errorbar(aes(ymin = error_min, ymax = error_max),
                  position = position_dodge(.2), width=.2, linewidth=.4) +
    geom_point(size=2.5,
               position = position_dodge(.2)) +
    scale_x_continuous(breaks = c(0,1), labels = c("Pre","Post"), limits = c(-.2, 1.2)) +
    scale_color_manual(values = c("hc"="orange", "sz"="red"), # #1F5B73 and #F29199
                       labels = c("Control", "Schizophrenia")) +
    scale_fill_manual(values = c("hc"="orange", "sz"="red"),
                      labels = c("Control", "Schizophrenia")) +
    scale_shape_manual(values = c("hc" = 21, "sz" = 23), # 25 and 24
                       labels = c("Control", "Schizophrenia")) +
    facet_wrap(sdt_parameter~., scales="free_y", ncol = 2,
               labeller = labeller(sdt_parameter = sdt.labs)) +
    theme_classic() + 
    ggh4x::facetted_pos_scales(
      y = list(
        sdt_parameter == "dprime" ~ scale_y_continuous(breaks = c(2.6,3.1)),
        sdt_parameter == "fradj" ~ scale_y_continuous(breaks = c(.07,.22)),
        sdt_parameter == "c" ~ scale_y_continuous(breaks = c(-.38,.3)),
        sdt_parameter == "hradj" ~ scale_y_continuous(breaks = c(.78,.93)),
        sdt_parameter == "pcorrect" ~ scale_y_continuous(breaks = c(.82,.89)),
        sdt_parameter == "preport" ~ scale_y_continuous(breaks = c(.43,.58))
      )))

beh_stats$coef <- factor(beh_stats$coef, levels = c("Interaction","Group[SZ]","Phase[Post]"))
(figure1B <- ggplot(beh_stats, aes(x=Std_Coefficient,y=coef)) +
    labs(x="Standarized Coefficients (Effect Size & 95% CI)", y="Regression Coefficients") +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin = Std_Coefficient_CI_low, 
                      xmax = Std_Coefficient_CI_high), width=.2, linewidth=.4) +
    scale_x_continuous(breaks = c(-.8, 0, .8)) +
    scale_y_discrete(labels=c(expression(Interaction:~beta[3]),
                              expression(Group[SZ]:~beta[2]),
                              expression(Phase[Post]:~beta[1]))) +
    geom_point(size=2) +
    facet_wrap(sdt_parameter~., ncol = 2, labeller = labeller(sdt_parameter = sdt.labs)) +
    theme_classic())
tab_fig1b <- cbind(beh_stats[,1:2],round(beh_stats[,3:7],4))

# View(tab_fig1b)
if (print_csv == 1) {
  write.csv(tab_fig1b, file = "results/tab_fig1b.csv", row.names = F)
}

# main figure
(figure1 <- ggarrange(figure1A, figure1B, labels = c("A","B"), common.legend = T))


# save Figure 2
if (print_csv == 1) {
  ggsave("results/figure2.pdf", figure1, dpi = 1800, scale = 1,
         units = "cm", width = 24, height = 18, bg = "white")
}



# # # # # # # # # # behaviour and all questionnaires # # # # # # # # # # # # # # 

# which behavioural variables you want to analyze from clean?
vec_quest <- colnames(clean)[grepl("bprs",colnames(clean))]
vec_quest <- vec_quest[!grepl("_mann",vec_quest)]
# vec_quest <- c("bprs_total","bprs_pos","bprs_neg","bprs_disorg",
#                "bprs_depression","bprs_mania","bprs_unus")

vec_beh <- c(vec_beh,"hradj_pre","fradj_pre","dprime_pre","c_pre","pcorrect_pre","preport_pre")

# create empty data frame where all stats will be stored in the following for loop:
stats <- data.frame(beh=NA, quest=NA, Std_Coefficient=NA,
                    p_val=NA, Std_Coefficient_CI_low=NA, Std_Coefficient_CI_high=NA,
                    rho=NA, p_val_cor=NA)
# remove rows with NAs, e.g.,bprs_mann does not vary, so we cannot compute a correlation
stats <- stats[!is.na(stats$Std_Coefficient),]

for (j in 1:length(vec_quest)) {
  for (i in 1:length(vec_beh)) {
    # # # formula for linear model # # # 
    formul <- as.formula(paste0(vec_beh[i],"~",vec_quest[j]))
    # linear model fit with table including effect sizes
    mod <- report_table(lm(formul, clean[,c(vec_beh[i], vec_quest[j])]))
    # correlations
    cor <- cor.test(clean[, vec_beh[i]], clean[, vec_quest[j]], method="spearman")
    # combine all
    stats <- rbind(stats, data.frame(beh=vec_beh[i],quest=vec_quest[j],
                                     Std_Coefficient=mod$Std_Coefficient[2],
                                     p_val=mod$p[2],
                                     Std_Coefficient_CI_low=mod$Std_Coefficient_CI_low[2],
                                     Std_Coefficient_CI_high=mod$Std_Coefficient_CI_high[2],
                                     rho=cor$estimate,
                                     p_val_cor=cor$p.value))
  }
}
# remove the rownames from stats
rownames(stats) <- NULL



# Multiple comparisons, correction methods
stats2 <- stats[order(stats$p_val_cor),]

# Step 2, only BPRS main scores (total, positive, and negative)
tmp <- stats2[stats2$quest %in% c("bprs_total","bprs_pos","bprs_neg","bprs_hall"),]
tmp <- tmp[!grepl("pcorrect",tmp$beh) & !grepl("preport",tmp$beh),]

# save Figure 2
if (print_csv == 1) {
  write.csv(tmp,"results/table2.csv",row.names = F, na = "")
}

# Bonferroni per Symptom and Condition (i.e. divide by 4)
tmp$p_val_cor < .05/4


# select only the change scores
tmp <- stats[grepl("change",stats$beh),]

# change subpanels
tmp$quest <- factor(tmp$quest, levels = vec_quest)

# change labels
levels(tmp$quest) <- c("Somatic Concern","Anxiety","Depression","Suicidality","Guilt",
                         "Hostility","Elated Mood","Grandiosity","Suspiciousness","Hallucinations",
                         "Unusual Thought Content","Bizarre Behavior","Self-neglect","Disorientation","Conceptual Disorganization",
                         "Blunted Affect","Emotional Withdrawal","Motor retardation","Tension","Uncooperativeness",
                         "Excitement","Distractibility","Motor Hyperactivity","Total","Total Positive",
                         "Total Negative","Total Disorganization","Total Depression","Total Mania")
tmp$significance <- ifelse(tmp$p_val < .01, "p<.01","ns")
# visualize the effect sizes in forest plot type.
(figureS1 <- ggplot(tmp, aes(x=Std_Coefficient,y=beh,shape=significance)) +
  labs(title="Brief Psychiatric Rating Scale (24-items + Subscales)",
       x="Standarized Coefficients (Effect Size)", 
       y="Signal Detection Theory parameters") +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin = Std_Coefficient_CI_low, 
                    xmax = Std_Coefficient_CI_high), width=.4, linewidth=.4) +
  scale_x_continuous(breaks = c(round(min(stats$Std_Coefficient_CI_low)*.9,2), 0, 
                                round(max(stats$Std_Coefficient_CI_high)*.9,2))) +
  scale_y_discrete(labels = c(expression(C[Post-Pre]),
                              expression(d*`'`[Post-Pre]),
                              expression(p*`(`*fa*`)`[Post-Pre]),
                              expression(p*`(`*h*`)`[Post-Pre]),
                              expression(p*`(`*correct*`)`[Post-Pre]),
                              expression(p*`(`*report~yes*`)`[Post-Pre]))) +
    scale_shape_manual(values=c(19,21)) +
  geom_point(size=2,fill="white") +
  facet_wrap(quest~.) +
  theme_classic() + theme(legend.position = c(.9,.1)))
if (print_csv == 1) {
  ggsave("results/figureS1.pdf", figureS1, dpi = 1800, scale = 1,
         units = "cm", width = 24, height = 18, bg = "white")
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
source("functions.R")

clean$missadj_pre <- 1 - clean$hradj_pre
clean$missadj_post <- 1 - clean$hradj_post

# combine 
relevant_columns <- c("age","demo_gender","race","parent_school","participant_school",
                      "bprs_total","bprs_pos","bprs_neg","bprs_disorg","bprs_depression","bprs_mania",
                      "pcorrect_pre","pcorrect_post",
                      "preport_pre","preport_post",
                      "dprime_pre","dprime_post","c_pre","c_post",
                      "fradj_pre","fradj_post","hradj_pre","hradj_post",
                      "missadj_pre","missadj_post")

# create descriptive guide skeleton
descrGuide <- matrix(c(relevant_columns,
                       1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),ncol=2)

tests <- list(); pvals <- as.vector(NA)
for (i in 1:nrow(descrGuide)) {
  # it must no be bprs, because only patients got this measure
  if (!grepl("bprs",descrGuide[i,1])) {
    # depending if is continuous or categorical
    if (descrGuide[i,2]==1) {
      tests[[i]] <- t.test(clean[clean$grp=="sz",relevant_columns[i]],
                           clean[clean$grp=="hc",relevant_columns[i]])
    } else {
      tests[[i]] <- chisq.test(clean$grp, clean[,relevant_columns[i]])
    }
    pvals[i] <- round(tests[[i]]$p.value,4)
  }
}
names(tests) <- relevant_columns

# produce appendix tables 
tabAll <- data.frame(version="all",f_suppTables(clean,descrGuide))
tabSz <- data.frame(version="sz",f_suppTables(clean[clean$grp=="sz",],descrGuide))
tabHc <- data.frame(version="hc",f_suppTables(clean[clean$grp=="hc",],descrGuide))

# combine
tabAll <- rbind(tabAll,tabSz,tabHc)

if (print_csv == 1) {
  write.csv(tabAll,"results/table1.csv",row.names = F, na = "")
}
