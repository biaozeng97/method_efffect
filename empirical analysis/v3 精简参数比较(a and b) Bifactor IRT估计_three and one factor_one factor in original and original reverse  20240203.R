library(mirt)
library(openxlsx)
setwd("D:/U盘数据 20220115备份/中心博士/学术会议/IMPS 2023/data analysis/Method Effect/区分度和难度参数化估计结果 20240203")


#######################################
##    Bifactor with three factors    ##
#######################################

dataA<-read.csv("A version raw data.csv",header=T)
#IRT full information Bifactor Model
specific_A <- c(1,2,1,2,2,1,2,1,2,2,1,2,1,2,1,2,1,2,2,2) # 1 is positively worded, 2 is negatively worded
model_A <- '
G1 = 1,10,16,14,19,8
G2 = 3,15,11,13,6,18
G3 = 4,7,9,17,5,2,20,12'
#quadpts dropped for faster estimation, but not as precise
modA <- bfactor(dataA, specific_A, model_A)
coef.A <- coef(modA, simplify=TRUE)
par.A <- as.data.frame(coef.A$items)
# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.A$a1^2 + par.A$a2^2 + par.A$a3^2 + par.A$a4^2 + par.A$a5^2)

# Calculate the b parameters
par.A$b1 <- par.A$d1 / -a_squared_sum
par.A$b2 <- par.A$d2 / -a_squared_sum
par.A$b3 <- par.A$d3 / -a_squared_sum

# Display the calculation results
par.A[c("b1", "b2", "b3")]

write.xlsx(par.A, "A version Bifactor result(three dimensional).xls")



dataB <- read.csv('B version raw data.csv')
#IRT full information Bifactor Model
specific_B <- c(2,1,2,1,1,2,1,2,1,1,2,1,2,1,2,1,2,1,1,1) # 1 is positively worded, 2 is negatively worded
model_B <- '
G1 = 1,10,16,14,19,8
G2 = 3,15,11,13,6,18
G3 = 4,7,9,17,5,2,20,12
'
modB <- bfactor(dataB, specific_B, model_B)
coef.B <- coef(modB, simplify=TRUE)
par.B <- as.data.frame(coef.B$items)
# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.B$a1^2 + par.B$a2^2 + par.B$a3^2 + par.B$a4^2 + par.B$a5^2)

# Calculate the b parameters
par.B$b1 <- par.B$d1 / -a_squared_sum
par.B$b2 <- par.B$d2 / -a_squared_sum
par.B$b3 <- par.B$d3 / -a_squared_sum

# Display the calculation results
par.B[c("b1", "b2", "b3")]

write.xlsx(par.B, "B version Bifactor result(three dimensional).xls")


#########################
##    three factors    ##
#########################

## new added in 20231212
dataA<-read.csv("A version raw data.csv",header=T)
#IRT full information Bifactor Model
model_A_threefactor <- '
G1 = 1,10,16,14,19,8
G2 = 3,15,11,13,6,18
G3 = 4,7,9,17,5,2,20,12'
modA_threefactor <- mirt(dataA,model_A_threefactor,itemtype = 'graded', SE=TRUE)
coef.A_threefactor <- coef(modA_threefactor, simplify=TRUE)
par.A_threefactor <- as.data.frame(coef.A_threefactor$items)

# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.A_threefactor$a1^2 + par.A_threefactor$a2^2 + par.A_threefactor$a3^2)

# Calculate the b parameters
par.A_threefactor$b1 <- par.A_threefactor$d1 / -a_squared_sum
par.A_threefactor$b2 <- par.A_threefactor$d2 / -a_squared_sum
par.A_threefactor$b3 <- par.A_threefactor$d3 / -a_squared_sum

# Display the calculation results
par.A_threefactor[c("b1", "b2", "b3")]

write.xlsx(par.A_threefactor, "A version three factor result(three dimensional).xls")



dataB <- read.csv('B version raw data.csv')
#IRT full information Bifactor Model
model_B_threefactor <-'
G1 = 1,10,16,14,19,8
G2 = 3,15,11,13,6,18
G3 = 4,7,9,17,5,2,20,12'
modB_threefactor <- mirt(dataB,model_B_threefactor,itemtype = 'graded', SE=TRUE)
coef.B_threefactor <- coef(modB_threefactor, simplify=TRUE)
par.B_threefactor <- as.data.frame(coef.B_threefactor$items)
# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.B_threefactor$a1^2 + par.B_threefactor$a2^2 + par.B_threefactor$a3^2)

# Calculate the b parameters
par.B_threefactor$b1 <- par.B_threefactor$d1 / -a_squared_sum
par.B_threefactor$b2 <- par.B_threefactor$d2 / -a_squared_sum
par.B_threefactor$b3 <- par.B_threefactor$d3 / -a_squared_sum

# Display the calculation results
par.B_threefactor[c("b1", "b2", "b3")]

write.xlsx(par.B_threefactor, "B version three factor result(three dimensional).xls")



dataC <- read.csv('C Version raw data.csv')
specific_C <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) # 1 is the positively worded effect
model_C <-'
F1 = 1,10,16,14,19,8
F2 = 3,15,11,13,6,18
F3 = 4,7,9,17,5,2,20,12'
modC <- bfactor(dataC, specific_C, model_C)
coef.C <- coef(modC, simplify=TRUE)
par.C <- as.data.frame(coef.C$items)
# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.C$a1^2 + par.C$a2^2 + par.C$a3^2)

# Calculate the b parameters
par.C$b1 <- par.C$d1 / -a_squared_sum
par.C$b2 <- par.C$d2 / -a_squared_sum
par.C$b3 <- par.C$d3 / -a_squared_sum

# Display the calculation results
par.C[c("b1", "b2", "b3")]

write.xlsx(par.C, "C version Bifactor result(three dimensional).xls")



dataD <- read.csv('D Version raw data.csv')
specific_D <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) # 1 is the negatively worded effect
model_D <-'
G1 = 1,10,16,14,19,8
G2 = 3,15,11,13,6,18
G3 = 4,7,9,17,5,2,20,12
'
modD <- bfactor(dataD, specific_D, model_D)
coef.D <- coef(modD, simplify=TRUE)
par.D <- as.data.frame(coef.D$items)
# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.D$a1^2 + par.D$a2^2 + par.D$a3^2)

# Calculate the b parameters
par.D$b1 <- par.D$d1 / -a_squared_sum
par.D$b2 <- par.D$d2 / -a_squared_sum
par.D$b3 <- par.D$d3 / -a_squared_sum

# Display the calculation results
par.D[c("b1", "b2", "b3")]

write.xlsx(par.D, "D version Bifactor result(three dimensional).xls")





###########################################################
##     Bi-factor for original and original reverse      ##
###########################################################

dataA<-read.csv("A version raw data.csv",header=T)
#IRT full information Bifactor Model
specific_A_uni <- c(1,2,1,2,2,1,2,1,2,2,1,2,1,2,1,2,1,2,2,2) # 1 is positively worded, 2 is negatively worded
model_A_uni <- '
G1 = 1-20'
#quadpts dropped for faster estimation, but not as precise
modA_uni <- bfactor(dataA, specific_A_uni, model_A_uni)
coef.A_uni <- coef(modA_uni, simplify=TRUE)
par.A_uni <- as.data.frame(coef.A_uni$items)

# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.A_uni$a1^2 + par.A_uni$a2^2 + par.A_uni$a3^2)

# Calculate the b parameters
par.A_uni$b1 <- par.A_uni$d1 / -a_squared_sum
par.A_uni$b2 <- par.A_uni$d2 / -a_squared_sum
par.A_uni$b3 <- par.A_uni$d3 / -a_squared_sum

# Display the calculation results
par.A_uni[c("b1", "b2", "b3")]

write.xlsx(par.A_uni, "A version Bifactor result(unidimensional).xls")



dataB <- read.csv('B version raw data.csv')
#IRT full information Bifactor Model
specific_B_uni <- c(2,1,2,1,1,2,1,2,1,1,2,1,2,1,2,1,2,1,1,1) # 1 is positively worded, 2 is negatively worded
model_B_uni <- '
G1 = 1-20
'
modB_uni <- bfactor(dataB, specific_B_uni, model_B_uni)
coef.B_uni <- coef(modB_uni, simplify=TRUE)
par.B_uni <- as.data.frame(coef.B_uni$items)
# Calculate the square root of the sum of squares of the a parameters
a_squared_sum <- sqrt(par.B_uni$a1^2 + par.B_uni$a2^2 + par.B_uni$a3^2)

# Calculate the b parameters
par.B_uni$b1 <- par.B_uni$d1 / -a_squared_sum
par.B_uni$b2 <- par.B_uni$d2 / -a_squared_sum
par.B_uni$b3 <- par.B_uni$d3 / -a_squared_sum

# Display the calculation results
par.B_uni[c("b1", "b2", "b3")]
write.xlsx(par.B_uni, "B version Bifactor result(unidimensional).xls")





#########################
##     one factor      ##
#########################

## new added in 20231212
dataA<-read.csv("A version raw data.csv",header=T)
#IRT full information Bifactor Model
model_A_onefactor <-'G1 = 1-20'
modA_onefactor <- mirt(dataA,model_A_onefactor,itemtype = 'graded', SE=TRUE)
coef.A_onefactor <- coef(modA_onefactor, simplify=TRUE, IRTpar = T)
par.A_onefactor <- as.data.frame(coef.A_onefactor$items)
write.xlsx(par.A_onefactor, "A version one factor result(unidimensional).xls")



dataB <- read.csv('B version raw data.csv')
#IRT full information Bifactor Model
model_B_onefactor <-'G1 = 1-20'
modB_onefactor <- mirt(dataB,model_B_onefactor,itemtype = 'graded', SE=TRUE)
coef.B_onefactor <- coef(modB_onefactor, simplify=TRUE, IRTpar = T)
par.B_onefactor <- as.data.frame(coef.B_onefactor$items)
write.xlsx(par.A_onefactor, "B version one factor result(unidimensional).xls")



dataC <- read.csv('C Version raw data.csv')
model_C_uni <-'
G1 = 1-20'
modC_uni <- mirt(dataC,model_C_uni,itemtype = 'graded', SE=TRUE)
coef.C_uni <- coef(modC_uni, simplify=TRUE, IRTpar = T)
par.C_uni <- as.data.frame(coef.C_uni$items)
write.xlsx(par.C_uni, "C version Bifactor result(unidimensional).xls")



dataD <- read.csv('D Version raw data.csv')
model_D_uni <-'
G1 = 1-20'
modD_uni <- mirt(dataD,model_D_uni,itemtype = 'graded', SE=TRUE)
coef.D_uni <- coef(modD_uni, simplify=TRUE, IRTpar = T)
par.D_uni <- as.data.frame(coef.D_uni$items)
write.xlsx(par.D_uni, "D version Bifactor result(unidimensional).xls")



#############################################
##    compare with three and one factor    ##
#############################################
# data A
AIC_A <- extract.mirt(modA, 'AIC')
BIC_A <- extract.mirt(modA, 'BIC')
SABIC_A <- extract.mirt(modA, 'SABIC')
logLik_A <- extract.mirt(modA, 'logLik')
G2_A <- extract.mirt(modA, 'G2')

AIC_A_uni <- extract.mirt(modA_uni, 'AIC')
BIC_A_uni <- extract.mirt(modA_uni, 'BIC')
SABIC_A_uni <- extract.mirt(modA_uni, 'SABIC')
logLik_A_uni <- extract.mirt(modA_uni, 'logLik')
G2_A_uni <- extract.mirt(modA_uni, 'G2')

# data B
AIC_B <- extract.mirt(modB, 'AIC')
BIC_B <- extract.mirt(modB, 'BIC')
SABIC_B <- extract.mirt(modB, 'SABIC')
logLik_B <- extract.mirt(modB, 'logLik')
G2_B <- extract.mirt(modB, 'G2')

AIC_B_uni <- extract.mirt(modB_uni, 'AIC')
BIC_B_uni <- extract.mirt(modB_uni, 'BIC')
SABIC_B_uni <- extract.mirt(modB_uni, 'SABIC')
logLik_B_uni <- extract.mirt(modB_uni, 'logLik')
G2_B_uni <- extract.mirt(modB_uni, 'G2')

# data C
AIC_C <- extract.mirt(modC, 'AIC')
BIC_C <- extract.mirt(modC, 'BIC')
SABIC_C <- extract.mirt(modC, 'SABIC')
logLik_C <- extract.mirt(modC, 'logLik')
G2_C <- extract.mirt(modC, 'G2')

AIC_C_uni <- extract.mirt(modC_uni, 'AIC')
BIC_C_uni <- extract.mirt(modC_uni, 'BIC')
SABIC_C_uni <- extract.mirt(modC_uni, 'SABIC')
logLik_C_uni <- extract.mirt(modC_uni, 'logLik')
G2_C_uni <- extract.mirt(modC_uni, 'G2')


# data D
AIC_D <- extract.mirt(modD, 'AIC')
BIC_D <- extract.mirt(modD, 'BIC')
SABIC_D <- extract.mirt(modD, 'SABIC')
logLik_D <- extract.mirt(modD, 'logLik')
G2_D <- extract.mirt(modD, 'G2')

AIC_D_uni <- extract.mirt(modD_uni, 'AIC')
BIC_D_uni <- extract.mirt(modD_uni, 'BIC')
SABIC_D_uni <- extract.mirt(modD_uni, 'SABIC')
logLik_D_uni <- extract.mirt(modD_uni, 'logLik')
G2_D_uni <- extract.mirt(modD_uni, 'G2')



##########################
#    one factor model   ##
##########################
##  theta and eta  ##
theta.A_onefactor <- fscores(modA_onefactor, full.scores.SE = T)
theta.B_onefactor <- fscores(modB_onefactor, full.scores.SE = T)
theta.C_uni <- fscores(modC_uni, full.scores.SE = T)
theta.D_uni <- fscores(modD_uni, full.scores.SE = T)

# Save theta data frames to CSV files
write.csv(theta.A_onefactor, "A version theta one factor trait.csv", row.names = FALSE)
write.csv(theta.B_onefactor, "B version theta one factor trait.csv", row.names = FALSE)
write.csv(theta.C_uni, "C version theta one factor traits.csv", row.names = FALSE)
write.csv(theta.D_uni, "D version theta one factor traits.csv", row.names = FALSE)



##########################################
#    one dimensional Bi-factor model   ##
##########################################
##  theta and eta  ##
theta.A_uni <- fscores(modA_uni, full.scores.SE = T)
theta.B_uni <- fscores(modB_uni, full.scores.SE = T)


# Save theta data frames to CSV files
write.csv(theta.A_uni, "A version theta bifactor with one dimensional traits.csv", row.names = FALSE)
write.csv(theta.B_uni, "B version theta bifactor with one dimensional traits.csv", row.names = FALSE)



# draw the scatter plot between theta and eta in six models
G_A <- theta.A_uni[,1]
PME_A <- theta.A_uni[,2]
NME_A <- theta.A_uni[,3]

G_B <- theta.B_uni[,1]
PME_B <- theta.B_uni[,2]
NME_B <- theta.B_uni[,3]

G_C <- theta.C_uni[,1]

G_D <- theta.D_uni[,1]




##########################################
# comparison of theta in four version ULB#
##########################################

# Assuming G_A, G_B, G_C, and G_D are your general factor scores from each version

# Create a data frame with all values and corresponding groups
theta_uni <- data.frame(
  values = c(G_A, G_B, G_C, G_D),
  group = factor(rep(c("A", "B", "C", "D"), 
                     times = c(length(G_A), length(G_B), length(G_C), length(G_D)))
  )
)

library(car)
### (1) regular ANOVA
# Optionally, check assumptions like homogeneity of variances
# You can use the Bartlett test or Levene's test for this
levene_result <- 
print(levene_result)

# Conduct ANOVA
result_anova <- aov(values ~ group, data = theta_uni)

# Check the results
summary(result_anova)

# Extract the summary as a dataframe
anova_summary <- summary(result_anova)

# Display the F-value and P-value with more precision
anova_summary$'F value' <- format(anova_summary$'F value', scientific = FALSE)
anova_summary$'Pr(>F)' <- format.pval(anova_summary$'Pr(>F)', digits = 10)

# Print the detailed summary
print(anova_summary)


### (2) weich ANOVA
## assumptions like homogeneity of variances is violted
# So performing Welch's ANOVA
result_welch <- oneway.test(values ~ group, data = theta_uni)

# Viewing the results
print(result_welch)


# ## Using ANOVA analyzing the difference of GLB in four models
# 创建采样后数据的数据框
# data_sampled <- data.frame(
#  values = c(G_A_sampled, G_B_sampled, G_C_sampled, G_D_sampled),
#  group = factor(rep(c("A", "B", "C", "D"), each = length(G_A_sampled)))
# )

# 使用 aov 函数进行方差分析
#result_sampled <- aov(values ~ group, data = data_sampled)

# 查看结果
#summary(result_sampled)




#############################################
#  scatter plot of theta and method factor ##
#############################################

## scatter plot
# library packages
library(GGally)
library(ggplot2)

# for data A
all.theta_A <- data.frame(
  G_A = G_A,
  PME_A = PME_A,
  NME_A = NME_A
)

plot_A <- ggpairs(all.theta_A,
                        columnLabels = c("G_Original", "PME_Original", "NME_Original")
)
print(plot_A)
ggsave(filename = "scatter plot of one factor model for original version.png", 
       plot = plot_A, width = 8, height = 8, dpi = 1200)


# for data B
all.theta_B <- data.frame(
  G_B = G_B,
  PME_B = PME_B,
  NME_B = NME_B
)

plot_B <- ggpairs(all.theta_B,
                        columnLabels = c("G_Original-Reverse", "PME_Original-Reverse", "NME_Original-Reverse")
)
print(plot_B)
ggsave(filename = "scatter plot of one factor model for original-reverse version.png", 
       plot = plot_B, width = 8, height = 8, dpi = 1200)




# for all data
# method 1: (not appliable due to different number of samples in four data)
all.theta <- data.frame(
  G_A = G_A,
  PME_A = PME_A,
  NME_A = NME_A,
  G_B = G_B,
  PME_B = PME_B,
  NME_B = NME_B,
  G_C = G_C,
  G_D = G_D
)

plot_all <- ggpairs(all.theta,
                  columnLabels = c("G_A", "PME_A", "NME_A",
                                   "G_B", "PME_B", "NME_B",
                                   "G_C", "G_D")
)


# method 2: 按照最低长度的数据集，其他数据集随机抽取相同数量的sample，再绘制散点图
# 获取所有数据集中的最小长度
min_len <- min(length(G_A), length(PME_A), length(NME_A), length(G_B), length(PME_B), length(NME_B), length(G_C), length(G_D))

# 随机抽样索引函数
random_sample_indices <- function(vec, len) {
  sample(seq_along(vec), size = len, replace = FALSE)
}

# 对于每个数据集A, B, C, D，生成随机索引
indices_A <- random_sample_indices(G_A, min_len)
indices_B <- random_sample_indices(G_B, min_len)
indices_C <- random_sample_indices(G_C, min_len)
indices_D <- random_sample_indices(G_D, min_len)

# 使用索引从各个向量中抽取样本
G_A_sampled <- G_A[indices_A]
PME_A_sampled <- PME_A[indices_A]
NME_A_sampled <- NME_A[indices_A]

G_B_sampled <- G_B[indices_B]
PME_B_sampled <- PME_B[indices_B]
NME_B_sampled <- NME_B[indices_B]

G_C_sampled <- G_C[indices_C]
G_D_sampled <- G_D[indices_D]

# 创建新的data.frame
all.theta.sampled <- data.frame(
  G_A = G_A_sampled,
  PME_A = PME_A_sampled,
  NME_A = NME_A_sampled,
  G_B = G_B_sampled,
  PME_B = PME_B_sampled,
  NME_B = NME_B_sampled,
  G_C = G_C_sampled,
  G_D = G_D_sampled
)

# 绘制散点图
library(GGally)
plot_all_sampled <- ggpairs(all.theta.sampled,
                            columnLabels = c("G_Original", "PME_Original", "NME_Original",
                                             "G_Original-Reverse", "PME_Original-Reverse", "NME_Original-Reverse",
                                             "G_Positive", "G_Negative")
)
print(plot_all_sampled)
ggsave(filename = "scatter plot of one factor model.png", plot = plot_all_sampled, width = 13, height = 13, dpi = 1200)


library(ggplotify)
### plot of latent traits in four version scales
# Create the ggpairs object
ggpairs_object <- ggpairs(theta_uni)



# boxplot
library(ggplot2)

# Create the boxplot without a title
boxplot <- ggplot(theta_uni, aes(x = group, y = values)) +
  geom_boxplot() +
  labs(y = "Latent Learning Burnout Trait") +  # Removed title
  theme_gray() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.title.x = element_blank(),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white", size = 0.5)
  ) +
  scale_x_discrete(labels = c("Original", "Original-Reverse", "Positive", "Negative"))

# View the plot
boxplot


# histogram
library(ggplot2)

# Create the histogram without a title
histogram <- ggplot(theta_uni, aes(x = values, fill = group)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ group, scales = "free_y", ncol = 1, 
             labeller = labeller(group = c(A = "Original", B = "Original-Reverse", 
                                           C = "Positive", D = "Negative"))) +
  scale_fill_manual(values = c("Original" = "darkblue", 
                               "Original-Reverse" = "darkred", 
                               "Positive" = "darkgreen", 
                               "Negative" = "darkpurple")) +
  theme_gray() +
  labs(x = "Latent Learning Burnout Trait", y = "Number of Participants")  # Removed title
theme(
  strip.text = element_text(angle = 0)
)

# View the plot
histogram


# combine them together
library(gridExtra)

# Combine the plots side by side
combined_plot <- grid.arrange(boxplot, histogram, ncol = 2)

# View the combined plot
combined_plot

# Save the combined plot to a file
ggsave("combined_plot.png", combined_plot, width = 10, height = 5)

       
       
       
       
       

############################################
#    three dimensional Bifactor model   ##
############################################
##  theta and eta  ##
theta.A <- fscores(modA, full.scores.SE = T)
theta.B <- fscores(modB, full.scores.SE = T)
theta.C <- fscores(modC, full.scores.SE = T)
theta.D <- fscores(modD, full.scores.SE = T)

# Save theta data frames to CSV files
write.csv(theta.A, "A version bifactor with three dimensional traits.csv", row.names = FALSE)
write.csv(theta.B, "B version bifactor with three dimensional traits.csv", row.names = FALSE)
write.csv(theta.C, "C version bifactor with three dimensional traits.csv", row.names = FALSE)
write.csv(theta.D, "D version bifactor with three dimensional traits.csv", row.names = FALSE)


# draw the scatter plot between theta and eta in six models
G_A <- theta.A[,1]
PME_A <- theta.A[,2]
NME_A <- theta.A[,3]

G_B <- theta.B[,1]
PME_B <- theta.B[,2]
NME_B <- theta.B[,3]

G_C <- theta.C[,1]

G_D <- theta.D[,1]

## Using ANOVA analyzing the difference of GLB in four models
# anova(G_A, G_B, G_C, G_D) (目前不可用)



# method 2: 按照最低长度的数据集，其他数据集随机抽取相同数量的sample，再绘制散点图
# 获取所有数据集中的最小长度
min_len <- min(length(G_A), length(PME_A), length(NME_A), length(G_B), length(PME_B), length(NME_B), length(G_C), length(G_D))

# 随机抽样索引函数
random_sample_indices <- function(vec, len) {
  sample(seq_along(vec), size = len, replace = FALSE)
}

# 对于每个数据集A, B, C, D，生成随机索引
indices_A <- random_sample_indices(G_A, min_len)
indices_B <- random_sample_indices(G_B, min_len)
indices_C <- random_sample_indices(G_C, min_len)
indices_D <- random_sample_indices(G_D, min_len)

# 使用索引从各个向量中抽取样本
G_A_sampled <- G_A[indices_A]
PME_A_sampled <- PME_A[indices_A]
NME_A_sampled <- NME_A[indices_A]

G_B_sampled <- G_B[indices_B]
PME_B_sampled <- PME_B[indices_B]
NME_B_sampled <- NME_B[indices_B]

G_C_sampled <- G_C[indices_C]
G_D_sampled <- G_D[indices_D]

# 创建新的data.frame
all.theta.sampled <- data.frame(
  G_A = G_A_sampled,
  PME_A = PME_A_sampled,
  NME_A = NME_A_sampled,
  G_B = G_B_sampled,
  PME_B = PME_B_sampled,
  NME_B = NME_B_sampled,
  G_C = G_C_sampled,
  G_D = G_D_sampled
)

# 绘制散点图
library(GGally)
plot_all_sampled_multi <- ggpairs(all.theta.sampled,
                            columnLabels = c("G_Original", "PME_Original", "NME_Original",
                                             "G_Original-Reverse", "PME_Original-Reverse", "NME_Original-Reverse",
                                             "G_Positive", "G_Negative")
)
print(plot_all_sampled_multi)

# save plot
ggsave(filename = "scatter plot of three factor model.png", plot = plot_all_sampled_multi, width = 13, height = 13, dpi = 1200)



# ## Using ANOVA analyzing the difference of GLB in four models
# 创建采样后数据的数据框
data_sampled <- data.frame(
  values = c(G_A_sampled, G_B_sampled, G_C_sampled, G_D_sampled),
  group = factor(rep(c("A", "B", "C", "D"), each = length(G_A_sampled)))
)

# 使用 aov 函数进行方差分析
result_sampled <- aov(values ~ group, data = data_sampled)

# 查看结果
summary(result_sampled)






##########################
#      reliability      ##
##########################

reliability_uni <- c(
  G.relia_Original <- empirical_rxx(theta.A_uni)[[1]],
  PME.relia_Original<- empirical_rxx(theta.A_uni)[[2]],
  NME.relia_Original<- empirical_rxx(theta.A_uni)[[3]],
  G.relia_Original_Reverse <- empirical_rxx(theta.B_uni)[[1]],
  PME.relia_Original_Reverse <- empirical_rxx(theta.B_uni)[[2]],
  NME.relia_Original_Reverse <- empirical_rxx(theta.B_uni)[[3]],
  G.relia_Positive <- empirical_rxx(theta.C_uni)[[1]],
  G.relia_Negative <- empirical_rxx(theta.D_uni)[[1]])
  
reliability_uni <- round(reliability_uni, 2)





#################################
#  compare with item parameter  #
#################################

# 对于数据集A
A_pos_indices <- c(1,3,6,8,11,13,15,17)
A_neg_indices <- c(2,4,5,7,9,10,12,14,16,18,19,20)

# 对于数据集B（其实就是A的反转）
B_pos_indices <- c(2,4,5,7,9,10,12,14,16,18,19,20)
B_neg_indices <- c(1,3,6,8,11,13,15,17)


par_A_pos <- par.A_uni[A_pos_indices, ]
par_A_neg <- par.A_uni[A_neg_indices, ]

par_B_pos <- par.B_uni[B_pos_indices, ]
par_B_neg <- par.B_uni[B_neg_indices, ]


par_C_pos1 <- par.C_uni[A_pos_indices, ]
par_C_pos2 <- par.C_uni[A_neg_indices, ]

par_D_neg1 <- par.D_uni[A_pos_indices, ]
par_D_neg2 <- par.D_uni[A_neg_indices, ]


## independent t test

## data A
# 初始化一个空的数据框来存储结果
t_results_A <- data.frame(Parameter = character(),
                           Dataset = character(),
                           t_value = numeric(),
                           Degrees_of_Freedom = numeric(),
                           p_value = numeric(),
                           stringsAsFactors = FALSE)

# 对于数据集A的每一个参数：
for (i in 1:9) {
  t_result <- t.test(par_A_pos[,i], par_A_neg[,i], paired = FALSE)
  
  temp_df <- data.frame(Parameter = colnames(par.A_uni)[i],
                        Dataset = "A",
                        t_value = t_result$statistic,
                        Degrees_of_Freedom = t_result$parameter,
                        p_value = t_result$p.value,
                        stringsAsFactors = FALSE)
  
  t_results_A <- rbind(t_results_A, temp_df)
}

## data B
t_results_B <- data.frame(Parameter = character(),
                          Dataset = character(),
                          t_value = numeric(),
                          Degrees_of_Freedom = numeric(),
                          p_value = numeric(),
                          stringsAsFactors = FALSE)
# 对于数据集B的每一个参数：
for (i in 1:9) {
  t_result <- t.test(par_B_pos[,i], par_B_neg[,i], paired = FALSE)
  
  temp_df <- data.frame(Parameter = colnames(par.B_uni)[i],
                        Dataset = "B",
                        t_value = t_result$statistic,
                        Degrees_of_Freedom = t_result$parameter,
                        p_value = t_result$p.value,
                        stringsAsFactors = FALSE)
  
  t_results_B <- rbind(t_results_B, temp_df)
}






#################################
# response to reviwer 2  #

#################################
library(openxlsx)
setwd("D:/U盘数据 20220115备份/中心博士/学术会议/IMPS 2023/data analysis/Method Effect/含背景信息数据")


#######################################
##  1. equivalence of four subsample   ##
#######################################

dataA<-read.csv("A卷结果（含背景信息+剔除极端值）.csv",header=T)
dataB<-read.csv("B卷结果（含背景信息+剔除极端值）.csv",header=T)
dataC<-read.csv("C卷结果（含背景信息+剔除极端值）.csv",header=T)
dataD<-read.csv("D卷结果（含背景信息+剔除极端值）.csv",header=T)
dataA <- dataA[, -c(30, 31, 32)]
dataB <- dataB[, -c(30)]
dataC <- dataC[, -c(30)]
dataD <- dataD[, -c(30)]

# Add a new column to each dataset to indicate the version
dataA$Version <- 'Original'
dataB$Version <- 'Original-Reverse'
dataC$Version <- 'Positive'
dataD$Version <- 'Negative'

# Merge the datasets
combined_data <- rbind(dataA, dataB, dataC, dataD)

# Rename the third and fourth columns
names(combined_data)[3] <- 'gender'
names(combined_data)[4] <- 'grade'
names(combined_data)[5] <- 'expense'

# 计算expense列的频率
expense_freq <- table(combined_data$expense)

# 计算expense列的比例
expense_prop <- prop.table(expense_freq)

# 打印结果
print(expense_prop)

# Now, to compare gender and grade compositions across versions
# You can use Chi-squared tests for categorical data

# Compare gender composition
 gender_table <- table(combined_data$Version, combined_data$gender)
 chisq.test(gender_table)

# 使用pairwise.prop.test进行两两比较
# 针对性别
# pairwise_gender_test <- pairwise.prop.test(table(combined_data$gender, combined_data$Version))

# 打印结果
# print(pairwise_gender_test)


# Compare grade composition
 grade_table <- table(combined_data$Version, combined_data$grade)
 chisq.test(grade_table)


### 不同版本两两比较
# 计算每个版本的性别比例
gender_proportions <- prop.table(table(combined_data$Version, combined_data$gender), margin = 1)
chisq_gender <- chisq.test(gender_proportions)

# 计算每个版本的年级比例
grade_proportions <- prop.table(table(combined_data$Version, combined_data$grade), margin = 1)
chisq_grade <- chisq.test(grade_proportions)

# 计算每个版本的可用支出比例
expense_proportions <- prop.table(table(combined_data$Version, combined_data$expense), margin = 1)
chisq_expense <- chisq.test(expense_proportions)

# 打印结果
print(gender_proportions)
print(grade_proportions)
print(expense_proportions)


# 重新编码年级变量：1-4为本科生，5为研究生
combined_data$grade_category <- ifelse(combined_data$grade == 5, "研究生", "本科生")

# 创建新的年级列联表
new_grade_table <- table(combined_data$Version, combined_data$grade_category)


# Negative vs Original
chisq.test(new_grade_table[c("Negative", "Original"),])

# Negative vs Original-Reverse
chisq.test(new_grade_table[c("Negative", "Original-Reverse"),])

# Negative vs Positive
chisq.test(new_grade_table[c("Negative", "Positive"),])

# Original vs Original-Reverse
chisq.test(new_grade_table[c("Original", "Original-Reverse"),])

# Original vs Positive
chisq.test(new_grade_table[c("Original", "Positive"),])

# Original-Reverse vs Positive
chisq.test(new_grade_table[c("Original-Reverse", "Positive"),])






###############################################
##  2. ANOVA between four version of scales ##
###############################################

#################################
#  compare with item parameter  #
#################################

# 对于数据集A
A_pos_indices <- c(1,3,6,8,11,13,15,17)
A_neg_indices <- c(2,4,5,7,9,10,12,14,16,18,19,20)

# 对于数据集B（其实就是A的反转）
B_pos_indices <- c(2,4,5,7,9,10,12,14,16,18,19,20)
B_neg_indices <- c(1,3,6,8,11,13,15,17)


par_A_pos <- par.A_uni[A_pos_indices, ]
par_A_neg <- par.A_uni[A_neg_indices, ]

par_B_pos <- par.B_uni[B_pos_indices, ]
par_B_neg <- par.B_uni[B_neg_indices, ]


par_C_pos1 <- par.C_uni[A_pos_indices, ]
par_C_pos2 <- par.C_uni[A_neg_indices, ]

par_D_neg1 <- par.D_uni[A_pos_indices, ]
par_D_neg2 <- par.D_uni[A_neg_indices, ]




####################
####     d      ####
####################

## ANOVA 1: Comparing par_A_pos, par_B_neg, par_D_neg1.
# Combine the data into one frame with a grouping variable
library(reshape2)

# Add an id column to each data frame for identification
par_A_pos$id <- seq(nrow(par_A_pos))
par_B_neg$id <- seq(nrow(par_B_neg))
par_D_neg1$id <- seq(nrow(par_D_neg1))

# Melt the data frames to long format
long_A_pos <- melt(par_A_pos, id.vars = "id", measure.vars = c("d1", "d2", "d3"), variable.name = "variable", value.name = "score")
long_B_neg <- melt(par_B_neg, id.vars = "id", measure.vars = c("d1", "d2", "d3"), variable.name = "variable", value.name = "score")
long_D_neg1 <- melt(par_D_neg1, id.vars = "id", measure.vars = c("d1", "d2", "d3"), variable.name = "variable", value.name = "score")

# Combine into one data frame
data_AposBD <- rbind(
  transform(long_A_pos, group = "A_pos"),
  transform(long_B_neg, group = "B_neg"),
  transform(long_D_neg1, group = "D_neg1")
)

# Subset data for each variable
data_d1 <- subset(data_AposBD, variable == "d1")
data_d2 <- subset(data_AposBD, variable == "d2")
data_d3 <- subset(data_AposBD, variable == "d3")

# Function to perform ANOVA and post hoc test
perform_anova <- function(data, variable_name) {
  anova_result <- aov(score ~ group, data = data)
  print(paste("ANOVA for", variable_name))
  print(summary(anova_result))
  
  # Post hoc test if ANOVA is significant
  if (summary(anova_result)[[1]]$'Pr(>F)'[1] < 0.05) {
    print(paste("Post hoc for", variable_name))
    posthoc_result <- TukeyHSD(anova_result)
    print(posthoc_result)
  }
}

# Perform ANOVAs and post hoc tests for each variable
perform_anova(data_d1, "d1")
perform_anova(data_d2, "d2")
perform_anova(data_d3, "d3")



## ANOVA 2: Comparing par_A_neg, par_B_pos, par_C_pos2.
library(reshape2)

# Add id column for identification, if not already present
par_A_neg$id <- seq(nrow(par_A_neg))
par_B_pos$id <- seq(nrow(par_B_pos))
par_C_pos2$id <- seq(nrow(par_C_pos2))

# Melt the data frames to long format
long_A_neg <- melt(par_A_neg, id.vars = "id", measure.vars = c("b1", "d2", "d3"), variable.name = "variable", value.name = "score")
long_B_pos <- melt(par_B_pos, id.vars = "id", measure.vars = c("b1", "d2", "d3"), variable.name = "variable", value.name = "score")
long_C_pos2 <- melt(par_C_pos2, id.vars = "id", measure.vars = c("b1", "d2", "d3"), variable.name = "variable", value.name = "score")

# Combine into one data frame
data_AnegB_C <- rbind(
  transform(long_A_neg, group = "A_neg"),
  transform(long_B_pos, group = "B_pos"),
  transform(long_C_pos2, group = "C_pos2")
)

# Function to perform ANOVA and post hoc test
perform_anova <- function(data, variable_name) {
  anova_result <- aov(score ~ group, data = data)
  print(paste("ANOVA for", variable_name))
  print(summary(anova_result))
  
  # Post hoc test if ANOVA is significant
  if (summary(anova_result)[[1]]$'Pr(>F)'[1] < 0.05) {
    print(paste("Post hoc for", variable_name))
    posthoc_result <- TukeyHSD(anova_result)
    print(posthoc_result)
  }
}

# Perform ANOVAs and post hoc tests for each variable
perform_anova(subset(data_AnegB_C, variable == "b1"), "b1")
perform_anova(subset(data_AnegB_C, variable == "d2"), "d2")
perform_anova(subset(data_AnegB_C, variable == "d3"), "d3")





####################
####     b      ####
####################

## ANOVA 1: Comparing par_A_pos, par_B_neg, par_D_neg1.
# Combine the data into one frame with a grouping variable
library(reshape2)

# Add an id column to each data frame for identification
par_A_pos$id <- seq(nrow(par_A_pos))
par_B_neg$id <- seq(nrow(par_B_neg))
par_D_neg1$id <- seq(nrow(par_D_neg1))

# Melt the data frames to long format
long_A_pos <- melt(par_A_pos, id.vars = "id", measure.vars = c("b1", "b2", "b3"), variable.name = "variable", value.name = "score")
long_B_neg <- melt(par_B_neg, id.vars = "id", measure.vars = c("b1", "b2", "b3"), variable.name = "variable", value.name = "score")
long_D_neg1 <- melt(par_D_neg1, id.vars = "id", measure.vars = c("b1", "b2", "b3"), variable.name = "variable", value.name = "score")

# Combine into one data frame
data_AposBD <- rbind(
  transform(long_A_pos, group = "A_pos"),
  transform(long_B_neg, group = "B_neg"),
  transform(long_D_neg1, group = "D_neg1")
)

# Subset data for each variable
data_b1 <- subset(data_AposBD, variable == "b1")
data_b2 <- subset(data_AposBD, variable == "b2")
data_b3 <- subset(data_AposBD, variable == "b3")

# Function to perform ANOVA and post hoc test
perform_anova <- function(data, variable_name) {
  anova_result <- aov(score ~ group, data = data)
  print(paste("ANOVA for", variable_name))
  print(summary(anova_result))
  
  # Post hoc test if ANOVA is significant
  if (summary(anova_result)[[1]]$'Pr(>F)'[1] < 0.05) {
    print(paste("Post hoc for", variable_name))
    posthoc_result <- TukeyHSD(anova_result)
    print(posthoc_result)
  }
}

# Perform ANOVAs and post hoc tests for each variable
perform_anova(data_b1, "b1")
perform_anova(data_b2, "b2")
perform_anova(data_b3, "b3")



## ANOVA 2: Comparing par_A_neg, par_B_pos, par_C_pos2.
library(reshape2)

# Add id column for identification, if not already present
par_A_neg$id <- seq(nrow(par_A_neg))
par_B_pos$id <- seq(nrow(par_B_pos))
par_C_pos2$id <- seq(nrow(par_C_pos2))

# Melt the data frames to long format
long_A_neg <- melt(par_A_neg, id.vars = "id", measure.vars = c("b1", "b2", "b3"), variable.name = "variable", value.name = "score")
long_B_pos <- melt(par_B_pos, id.vars = "id", measure.vars = c("b1", "b2", "b3"), variable.name = "variable", value.name = "score")
long_C_pos2 <- melt(par_C_pos2, id.vars = "id", measure.vars = c("b1", "b2", "b3"), variable.name = "variable", value.name = "score")

# Combine into one data frame
data_AnegB_C <- rbind(
  transform(long_A_neg, group = "A_neg"),
  transform(long_B_pos, group = "B_pos"),
  transform(long_C_pos2, group = "C_pos2")
)

# Function to perform ANOVA and post hoc test
perform_anova <- function(data, variable_name) {
  anova_result <- aov(score ~ group, data = data)
  print(paste("ANOVA for", variable_name))
  print(summary(anova_result))
  
  # Post hoc test if ANOVA is significant
  if (summary(anova_result)[[1]]$'Pr(>F)'[1] < 0.05) {
    print(paste("Post hoc for", variable_name))
    posthoc_result <- TukeyHSD(anova_result)
    print(posthoc_result)
  }
}

# Perform ANOVAs and post hoc tests for each variable
perform_anova(subset(data_AnegB_C, variable == "b1"), "b1")
perform_anova(subset(data_AnegB_C, variable == "b2"), "b2")
perform_anova(subset(data_AnegB_C, variable == "b3"), "b3")








####################
####     a      ####
####################
## ANOVA 1: Comparing par_A_pos, par_B_neg, par_D_neg1.
library(reshape2)

# Extracting and Melting the 'a' parameters
long_A_a <- melt(par_A_pos, id.vars = "id", measure.vars = c("a1", "a2", "a3"), variable.name = "variable", value.name = "score")
long_B_a <- melt(par_B_neg, id.vars = "id", measure.vars = c("a1", "a2", "a3"), variable.name = "variable", value.name = "score")
long_D_a <- melt(par_D_neg1, id.vars = "id", measure.vars = c("a1"), variable.name = "variable", value.name = "score")  # Only a1 present

# Combining data
data_ABD_a <- rbind(
  transform(long_A_a, group = "A_pos"),
  transform(long_B_a, group = "B_neg"),
  transform(long_D_a, group = "D_neg1")
)

# Filtering data for 'a1' variable
data_a1 <- subset(data_ABD_a, variable == "a1")

# ANOVA and post hoc for 'a1'
perform_anova(data_a1, "a1")

# Extracting a2 and a3 parameters
a2_A <- par_A_pos$a2
a3_B <- par_B_neg$a3

# Check if sample sizes are equal
if (length(a2_A) == length(a3_B)) {
  # Perform t-test
  t_test_result <- t.test(a2_A, a3_B)
  print(t_test_result)
} else {
  # Perform ANOVA
  data_a2_a3 <- data.frame(
    score = c(a2_A, a3_B),
    group = factor(rep(c("A_a2", "B_a3"), c(length(a2_A), length(a3_B))))
  )
  anova_a2_a3 <- aov(score ~ group, data = data_a2_a3)
  summary(anova_a2_a3)
  
  # Post hoc test if ANOVA is significant
  if (summary(anova_a2_a3)[[1]]$'Pr(>F)'[1] < 0.05) {
    posthoc_a2_a3 <- TukeyHSD(anova_a2_a3)
    print(posthoc_a2_a3)
  }
}



## ANOVA 2: Comparing par_A_neg, par_B_pos, par_C_pos2.
library(reshape2)

# Extracting and Melting the 'a' parameters
long_A_a <- melt(par_A_neg, id.vars = "id", measure.vars = c("a1", "a2", "a3"), variable.name = "variable", value.name = "score")
long_B_a <- melt(par_B_pos, id.vars = "id", measure.vars = c("a1", "a2", "a3"), variable.name = "variable", value.name = "score")
long_C_a <- melt(par_C_pos2, id.vars = "id", measure.vars = c("a1"), variable.name = "variable", value.name = "score")  # Only a1 present

# Combining data
data_AnegBC_a <- rbind(
  transform(long_A_a, group = "A_neg"),
  transform(long_B_a, group = "B_pos"),
  transform(long_C_a, group = "D_pos2")
)

# Filtering data for 'a1' variable
data_a1 <- subset(data_AnegBC_a, variable == "a1")

# ANOVA and post hoc for 'a1'
perform_anova(data_a1, "a1")

# Extracting a2 and a3 parameters
a3_A <- par_A_neg$a3
a2_B <- par_B_pos$a2

# Check if sample sizes are equal
if (length(a3_A) == length(a2_B)) {
  # Perform t-test
  t_test_result <- t.test(a3_A, a2_B)
  print(t_test_result)
} else {
  # Perform ANOVA
  data_a2_a3 <- data.frame(
    score = c(a3_A, a2_B),
    group = factor(rep(c("A_a3", "B_a2"), c(length(a3_A), length(a2_B))))
  )
  anova_a2_a3 <- aov(score ~ group, data = data_a2_a3)
  summary(anova_a2_a3)
  
  # Post hoc test if ANOVA is significant
  if (summary(anova_a2_a3)[[1]]$'Pr(>F)'[1] < 0.05) {
    posthoc_a2_a3 <- TukeyHSD(anova_a2_a3)
    print(posthoc_a2_a3)
  }
}




##########################
#          ECV          #
##########################
# Function to calculate ECV
calculate_ecv <- function(params) {
  # Extracting the discrimination parameters for general (a1) and group factors (a2, a3)
  a1_squared <- params$a1^2  # Squaring general factor loadings
  total_squared <- rowSums(params[, c('a1', 'a2', 'a3')]^2)  # Squaring and summing all factor loadings
  
  # Calculating ECV
  ecv <- sum(a1_squared) / sum(total_squared)  # Summing across items and dividing
  return(ecv)
}

# Calculate ECV for Dataset A
ecv_A <- calculate_ecv(par.A_uni)

# Calculate ECV for Dataset B
ecv_B <- calculate_ecv(par.B_uni)

# Output the ECV values
cat("ECV for Dataset A:", ecv_A, "\n")
cat("ECV for Dataset B:", ecv_B, "\n")








