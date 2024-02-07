library(mirt)
library(openxlsx)

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

