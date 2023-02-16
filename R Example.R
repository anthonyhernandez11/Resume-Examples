###########
#### 1 ####
###########

# We are conducting a generalized randomized block design with two factors, two levels each
# (Explanation of the process will be written in the document "R Example Process")


###########
#### 2 ####
###########

#####
# a #
#####
ECE <- as.data.frame(read.table(url("https://users.stat.ufl.edu/~burrdoss/Courses/4211/Data-and-R/Datasets/CH19PR12.txt")))

#####
# b #
#####

# naming factors and factor levels ; setting them as factors
names(ECE) <- c("Success","Contact","Gender","Group")
ECE["Gender"][ECE["Gender"] == 1] <- "M"
ECE["Gender"][ECE["Gender"] == 2] <- "F"
ECE["Contact"][ECE["Contact"] == 1] <- "Yes"
ECE["Contact"][ECE["Contact"] == 2] <- "No"
ECE$Contact <- as.factor(ECE$Contact)
ECE$Gender <- as.factor(ECE$Gender)

#####
# c #
#####
subset(ECE, ECE['Group'] == 1) # printing 4 lines from the dataset, one from each cell.


###########
#### 3 ####
###########

#####
# a #
#####
cellmeans <- t(tapply(ECE$Success, list(ECE$Contact, ECE$Gender), mean)) # cell means
cellsds <- t(tapply(ECE$Success, list(ECE$Contact, ECE$Gender), sd)) # cell standard deviations

# Creating their tables
CMT <- list()
CMT[[1]] <- "Table of Cell Means"
CMT[[2]] <- cellmeans
CMSD <- list()
CMSD[[1]] <- "Table of Cell Standard Deviations"
CMSD[[2]] <- cellsds

CMT ; CMSD


#####
# b #
#####

plot.design(Success ~ Contact + Gender, data=ECE) # Plot of Factor Level Means for Gender and Eye Contact with Mean rate of Success as the response

interaction.plot(x.factor = ECE$Contact, trace.factor = ECE$Gender, response = ECE$Success,
                 fun = mean,
                 ylab = "Likely Job Success of Applicant",
                 xlab = "Contact",
                 trace.label = "Gender",
                 col = c("#0198f9", "#f95801"),
                 lwd = 3) # Interaction plot between Gender and Eye Contact with Success as the response



###########
#### 5 ####
###########

ECE.aov <- aov(Success~ Contact*Gender, data=ECE)
plot(ECE.aov, which = 1:2)

# The Q-Q plot shows a short-tailed distribution.
# The ends of the data have a few outliers (labeled as 4, 18, and 19) that could be an issue in the Normal Q-Q plot.
# It mostly satisfies the law of constant variance according to the Residuals vs Fitted plot. tIn this plot, other than the aformentioned outliers the data looks relatively normal.
# The model starts to lie somewhat above the line at the values near 0 sd in the Q-Q plot meaning the model may not be the absolute best fit.

###########
#### 6 ####
###########

anova(ECE.aov)

# Ho: There is no interaction between Contact and Gender
# Ha: There is an interaction between Contact and Gender
# F-test statistic for interaction is 0.2058 and has a p-value of 0.656202

# Do not reject the null hypothesis. There is no significant interaction between Contact and Gender.
# Given a significance level of a = 0.05, there is a p-value of 0.656202 above the stated significance level.

###########
#### 7 ####
###########

# Formula for CI of D1: D1 +/- t_(N-k, 1- a/(2*k)) * sqrt(Varhat_Lhat_D1)
# Formula for CI of D2: D2 +/- t_(N-k, 1- a/(2*k)) * sqrt(Varhat_Lhat_D2)

b <- 2 # number of means
a <- 2 # number of means
n <- 5 # number of observations per cell
k <- 2 # number of comparisons
df <- 20-2 # N-k
D1 <- ((16.4+13.0)/2) - ((13.6+9.2)/2) # D1 = Mu2. - Mu1.
D2 <- ((13.6+16.4)/2) - ((13+9.2)/2) # D2 = Mu.2 - Mu.1
B <- qt(1-(.05/(2*k)), df=18) # P = 1 - (a /2*k)  ; df = N-k

Varhat_Lhat_D1 <- ((6.075) / (b*n))*(((-1)^2) + (1)^2) # Varhat_Lhat_D1 = (sigma_hat^2 / b*n)*sum(ci^2)
Varhat_Lhat_D2 <- ((6.075) / (a*n))*(((-1)^2) + (1)^2) # Varhat_Lhat_D2 = (sigma_hat^2 / a*n)*sum(ci^2)
Varhat_Lhat_D1 == Varhat_Lhat_D2 # since a = b, the variances are equal. Therefore, we can use the same Standard Error for both.
SE <- sqrt(Varhat_Lhat_D2) # Standard Error = sqrt(Varhat_Lhat_D2)
ME <- B*SE # Margin of Error

D1_CI <- D1 + c(-1, 1)*ME # Confidence Interval for D1
D2_CI <- D2 + c(-1, 1)*ME # Confidence Interval for D2

noquote(paste("Confidence Interval for D1: (", D1_CI[1], ",", D1_CI[2], ")"))
noquote(paste("Confidence Interval for D2: (", D2_CI[1], ",", D2_CI[2], ")"))