# Solutions to the second part of the Churn & Lifetime workshop of the 
# Aegon CI tRansition, held on 2016/09/29

# Previous Questions (11-15) --------------------------------------------------------------------------

# Initialize
iDuration = 100
iNoClients = 100000
load("X:/Analisten/TRaining/Casus/Week 1/Churn_nonstationary.RData")

# ADDING SOME CODE HERE TO TEST HOW CHANGES ARE REGISTERED
A = 5
B = 2*A

# 11
mChurnDynamic = mChurnDynamic == "CHURN"

#12
mHistoryDynamic = matrix(0,iNoClients,iDuration)
for (i in 2:iDuration){
    mHistoryDynamic[!mChurnDynamic[,i-1],i] = mHistoryDynamic[!mChurnDynamic[,i-1],i-1]+1  
}

#13
mean(mChurnDynamic[,51])
mean(mChurnDynamic[mHistoryDynamic==0])

#14
mHistoryChurn = matrix(0,11,2)
for (i in 1:11){
    mHistoryChurn[i,1] = i - 1
    mHistoryChurn[i,2] = mean(mChurnDynamic[mHistoryDynamic==(i-1)])
}

colnames(mHistoryChurn) = c("History", "ChurnProb")
print(round(mHistoryChurn,3))

#15
mHistoryChurn = round(mHistoryChurn,3)
vLifetimeStatic = 1/mHistoryChurn[,2]


# Questions 16-23 -----------------------------------------------------------------------------

#16
vLifetimeObserved = apply(mChurnDynamic, 1, function(x) min(which(x)))

#17
vLifetimeObserved[vLifetimeObserved==Inf] = 100+1/0.05

#18
summary(vLifetimeObserved)

#19
vLifetimeObserved51=apply(mChurnDynamic[,-(1:50)],1,function(x) min(which(x)))
vLifetimeObserved51[vLifetimeObserved51==Inf] = 50+1/0.05
summary(vLifetimeObserved51)

#20
mHistoryLifetime = matrix(0,11,3)
for (i in 0:10){
    mHistoryLifetime[i+1,2] = mean(vLifetimeObserved51[mHistoryDynamic[,51]==i])
}
mHistoryLifetime[,1] = 0:10
mHistoryLifetime[,3] = t(vLifetimeStatic)
colnames(mHistoryLifetime) = c("History", "Observed LT","Static LT")
print(round(mHistoryLifetime,1))

#21
sum(cumprod(c(1,seq(0.75,0.95,by=0.05),rep(0.95,195))))

#22
vLifetimeDynamic = matrix(0,11,1)
for (i in 0:10){
    vLifetimeDynamic[i+1] = sum(cumprod(c(1,seq(min(0.75+i*0.05,0.95),0.95,by=0.05),rep(0.95,195-i))))
}
mHistoryLifetime = cbind(mHistoryLifetime[,1:3], vLifetimeDynamic)
colnames(mHistoryLifetime) = c("History", "Observed LT","Static LT", "Dynamic LT")
print(round(mHistoryLifetime,1))
max(mHistoryLifetime[,4]/ mHistoryLifetime[,3])

#23
vStaticPrediction51 = rep(NaN,iNoClients)
vDynamicPrediction51 = rep(NaN,iNoClients)
for (i in 0:10){
    vStaticPrediction51[mHistoryDynamic[,51]==i] = vLifetimeStatic[i+1]
    vDynamicPrediction51[mHistoryDynamic[,51]==i] = vLifetimeDynamic[i+1]
}
vStaticPrediction51[mHistoryDynamic[,51]>10] = vLifetimeStatic[11]
vDynamicPrediction51[mHistoryDynamic[,51]>10] = vLifetimeDynamic[11]

summary(vStaticPrediction51)
summary(vDynamicPrediction51)
summary(vLifetimeObserved51)


# Questions 24-30 -----------------------------------------------------------------------------

# Clear workspace and load XINSURANCE data
rm(list = ls())
load("X:/Analisten/TRaining/Casus/Week 2/XINSURANCE.RData")
source('X:/Analisten/TRaining/Casus/Week 2/Define_fExtend.R')

# 24
dfMINI = dfXINSURANCE[1:1000,c("ID", "Year", "Churn")]

# 25
Model = glm(Churn ~ ID + Year, data = dfMINI, family = "binomial")
summary(Model)

#26
dfMINI_S2015 = dfMINI[dfMINI$Year==2015 & dfMINI$Churn=="STAY",]

# 27
fExtend = function(mOriginal, iNoColsNew = 25, dStepSize = 1){
    out = rep(mOriginal, iNoColsNew) + dStepSize*rep((1:iNoColsNew), length(mOriginal))
}
dfMINI_F = data.frame(ID = rep(dfMINI_S2015$ID, 25), Year = fExtend(dfMINI_S2015$Year))

# 28
dfMINI_F = dfMINI_F[order(dfMINI_F$ID, dfMINI_F$Year),]

# 29
dfMINI_F$Prediction = predict(Model, newdata = dfMINI_F, type = "response")

# 30
vStaticLifetimes =  aggregate(Prediction ~ ID, dfMINI_F, function(x){1/(1-x[1])} )
summary(vStaticLifetimes)


# Question 31 ---------------------------------------------------------------------------------

Model = glm(Churn ~ Gender + Age + History + Year*Gender , data=dfXINSURANCE, family = binomial (link="probit"))
summary(Model)
df2015Stay = dfXINSURANCE[dfXINSURANCE$Year==2015 & dfXINSURANCE$Churn=="STAY",]
dfNEW= data.frame(ID      = rep(df2015Stay$ID, 25), 
                  Year    = fExtend(df2015Stay$Year),
                  Gender  = rep(df2015Stay$Gender,25),
                  Age     = fExtend(df2015Stay$Age),
                  History = fExtend(df2015Stay$History),
                  Weight  = rep(df2015Stay$Weight,25) )                
dfNEW = dfNEW[order(dfNEW$ID, dfNEW$Year),]
vStayPred = predict(Model, newdata = dfNEW, type = "response")
# In the year 2040, we may assume everyone churns (Pr(Stay) = 0)
vStayPred[dfNEW$Year==2040] = 0
dfNEW$Prediction = vStayPred
# Calculate the predicted lifetime (add .5 because on average someone churns halfway through the year)
mLTP_Rogier   = aggregate(Prediction ~ ID, dfNEW, function(x){0.5+sum(cumprod(x))} )
summary(mLTP_Rogier)
# save(mLTP_Rogier,file = "X:/Analisten/Training/Casus/Week 2/Hand in - LT Predictions/mLTP_Rogier")


# Generating data -----------------------------------------------------------------------------


vUniform = function(iLength = NULL) {
    # creates a uniformly distributed vector
    if (is.null(iLength)) { iLength = iNoClients}
    vUniform = matrix(runif(iLength), nrow = iLength)
}

fExtend = function(mOriginal, iNoColsNew = 28, dStepSize = 1){
    # Extend an existing vector, making it into a matrix with a constant added each column
    # (used in dynamic lifetim calculation)
    iNoClients = dim(mOriginal)[1]
    if (is.null(iNoClients)) { iNoClients = length(mOriginal)}
    mOriginalRepeated = kronecker(mOriginal, matrix(1,1,iNoColsNew))
    mPlusone = kronecker(matrix(1,iNoClients,1), matrix((1:iNoColsNew)-1,1, iNoColsNew) )
    mNew = mOriginalRepeated + mPlusone * dStepSize
}

fChurnProb = function(i) {
    # The determination of the churn probability. Note that a standard-normal (probit)
    # link was used, and there is one interaction term included (Year*Gender)
    vChurnProb = pnorm(0.7 + 0.5*mGenderL[,i] - 0.03*mAge[,i] - 0.07*mHistory[,i] + 
                           0.02*(mYear[,i]-2013)*mGenderL[,i] + 0.01*(mYear[,i]-2013))    
}

# Set random seed
set.seed(pi)

# Set up variables 
iNoYears = 28
iNoClients = 100000
mAge = fExtend(round(30+40*vUniform())) # ages between 30 and 70
mGender = sample(c("Male", "Female"), iNoClients, replace = TRUE, prob = c(0.4, 0.6))
mGender = matrix(rep(mGender,iNoYears), ncol = iNoYears)
mGenderL = mGender=="Female" # Logical Matrix 
mWeight = round(50+60*vUniform())
# For show, weight differs a bit in the first 3 years, then stays constant
mWeight = round(cbind(mWeight, mWeight + 2*vUniform()-1, mWeight + 4*vUniform()-2,
                      fExtend(mWeight, 25, 0))) 
mYear = kronecker(matrix(1,iNoClients,1),t(2013:2040))
mID = fExtend(matrix(1:iNoClients,iNoClients,1), dStepSize = 0)

mHistory = matrix(0,iNoClients,iNoYears)
mChurnProb = mHistory
mChurn = mHistory
mHistory[,1] = round(5*vUniform()) 
mChurnMoment = matrix(runif(iNoClients*iNoYears), iNoClients, 1)

for (i in 1:28){
    # Calculate churn probabilities (secret function)
    mChurnProb[,i] = fChurnProb(i)
    
    # in the last year (28), the churn probability is zero
    if (i == 28) {mChurnProb[,i] = 1}
    
    # Determine who churns (with probability mChurnProb)
    vChurners = mChurnProb[,i] > vUniform()
    
    # Update the churn matrix
    mChurn[vChurners, i] = "CHURN"
    mChurn[!vChurners,i] = "STAY"
    
    # Update the history & the ID
    if (i < 28) {
        mHistory[vChurners,i+1] = 0 # abundant, but for ease of reading
        mHistory[!vChurners,i+1] = mHistory[!vChurners,i] + 1
        
        # mID[!vChurners,i+1] = mID[!vChurners,i] #ch@nged
        # mID[vChurners,i+1] = mID[vChurners,i]+100000
    }
}

# Logical matrix (useful in calculation later on)
mChurnL = mChurn == "CHURN"
# Realized lifetime is the first moment someone churns. If someone churns on September 1st (0.75)
# 2018 (Year 3), his lifetime is 3 + 0.75 - 1 = 2.75
# All relevant clients database have History >= 1 in year 4 (they stayed in year 3), 
#  so no need to do something different for new clients (history = 0)
vStay2015 = !mChurnL[,3] & rowSums(mChurnL[,1:3])==0
vLifetimesRealized = apply(mChurnL[ vStay2015, 4:28], 1, FUN = function(x) min(which(x))) + 
    mChurnMoment[vStay2015] - 1
mLifetimesRealized = cbind(mID[vStay2015,3], vLifetimesRealized)
save(mLifetimesRealized, file = "X:/Analisten/Training/Casus/Week 2/Hand in - LT Predictions/Realized_Lifetimes.RData")

# Put first three years in dataframe 

# year 1: show all, year 2: show those who haven't churned in 2013
# year 3: show those who haven't churned in 2013 & 2014
vStayedBoth = rowSums(mChurn[,1:2] == "STAY") == 2 # matrix(TRUE, iNoClients,1) # 
vStayedFirst =  mChurn[,1] == "STAY"  # matrix(TRUE, iNoClients,1)

mYear1 = cbind(mID[,1], mYear[,1], mGender[,1], mAge[,1], mHistory[,1], mWeight[,1], mChurn[,1])
mYear2 = cbind(mID[vStayedFirst,2], mYear[vStayedFirst,2], mGender[vStayedFirst,2], 
               mAge[vStayedFirst,2], mHistory[vStayedFirst,2], mWeight[vStayedFirst,2], 
               mChurn[vStayedFirst,2])
mYear3 = cbind(mID[vStayedBoth,3], mYear[vStayedBoth,3], mGender[vStayedBoth,3], 
               mAge[vStayedBoth,3], mHistory[vStayedBoth,3], mWeight[vStayedBoth,3], 
               mChurn[vStayedBoth,3])

# Create dataframe, and tidy it up
dfXINSURANCE = data.frame(rbind(mYear1, mYear2, mYear3), stringsAsFactors = FALSE)
names(dfXINSURANCE) = c("ID", "Year", "Gender", "Age", "History", "Weight", "Churn")

require(dplyr)
dfXINSURANCE = dfXINSURANCE %>%  mutate_each_(funs(as.numeric), c("ID", "Year", "Age", "History", "Weight"))
dfXINSURANCE = dfXINSURANCE %>%  mutate_each_(funs(factor), c("Gender", "Churn"))
dfXINSURANCE <- arrange(dfXINSURANCE, ID, Year) 
str(dfXINSURANCE)



