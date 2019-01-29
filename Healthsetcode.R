####DOWNLOADING SET AND INFO
#Before doing anything we need to download the data from
#https://data.oecd.org/healthres/health-spending.htm#indicator-chart
#four datasets are necessary to combine into one data set
#They're the health expenditure, pharmaceutical exp, doctors, and nurses
#uncheck latest data available for all and set the range to 1980-2016
#that is to make sure the same time data is available for all sets
#Then download the health spending and pharm spending indicators (in USD/Capita), and the Doctors and Nurses
#We’ll need to clean and combine the data sets
#I downloaded each data set as HEALTH,  PHARMA, DOCTOR, and NURSE
#All we need to do is change the names of the values, and keep the values, times, and locations
#I'm including the code and outputs for everything, my comments/explanations
#are behind the hashtags and should be read/reported
#packages in use are ggplot2,stats,mass,plyr,dplyr,dplyrAssist,magrittr
#Important mention: Doctors and Nurses per are the amount per 1000 people
#HealthExp and Pharmaexp are in USD/Capita

#######CLEANING THE DATASET
> NURSE$INDICATOR <- NULL
> NURSE$SUBJECT <-NULL
> NURSE$MEASURE <- NULL
> NURSE$FREQUENCY <- NULL
> NURSE$`Flag Codes` <-NULL
> PHARMA$INDICATOR <- NULL
> PHARMA$SUBJECT <-NULL
> PHARMA$MEASURE <- NULL
> PHARMA$FREQUENCY <- NULL
> PHARMA$`Flag Codes` <-NULL
> DOCTOR$INDICATOR <- NULL
> DOCTOR$SUBJECT <-NULL
> DOCTOR$MEASURE <- NULL
> DOCTOR$FREQUENCY <- NULL
> DOCTOR$`Flag Codes` <-NULL
> HEALTH$INDICATOR <- NULL
> HEALTH$SUBJECT <- NULL
> HEALTH$MEASURE <- NULL
> HEALTH$FREQUENCY <- NULL
> HEALTH$`Flag Codes` <- NULL

####RENAMING
> colnames(HEALTH)[3] <- "HealthExp"
> colnames(PHARMA)[3] <- "PharmaExp"
> colnames(NURSE)[3] <- "NurPer1000"
> colnames(DOCTOR)[3] <- "DoctorsPer1000"

#####COMBINING SETS WITH dplyr
> half <- full_join(HEALTH,PHARMA, by = c("LOCATION", "TIME"))
> half_b <- full_join(DOCTOR,NURSE, by = c("LOCATION", "TIME"))
> healthset<- full_join(half,half_b, by = c("LOCATION", "TIME"))



########DESCRIPTIVE STATISTICS
> summary(healthset)
TIME         LOCATION     HealthExp        PharmaExp       DoctorsPer1000 
Min.   :1980   ISL    : 36   Min.   : 238.2   Min.   :  56.97   Min.   :1.090  
1st Qu.:2001   AUS    : 33   1st Qu.:1341.1   1st Qu.: 267.43   1st Qu.:2.440  
Median :2006   CZE    : 24   Median :2372.7   Median : 393.86   Median :3.030  
Mean   :2005   DNK    : 23   Mean   :2683.2   Mean   : 408.53   Mean   :2.997  
3rd Qu.:2011   ESP    : 23   3rd Qu.:3722.3   3rd Qu.: 524.35   3rd Qu.:3.442  
Max.   :2016   HUN    : 22   Max.   :9035.5   Max.   :1081.40   Max.   :5.100  
(Other):335                                                     
NurPer1000    
Min.   : 0.930  
1st Qu.: 6.100  
Median : 8.480  
Mean   : 8.765  
3rd Qu.:11.140  
Max.   :17.950  

> ggplot(healthset, aes(x=HealthExp)) + geom_histogram(aes(y=..density..),bins = 20) + geom_density()
> ggplot(healthset, aes(x=PharmaExp)) + geom_histogram(aes(y=..density..),bins = 20) + geom_density()
> ggplot(healthset, aes(x=DoctorsPer1000)) + geom_histogram(aes(y=..density..),bins = 20) + geom_density()
> ggplot(healthset, aes(x=NurPer1000)) + geom_histogram(aes(y=..density..),bins = 20) + geom_density()

#These are the density plots, in folder labeled as density.  Ggplot was used but it's still
#a histogram

#The skew and kurtosis are in the folder: positive skewness on all, 3 is of normal kurtosis
#health and pharm exp are greater than 3 so they are leptokurtic and produces
#more outliers than a normal distribution
#Doctors is almost normal distribtion (2.96 is norm)
#Nurses is less than 3, palykurtic, less outliers than norm distribution
#These results make sense for the fact that regression follows an F-distribution

> str(healthset)
Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	496 obs. of  6 variables:
  $ TIME          : int  1980 1980 1980 1981 1981 1982 1982 1983 1983 1984 ...
$ LOCATION      : chr  "AUS" "ESP" "ISL" "AUS" ...
$ HealthExp     : num  615 349 734 697 845 ...
$ PharmaExp     : num  57 76.2 122 65.4 131.6 ...
$ DoctorsPer1000: num  1.85 1.84 2.14 1.86 2.21 1.9 2.27 1.95 2.3 1.83 ...
$ NurPer1000    : num  10.33 2.1 8.89 9.91 9.34 ...
#looking to change location to a factor so it can be read
> healthset$LOCATION <- as.factor(healthset$LOCATION)
> str(healthset$LOCATION)
$ LOCATION      : Factor w/ 34 levels "AUS","AUT","BEL",..: 1 9 16 1 16 1 16 1 16 1 ...

########SCATTERPLOT

> pairs(healthset, main = "Healthcare data scatterplots and relation")
#Pairs are scatterplot in folder, lots of data but you can see  upward trends w
# respect to time, marginal data is also shown, to see relations use your own judgement

#######CORRELATION

#We're looking to create a model with respect on pharmaexp
#Made a new data set with the names removed so we can see correlation
> healthset2 <- healthset[, c(1, 3:6)]

#focusing on the Pharma Exp column, we see fairly high correlation on everything
#(1 being the correlation to itself), but especially high correlation value on 
# healthcare expenditure to  pharm expenditure (0.8189234) This should be noted

#another interesting note is how high nurses per 1000 people relates on healthexp
#Weakest association to pharm exp is doctors at (0.2943713)
#(0.63277351) which doesn't prove our point but is just a nice mention
> cor(healthset2)
TIME HealthExp PharmaExp DoctorsPer1000 NurPer1000
TIME           1.00000000 0.5072429 0.6689116      0.3918010 0.06962438
HealthExp      0.50724289 1.0000000 0.8189234      0.3343370 0.63277351
PharmaExp      0.66891158 0.8189234 1.0000000      0.2943713 0.34073287
DoctorsPer1000 0.39180101 0.3343370 0.2943713      1.0000000 0.34760496
NurPer1000     0.06962438 0.6327735 0.3407329      0.3476050 1.00000000

########MODEL BUILDING: Unrestricted Model   (Y= b1x1+ .... +b4x4 + b0)
#Y=        x1 +     x2     +     x3        +     x4  (b0 through b4 are the estimates for our model)
> m1 <- lm(PharmaExp ~ TIME + HealthExp + DoctorsPer1000 + NurPer1000, data = healthset)
> summary(m1)

Residuals:
  Min      1Q  Median      3Q     Max 
-262.31  -59.67   12.93   59.05  237.23 

Coefficients:    #Our b0 through b4 is the estimate
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -1.445e+04  1.459e+03  -9.907  < 2e-16 ***      #b0= -14450
  TIME            7.343e+00  7.311e-01  10.044  < 2e-16 ***    #b1= 7.343
  HealthExp       8.838e-02  3.918e-03  22.558  < 2e-16 ***    #b2= 0.0838
  DoctorsPer1000 -6.413e+00  6.687e+00  -0.959    0.338        #b3= -6.413
NurPer1000     -9.019e+00  1.695e+00  -5.321 1.57e-07 ***      #b4= -9.019
  
  
  #Model assesment, 3 stars equal high significance, less stars less significance
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 90.68 on 491 degrees of freedom
Multiple R-squared:  0.7739,	Adjusted R-squared:  0.7721 
F-statistic: 420.2 on 4 and 491 DF,  p-value: < 2.2e-16
#F test tests the model against a model with no predictors, or the intercept only model
#H0: Fit of int-only model and the model are equal
#Ha: Fit of int model is significantly reduced compared to our model
# Pvalue: 2.2e-16 < 420.2,  reject null


########Second model without doctors: Restricted
> m2 <- lm(PharmaExp ~ TIME + HealthExp + NurPer1000, data = healthset)
> summary(m2)

Residuals:
  Min      1Q  Median      3Q     Max 
-260.93  -63.57   10.79   61.98  235.03 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.394e+04  1.356e+03 -10.277  < 2e-16 ***
  TIME         7.078e+00  6.768e-01  10.458  < 2e-16 ***
  HealthExp    8.878e-02  3.895e-03  22.790  < 2e-16 ***
  NurPer1000  -9.547e+00  1.603e+00  -5.956 4.93e-09 ***  #higher, but still stays under 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 90.68 on 492 degrees of freedom
Multiple R-squared:  0.7735,	Adjusted R-squared:  0.7721 
F-statistic: 560.1 on 3 and 492 DF,  p-value: < 2.2e-16

#Adjusted Rsq. usually tells which model is better fit, but since it is the same we'll use

#F test for comparison of 2 models
#H0:F<crit. Restricted model is better fit
#Ha:F>crit. Unrestricted model is better fit
#F= ((Rsq.un-Rsq.restrict)/q)/(1-Rsq.un)(n-k.un-1) 
#Following F distribution with q and (n-k-1) dof
((0.7739-0.7735)/4)/((1-0.7739)/491)
F=0.2171605
> qf(0.95,4,491)  #qf function gives critical f value given DOF and percentage
Fcrit= 2.390095  

#0.2171605<2.390095  
#Cannot reject at 0.95 level, restricted model is better fit

######PREDICTION USING MODELS 
#Our model uses the global average, so all items are taken into 
> m3 <- lm(PharmaExp ~ TIME + LOCATION + HealthExp + DoctorsPer1000 + NurPer1000, data = healthset)
> summary(m3)
lm(formula = PharmaExp ~ TIME + LOCATION + HealthExp + DoctorsPer1000 + NurPer1000, data = healthset)

Residuals:
  Min       1Q   Median       3Q      Max 
-236.983  -18.771    3.479   25.010  132.696 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -2.631e+04  1.571e+03 -16.750  < 2e-16 ***
  TIME            1.341e+01  7.972e-01  16.827  < 2e-16 ***
  LOCATIONAUT    -4.531e+01  2.720e+01  -1.666 0.096441 .  
LOCATIONBEL     1.914e+01  1.594e+01   1.201 0.230449    
LOCATIONCAN     9.489e+01  1.789e+01   5.304 1.76e-07 ***
  LOCATIONCHE     2.684e+02  2.221e+01  12.086  < 2e-16 ***
  LOCATIONCZE    -4.816e+01  1.709e+01  -2.818 0.005043 ** 
  LOCATIONDEU     9.497e+01  1.528e+01   6.214 1.16e-09 ***
  LOCATIONDNK    -1.005e+02  1.245e+01  -8.077 5.91e-15 ***
  LOCATIONESP    -8.835e+01  2.308e+01  -3.828 0.000147 ***
  LOCATIONEST    -2.059e+02  2.056e+01 -10.013  < 2e-16 ***
  LOCATIONFIN     1.460e+00  1.504e+01   0.097 0.922672    
LOCATIONFRA     2.456e+01  1.747e+01   1.406 0.160476    
LOCATIONGBR    -2.022e+02  2.782e+01  -7.270 1.57e-12 ***
  LOCATIONHUN    -3.702e+01  1.890e+01  -1.959 0.050726 .  
LOCATIONIRL     4.683e+01  2.167e+01   2.161 0.031220 *  
  LOCATIONISL     1.072e+02  1.283e+01   8.355 7.86e-16 ***
  LOCATIONISR    -2.757e+02  2.495e+01 -11.052  < 2e-16 ***
  LOCATIONITA    -7.008e+01  2.771e+01  -2.529 0.011789 *  
  LOCATIONJPN     5.853e+01  2.024e+01   2.891 0.004019 ** 
  LOCATIONKOR    -1.740e+02  2.388e+01  -7.285 1.42e-12 ***
  LOCATIONLTU    -4.915e+01  2.435e+01  -2.019 0.044099 *  
  LOCATIONLUX    -1.183e+02  1.785e+01  -6.627 9.63e-11 ***
  LOCATIONLVA    -2.213e+02  2.328e+01  -9.503  < 2e-16 ***
  LOCATIONMEX    -3.143e+02  2.704e+01 -11.622  < 2e-16 ***
  LOCATIONNLD    -1.105e+02  1.422e+01  -7.774 5.07e-14 ***
  LOCATIONNOR    -6.745e+01  1.822e+01  -3.702 0.000240 ***
  LOCATIONNZL    -2.198e+02  2.632e+01  -8.352 8.07e-16 ***
  LOCATIONPOL    -2.224e+02  2.235e+01  -9.952  < 2e-16 ***
  LOCATIONRUS    -1.634e+02  4.547e+01  -3.595 0.000360 ***
  LOCATIONSVK    -2.435e+01  2.024e+01  -1.204 0.229401    
LOCATIONSVN    -6.768e+01  1.723e+01  -3.928 9.88e-05 ***
  LOCATIONSWE    -3.125e+01  1.477e+01  -2.116 0.034851 *  
  LOCATIONTUR    -3.493e+02  3.549e+01  -9.844  < 2e-16 ***
  LOCATIONUSA     1.019e+02  2.647e+01   3.848 0.000136 ***
  HealthExp       6.392e-02  5.499e-03  11.624  < 2e-16 ***
  DoctorsPer1000 -3.712e+01  1.240e+01  -2.995 0.002895 ** 
  NurPer1000     -2.202e+01  2.702e+00  -8.152 3.46e-15 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 40.51 on 458 degrees of freedom
Multiple R-squared:  0.9579,	Adjusted R-squared:  0.9545 
F-statistic: 281.7 on 37 and 458 DF,  p-value: < 2.2e-16

#Now lets filter out anything less than 95% confidence
#Removing bulk countries
> healthset3 <- subset(healthset3, LOCATION !="SVK")
> healthset3 <- subset(healthset3, LOCATION !="FRA")
> healthset3 <- subset(healthset3, LOCATION !="FIN")
> healthset3 <- subset(healthset3, LOCATION !="HUN")
> healthset3 <- subset(healthset3, LOCATION !="BEL")
> healthset3 <- subset(healthset3, LOCATION !="NAUT")
#second cleaning:  doctors per 1000 was also proven insignificant
> m4 <- lm(PharmaExp ~ TIME + LOCATION + HealthExp + NurPer1000, data = healthset3)
> m5 <- lm(formula = PharmaExp ~ TIME + LOCATION + HealthExp + DoctorsPer1000 + NurPer1000, data = healthset3) #m4 and m5 take from the same cleaned data set
#residuals below
> summary(m4)

Call:
  lm(formula = PharmaExp ~ TIME + LOCATION + HealthExp + NurPer1000, 
     data = healthset3)

Residuals:
  Min       1Q   Median       3Q      Max 
-236.775  -17.879    2.841   23.706  127.616 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2.104e+04  1.435e+03 -14.662  < 2e-16 ***
  TIME         1.072e+01  7.238e-01  14.813  < 2e-16 ***
  LOCATIONAUT -9.374e+01  1.857e+01  -5.048 6.93e-07 ***
  LOCATIONCAN  1.288e+02  1.687e+01   7.635 1.82e-13 ***
  LOCATIONCHE  2.365e+02  2.191e+01  10.794  < 2e-16 ***
  LOCATIONCZE -5.953e+01  1.482e+01  -4.017 7.09e-05 ***
  LOCATIONDEU  7.562e+01  1.345e+01   5.622 3.65e-08 ***
  LOCATIONDNK -1.125e+02  1.219e+01  -9.229  < 2e-16 ***
  LOCATIONESP -9.373e+01  2.227e+01  -4.209 3.20e-05 ***
  LOCATIONEST -1.984e+02  2.041e+01  -9.719  < 2e-16 ***
  LOCATIONGBR -1.703e+02  2.838e+01  -6.000 4.56e-09 ***
  LOCATIONIRL  7.103e+01  2.115e+01   3.358 0.000865 ***
  LOCATIONISL  8.263e+01  1.154e+01   7.163 4.08e-12 ***
  LOCATIONISR -2.675e+02  2.510e+01 -10.655  < 2e-16 ***
  LOCATIONITA -7.557e+01  2.648e+01  -2.854 0.004552 ** 
  LOCATIONJPN  9.926e+01  1.848e+01   5.370 1.37e-07 ***
  LOCATIONKOR -1.127e+02  2.306e+01  -4.890 1.49e-06 ***
  LOCATIONLTU -6.545e+01  2.029e+01  -3.226 0.001365 ** 
  LOCATIONLUX -1.051e+02  1.809e+01  -5.812 1.30e-08 ***
  LOCATIONLVA -2.023e+02  2.408e+01  -8.400 8.83e-16 ***
  LOCATIONMEX -2.527e+02  2.781e+01  -9.086  < 2e-16 ***
  LOCATIONNLD -1.024e+02  1.440e+01  -7.114 5.57e-12 ***
  LOCATIONNOR -1.012e+02  1.624e+01  -6.228 1.24e-09 ***
  LOCATIONNZL -1.843e+02  2.595e+01  -7.102 6.02e-12 ***
  LOCATIONPOL -1.746e+02  2.311e+01  -7.556 3.09e-13 ***
  LOCATIONRUS -1.646e+02  4.504e+01  -3.655 0.000293 ***
  LOCATIONSVN -3.270e+01  1.743e+01  -1.876 0.061370 .  
LOCATIONSWE -5.013e+01  1.235e+01  -4.060 5.94e-05 ***
  LOCATIONTUR -2.853e+02  3.532e+01  -8.079 8.63e-15 ***
  LOCATIONUSA  1.168e+02  2.641e+01   4.421 1.28e-05 ***
  HealthExp    6.706e-02  5.789e-03  11.584  < 2e-16 ***
  NurPer1000  -2.122e+01  2.855e+00  -7.432 7.06e-13 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 41.05 on 383 degrees of freedom
Multiple R-squared:  0.9593,	Adjusted R-squared:  0.9561 
F-statistic: 291.5 on 31 and 383 DF,  p-value: < 2.2e-16
#Up to 95% is a standard prediction, from here we prove that US has the most pull

#m3:Natural set
> summary(aov(m3))
Df Sum Sq Mean Sq F value   Pr(>F)    
TIME             1 7991502 7991502 4869.32  < 2e-16 ***
  LOCATION        33 8857954  268423  163.55  < 2e-16 ***
  HealthExp        1  115708  115708   70.50 5.80e-16 ***
  DoctorsPer1000   1   34505   34505   21.02 5.86e-06 ***
  NurPer1000       1  109056  109056   66.45 3.46e-15 ***
  Residuals      458  751667    1641                     

Multiple R-squared:  0.9579,	Adjusted R-squared:  0.954

5#Not highest adjusted and variables of varying significance


#m4:Cleaned and no doctors, lower p values, more significant
> summary(aov(m4))
Df  Sum Sq Mean Sq F value   Pr(>F)    
TIME          1 6543066 6543066 3765.99  < 2e-16 ***
  LOCATION     27 8381513  310426  178.67  < 2e-16 ***
  HealthExp     1  143104  143104   82.37  < 2e-16 ***
  NurPer1000    1   92157   92157   53.04 1.98e-12 ***
  Residuals   370  642841    1737                  

Multiple R-squared:  0.9593,	Adjusted R-squared:  0.956 
#High Adjusted and all factors are significant, three are up to 99.999% certainty

#m5:Cleaned and with doctors
> summary(aov(m5))
Df  Sum Sq Mean Sq  F value   Pr(>F)    
TIME             1 6543066 6543066 3761.630  < 2e-16 ***
  LOCATION        27 8381513  310426  178.465  < 2e-16 ***
  HealthExp        1  143104  143104   82.271  < 2e-16 ***
  DoctorsPer1000   1     819     819    0.471    0.493    
NurPer1000       1   92332   92332   53.082 1.96e-12 ***
  Residuals      369  641847    1739   
Multiple R-squared:  0.9594,	Adjusted R-squared:  0.956 

#Same adjusted, doctors do not count and factor into the r^2, two at 99.999% certainty


######PLOT POINTS
> ggplot(healthset, aes(x= TIME, y = PharmaExp, colour = LOCATION)) + geom_point(position = position_jitter())
> ggplot(healthset, aes(x= TIME, y = PharmaExp, colour = LOCATION)) + geom_point(alpha = 0.3, position = position_jitter()) + stat_smooth(method = "lm")

#box plot
> ggplot(subset(healthset, !is.na(TIME)), aes(x = LOCATION, y = PharmaExp)) +
  +     geom_boxplot()
> ggplot(subset(healthset, !is.na(TIME)), aes(x = LOCATION, y = HealthExp)) +
  +     geom_boxplot()
> ggplot(subset(healthset, !is.na(TIME)), aes(x = LOCATION, y = Doctorsper1000)) +
  +     geom_boxplot()
> ggplot(subset(healthset, !is.na(TIME)), aes(x = LOCATION, y = NurPer1000)) +
  +     geom_boxplot()

#barplot (issues)
> healthset %>%
  +     filter(!is.na(PharmaExp)) %>%
  +     group_by(LOCATION) %>%
  +     summarize(mean_PharmaExp = mean(PharmaExp)) %>%
  +     ggplot(aes(x = LOCATION, y = mean_PharmaExp)) + geom_bar(stat = "identity")



#ggplot time series
> ggplot(healthset, aes(x=TIME, y=PharmaExp, group=LOCATION, color=LOCATION)) +     geom_line()
> ggplot(healthset, aes(x=TIME, y=HealthExp, group=LOCATION, color=LOCATION)) +     geom_line()

#ts of the top 5 most expensive pharm expenditures
> healthset %>%
  +     filter(LOCATION %in% c("USA", "CHE", "CAN", "JPN", "DEU")) %>%
  +     ggplot(aes(x=TIME, y=PharmaExp, group=LOCATION, color=LOCATION)) + geom_line()


#regression
> smp_size <- floor(0.75 * nrow(healthset))
> set.seed(123)
> train_ind <- sample(seq_len(nrow(healthset)), size = smp_size)
> trainset <- healthset[train_ind, ]
> testset <- healthset[-train_ind, ]
> testset2 <- testset
> testset2$PharmaExp <- NA

#split data set to 3 levels to lower standard error (low,mid,high exp)
#used for various regression models and will fill our testing data
#from therest