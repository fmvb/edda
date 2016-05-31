################################################
### Setting up models to predict total_score ###
################################################

### Read cleaned data set ###
universities = read.csv(file='csv/universities-cleaned.csv',header=TRUE,sep=",",nrows=200)

# Distribution total_score
hist(universities$total_score,
     main="Histogram of total_score Top 200 universities",
     xlab="Total score")

boxplot(universities$total_score, main="Boxplot of total_score Top 200 universities", 
        ylab="Total score")

# Explore correlations
cor(universities)
pairs(universities)

columns = c("total_score", "teaching", "research", "citations", "international_students")
round(cor(universities[,which(names(universities) %in% columns)]),3)
pairs(universities[,which(names(universities) %in% columns)])
columns = c("teaching", "research")
pairs(universities[,which(names(universities) %in% columns)])

################################################
###              Step-up Method              ###
################################################
# first explanatory variable
summary(lm(total_score~teaching, data=universities))                  # R2: 0.828,    p: <2e-16
summary(lm(total_score~international, data=universities))             # R2: 0.02283,  p: 0.0327
summary(lm(total_score~research, data=universities))                 ## R2: 0.8524,   p: <2e-16 ##
summary(lm(total_score~citations, data=universities))                 # R2: 0.2177,   p: 3.32e-12
summary(lm(total_score~income, data=universities))                    # R2: 0.03142,  p: 0.012
summary(lm(total_score~num_students, data=universities))              # R2: 0.005034, p: 0.318
summary(lm(total_score~student_staff_ratio, data=universities))       # R2: 0.04352,  p: 0.00303
summary(lm(total_score~international_students, data=universities))    # R2: 0.07823,  p: 6.04e-05
summary(lm(total_score~female_male_ratio, data=universities))         # R2: 0.01948,  p: 0.0487

# second explanatory variable
summary(lm(total_score~research+teaching, data=universities))                # R2: 0.8876
summary(lm(total_score~research+international, data=universities))           # R2: 0.8619
summary(lm(total_score~research+citations, data=universities))              ## R2: 0.9621 ##
summary(lm(total_score~research+income, data=universities))                  # R2: 0.8563
summary(lm(total_score~research+num_students, data=universities))            # R2: 0.8711
summary(lm(total_score~research+student_staff_ratio, data=universities))     # R2: 0.8692
summary(lm(total_score~research+international_students, data=universities))  # R2: 0.8662
summary(lm(total_score~research+female_male_ratio, data=universities))       # R2: 0.8531

# third explanatory variable
summary(lm(total_score~research+citations+teaching, data=universities))               ## R2: 0.9846 ##
summary(lm(total_score~research+citations+international, data=universities))           # R2: 0.9684
summary(lm(total_score~research+citations+income, data=universities))                  # R2: 0.9628
summary(lm(total_score~research+citations+num_students, data=universities))            # R2: 0.9662
summary(lm(total_score~research+citations+student_staff_ratio, data=universities))     # R2: 0.9656
summary(lm(total_score~research+citations+international_students, data=universities))  # R2: 0.9682
summary(lm(total_score~research+citations+female_male_ratio, data=universities))       # R2: 0.9629

# fourth explanatory variable
summary(lm(total_score~research+citations+teaching+international,
           data=universities))                                             ## R2: 0.9979 ##
summary(lm(total_score~research+citations+teaching+income,
           data=universities))                                              # R2: 0.9858
summary(lm(total_score~research+citations+teaching+num_students,
           data=universities))                                              # R2: 0.9864
summary(lm(total_score~research+citations+teaching+student_staff_ratio,
           data=universities))                                              # R2: 0.9846 -> insig. var.
summary(lm(total_score~research+citations+teaching+international_students,
           data=universities))                                              # R2: 0.9932
summary(lm(total_score~research+citations+teaching+female_male_ratio,
           data=universities))                                              # R2: 0.9846 -> insig. var.

# fifth explanatory variable
summary(lm(total_score~research+citations+teaching+international+
             income,data=universities))                                    ## R2: 0.9999 ##
summary(lm(total_score~research+citations+teaching+international+
             num_students,data=universities))                               # R2: 0.998
summary(lm(total_score~research+citations+teaching+international+
             student_staff_ratio,data=universities))                        # R2: 0.998
summary(lm(total_score~research+citations+teaching+international+
             international_students,data=universities))                     # R2: 0.9979 -> insig. var.
summary(lm(total_score~research+citations+teaching+international+
             female_male_ratio,data=universities))                          # R2: 0.9981

# sixth explanatory variable
summary(lm(total_score~research+citations+teaching+international+
             income+num_students,data=universities))                        # R2: 0.9999 -> insig. var.
summary(lm(total_score~research+citations+teaching+international+
             income+student_staff_ratio,data=universities))                ## R2: 0.9999 ##
summary(lm(total_score~research+citations+teaching+international+
             income+international_students,data=universities))              # R2: 0.9999 -> insig. var.
summary(lm(total_score~research+citations+teaching+international+
             income+female_male_ratio,data=universities))                   # R2: 0.9999 -> insig. var.

# seventh explanatory variable
summary(lm(total_score~research+citations+teaching+international+
             income+student_staff_ratio+num_students,
           data=universities))                                              # R2: 0.9999 -> insig. var.
summary(lm(total_score~research+citations+teaching+international+
             income+student_staff_ratio+international_students,
           data=universities))                                              # R2: 0.9999 -> insig. var.
summary(lm(total_score~research+citations+teaching+international+
             income+student_staff_ratio+female_male_ratio,
           data=universities))                                              # R2: 0.9999 -> insig. var.

# step-up model, found in sixth step
unilm_su = lm(total_score~research+citations+teaching+international+
                    income+student_staff_ratio,data=universities)
summary(unilm_su)
coef(unilm_su)
par(mfrow=c(1,2));qqnorm(residuals(unilm_su));plot(fitted(unilm_su),residuals(unilm_su))
round(cooks.distance(unilm_su),5)


################################################
###             Step-down Method             ###
################################################
summary(lm(total_score~teaching+international+research+
             citations+income+num_students+student_staff_ratio+
             international_students+female_male_ratio,
           data=universities))                                    # R2: 0.9999 -> rm female_male_ratio
summary(lm(total_score~teaching+international+research+
             citations+income+num_students+student_staff_ratio+
             international_students,data=universities))           # R2: 0.9999 -> rm num_students
summary(lm(total_score~teaching+international+research+
             citations+income+student_staff_ratio+
             international_students,data=universities))           # R2: 0.9999 -> rm international_students
summary(lm(total_score~teaching+international+research+
             citations+income+student_staff_ratio,
           data=universities))                                    # R2: 0.9999 -> all significant


unilm_sd = lm(total_score~teaching+international+research+
                citations+income+student_staff_ratio,
              data=universities)

summary(unilm_sd)

############################################################
###                    Resulting model                   ###
############################################################
# Resulting model is the same for step-up and step-down
unilm = lm(total_score~teaching+international+research+
                citations+income+student_staff_ratio,
              data=universities)
summary(unilm)

coef(unilm)
round(coef(unilm),3)
par(mfrow=c(1,2));qqnorm(residuals(unilm));plot(fitted(unilm),residuals(unilm))
round(cooks.distance(unilm),5)