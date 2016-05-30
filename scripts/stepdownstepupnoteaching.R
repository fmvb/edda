#step-up model without teaching
#Fourth explanatory variable
summary(lm(total_score~research+citations+international+
             income, data=universities))                                    #R = 0.9695 
summary(lm(total_score~research+citations+international+
             num_students, data=universities))                              #R = 0.9704 
summary(lm(total_score~research+citations+international+
             student_staff_ratio, data=universities))                       ## R = 0.9723  ## 
summary(lm(total_score~research+citations+international+
             international_students, data=universities))                    #R = 0.9689 insign. var 
summary(lm(total_score~research+citations+international+
             female_male_ratio, data=universities))                         #R = 0.9698

#Fifth explanatory variable
summary(lm(total_score~research+citations+international+
             student_staff_ratio+income,
           data=universities))                                              ##R = 0.9745  ##
summary(lm(total_score~research+citations+international+
             student_staff_ratio+num_students,
           data=universities))                                              #R = 0.9732
summary(lm(total_score~research+citations+international+
             student_staff_ratio+international_students,
           data=universities))                                              #R = 0.9724 insign. var
summary(lm(total_score~research+citations+international+
             student_staff_ratio+female_male_ratio,
           data=universities))                                              #R = 0.9732

#Sixth explanatory variable
summary(lm(total_score~research+citations+international+
             student_staff_ratio+income+num_students,
           data=universities))                                              #R = 0.9749 -> insig. var
summary(lm(total_score~research+citations+international+
             student_staff_ratio+income+international_students,
           data=universities))                                              #R = 0.9746 -> insig. var
summary(lm(total_score~research+citations+international+
             student_staff_ratio+income+female_male_ratio,
           data=universities))                                              #R = 0.9748 -> insig. var

# step-up model without teaching, found in fifth step
unilm_sunoteaching = lm(total_score~research+citations+international+
                          student_staff_ratio+income,data=universities)
summary(unilm_sunoteaching)
coef(unilm_sunoteaching)
par(mfrow=c(1,2));qqnorm(residuals(unilm_sunoteaching));plot(fitted(unilm_sunoteaching),residuals(unilm_sunoteaching))
round(cooks.distance(unilm_sunoteaching),5)


# step down without teaching (colinear)
summary(lm(total_score~international+research+
             citations+income+num_students+student_staff_ratio+
             international_students+female_male_ratio,
           data=universities))                                    # R2: 0.9752 -> rm international_students
summary(lm(total_score~international+research+
             citations+income+num_students+student_staff_ratio+
             female_male_ratio ,data=universities))               # R2: 0.9751 -> rm female_male_ratio 
summary(lm(total_score~international+research+
             citations+income+num_students+student_staff_ratio
           ,data=universities))                                 # R2: 0.9749 -> rm num_students
summary(lm(total_score~international+research+
             citations+income+student_staff_ratio
           ,data=universities))                                   # R2: 0.9745 -> all significant

unilm_sdnoteaching = lm(total_score~international+research+
                          citations+income+student_staff_ratio,
                        data=universities)
coef(unilm_sdnoteaching)
par(mfrow=c(1,2));qqnorm(residuals(unilm_sdnoteaching));plot(fitted(unilm_sdnoteaching),residuals(unilm_sdnoteaching))
round(cooks.distance(unilm_sdnoteaching),5)
