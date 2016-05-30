################################################
###              Validate model              ###
################################################

### Read cleaned data set ###
universities = read.csv(file='csv/universities-cleaned.csv',header=TRUE,sep=",")


### Predict score based on model ###
unilm_sd = lm(total_score~teaching+international+research+citations+income+student_staff_ratio,
              data=universities)

universities$predict_score = as.numeric(round(predict.lm(unilm_sd),1))


### Assign rank based on prediction
prediction = universities[ , which(names(universities) %in% c("id", "predict_score"))]
prediction = prediction[order(-prediction$predict_score),]
prediction$predict_rank = 1:200


### Compare predicted and actual rank
universities = read.csv(file='csv/universities-info.csv',header=TRUE,sep=",")
result = merge(prediction, universities[c(1,4)], by="id")
#result = result[, which(names(result) %in% c("world_rank", "predict_rank"))]
sum(result$predict_rank == result$id)/nrow(result)