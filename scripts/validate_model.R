################################################
###              Validate model              ###
################################################

### Read top 200 cleaned data set ###
universities = read.csv(file='csv/universities-cleaned.csv',header=TRUE,sep=",",nrows=200)

### Predict score based on model ###
unilm = lm(total_score~teaching+international+research+citations+income+student_staff_ratio,
              data=universities)
universities$predict_score = as.numeric(round(predict.lm(unilm),1))

### Assign rank based on prediction
prediction = universities[ , which(names(universities) %in% c("id", "predict_score"))]
prediction = prediction[order(-prediction$predict_score),]
prediction$predict_rank = 1:nrow(prediction)

### Compare predicted and actual rank
uni_info = read.csv(file='csv/universities-info.csv',header=TRUE,sep=",",nrows=200)
result = merge(prediction, uni_info[c(1,4)], by="id")
exact = sum(result$predict_rank == result$world_rank)
one_off = sum(abs(result$predict_rank-result$world_rank) <= 1)
at_most_five = sum(abs(result$predict_rank-result$world_rank) <= 3)

exact = sum(result$predict_rank == result$id)
one_off = sum(abs(result$predict_rank-result$id) <= 1)
at_most_five = sum(abs(result$predict_rank-result$id) <= 3)

printResults = function(exact, one_off, at_most_five, nresult)
{
  cat("Accuracy of prediction top 200: \n");
  cat(paste("Exact matches:\t", (exact/nresult)*100.0, "%\n", sep=""));
  cat(paste("One off:\t", (one_off/nresult)*100.0, "%\n", sep=""));
  cat(paste("At most 3 off:\t", (at_most_five/nresult)*100.0, "%", sep=""));
}
printResults(exact, one_off, at_most_five, nrow(universities))