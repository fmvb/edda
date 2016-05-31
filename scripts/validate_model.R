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
# If universities had the same score, they should get the same rank
for (i in 2:nrow(prediction))
{
  if (prediction$predict_score[i] == prediction$predict_score[i-1])
  {
    prediction$predict_rank[i] = prediction$predict_rank[i-1]
  }
}

### Compare predicted and actual rank
uni_info = read.csv(file='csv/universities-info.csv',header=TRUE,sep=",",nrows=200)
result = merge(prediction, uni_info[c(1,4,5)], by="id")
exact = sum(result$predict_rank == result$world_rank)
one_off = sum(abs(result$predict_rank-result$world_rank) <= 1)
at_most_five = sum(abs(result$predict_rank-result$world_rank) <= 3)

# Since we gave ids from 1:200 when sorted on the world_rank it gives
# a more accurate score when we compare with id since world_rank can 
# have the same rank twice when two universities have the same score, 
# and thus some ranks are not given (eg 1, 2, 3, 4, 4, 6, 7, 8, 8, 10)

#exact = sum(result$predict_rank == result$id)
#one_off = sum(abs(result$predict_rank-result$id) <= 1)
#at_most_five = sum(abs(result$predict_rank-result$id) <= 3)

printResults = function(exact, one_off, at_most_five, nresult)
{
  cat("Accuracy of prediction top 200: \n");
  cat(paste("Exact matches:\t", (exact/nresult)*100.0, "%\n", sep=""));
  cat(paste("One off:\t", (one_off/nresult)*100.0, "%\n", sep=""));
  cat(paste("At most 3 off:\t", (at_most_five/nresult)*100.0, "%", sep=""));
}
printResults(exact, one_off, at_most_five, nrow(result))
error_total_score = abs(result$predict_score - result$total_score)
hist(error_total_score, prob=TRUE)
summary(error_total_score)