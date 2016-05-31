################################################
###        Predict full ranking model        ###
################################################

### Read top 200 cleaned data set  to create model ### TODO: import model binary
universities = read.csv(file='csv/universities-cleaned.csv',header=TRUE,sep=",",nrows=200)

### Predict score based on model ###
unilm = lm(total_score~teaching+international+research+citations+income+student_staff_ratio,
           data=universities)

### Read all 800 universities ###
universities = read.csv(file='csv/universities-cleaned.csv',header=TRUE,sep=",")
universities = universities[-c(6,7,9,10)]

universities$predict_score = as.numeric(round(predict.lm(unilm, universities),1))

prediction = universities[ , which(names(universities) %in% c("id", "predict_score"))]
prediction = prediction[order(-prediction$predict_score),]
prediction$predict_rank = 1:nrow(prediction)

### Compare predicted and actual rank
uni_info = read.csv(file='csv/universities-info.csv',header=TRUE,sep=",")
result = merge(prediction, uni_info[c(1,4,5)], by="id")
result$predict_rank_group = result$predict_rank
# For Rank 200+ the world_rank is binned
# Bins: [201-250], [251-300], [301-350], [401-500], [501-600], [601-800]
# Use floor and ceiling to get the right bin for each rank over 200
for (i in 1:nrow(result))
{
  rank = result$predict_rank[i]
  if (rank > 200)
  {
    if (rank <= 400) {bin = 50} 
    else if (rank <= 600){bin = 100}
    else if (rank <= 800){bin = 200}
    result$predict_rank_group[i] = paste((floor(rank/bin)*bin)+1,(ceiling(rank/bin)*bin),sep="-")
  }
}
exact = sum(result$predict_rank_group == result$world_rank)
cat(paste("Exact matches:\t", (exact/nrow(result))*100.0, "%\n", sep=""))