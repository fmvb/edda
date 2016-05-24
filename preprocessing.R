universities = read.csv(file='csv/universities.csv',header=TRUE,sep=",",
                        nrows=200,stringsAsFactors=FALSE,
                        na.strings=c("","NA"))

universities = universities[ , -which(names(universities) %in% c("year"))]
universities$income[universities$income=="-"] = NA

# Print number of missing values per column
for (i in 1:length(names(universities)))
{
  print(paste(names(universities)[i],
  length(universities[i][is.na(universities[i])])))
}

# Convert income to numeric
sapply(universities, class)
universities$income=as.numeric(universities$income)
# Fill missing income values with median
hist(universities$income)
universities$income[is.na(universities$income)] = 
  median(universities$income,na.rm = T)

# Remove '%' from international_students and convert to numeric
universities$international_students = 
  as.numeric(substr(universities$international_students,1,
                    nchar(universities$international_students)-1))/100

# Convert female_male_ratio to numeric
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
universities$f_m_ratio = NA
for (i in 1:length(universities$female_male_ratio))
{
  tmp = as.numeric(trim(strsplit(universities$female_male_ratio, ":")[[i]]))
  universities$f_m_ratio[i] = round(tmp[1]/tmp[2], 2)
}
hist(universities$f_m_ratio)
length(universities$f_m_ratio[is.na(universities$f_m_ratio)])/
  length(universities$f_m_ratio)
# Fill missing female/male ratios with average of mean and median (close together)
universities$f_m_ratio[is.na(universities$f_m_ratio)] = 
  round(mean(c(mean(universities$f_m_ratio, na.rm=TRUE),
               median(universities$f_m_ratio, na.rm=TRUE))),2)
# remove temp column f_m_ratio
universities$female_male_ratio = universities$f_m_ratio
universities = universities[ , -which(names(universities) %in% c("f_m_ratio"))]

# TO DO: num_students
