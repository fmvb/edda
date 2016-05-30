###############################
### Pre-processing data set ###
###############################

### Read raw data for top 200 universities ###
universities = read.csv(file='csv/universities.csv',header=TRUE,sep=",",
                        stringsAsFactors=FALSE, na.strings=c("","NA"))
# remove column year, all data is from 2016
universities = universities[ , -which(names(universities) %in% c("year"))]
universities$income[universities$income=="-"] = NA
universities$female_male_ratio[universities$female_male_ratio=="-"] = NA

### Initial data exploration
# print number of missing values per variable
for (i in 1:length(names(universities)))
{
  print(paste(names(universities)[i],
  length(universities[i][is.na(universities[i])])))
}
# data types variables
sapply(universities, class)
# auxiliary function to remove leading and trailing white spaces
trim = function (x) gsub("^\\s+|\\s+$", "", x)

### variable: income ###
# convert income to numeric
universities$income=as.numeric(universities$income)
# Fill missing income values with median
hist(universities$income, prob=TRUE, main="Histogram of income",xlab="income")
universities$income[is.na(universities$income)] = 
  median(universities$income, na.rm=TRUE)

### variable: international_students ###
# remove '%' from international_students and convert to numeric (normalized)
universities$international_students = 
  as.numeric(substr(universities$international_students,1,
                    nchar(universities$international_students)-1))/100
# fill missing values with median
hist(universities$international_students,prob=TRUE, 
     main="Histogram of international_students",xlab="international_students")
universities$international_students[is.na(universities$international_students)] = 
  median(universities$international_students,na.rm=TRUE)

### variable: female_male_ratio ###
# original format "a : b", extract a and b, compute a/b, convert to numeric
universities$f_m_ratio = NA
for (i in 1:length(universities$female_male_ratio))
{
  tmp = as.numeric(trim(strsplit(universities$female_male_ratio, ":")[[i]]))
  universities$f_m_ratio[i] = round(tmp[1]/tmp[2], 2)
}

# fill missing female/male ratios with average of mean and median (close together)
hist(universities$f_m_ratio,prob=TRUE, 
     main="Histogram of female_male_ratio",xlab="female_male_ratio")

universities$f_m_ratio[is.na(universities$f_m_ratio)] = 
  round(median(universities$f_m_ratio[universities$f_m_ratio<4], na.rm=TRUE),2)
# remove temp column f_m_ratio
universities$female_male_ratio = universities$f_m_ratio
universities = universities[ , -which(names(universities) %in% c("f_m_ratio"))]

### variable: num_students ###
# create temp column nr_students
universities$nr_students = universities$num_students
# remove ',', add 0's to trailing part while length < 3, paste split results
for (i in 1:length(universities$num_students))
{
  if (!is.na(universities$num_students[i]))
  {
    tmp = trim(strsplit(universities$num_students,",")[[i]])
    if (!is.na(tmp[2]))
    {
      while (nchar(tmp[2]) < 3)
      {
        tmp[2] = paste(tmp[2],"0",sep="")
      }
      universities$nr_students[i] = paste(tmp[1],tmp[2],sep="")
    } else
    {
      universities$nr_students[i] = tmp[1]
    }
    universities$nr_students = as.numeric(universities$nr_students)
  }
}
# fill missing values with median
universities$nr_students[is.na(universities$nr_students)] = round(median(universities$nr_students,na.rm=TRUE))
# remove temp column nr_students
universities$num_students = universities$nr_students
universities = universities[ , -which(names(universities) %in% c("nr_students"))]

### variable: student_staff_ratio ###
# fill missing student staff ratios with median
hist(universities$student_staff_ratio,prob=TRUE)
universities$student_staff_ratio[is.na(universities$student_staff_ratio)] = 
  median(universities$student_staff_ratio,na.rm=TRUE)

# create id column to identify university
universities$id = 1:nrow(universities)

# move descriptive columns to seperate dataframe, keep id in both to cross-reference
columns = c("id", "university_name", "country", "world_rank")
university_info = universities[ , which(names(universities) %in% columns)]
universities = universities[,-which(names(universities) %in% setdiff(columns,"id"))]

# write cleaned data set to csv
write.csv(universities, file="csv/universities-cleaned.csv", row.names=FALSE)
write.csv(university_info, file="csv/universities-info.csv", row.names=FALSE)