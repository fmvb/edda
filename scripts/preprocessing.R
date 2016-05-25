###############################
### Pre-processing data set ###
###############################

### Read raw data for top 200 universities ###
universities = read.csv(file='csv/universities.csv',header=TRUE,sep=",",
                        nrows=200,stringsAsFactors=FALSE,
                        na.strings=c("","NA"))
# remove column year, all data is from 2016
universities = universities[ , -which(names(universities) %in% c("year"))]
universities$income[universities$income=="-"] = NA

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
# Convert income to numeric
universities$income=as.numeric(universities$income)
# Fill missing income values with median
hist(universities$income)
universities$income[is.na(universities$income)] = 
  median(universities$income, na.rm=TRUE)

### variable: international_students ###
# remove '%' from international_students and convert to numeric (normalized)
universities$international_students = 
  as.numeric(substr(universities$international_students,1,
                    nchar(universities$international_students)-1))/100
# Fill missing values with median
hist(universities$international_students,prob=TRUE)
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

# Fill missing female/male ratios with average of mean and median (close together)
hist(universities$f_m_ratio)
length(universities$f_m_ratio[is.na(universities$f_m_ratio)])/
  length(universities$f_m_ratio)

universities$f_m_ratio[is.na(universities$f_m_ratio)] = 
  round(mean(c(mean(universities$f_m_ratio, na.rm=TRUE),
               median(universities$f_m_ratio, na.rm=TRUE))),2)
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

# Fill missing student staff ratios with median
hist(universities$student_staff_ratio,prob=TRUE)
universities$student_staff_ratio[is.na(universities$student_staff_ratio)] = 
  median(universities$student_staff_ratio,na.rm=TRUE)

# Create id column to identify university
universities$id = 1:200

# Move descriptive columns to seperate dataframe
university_info = universities[c(14,2,3)]
universities = universities[-c(2,3)]

write.csv(universities, file="csv/universities-cleaned.csv", row.names=FALSE)
write.csv(university_info, file="csv/universities-info.csv", row.names=FALSE)