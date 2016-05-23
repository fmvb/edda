universities = read.csv(file='csv/universities.csv',header=TRUE,sep=",",
                        nrows=200,stringsAsFactors=FALSE)

columns_to_drop = c("year")

# Convert income to numeric
sapply(universities, class)
universities$income=as.numeric(universities$income)

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


hist(universities$income)
cor(universities[c(4:9)])
pairs(universities[c(4:9)])
