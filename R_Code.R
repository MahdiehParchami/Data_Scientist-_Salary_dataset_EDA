setwd("E:/Group")

# install.packages("readr")
library(readr)
df <- read_csv("ds_salaries.csv")

# ******************************************************
# EDA and data cleaning

#  data cleaning

# find data type
# convert need for categorical data , as a factor

library(dplyr)
glimpse(df)

df$company_size <- as.factor(df$company_size)
df$experience_level <- as.factor(df$experience_level)
df$employment_type <- as.factor(df$employment_type)
df$remote_ratio <- as.factor(df$remote_ratio)
df$work_year <- as.factor(df$work_year)

# ******************************************************

# find missing value
sum(is.na(df))

library(visdat)
vis_miss(df)

# ******************************************************
#  find duplicate
sum(duplicated(df))

# ******************************************************
# find levels

levels(df$experience_level)
# EN Entry-level  
# MI Junior MI Mid-level
# SE Intermediate   Senior-level
# EX Expert EX Executive-level / Director

levels(df$company_size)
levels(df$employment_type)

# PT Part-time 
# FT Full-time 
# CT Contract 
# FL Freelance

levels(df$remote_ratio)
# 0 No remote work (less than 20%)
# 50 Partially remote 
# 100 Fully remote (more than 80%)

# *****************************************************
# descriptive analysis
summary(df)

str(df)

head

tail(df)


# ******************************************************

# Create new variables

df$experience_Categories <- ifelse(df$experience_level == "EN" , "Junior",
                                   ifelse(df$experience_level == "MI" , "Intermediate ",
                                          ifelse(df$experience_level == "SE" , "Senior",
                                                 ifelse(df$experience_level == "EX" , "Executive",df$experience_level))))


# data visualization
# correlation
library(dplyr)

dfcor <- print(select_if(df, is.numeric))
corr <- round(cor(dfcor), 1)

#-------------------------------------------------------------------------------
library(ggcorrplot)
ggcorrplot(corr, tl.cex = 7 , title = "Correlation Plot", method  =  "circle",
           lab = TRUE , lab_size = 3)


# **********************************************************
# Distribution of salary
# dev.off()

library(scales)

gs <- ggplot(df, aes(x = salary_in_usd)) +
  geom_histogram(color="#008080", fill="#00FF00") +
  
  labs(subtitle="Distribution of salary", 
       y="count", 
       x="salary", 
       title="Histogram", 
       caption = "Source: Kaggle")

gs + scale_x_continuous(labels = comma)

# **********************************************************

# experience_level

dfex <- df %>% 
  group_by(experience_Categories)%>%
  summarize(name_rows= n())

dfex$experience_Categories <- factor(dfex$experience_Categories,                                   
                                     levels = dfex$experience_Categories[order(dfex$name_rows, decreasing = TRUE)])

ggplot(dfex, aes(x = experience_Categories, y= name_rows, fill = experience_Categories)) +
  geom_bar(stat = "identity")+
  labs(subtitle="Experience level", 
       y="Number", 
       x="level", 
       title="Bar chart", 
       caption = "Source: Kaggle")

# *******************************************************

# employment_type

dfet <- df %>% 
  group_by(employment_type)%>%
  summarize(name_rows= n())

dfet$employment_type <- factor(dfet$employment_type,                                   
                               levels = dfet$employment_type[order(dfet$name_rows, decreasing = TRUE)])

ggplot(dfet, aes(x = employment_type, y= name_rows, fill = employment_type)) +
  geom_bar(stat = "identity")+
  labs(subtitle="Employment Type", 
       y="Number", 
       x="level", 
       title="Bar chart", 
       caption = "Source: Kaggle")

# ***************************************************************
# remote_ratio


dfr <- df %>% 
  group_by(remote_ratio)%>%
  summarize(name_rows= n())

dfr$remote_ratio <- factor(dfr$remote_ratio,                                   
                           levels = dfr$remote_ratio[order(dfr$name_rows, decreasing = TRUE)])

ggplot(dfr, aes(x = remote_ratio, y= name_rows, fill = remote_ratio)) +
  geom_bar(stat = "identity")+
  labs(subtitle="Remote ratio", 
       y="Number", 
       x="level", 
       title="Bar chart", 
       caption = "Source: Kaggle")

# **********************************************************
# company size

dfcs <- df %>% 
  group_by(company_size)%>%
  summarize(name_rows= n())

dfcs$company_size <- factor(dfcs$company_size,                                   
                            levels = dfcs$company_size[order(dfcs$name_rows, decreasing = TRUE)])

ggplot(dfcs, aes(x = company_size, y= name_rows, fill = company_size)) +
  geom_bar(stat = "identity")+
  labs(subtitle="Company size", 
       y="Number", 
       x="level", 
       title="Bar chart", 
       caption = "Source: Kaggle")

# ***********************************************************
# job title

dfmj <- df %>% 
  group_by(job_title)%>%
  summarize(name_rows= n())

dfmjo <- dfmj[order(dfmj$name_rows,decreasing = FALSE),]
dfmjt <- top_n(dfmjo, n=10, name_rows)
dfmjtt <- transform(dfmjt,job_title = reorder(job_title,order(name_rows, decreasing = FALSE)))

ggplot(dfmjtt) + 
  geom_bar(aes(x = job_title, y = name_rows), stat = 'identity',color = "#BA55D3" , fill = "#9932CC") +
  coord_flip() + 
  labs(subtitle="Top 10 More popular job title", 
       y="Number", 
       x="job title", 
       title="Bar plot", 
       caption = "Source: Kaggle")

# *************************************************************
# job title by salary

library(ggplot2)
library(dplyr)
library(scales)

dfjs <- df[order(df$salary_in_usd,decreasing = FALSE),]

dfjst <- top_n(dfjs, n=10, salary_in_usd)
dfjstt <- transform(dfjst,job_title = reorder(job_title,order(salary_in_usd, decreasing = FALSE)))

ggjs <- ggplot(dfjstt) + 
  geom_bar(aes(x = job_title, y = salary_in_usd, fill = experience_level), stat = 'identity', position = 'dodge') +
  geom_text(aes(x = job_title, y = salary_in_usd, label=salary_in_usd), position = position_dodge(width=0.6), vjust=-0.15)+
  coord_flip()  
labs(subtitle="Top ten high-paying jobs ", 
     y="salary", 
     x="Name", 
     title="Bar plot", 
     caption = "Source: Kaggle")

ggjs +  scale_y_continuous(labels = comma) # y axis label will be separated with a comma


# ********************************************************************
# salary by employment_type

gge <- ggplot(df, aes(x = employment_type,y= salary_in_usd, fill = employment_type)) +
  geom_boxplot()+
  labs(subtitle="which employment type is high-paid than others? ", 
       y="salary", 
       x="employment_type", 
       title="Box Plot", 
       caption = "Source: kaggle")
gge +  scale_y_continuous(labels = comma) # y axis label will be separated with a comma
# ********************************************************************

# salary by experience_level


ggel <- ggplot(df, aes(x = experience_level,y= salary_in_usd, fill = experience_level)) +
  geom_boxplot()+
  labs(subtitle="which experience level is high-paid than others? ", 
       y="salary", 
       x="experience_level", 
       title="Box Plot", 
       caption = "Source: kaggle")

ggel +  scale_y_continuous(labels = comma) # y axis label will be separated with a comma

# *********************************************************************

# salary by Company size

ggcs <- ggplot(df, aes(x = company_size,y= salary_in_usd, fill = company_size)) +
  geom_boxplot()+
  labs(subtitle="which size of the company is high-paid than others? ", 
       y="salary", 
       x="company_size", 
       title="Box Plot", 
       caption = "Source: kaggle")

ggcs + scale_y_continuous(labels = comma)# y axis label will be separated with a comma

# ***********************************************************************
# salary by remote_ratio


ggrr <- ggplot(df, aes(x = remote_ratio,y= salary_in_usd, fill = remote_ratio)) +
  geom_boxplot()+
  labs(subtitle="Does remotely working impact the salary? ", 
       y="salary", 
       x="remote_ratio", 
       title="Box Plot", 
       caption = "Source: kaggle")

ggrr + scale_y_continuous(labels = comma)# y axis label will be separated with a comma

# *************************************************************************
# employee_residence by salary


library(ggplot2)
library(dplyr)
library(scales)

dfre <- df[order(df$salary_in_usd,decreasing = TRUE),]

dfret <- top_n(dfre, n=100, salary_in_usd)
dfrett <- transform(dfret,employee_residence = reorder(employee_residence,order(salary_in_usd, decreasing = TRUE)))

ggjr <- ggplot(dfrett) + 
  geom_bar(aes(x = employee_residence, y = salary_in_usd), stat = 'identity',color = "#7FFF00" , fill = "#7CFC00" , position = 'dodge') +
  # coord_flip()  
  labs(subtitle="Top ten high-paying jobs ", 
       y="salary", 
       x="Name", 
       title="Bar plot", 
       caption = "Source: Kaggle")

ggjr +  scale_y_continuous(labels = comma)# y axis label will be separated with a comma

# ******************************************************************
# salary by work_year


ggsw <- ggplot(df, aes(x = work_year,y= salary_in_usd, fill = work_year)) +
  geom_boxplot()+
  labs(subtitle="is there any change in salary between 2020 and 2022? ", 
       y="salary", 
       x="remote_ratio", 
       title="Box Plot", 
       caption = "Source: kaggle")

ggsw + scale_y_continuous(labels = comma)# y axis label will be separated with a comma


# ****************************************************************************

# Company location and remote_ratio


ggplot(df, aes(x = company_location , fill = remote_ratio)) +
  geom_bar() +
  labs(subtitle="which country has the most remote working ratio?  ", 
       y="Number", 
       x="Company location", 
       title="Bar chart", 
       caption = "Source: Netflix")

# *********************************************************************
# employment_type and remote_ratio by salary
ggers<- ggplot(df, aes(x = employment_type,y= salary_in_usd, fill = remote_ratio)) +
  geom_boxplot()+
  labs(subtitle="Does employment type has a relationship with remotely working and the salary? ", 
       y="salary", 
       x="employment_type", 
       title="Box Plot", 
       caption = "Source: kaggle")

ggers + scale_y_continuous(labels = comma) # y axis label will be separated with a comma 

# ************************************************************************
# top Job title and remote_ratio 

ggplot(df, aes(x = job_title,fill = remote_ratio)) +
  geom_bar()+
  coord_flip() +
  labs(subtitle="which job has the most remote ratio ", 
       
       y="salary", 
       x="employment_type", 
       title="Box Plot", 
       caption = "Source: kaggle")

# ********************************************************************
# install.packages("wordcloud2")
library(wordcloud2 )

demoFreq <- df %>% 
  group_by(job_title)%>%
  summarize(name_rows= n())

dfmj <- df %>% 
  group_by(job_title)%>%
  summarize(name_rows= n())

wordcloud2(demoFreq$job_title , demoFreq$name_rows, size=1.6 , shape = 'star')

dfmjo <- dfmj[order(dfmj$name_rows,decreasing = TRUE),]
dfmjt <- top_n(dfmjo, n=10, name_rows)
dfmjtt <- transform(dfmjt,job_title = reorder(job_title,order(name_rows, decreasing = TRUE)))


wordcloud2(data= demoFreq, size=1.6 , shape = 'star')

# write.csv(df,"E:\\Mahdieh_CourseUniversity\\dt_salaries.csv")
