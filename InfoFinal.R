# Mumtaz, Cindy, Rayyan
# Info 201 Data wrangling assignment

library(dplyr)

# Datasets
school_attendance_2021_df <- read.csv("School_Attendance_by_Student_Group_and_District__2021-2022.csv")
diversity_report_df <- read.csv("2018-2019_Diversity_Report_-_Pre-Kindergarten__K-8___Grades_9-12_District__Schools__Special_Programs__Diversity_Efforts__Admissions_Methods.csv")


# Numeric column
school_attendance_2021_df$MultipleAbsences <- NA

for (i in 1:nrow(school_attendance_2021_df)) {
  if (school_attendance_2021_df$X2021.2022.attendance.rate...year.to.date[i] < 90) {
    school_attendance_2021_df$MultipleAbsences[i] <- 1
  } else {
    school_attendance_2021_df$MultipleAbsences[i] <- 0
  }
}

# Categorical column
diversity_report_df <- mutate(diversity_report_df,
                              GenderRatio = X.Male / X.Female)

#Merged code
merged_data <- full_join(
  school_attendance_2021_df,
  diversity_report_df,
  by = "Category",
)

# Summarize column, this summarizes the average attendance by district and category.
average_attendance <- summarize(
  group_by(merged_data, District.code, Category),
  average = mean(X2021.2022.attendance.rate...year.to.date, na.rm = TRUE),
)

# This attaches the average column to the merged data file
# it is the last column in the dataset
merged_data <- merge(
  merged_data,
  average_attendance,
  by = c("District.code", "Category"),
  all.x = TRUE,
  suffixes = c("", "_avg")
)

