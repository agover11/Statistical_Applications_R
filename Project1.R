optimum_base = read.csv("optimum_base.csv", header=TRUE)

optimum_base$PAIN_CAT <- ifelse(optimum_base$PAINDUR_Y < 5, 1,
                                ifelse(optimum_base$PAINDUR_Y <= 15, 2, 3))

optimum_base$PEG_HIGH <- ifelse(optimum_base$PEG < 6, 0, 1)


optimum_base$COLLEGE_ANY <- ifelse(is.na(optimum_base$EDLEVEL), NA,
                                   ifelse(optimum_base$EDLEVEL <= 3, 0, 1))

optimum_base <- subset(optimum_base, STATE == "MA")

optimum_base$PAIN_CAT <- factor(optimum_base$PAIN_CAT, levels = c(1, 2, 3), 
                                labels = c("Less than 5 years of back pain", "Between 5 and 15 years of back pain", "More than 15 years of back pain")) 


optimum_base$EMPLOYMENTSTAT <- factor(optimum_base$EMPLOYMENTSTAT, levels = c(1, 2, 3), 
                                labels = c("Full-time", "Not employed", "Part-time")) 


optimum_base$PEG_HIGH <- factor(optimum_base$PEG_HIGH, levels = c(0, 1), 
                                      labels = c("No", "Yes")) 
optimum_base$COLLEGE_ANY <- factor(optimum_base$COLLEGE_ANY, levels = c(0, 1), 
                                labels = c("No", "Yes")) 
optimum_base$MED_OPIOID_YN <- factor(optimum_base$MED_OPIOID_YN, levels = c(0, 1), 
                                   labels = c("No", "Yes")) 



optimum_base$OVERALL_SVI_QUARTILE <- factor(optimum_base$OVERALL_SVI_QUARTILE, levels = c(1, 2, 3, 4), 
                                     labels = c("1", "2", "3", "4")) 

str(optimum_base[, c("PAIN_CAT", "EMPLOYMENTSTAT", "PEG_HIGH", "COLLEGE_ANY", "MED_OPIOID_YN", "OVERALL_SVI_QUARTILE" )])

#Part 4, Question 1
PEG_table = table(optimum_base$PEG_HIGH)
PEG_table

round(tapply(optimum_base$AGEVAL, optimum_base$PEG_HIGH, mean, na.rm = TRUE),2)
round(tapply(optimum_base$AGEVAL, optimum_base$PEG_HIGH, sd, na.rm = TRUE),2)
t.test(AGEVAL ~ PEG_HIGH, data = optimum_base, var.equal = TRUE)

college = table(optimum_base$COLLEGE_ANY, optimum_base$PEG_HIGH)
college
round(prop.table(college, 1) * 100,2)
chisq.test(college, correct = FALSE)$expected
chisq.test(college, correct = FALSE)

pain = table(optimum_base$PAIN_CAT, optimum_base$PEG_HIGH)
pain
round(prop.table(pain, 1) * 100,2)
chisq.test(pain, correct = FALSE)$expected
chisq.test(pain, correct = FALSE)

employed = table(optimum_base$EMPLOYMENTSTAT, optimum_base$PEG_HIGH)
employed
round(prop.table(employed, 1) * 100,2)
chisq.test(employed, correct = FALSE)$expected
chisq.test(employed, correct = FALSE)

opiod = table(optimum_base$MED_OPIOID_YN, optimum_base$PEG_HIGH)
opiod
round(prop.table(opiod, 1) * 100,2)
chisq.test(opiod, correct = FALSE)$expected
chisq.test(opiod, correct = FALSE)


SVI = table(optimum_base$OVERALL_SVI_QUARTILE, optimum_base$PEG_HIGH)
SVI
round(prop.table(SVI, 1) * 100,2)
chisq.test(SVI, correct = FALSE)$expected
fisher.test(SVI)

#Part 4, Question 2
round(prop.table(PEG_table)*100,2)
var.test(AGEVAL ~ PEG_HIGH, data = optimum_base)
t.test(AGEVAL ~ PEG_HIGH, data = optimum_base, var.equal = TRUE)


#Part 4, question 3
chisq.test(SVI, correct = FALSE)$expected
fisher.test(SVI)
