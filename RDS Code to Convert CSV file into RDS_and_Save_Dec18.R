

#Export to a CSV file


attack_fre_countPMI <- read.csv("C:/VC_Indicator/1_VC_GLOBAL_Leadership_Review/4_Ento_ResEfficacy_Tracker_QterlyReview/attack_fre_countPMI.csv")


write.csv(attack_fre_countPMI,"C:/VC_Indicator/1_VC_GLOBAL_Leadership_Review/4_Ento_ResEfficacy_Tracker_QterlyReview/attack_fre_countPMI.csv")


#Export to a Rds file

saveRDS(attack_fre_countPMI, "C:/VC_Indicator/1_VC_GLOBAL_Leadership_Review/4_Ento_ResEfficacy_Tracker_QterlyReview/attack_fre_countPMI.rds")
